namespace fsharp2.Tests

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading
open Expecto
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.AstTraversal

/// Writes the vendored XParsec tree in the FCS normal form of the Issue #29 probe kit
/// (`fnorm.fsx`, FCS 43.10.101), and classifies a probe against the FCS tree and the
/// Compatibility Oracle diagnostics as the kit's `cls.py` does.
module XParsecComparison =

    type Outcome = {
        Tree: string
        Diagnostics: string list
        Crash: string option
    }

    type Classification =
        | Exact
        | ExactUncertain
        | Uncomparable
        | WrongTree
        | Explicit
        | ExplicitOnAccepted
        | ExplicitElsewhere
        | Missing
        | Invented
        | Partial
        | Crash

    let classificationName classification =
        match classification with
        | Exact -> "EXACT"
        | ExactUncertain -> "EXACT?"
        | Uncomparable -> "UNCMP"
        | WrongTree -> "WRONGTREE"
        | Explicit -> "EXPLICIT"
        | ExplicitOnAccepted -> "EXPLICIT+acc"
        | ExplicitElsewhere -> "EXPLICIT@other"
        | Missing -> "MISSING"
        | Invented -> "INVENTED"
        | Partial -> "PARTIAL"
        | Crash -> "EXC"

    type private Source(lexed: Lexed) =
        let text = lexed.Input

        let lineStarts = [|
            yield 0

            for index in
                0 .. text.Length
                     - 1 do
                if text[index] = '\n' then
                    yield index + 1
        |]

        member _.Position(offset: int) =
            let line =
                match Array.BinarySearch(lineStarts, offset) with
                | found when found >= 0 -> found
                | insert ->
                    ~~~insert
                    - 1

            line + 1,
            offset
            - lineStarts[line]
            + 1

        member this.Range(startOffset: int, endOffset: int) =
            let startLine, startColumn = this.Position startOffset
            let endLine, endColumn = this.Position endOffset
            $"{startLine}:{startColumn}-{endLine}:{endColumn}"

        member _.Text(startOffset: int, endOffset: int) =
            text
                .Substring(
                    startOffset,
                    endOffset
                    - startOffset
                )
                .Replace("\n", "\\n")
                .Replace("\r", "")

        member _.TokenEnd(index: int<token>) =
            lexed.Tokens[index
                         + 1<token>]
                .StartIndex

        member _.TokenText(token: SyntaxToken) =
            match token.Index with
            | TokenIndex.Regular index -> lexed.GetTokenString index
            | TokenIndex.Virtual -> ""

    [<Struct>]
    type private Span = { Start: int; End: int }

    let private spanOf (source: Source) (walk: AstVisitor<SyntaxToken> -> unit) =
        let mutable first = Int32.MaxValue
        let mutable last = -1

        let visitor: AstVisitor<SyntaxToken> = {
            VisitToken =
                fun _ token ->
                    match token.Index with
                    | TokenIndex.Regular index ->
                        first <- min first token.StartIndex
                        last <- max last (source.TokenEnd index)
                    | TokenIndex.Virtual -> ()
            EnterSection = ignore
            ExitSection = ignore
            WriteLine = ignore
            EqualTokens = fun left right -> left.Index = right.Index
        }

        walk visitor

        if last < 0 then
            ValueNone
        else
            ValueSome { Start = first; End = last }

    let private tokenSpan (source: Source) (token: SyntaxToken) =
        match token.Index with
        | TokenIndex.Regular index ->
            ValueSome {
                Start = token.StartIndex
                End = source.TokenEnd index
            }
        | TokenIndex.Virtual -> ValueNone

    let private join (left: Span voption) (right: Span voption) =
        match left, right with
        | ValueSome l, ValueSome r ->
            ValueSome {
                Start = min l.Start r.Start
                End = max l.End r.End
            }
        | ValueSome only, ValueNone
        | ValueNone, ValueSome only -> ValueSome only
        | ValueNone, ValueNone -> ValueNone

    let private identText (text: string) =
        if
            text.StartsWith("``")
            && text.EndsWith("``")
            && text.Length
               >= 4
        then
            text.Substring(
                2,
                text.Length
                - 4
            )
        else
            text

    type private Writer(source: Source) =

        member _.Range(span: Span voption) =
            match span with
            | ValueSome span -> source.Range(span.Start, span.End)
            | ValueNone -> "?"

        member _.Text(span: Span voption) =
            match span with
            | ValueSome span -> source.Text(span.Start, span.End)
            | ValueNone -> "?"

        // AstTraversal does not visit the delimiters of struct tuples and type applications.
        member this.ExprSpan(expr: Expr<SyntaxToken>) =
            match expr with
            | Expr.StructTuple(structToken, _, _, _, rParen) ->
                join (tokenSpan source structToken) (tokenSpan source rParen)
            | Expr.TypeApp(inner, _, _, _, rAngle) ->
                join (this.ExprSpan inner) (tokenSpan source rAngle)
            | Expr.App(func, args) when args.Length > 0 ->
                join
                    (this.ExprSpan func)
                    (this.ExprSpan
                        args[args.Length
                             - 1])
            | Expr.InfixApp(left, _, right) -> join (this.ExprSpan left) (this.ExprSpan right)
            | Expr.Tuple(items, _)
            | Expr.Sequential(items, _) when items.Length > 0 ->
                join
                    (this.ExprSpan items[0])
                    (this.ExprSpan
                        items[items.Length
                              - 1])
            | _ -> spanOf source (fun visitor -> walkExpr visitor expr)

        member _.PatSpan(pat: Pat<SyntaxToken>) =
            match pat with
            | Pat.StructTuple(structToken, _, _, _, rParen) ->
                join (tokenSpan source structToken) (tokenSpan source rParen)
            | _ -> spanOf source (fun visitor -> walkPat visitor pat)

        member _.TypeSpan(typ: Type<SyntaxToken>) =
            match typ with
            | Type.StructTupleType(structToken, _, _, _, rParen) ->
                join (tokenSpan source structToken) (tokenSpan source rParen)
            | _ -> spanOf source (fun visitor -> walkType visitor typ)

        member _.TokenSpan(token: SyntaxToken) = tokenSpan source token

        member _.TokenText(token: SyntaxToken) = source.TokenText token

        member this.LongIdentText(longIdent: LongIdent<SyntaxToken>) =
            longIdent.Idents
            |> Seq.map (fun ident -> identText (this.TokenText ident))
            |> String.concat "."

        member this.Typed (name: string) (expr: Expr<SyntaxToken>) (typ: Type<SyntaxToken>) =
            let typeSpan = this.TypeSpan typ

            $"{name}({this.Expr expr},{this.Text typeSpan}@{this.Range typeSpan})@{this.Range(
                                                                                       this.ExprSpan expr
                                                                                       |> join typeSpan
                                                                                   )}"

        member this.Items(expr: Expr<SyntaxToken>) =
            match expr with
            | Expr.Sequential(items, _) ->
                items
                |> Seq.map this.Expr
                |> String.concat ","
            | other -> this.Expr other

        member this.Clauses(Rules(_, rules, _)) =
            rules
            |> Seq.map (fun rule ->
                match rule with
                | Rule.Rule(pat, guard, _, result) ->
                    let patSpan = this.PatSpan pat

                    let guardText =
                        match guard with
                        | ValueSome(PatternGuard(_, condition)) ->
                            "when "
                            + this.Expr condition
                            + " "
                        | ValueNone -> ""

                    $"{this.Text patSpan}@{this.Range patSpan} "
                    + guardText
                    + this.Expr result
                | Rule.Missing
                | Rule.SkipsTokens _ -> "MISSING"
            )
            |> String.concat ";"

        member this.Binding(binding: Binding<SyntaxToken>) = this.Expr binding.expr

        member this.Conditional
            (start: Span voption)
            (condition: Expr<SyntaxToken>)
            (thenExpr: Expr<SyntaxToken>)
            (rest: ElifBranch<SyntaxToken> list)
            (elseBranch: ElseBranch<SyntaxToken> voption)
            =
            let elseText, elseSpan =
                match rest, elseBranch with
                | ElifBranch.Elif(elifToken, nextCondition, _, nextThen) :: more, _ ->
                    let text, span =
                        this.Conditional
                            (this.TokenSpan elifToken)
                            nextCondition
                            nextThen
                            more
                            elseBranch

                    text, span
                | ElifBranch.ElseIf(_, ifToken, nextCondition, _, nextThen) :: more, _ ->
                    this.Conditional (this.TokenSpan ifToken) nextCondition nextThen more elseBranch
                | [], ValueSome(ElseBranch(_, elseExpr)) ->
                    this.Expr elseExpr, this.ExprSpan elseExpr
                | [], ValueNone -> "-", ValueNone

            let span =
                start
                |> join (this.ExprSpan thenExpr)
                |> join elseSpan

            $"If({this.Expr condition},{this.Expr thenExpr},{elseText})@{this.Range span}", span

        member this.Expr(expr: Expr<SyntaxToken>) : string =
            let span () = this.ExprSpan expr
            let range () = this.Range(span ())

            match expr with
            | Expr.Const _ -> $"C[{this.Text(span ())}]@{range ()}"
            | Expr.String(kind, _, _) ->
                match kind with
                | StringKind.String _
                | StringKind.VerbatimString _
                | StringKind.String3 _ -> $"C[{this.Text(span ())}]@{range ()}"
                | _ -> $"?InterpolatedString@{range ()}"
            | Expr.Ident ident -> $"I[{identText (this.TokenText ident)}]@{range ()}"
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdent) ->
                $"I[{this.LongIdentText longIdent}]@{range ()}"
            | Expr.LongIdentOrOp _ -> $"?Op@{range ()}"
            | Expr.EnclosedBlock(kind, inner, _) ->
                match kind with
                | ParenKind.Paren _
                | ParenKind.BeginEnd _ -> $"P({this.Expr inner})@{range ()}"
                | ParenKind.List _ -> $"L({this.Items inner})@{range ()}"
                | ParenKind.Array _ -> $"AR({this.Items inner})@{range ()}"
                | ParenKind.Brace _ -> $"?ComputationExpr@{range ()}"
                | ParenKind.BraceBar _ -> $"?AnonRecd@{range ()}"
                | ParenKind.Quoted _
                | ParenKind.DoubleQuoted _ -> $"?Quote@{range ()}"
            | Expr.EmptyBlock(kind, _) ->
                match kind with
                | ParenKind.Paren _
                | ParenKind.BeginEnd _ -> $"C[{this.Text(span ())}]@{range ()}"
                | ParenKind.List _ -> $"L()@{range ()}"
                | ParenKind.Array _ -> $"AR()@{range ()}"
                | ParenKind.Brace _ -> $"?ComputationExpr@{range ()}"
                | ParenKind.BraceBar _ -> $"?AnonRecd@{range ()}"
                | ParenKind.Quoted _
                | ParenKind.DoubleQuoted _ -> $"?Quote@{range ()}"
            | Expr.DotLookup(target, _, member') ->
                match target, member' with
                | Expr.Wildcard _, _ -> $"?DotLambda@{range ()}"
                | Expr.Ident ident, LongIdentOrOp.LongIdent longIdent ->
                    $"I[{identText (this.TokenText ident)}.{this.LongIdentText longIdent}]@{range ()}"
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent head),
                  LongIdentOrOp.LongIdent longIdent ->
                    $"I[{this.LongIdentText head}.{this.LongIdentText longIdent}]@{range ()}"
                | _, LongIdentOrOp.LongIdent longIdent ->
                    let rec chain (inner: Expr<SyntaxToken>) (names: string list) =
                        match inner with
                        | Expr.DotLookup(deeper, _, LongIdentOrOp.LongIdent more) when
                            (match deeper with
                             | Expr.Wildcard _
                             | Expr.Ident _
                             | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) -> false
                             | _ -> true)
                            ->
                            chain
                                deeper
                                (this.LongIdentText more
                                 :: names)
                        | _ -> inner, names

                    let baseExpr, names = chain target [ this.LongIdentText longIdent ]
                    let path = String.concat "." names
                    $"DG({this.Expr baseExpr},{path})@{range ()}"
                | _ -> $"?DotGet@{range ()}"
            | Expr.App(func, args) ->
                let mutable text = this.Expr func
                let mutable applied = this.ExprSpan func

                for arg in args do
                    applied <- join applied (this.ExprSpan arg)
                    text <- $"A({text},{this.Expr arg})@{this.Range applied}"

                text
            | Expr.HighPrecedenceApp(func, lParen, arg, rParen) ->
                let parenSpan = join (this.TokenSpan lParen) (this.TokenSpan rParen)

                let argText =
                    match arg with
                    | _ when
                        lParen.Token
                        <> Token.KWLParen
                        ->
                        this.Expr arg
                    | Expr.Missing
                    | Expr.EmptyBlock _ -> $"C[{this.Text parenSpan}]@{this.Range parenSpan}"
                    | _ -> $"P({this.Expr arg})@{this.Range parenSpan}"

                $"A({this.Expr func},{argText})@{range ()}"
            | Expr.TypeApp _ -> $"?TypeApp@{range ()}"
            | Expr.InfixApp(left, op, right) ->
                $"X[{this.TokenText op}]({this.Expr left},{this.Expr right})@{range ()}"
            | Expr.PrefixApp(op, operand) ->
                match op.Token with
                | Token.KWLazy -> $"?Lazy@{range ()}"
                | Token.KWAssert -> $"?Assert@{range ()}"
                | Token.KWUpcast -> $"?InferredUpcast@{range ()}"
                | Token.KWDowncast -> $"?InferredDowncast@{range ()}"
                | _ ->
                    match this.TokenText op with
                    | "&"
                    | "&&" -> $"?AddressOf@{range ()}"
                    | "+" when
                        (match operand, this.TokenSpan op, this.ExprSpan operand with
                         | Expr.Const(Constant.Literal _), ValueSome opSpan, ValueSome operandSpan ->
                             opSpan.End = operandSpan.Start
                         | _ -> false)
                        ->
                        $"C[{this.Text(span ())}]@{range ()}"
                    | text -> $"U[{text}]({this.Expr operand})@{range ()}"
            | Expr.Tuple(items, _) ->
                let items =
                    items
                    |> Seq.map this.Expr
                    |> String.concat ","

                $"T({items})@{range ()}"
            | Expr.StructTuple(_, _, items, _, _) ->
                let items =
                    items
                    |> Seq.map this.Expr
                    |> String.concat ","

                $"T({items})@{range ()}"
            | Expr.Assignment(target, op, value) when this.TokenText op = ":=" ->
                $"X[:=]({this.Expr target},{this.Expr value})@{range ()}"
            | Expr.Assignment(target, _, value) ->
                match target with
                | Expr.Ident ident ->
                    $"Set[{identText (this.TokenText ident)}]({this.Expr value})@{range ()}"
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdent) ->
                    $"Set[{this.LongIdentText longIdent}]({this.Expr value})@{range ()}"
                | Expr.DotLookup(Expr.Ident ident, _, LongIdentOrOp.LongIdent longIdent) ->
                    $"Set[{identText (this.TokenText ident)}.{this.LongIdentText longIdent}]({this.Expr value})@{range ()}"
                | Expr.IndexedLookup _ -> $"?DotIndexedSet@{range ()}"
                | Expr.DotLookup _ -> $"?DotSet@{range ()}"
                | _ -> $"?Set@{range ()}"
            | Expr.New _ -> $"?New@{range ()}"
            | Expr.Object _ -> $"?ObjExpr@{range ()}"
            | Expr.Record(ParenKind.BraceBar _, _, _, _)
            | Expr.RecordClone(ParenKind.BraceBar _, _, _, _, _, _) -> $"?AnonRecd@{range ()}"
            | Expr.Record(_, fields, _, _)
            | Expr.RecordClone(_, _, _, fields, _, _) ->
                let fields =
                    fields
                    |> Seq.map (fun (FieldInitializer(_, _, value)) -> this.Expr value)
                    |> String.concat ","

                $"R({fields})@{range ()}"
            | Expr.ControlFlow(keyword, inner) ->
                match keyword with
                | ControlFlowKeyword.Yield _
                | ControlFlowKeyword.Return _ -> $"?YieldOrReturn@{range ()}"
                | ControlFlowKeyword.YieldBang _
                | ControlFlowKeyword.ReturnBang _ -> $"?YieldOrReturnFrom@{range ()}"
                | ControlFlowKeyword.Do _ -> this.Expr inner
                | ControlFlowKeyword.DoBang _ -> $"?DoBang@{range ()}"
            | Expr.Null _ -> $"?Null@{range ()}"
            | Expr.TypeAnnotation(inner, _, typ) -> this.Typed "TY" inner typ
            | Expr.StaticUpcast(inner, _, typ) -> this.Typed "UC" inner typ
            | Expr.DynamicTypeTest(inner, _, typ) -> this.Typed "TT" inner typ
            | Expr.DynamicDowncast(inner, _, typ) -> this.Typed "DC" inner typ
            | Expr.LetOrUse(keyword, _, bindings, _, _, body) ->
                match keyword, body with
                | (LetOrUseKeyword.Let _ | LetOrUseKeyword.Use _), ValueSome body ->
                    let bindings =
                        bindings
                        |> Seq.map this.Binding
                        |> String.concat ","

                    $"Let({bindings};{this.Expr body})@{range ()}"
                | _ -> $"?LetOrUseBang@{range ()}"
            | Expr.Fun(_, _, _, body) -> $"Fun({this.Expr body})@{range ()}"
            | Expr.Function _ -> $"?MatchLambda@{range ()}"
            | Expr.Sequential(items, _) ->
                let rec nest index =
                    if
                        index = items.Length
                                - 1
                    then
                        this.Expr items[index], this.ExprSpan items[index]
                    else
                        let restText, restSpan = nest (index + 1)
                        let span = join (this.ExprSpan items[index]) restSpan
                        $"S({this.Expr items[index]},{restText})@{this.Range span}", span

                fst (nest 0)
            | Expr.Match(_, input, _, rules) ->
                $"M({this.Expr input},[{this.Clauses rules}])@{range ()}"
            | Expr.TryWith(_, body, _, rules) ->
                $"TryW({this.Expr body},[{this.Clauses rules}])@{range ()}"
            | Expr.TryFinally(_, body, _, finallyExpr) ->
                $"TryF({this.Expr body},{this.Expr finallyExpr})@{range ()}"
            | Expr.IfThenElse(ifToken, condition, _, thenExpr, elifBranches, elseBranch) ->
                fst (
                    this.Conditional
                        (this.TokenSpan ifToken)
                        condition
                        thenExpr
                        (List.ofSeq elifBranches)
                        elseBranch
                )
            | Expr.While(_, condition, _, body, _) ->
                $"While({this.Expr condition},{this.Expr body})@{range ()}"
            | Expr.ForTo(_, ident, _, startExpr, toToken, endExpr, _, body, _) ->
                let direction = if this.TokenText toToken = "downto" then "downto" else "to"

                $"For({identText (this.TokenText ident)},{this.Expr startExpr},{direction},{this.Expr endExpr},{this.Expr body})@{range ()}"
            | Expr.ForIn(_, pat, _, enumerable, _, body, _) ->
                let patSpan = this.PatSpan pat

                $"ForIn({this.Text patSpan}@{this.Range patSpan},{this.Expr enumerable},{this.Expr body})@{range ()}"
            | Expr.IndexedLookup(target, dot, _, index, _) ->
                match dot with
                | ValueSome _ -> $"DI({this.Expr target},{this.Expr index})@{range ()}"
                | ValueNone -> $"B({this.Expr target},{this.Expr index})@{range ()}"
            | Expr.Range(fromExpr, _, toExpr)
            | Expr.SliceFromTo(fromExpr, _, toExpr) ->
                $"RG({this.Expr fromExpr},{this.Expr toExpr})@{range ()}"
            | Expr.SteppedRange(fromExpr, _, stepExpr, _, toExpr) ->
                let stepped = join (this.ExprSpan fromExpr) (this.ExprSpan stepExpr)

                $"RG(RG({this.Expr fromExpr},{this.Expr stepExpr})@{this.Range stepped},{this.Expr toExpr})@{range ()}"
            | Expr.SliceFrom(fromExpr, _) -> $"RG({this.Expr fromExpr},-)@{range ()}"
            | Expr.SliceTo(_, toExpr) -> $"RG(-,{this.Expr toExpr})@{range ()}"
            | Expr.SliceAll _ -> $"RG(-,-)@{range ()}"
            | Expr.DynamicLookup _ -> $"?Dynamic@{range ()}"
            | Expr.Missing
            | Expr.SkipsTokens _ -> "MISSING"
            | Expr.OptionalArgExpr _
            | Expr.StaticMemberInvocation _
            | Expr.LibraryOnlyStaticOptimization _
            | Expr.ILIntrinsic _
            | Expr.Wildcard _
            | Expr.Pat _ -> $"?Other@{range ()}"

        member this.Members(elements: TypeDefnElement<SyntaxToken> seq) =
            elements
            |> Seq.choose (fun element ->
                match element with
                | TypeDefnElement.Member(MemberDefn.Member(
                    defn = MethodOrPropDefn.Method(defn = binding)))
                | TypeDefnElement.Member(MemberDefn.Member(
                    defn = MethodOrPropDefn.Property(defn = binding))) ->
                    Some(
                        "member = "
                        + this.Binding binding
                    )
                | _ -> None
            )
            |> List.ofSeq

        member this.TypeMembers(defn: TypeDefn<SyntaxToken>) =
            let extensions (value: TypeExtensionElements<SyntaxToken> voption) =
                match value with
                | ValueSome(TypeExtensionElements(_, elements, _)) -> this.Members elements
                | ValueNone -> []

            match defn with
            | TypeDefn.Anon(body = body)
            | TypeDefn.Class(body = body)
            | TypeDefn.Struct(body = body)
            | TypeDefn.Interface(body = body) -> this.Members body.elements
            | TypeDefn.Abbrev(extensions = more)
            | TypeDefn.Record(extensions = more)
            | TypeDefn.Union(extensions = more) -> extensions more
            | TypeDefn.TypeExtension(elements = TypeExtensionElements(_, elements, _)) ->
                this.Members elements
            | _ -> []

        member this.Declaration(element: ModuleElem<SyntaxToken>) =
            match element with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                bindings
                |> Seq.map (fun binding ->
                    let headSpan =
                        binding.argumentPats
                        |> Seq.fold
                            (fun span pat -> join span (this.PatSpan pat))
                            (this.PatSpan binding.pattern)

                    $"let {this.Text headSpan} = {this.Binding binding}"
                )
                |> String.concat " and "
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Do(expr = expr))
            | ModuleElem.Expression expr ->
                "do "
                + this.Expr expr
            | ModuleElem.Type defns ->
                defns
                |> Seq.collect this.TypeMembers
                |> String.concat " ;; "
            | ModuleElem.Module _ -> "?NestedModule"
            | ModuleElem.Import _ -> "?Open"
            | ModuleElem.ModuleAbbrev _ -> "?ModuleAbbrev"
            | ModuleElem.Exception _ -> "?Exception"
            | ModuleElem.CompilerDirective _ -> "?HashDirective"
            | _ -> "?Other"

        /// FCS keeps a top-level `a; b` as one declaration for each expression, and a
        /// module-level `let ... in` as a `do` of a `LetOrUse` over the next expression.
        member this.Declarations(elements: ModuleElem<SyntaxToken> list) =
            match elements with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(
                letToken = letToken; bindings = bindings; inToken = ValueSome _)) :: ModuleElem.Expression body :: rest ->
                let bindings =
                    bindings
                    |> Seq.map this.Binding
                    |> String.concat ","

                let span = join (this.TokenSpan letToken) (this.ExprSpan body)

                $"do Let({bindings};{this.Expr body})@{this.Range span}"
                :: this.Declarations rest
            | ModuleElem.Expression(Expr.Sequential(items, _)) :: rest ->
                (items
                 |> Seq.map (fun item ->
                     "do "
                     + this.Expr item
                 )
                 |> List.ofSeq)
                @ this.Declarations rest
            | element :: rest ->
                this.Declaration element
                :: this.Declarations rest
            | [] -> []

    let private declarations (file: ImplementationFile<SyntaxToken>) =
        let groupElements (group: NamespaceDeclGroup<SyntaxToken>) =
            match group with
            | NamespaceDeclGroup.Named(elements = elements) -> List.ofSeq elements
            | NamespaceDeclGroup.Global(elements = elements) -> List.ofSeq elements

        match file with
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elements)) ->
            List.ofSeq elements
        | ImplementationFile.AnonymousModule elements -> List.ofSeq elements
        | ImplementationFile.Namespaces groups ->
            groups
            |> Seq.collect groupElements
            |> List.ofSeq

    /// Every vendored diagnostic is at least an explicit rejection. FSharp2 has no Oracle code or
    /// message for the vendored recovery codes, so each one maps to `FSC2P1001`.
    let private oracleCode (code: DiagnosticCode) =
        match DiagnosticCode.fsharp2Code code with
        | ValueSome fsharp2Code -> fsharp2Code
        | ValueNone -> "FSC2P1001"

    let normalForm (text: string) : Outcome =
        let lexed = Lexing.lexString text
        let source = Source(lexed)
        let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

        try
            let result = FSharpAst.parse reader
            let writer = Writer(source)

            let diagnostics =
                reader.State.Diagnostics
                |> List.rev
                |> List.map (fun diagnostic ->
                    let line, column = source.Position diagnostic.Token.StartIndex
                    $"({line},{column}) {oracleCode diagnostic.Code}"
                )

            let tree =
                match result with
                | Ok(FSharpAst.ImplementationFile file) ->
                    declarations file
                    |> writer.Declarations
                    |> String.concat " ;; "
                | Ok _ -> "?notImplementation"
                | Error _ -> ""

            let diagnostics =
                match result, diagnostics with
                | Error _, [] -> [ "(1,1) FSC2P1001" ]
                | _ -> diagnostics

            {
                Tree = tree
                Diagnostics = diagnostics
                Crash = None
            }
        with failure -> {
            Tree = ""
            Diagnostics = []
            Crash =
                Some(
                    failure.GetType().Name
                    + ": "
                    + failure.Message.Split('\n')[0]
                )
        }

    let private positionKey (diagnostic: string) =
        let close = diagnostic.IndexOf(')')
        let parts = diagnostic.Substring(1, close - 1).Split(',')
        int parts[0], int parts[1]

    let private firstPosition (diagnostics: string seq) =
        diagnostics
        |> Seq.minBy positionKey
        |> fun diagnostic ->
            diagnostic.Substring(
                0,
                diagnostic.IndexOf(')')
                + 1
            )

    let classify (oracle: string list) (fcsTree: string option) (outcome: Outcome) =
        match outcome.Crash with
        | Some _ -> Crash
        | None ->
            let oracleSet = set oracle
            let parserSet = set outcome.Diagnostics

            if parserSet = oracleSet then
                if oracleSet.IsEmpty then
                    let expected = defaultArg fcsTree ""

                    if
                        outcome.Tree.Contains('?')
                        || expected.Contains('?')
                        || expected.Contains("MISSING")
                    then
                        if outcome.Tree.Replace("?", "") = expected.Replace("?", "") then
                            ExactUncertain
                        else
                            Uncomparable
                    elif outcome.Tree = expected then
                        Exact
                    else
                        WrongTree
                else
                    Exact
            else
                let explicit =
                    parserSet
                    |> Set.filter _.EndsWith("FSC2P1001")

                if not explicit.IsEmpty then
                    if oracleSet.IsEmpty then
                        ExplicitOnAccepted
                    elif
                        firstPosition explicit = firstPosition oracleSet
                        || firstPosition parserSet = firstPosition oracleSet
                    then
                        Explicit
                    else
                        ExplicitElsewhere
                elif parserSet.IsEmpty then
                    Missing
                elif not (Set.difference parserSet oracleSet).IsEmpty then
                    Invented
                else
                    Partial

    let private kitDiagnostics (text: string) =
        Regex.Matches(text, @"\(\d+,\d+\) [A-Z0-9]+")
        |> Seq.map _.Value
        |> List.ofSeq

    let private kitColumns (line: string) =
        let parts = line.Split('\t')
        let columns = ResizeArray<string * string>()

        for part in parts[1..] do
            match Regex.Match(part, "^([A-Z]+):") with
            | found when found.Success ->
                columns.Add(found.Groups[1].Value, part.Substring(found.Length))
            | _ when columns.Count > 0 ->
                let key, value =
                    columns[columns.Count
                            - 1]

                columns[columns.Count
                        - 1] <-
                    (key,
                     value
                     + "\t"
                     + part)
            | _ -> ()

        parts[0], dict columns

    let private kitRows (path: string) =
        File.ReadLines path
        |> Seq.filter (fun line -> line <> "")
        |> Seq.map kitColumns
        |> List.ofSeq

    let private compareSet (input: string) (output: string) (oracleFile: string) =
        let setPath =
            oracleFile.Substring(
                0,
                oracleFile.Length
                - "-oracle.tsv".Length
            )

        let sources =
            Directory.EnumerateFiles(setPath, "*.fs", SearchOption.AllDirectories)
            |> Seq.map (fun path -> Path.GetFileName path, path)
            |> dict

        let fcsTrees =
            let fcsFile =
                setPath
                + "-fcs.tsv"

            if File.Exists fcsFile then
                kitRows fcsFile
                |> List.map (fun (name, columns) -> name, columns["T"])
                |> dict
                |> Some
            else
                None

        let outputPrefix = Path.Combine(output, Path.GetRelativePath(input, setPath))

        Directory.CreateDirectory(Path.GetDirectoryName outputPrefix)
        |> ignore

        let sideRows = ResizeArray<string>()
        let classRows = ResizeArray<string>()

        for name, columns in kitRows oracleFile do
            let source = File.ReadAllText(sources[name]).Replace("\r\n", "\n")
            let outcome = normalForm source

            let fcsTree =
                fcsTrees
                |> Option.bind (fun trees ->
                    match trees.TryGetValue name with
                    | true, tree -> Some tree
                    | false, _ -> None
                )

            let classification =
                match fcsTrees, kitDiagnostics columns["O"] with
                | None, [] when
                    outcome.Diagnostics.IsEmpty
                    && outcome.Crash.IsNone
                    ->
                    Uncomparable
                | _, oracle -> classify oracle fcsTree outcome

            let diagnostics = String.concat " " outcome.Diagnostics
            let crash = defaultArg outcome.Crash ""
            sideRows.Add($"{name}\tL:\tP:{diagnostics}\tT:{outcome.Tree}\tM:{crash}")
            classRows.Add($"{name}\t{classificationName classification}")

        File.WriteAllLines(
            outputPrefix
            + "-X.tsv",
            sideRows
        )

        File.WriteAllLines(
            outputPrefix
            + "-X-cls.tsv",
            classRows
        )

    let private compareDirectory (input: string) (output: string) =
        let work () =
            for oracleFile in
                Directory.EnumerateFiles(input, "*-oracle.tsv", SearchOption.AllDirectories)
                |> Seq.sort do
                compareSet input output oracleFile

        let mutable failure = None

        let thread =
            Thread(
                (fun () ->
                    try
                        work ()
                    with error ->
                        failure <- Some error
                ),
                256
                * 1024
                * 1024
            )

        thread.Start()
        thread.Join()

        match failure with
        | Some error -> raise error
        | None -> ()

    // FCS 43.10.101 normal forms from the slice 42 Oracle evidence. The Compatibility Oracle reports no diagnostic.
    let private fcsTreeCases = [
        "let f () =\n    a; b\n    z\n",
        "let f () = S(I[a]@2:5-2:6,S(I[b]@2:8-2:9,I[z]@3:5-3:6)@2:8-3:6)@2:5-3:6"
        "let y = [|1|].[0]\n", "let y = DI(AR(C[1]@1:11-1:12)@1:9-1:14,C[0]@1:16-1:17)@1:9-1:18"
        "let y = [ a..b ]\n", "let y = L(RG(I[a]@1:11-1:12,I[b]@1:14-1:15)@1:11-1:15)@1:9-1:17"
        "for x in 1 .. 10 do f x\n",
        "do ForIn(x@1:5-1:6,RG(C[1]@1:10-1:11,C[10]@1:15-1:17)@1:10-1:17,A(I[f]@1:21-1:22,I[x]@1:23-1:24)@1:21-1:24)@1:1-1:24"
        "let y = try f x with | e -> g e\n",
        "let y = TryW(A(I[f]@1:13-1:14,I[x]@1:15-1:16)@1:13-1:16,[e@1:24-1:25 A(I[g]@1:29-1:30,I[e]@1:31-1:32)@1:29-1:32])@1:9-1:32"
    ]

    let private adapterTests = [
        testCase "writes the FCS normal form of accepted input"
        <| fun _ ->
            for source, fcsTree in fcsTreeCases do
                let outcome = normalForm source
                Expect.equal outcome.Tree fcsTree source
                Expect.equal (classify [] (Some fcsTree) outcome) Exact source

        testCase "classifies a tree that differs from FCS as WRONGTREE"
        <| fun _ ->
            let outcome = normalForm "let y = [ a..b ]\n"

            Expect.equal
                (classify
                    []
                    (Some "let y = L(RG(I[a]@1:11-1:12,I[b]@1:14-1:15)@1:11-1:16)@1:9-1:17")
                    outcome)
                WrongTree
                "a range that differs from FCS must not count as EXACT"

        testCase "maps vendored recovery diagnostics to FSC2P1001 at the Oracle position"
        <| fun _ ->
            // Oracle: Seq1_0084_let.fs(1,9): error FS0010: Unexpected symbol ';' in binding
            let outcome = normalForm "let y = ; a\n"

            Expect.contains
                outcome.Diagnostics
                "(1,9) FSC2P1001"
                "the recovery diagnostic must keep its position"

            Expect.equal (classify [ "(1,9) FS0010" ] None outcome) Explicit "let y = ; a"

            Expect.equal
                (classify [ "(1,9) FS0010" ] None (normalForm "let y = a; b\n"))
                Missing
                "let y = a; b"
    ]

    [<Tests>]
    let tests =
        match
            Environment.GetEnvironmentVariable "FSHARP2_XPARSEC_COMPARE_DIRECTORY",
            Environment.GetEnvironmentVariable "FSHARP2_XPARSEC_COMPARE_OUTPUT"
        with
        | (null | ""), _
        | _, (null | "") -> testList "XParsec comparison lane" adapterTests
        | input, output ->
            testList "XParsec comparison lane" [
                yield! adapterTests
                testCase "classifies each probe set against FCS trees and Oracle diagnostics"
                <| fun _ -> compareDirectory input output
            ]

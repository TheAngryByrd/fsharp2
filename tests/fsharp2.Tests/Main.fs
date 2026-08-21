namespace fsharp2.Tests

module ExpectoTemplate =

    open Expecto

    let private translateFilterArguments (argv: string array) =
        let translated = ResizeArray<string>(argv.Length)
        let mutable index = 0

        while index < argv.Length do
            match argv[index] with
            | "--filter-method" when index + 1 < argv.Length ->
                translated.Add("--filter-test-case")
                translated.Add(argv[index + 1].Trim([| '*' |]))
                index <- index + 2
            | "--filter-class" when index + 1 < argv.Length ->
                translated.Add("--filter-test-list")
                translated.Add(argv[index + 1].Trim([| '*' |]))
                index <- index + 2
            | argument ->
                translated.Add(argument)
                index <- index + 1

        translated.ToArray()

    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssemblyWithCLIArgs [] (translateFilterArguments argv)

# Forbid production fallback to the official compiler

Every successful compilation must be performed entirely by the new compiler. The official compiler and `FSharp.Compiler.Service` may appear in differential-test infrastructure, but production code may neither invoke nor load them to handle unsupported inputs; unsupported experimental cases must fail explicitly.

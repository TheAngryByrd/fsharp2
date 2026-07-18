using System;
using System.IO;
using System.Reflection;

if (args.Length is < 1 or > 2)
{
    Console.Error.WriteLine("expected an emitted assembly path and optional integer result");
    return 2;
}

var expected = args.Length == 2 ? int.Parse(args[1]) : 42;

var assembly = Assembly.LoadFrom(Path.GetFullPath(args[0]));
var tracerType = assembly.GetType("Tracer", throwOnError: true)!;
var answer = tracerType.GetMethod("answer", BindingFlags.Public | BindingFlags.Static)!;
var actual = answer.Invoke(null, []);

if (!Equals(actual, expected))
{
    Console.Error.WriteLine("unexpected answer");
    return 1;
}

Console.WriteLine(actual);
return 0;

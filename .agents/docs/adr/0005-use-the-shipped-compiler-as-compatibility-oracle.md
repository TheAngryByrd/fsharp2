# Use the shipped compiler as the compatibility oracle

The F# specification guides the independent implementation, but the latest compiler shipped with the .NET 10 SDK is authoritative when its observable behavior differs from or extends the written specification. Compatibility will therefore be judged with differential tests against that compiler.

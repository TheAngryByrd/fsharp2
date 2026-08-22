namespace FSharp2.Conformance;

public static class FallbackSentinel
{
    public static string CreateReceipt(string runRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(runRoot);
        var fullRoot = Path.GetFullPath(runRoot);
        Directory.CreateDirectory(fullRoot);
        var receiptPath = Path.Combine(fullRoot, "fallback-sentinel.receipt");
        File.WriteAllBytes(receiptPath, []);
        return receiptPath;
    }

    public static ValidationResult ValidateReceipt(string receiptPath)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(receiptPath);
        var fullPath = Path.GetFullPath(receiptPath);
        if (!File.Exists(fullPath))
        {
            return ValidationResult.Invalid(
                new ValidationIssue(
                    "fallback-sentinel",
                    fullPath,
                    "The fallback sentinel receipt does not exist."));
        }
        if (new FileInfo(fullPath).Length == 0)
        {
            return ValidationResult.Valid;
        }
        return ValidationResult.Invalid(
            new ValidationIssue(
                "fallback-sentinel",
                fullPath,
                "The fallback sentinel received a compiler invocation."));
    }
}

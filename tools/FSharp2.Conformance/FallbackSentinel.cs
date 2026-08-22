namespace FSharp2.Conformance;

public static class FallbackSentinel
{
    public const string ReceiptEnvironmentVariable = "FSharp2FallbackSentinelReceiptPath";

    public static string CreateReceipt(string runRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(runRoot);
        var fullRoot = Path.GetFullPath(runRoot);
        Directory.CreateDirectory(fullRoot);
        var receiptPath = Path.Combine(fullRoot, "fallback-sentinel.receipt");
        File.WriteAllBytes(receiptPath, []);
        PrepareSelectedReceipt();
        return receiptPath;
    }

    public static void RetainSelectedReceipt(string receiptPath)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(receiptPath);
        var selectedReceiptPath = SelectedReceiptPath();
        if (selectedReceiptPath is null
            || !File.Exists(selectedReceiptPath)
            || new FileInfo(selectedReceiptPath).Length == 0)
        {
            return;
        }

        var fullReceiptPath = Path.GetFullPath(receiptPath);
        var pathComparison = OperatingSystem.IsWindows()
            ? StringComparison.OrdinalIgnoreCase
            : StringComparison.Ordinal;
        if (!string.Equals(selectedReceiptPath, fullReceiptPath, pathComparison))
        {
            File.WriteAllBytes(fullReceiptPath, File.ReadAllBytes(selectedReceiptPath));
        }
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

    private static void PrepareSelectedReceipt()
    {
        var receiptPath = SelectedReceiptPath();
        if (receiptPath is null)
        {
            return;
        }

        Directory.CreateDirectory(Path.GetDirectoryName(receiptPath)!);
        File.WriteAllBytes(receiptPath, []);
    }

    private static string? SelectedReceiptPath()
    {
        var receiptPath = Environment.GetEnvironmentVariable(ReceiptEnvironmentVariable);
        return string.IsNullOrWhiteSpace(receiptPath) ? null : Path.GetFullPath(receiptPath);
    }
}

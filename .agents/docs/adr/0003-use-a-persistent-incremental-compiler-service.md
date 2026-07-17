# Use a persistent incremental compiler service

Warm MSBuild invocations may connect to a long-lived NativeAOT compiler service that retains reusable compilation state. A deterministic standalone compiler remains available for cold builds and fallback operation, and both execution paths must produce equivalent observable results.

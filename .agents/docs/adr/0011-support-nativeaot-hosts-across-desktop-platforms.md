# Support NativeAOT hosts across desktop platforms

The compiler host must publish and pass its release gates on Windows, Linux, and macOS for both x64 and ARM64. Platform-specific acceleration is permitted behind behaviorally equivalent implementations, while externally persisted compiler state must remain portable where the underlying build inputs are portable.

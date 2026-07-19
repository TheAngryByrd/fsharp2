# Support NativeAOT hosts across desktop platforms

The compiler host must publish and pass its release gates on Windows, Linux, and macOS for both x64 and ARM64. Linux includes separate glibc and musl artifacts under [ADR 0026](0026-support-versioned-glibc-and-musl-linux-floors.md), making eight required host payloads across the three OS families. Platform-specific acceleration is permitted behind behaviorally equivalent implementations, while externally persisted compiler state must remain portable where the underlying build inputs are portable.

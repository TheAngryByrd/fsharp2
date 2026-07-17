# Distribute opt-in MSBuild integration by NuGet

Experimental projects will adopt the compiler through a NuGet package that supplies compatible MSBuild props and targets plus RID-specific NativeAOT compiler hosts. One project property selects the replacement without changing source ordering or normal F# options; the same seam becomes transparent after the Compatibility Gate passes.

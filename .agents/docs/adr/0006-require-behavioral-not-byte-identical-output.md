# Require behavioral rather than byte-identical output compatibility

Generated artifacts must preserve public API shape, metadata and runtime semantics, diagnostics, debugging behavior, signing, resources, and deterministic repeatability, but they need not match the official compiler byte for byte. Different valid IL and optimization choices are allowed so the independent compiler can improve code generation.

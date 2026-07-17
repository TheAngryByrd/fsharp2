# Use memory and content-addressed disk caches

Warm compilation may reuse both live compiler-service state and a versioned content-addressed disk cache shared across process lifetimes. Cache validity is never allowed to change compilation behavior: uncertain or incompatible entries are recomputed, and Cold Compilation explicitly bypasses both cache levels.

# Track incremental dependencies at declaration granularity

Warm Compilation will reuse typed declarations and emitted method fragments through explicit semantic dependency tracking, with file-level checkpoints preserving F# source-order semantics. When dependency impact cannot be proven narrow, invalidation expands conservatively rather than publishing stale semantic or binary results.

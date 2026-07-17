# Select memory techniques by benchmark

No memory technique is mandated throughout the compiler. Spans, ref structs, pools, arenas, interning, compact snapshots, unsafe code, hardware intrinsics, and ordinary managed objects may each be used where representative benchmarks prove the best trade-off while correctness, NativeAOT compatibility, and maintainability remain release requirements.

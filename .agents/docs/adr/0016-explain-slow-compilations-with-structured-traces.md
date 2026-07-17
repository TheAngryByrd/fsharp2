# Explain slow compilations with structured traces

A Compiler Target Invocation exceeding three seconds still succeeds but emits an MSBuild performance warning and a structured trace covering phase timings, allocation and GC activity, cache decisions, input scale, and the critical path. CI may opt into treating this warning as an error without making local slowdowns correctness failures.

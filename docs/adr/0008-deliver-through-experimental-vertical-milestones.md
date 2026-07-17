# Deliver through experimental vertical milestones

Development may ship opt-in milestones with explicitly limited compatibility envelopes before the compiler is a full drop-in replacement. Every milestone must execute its supported cases through parsing, typechecking, optimization, emission, loading, and execution; the replacement MSBuild target remains experimental until the compatibility gate passes.

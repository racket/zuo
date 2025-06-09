Sources:

 * "grammar.rkt": Shared grammar for all reductions.

 * "interp.rkt": Defines the `-->interp` reduction, which is a,
   Reynolds-style interp--continue interpreter, except that
   environment extensions are allocated explicitly in a store. Space
   complexity matches "linked environments" in Clinger (ICFP 1998)
   when combined with GC.

   Each step in this machine is meant to have a clear constant-time(*)
   implementation; we imagine a HAMT or binary-tree implementation of
   environments, instead of a plain linked list, which provides
   constant-time(*) access and asymptotically the same amount of
   sharing as linked lists. In the case of a binary tree, each
   variable needs to be mapped to its binding depth (inverse of de
   Bruijn) in a one-time complation pass before evaluation.

 * "gc.rkt": Defines the `-->gc` reduction, which implements a 2-space
   garbage collector for the store that's used by `-->interp`. Like
   the `-->interp` reduction, each step in this machine is meant to
   have a clear constant-time(*) implementation.

 * "combine.rkt": Defines `combine` for create a reduction from a
   given evaluation reduction like `-->interp` and GC reduction like
   `-->gc`. So, `(combine -->interp -->gc)` is linked-environments
   S_tail in Clinger's terminology.

 * "interp-sfs.rkt": Defines the `-->interp/sfs` reduction, which
   simulates flat closures by constructing a new environment at each
   closure formation that includes only variables that are free in the
   expression component of the closure. To expose the cost of that
   flattening, the newly constructed environment is created
   step-by-step. A complete SFS implementation needs combination with
   GC, `(combine -->sfs -->gc)`. Note that S_sfs is not always better
   than linked-enviornment S_tail in space complexity; see section 13
   in Clinger.

   Time complexity is higher for `-->interp/sfs` compared to
   `-->interp`, since the flattening operation is explicit. The new
   environment after flattening is still a linked structure, where a
   realistic implementation would use an array, but that choice
   doesn't affect the asymptotic time or space complexity. The free
   variables of an expression need to be precomputed, perhaps in a
   one-time compilation pass.

 * "gc-sfs.rkt": Defines the `-->gc/sfs` reduction, which is meant to
   be combined with `-->interp` to produce a reduction whose
   asymptotic space complexity is within a size-of-program factor of
   being at least as good as of `-->interp/sfs` plus `-->gc`, but also
   with that factor of being at least as good as `-->interp` plus
   `-->gc`. Asymptotic time complexity is within a size-of-program
   factor of `-->interp`.

   Deferring to GC the extra work that SFS does means that it happens
   much less often; short-lived flat closures are never created. But
   with the simplest implementation, GC time is no longer proportional
   to heap size; so setting evaluation fuel based on heap size
   triggers GC too often, while setting evaluation fuel based on GC
   during allows higher memory use. The resolution of this tension is
   to keep track of distinct enviornment shapes, and sweep an
   enviornment only once for each distinct shape. The number of
   distinct shapes is limited by the original program's size. That
   factor the limits time expansion, but also creates a space
   expansion, since a table of completed shapes must be kept with each
   enviornment. With time and space back in sync, either metric works
   for setting evaluator fuel.

   Precomputed free-variable sets need to be in a form to allow a
   constant-time(*) jump to a linked environment that binds the first
   free variable. A binary tree with the same shape as an environment
   can stisfy that goal.

 * "store.rkt", "free-vars.rkt", and "skip.rkt": Helper metafunctions.
   Every metafunction is intended to represent a constant-time(*)
   operation (even if it is not implemented that way in the model).

 * "test-loop.rkt", "test-run.rkt", "test-time.rkt": Tests and
   examples.

(*) Either constant-time or logarithmic-time.

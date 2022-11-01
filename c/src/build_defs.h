
// defines that control various optimizations

#ifndef __build_defs_h__
#define __build_defs_h__

// judging by the profiling results, both of these should give a decent speedup,
// however, in practice no speedup is seen. since both of them could degrade the
// build in some way, i've turned them off for now.

// #define USE_FAST_MACROS
// #define NO_CLEAR_CALLFRAMES

#endif // __build_defs_h__


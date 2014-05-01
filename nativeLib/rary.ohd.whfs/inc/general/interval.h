#ifndef LIBSRA_DEF
#define LIBSRA_DEF
#include "pgtypes_timestamp.h"
extern int PGTYPEStimestamp_sub_interval ( timestamp *, interval * ,  timestamp *tout);
extern int PGTYPEStimestamp_add_interval ( timestamp *, interval * ,  timestamp *tout);
#endif

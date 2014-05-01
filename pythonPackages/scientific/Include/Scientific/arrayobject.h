#if defined(NUMPY)
#include "numpy/oldnumeric.h"
#define NUMERICS_PACKAGE "NumPy"
#else
#if defined(NUMARRAY)
#include "Numeric/arrayobject.h"
#define NUMERICS_PACKAGE "Numarray"
#else
#include "Numeric/arrayobject.h"
#define NUMERICS_PACKAGE "Numeric"
#endif
#endif

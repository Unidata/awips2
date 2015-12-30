# This module contains nothing but the name of the numerics package
# (Numeric, NumPy, Numarray) that was used to compile the C extensions
#
# Written by Konrad Hinsen
# last revision: 2007-4-17
#

cdef extern from 'Scientific/arrayobject.h':
    char *NUMERICS_PACKAGE

def getNumericsPackageName():
    return NUMERICS_PACKAGE

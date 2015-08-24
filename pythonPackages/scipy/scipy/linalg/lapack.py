<<<<<<< HEAD
=======
"""
Low-level LAPACK functions
==========================

This module contains low-level functions from the LAPACK library.

.. versionadded:: 0.12.0

.. warning::

   These functions do little to no error checking.
   It is possible to cause crashes by mis-using them,
   so prefer using the higher-level routines in `scipy.linalg`.

Finding functions
=================

.. autosummary::

   get_lapack_funcs

All functions
=============

.. autosummary::
   :toctree: generated/


   sgbsv
   dgbsv
   cgbsv
   zgbsv

   sgbtrf
   dgbtrf
   cgbtrf
   zgbtrf

   sgbtrs
   dgbtrs
   cgbtrs
   zgbtrs

   sgebal
   dgebal
   cgebal
   zgebal

   sgees
   dgees
   cgees
   zgees

   sgeev
   dgeev
   cgeev
   zgeev

   sgeev_lwork
   dgeev_lwork
   cgeev_lwork
   zgeev_lwork

   sgegv
   dgegv
   cgegv
   zgegv

   sgehrd
   dgehrd
   cgehrd
   zgehrd

   sgehrd_lwork
   dgehrd_lwork
   cgehrd_lwork
   zgehrd_lwork

   sgelss
   dgelss
   cgelss
   zgelss
   
   sgelss_lwork
   dgelss_lwork
   cgelss_lwork
   zgelss_lwork
   
   sgelsd
   dgelsd
   cgelsd
   zgelsd
   
   sgelsd_lwork
   dgelsd_lwork
   cgelsd_lwork
   zgelsd_lwork
   
   sgelsy
   dgelsy
   cgelsy
   zgelsy

   sgelsy_lwork
   dgelsy_lwork
   cgelsy_lwork
   zgelsy_lwork
   
   sgeqp3
   dgeqp3
   cgeqp3
   zgeqp3

   sgeqrf
   dgeqrf
   cgeqrf
   zgeqrf

   sgerqf
   dgerqf
   cgerqf
   zgerqf

   sgesdd
   dgesdd
   cgesdd
   zgesdd

   sgesdd_lwork
   dgesdd_lwork
   cgesdd_lwork
   zgesdd_lwork

   sgesv
   dgesv
   cgesv
   zgesv

   sgetrf
   dgetrf
   cgetrf
   zgetrf

   sgetri
   dgetri
   cgetri
   zgetri

   sgetri_lwork
   dgetri_lwork
   cgetri_lwork
   zgetri_lwork

   sgetrs
   dgetrs
   cgetrs
   zgetrs

   sgges
   dgges
   cgges
   zgges

   sggev
   dggev
   cggev
   zggev

   chbevd
   zhbevd

   chbevx
   zhbevx

   cheev
   zheev

   cheevd
   zheevd

   cheevr
   zheevr

   chegv
   zhegv

   chegvd
   zhegvd

   chegvx
   zhegvx
   
   slarf
   dlarf
   clarf
   zlarf

   slarfg
   dlarfg
   clarfg
   zlarfg

   slartg
   dlartg
   clartg
   zlartg

   dlasd4
   slasd4

   slaswp
   dlaswp
   claswp
   zlaswp

   slauum
   dlauum
   clauum
   zlauum

   spbsv
   dpbsv
   cpbsv
   zpbsv

   spbtrf
   dpbtrf
   cpbtrf
   zpbtrf

   spbtrs
   dpbtrs
   cpbtrs
   zpbtrs

   sposv
   dposv
   cposv
   zposv

   spotrf
   dpotrf
   cpotrf
   zpotrf

   spotri
   dpotri
   cpotri
   zpotri

   spotrs
   dpotrs
   cpotrs
   zpotrs

   crot
   zrot

   strsyl
   dtrsyl
   ctrsyl
   ztrsyl

   strtri
   dtrtri
   ctrtri
   ztrtri

   strtrs
   dtrtrs
   ctrtrs
   ztrtrs

   cunghr
   zunghr

   cungqr
   zungqr

   cungrq
   zungrq

   cunmqr
   zunmqr

   sgtsv
   dgtsv
   cgtsv
   zgtsv

   sptsv
   dptsv
   cptsv
   zptsv

   slamch
   dlamch

   sorghr
   dorghr
   sorgqr
   dorgqr

   sorgrq
   dorgrq

   sormqr
   dormqr

   ssbev
   dsbev

   ssbevd
   dsbevd

   ssbevx
   dsbevx

   ssyev
   dsyev

   ssyevd
   dsyevd

   ssyevr
   dsyevr

   ssygv
   dsygv

   ssygvd
   dsygvd

   ssygvx
   dsygvx

   slange
   dlange
   clange
   zlange

"""
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#
# Author: Pearu Peterson, March 2002
#

<<<<<<< HEAD
__all__ = ['get_lapack_funcs']

# The following ensures that possibly missing flavor (C or Fortran) is
# replaced with the available one. If none is available, exception
# is raised at the first attempt to use the resources.
import types

import numpy

from scipy.linalg import flapack
from scipy.linalg import clapack
_use_force_clapack = 1
if hasattr(clapack,'empty_module'):
    clapack = flapack
    _use_force_clapack = 0
elif hasattr(flapack,'empty_module'):
    flapack = clapack

def cast_to_lapack_prefix(t):
    if issubclass(t, numpy.single):
        prefix = 's'
    elif issubclass(t, numpy.double):
        prefix = 'd'
    elif issubclass(t, numpy.longdouble):
        prefix = 'd'
    elif issubclass(t, numpy.csingle):
        prefix = 'c'
    elif issubclass(t, numpy.cdouble):
        prefix = 'z'
    elif issubclass(t, numpy.clongdouble):
        prefix = 'z'
    else:
        prefix = 'd'
    return prefix

prefix_to_order = dict(s=3, d=2, c=1, z=0)
order_to_prefix = ['s', 'd', 'c', 'z']
prefix_to_dtype = dict(s=numpy.single, d=numpy.double,
                       c=numpy.csingle, z=numpy.cdouble)

def find_best_lapack_type(arrays):
    if not arrays:
        return 'd', numpy.double, False
    ordering = []
    for i in range(len(arrays)):
        t = arrays[i].dtype.type
        prefix = cast_to_lapack_prefix(t)
        order = prefix_to_order[prefix]
        ordering.append((order, prefix, i))
    ordering.sort()
    _, required_prefix, lowest_array_index = ordering[0]
    dtype = prefix_to_dtype[required_prefix]
    isfortran = numpy.isfortran(arrays[lowest_array_index])
    return required_prefix, dtype, isfortran

def get_lapack_funcs(names, arrays=()):
    """Return available LAPACK function objects with names.
    arrays are used to determine the optimal prefix of
    LAPACK routines.
    """
    #If force_clapack is True then available Atlas routine
    #is returned for column major storaged arrays with
    #rowmajor argument set to False.
    force_clapack=False  #XXX: Don't set it true! The feature is unreliable
                         #     and may cause incorrect results.
                         #     See test_basic.test_solve.check_20Feb04_bug.

    required_prefix, dtype, isfortran = find_best_lapack_type(arrays)
    # Default lookup:
    if isfortran:
        # prefer Fortran code for leading array with column major order
        m1, m2 = flapack, clapack
    else:
        # in all other cases, C code is preferred
        m1, m2 = clapack, flapack
    if not _use_force_clapack:
        force_clapack = False
    funcs = []
    m1_name = m1.__name__.split('.')[-1]
    m2_name = m2.__name__.split('.')[-1]
    for name in names:
        func_name = required_prefix + name
        func = getattr(m1,func_name,None)
        if func is None:
            func = getattr(m2,func_name)
            func.module_name = m2_name
        else:
            func.module_name = m1_name
            if force_clapack and m1 is flapack:
                func2 = getattr(m2,func_name,None)
                if func2 is not None:
                    exec _colmajor_func_template % {'func_name':func_name}
                    func = types.FunctionType(func_code,
                                              {'clapack_func':func2},
                                              func_name)
                    func.module_name = m2_name
                    func.__doc__ = func2.__doc__
        func.prefix = required_prefix
        func.dtype = dtype
        funcs.append(func)
    return tuple(funcs)



_colmajor_func_template = '''\
def %(func_name)s(*args,**kws):
    if "rowmajor" not in kws:
        kws["rowmajor"] = 0
    return clapack_func(*args,**kws)
func_code = %(func_name)s.func_code
'''
=======
from __future__ import division, print_function, absolute_import

__all__ = ['get_lapack_funcs']

from .blas import _get_funcs

# Backward compatibility:
from .blas import find_best_blas_type as find_best_lapack_type

from scipy.linalg import _flapack
try:
    from scipy.linalg import _clapack
except ImportError:
    _clapack = None

# Backward compatibility
from scipy._lib._util import DeprecatedImport as _DeprecatedImport
clapack = _DeprecatedImport("scipy.linalg.blas.clapack", "scipy.linalg.lapack")
flapack = _DeprecatedImport("scipy.linalg.blas.flapack", "scipy.linalg.lapack")

# Expose all functions (only flapack --- clapack is an implementation detail)
empty_module = None
from scipy.linalg._flapack import *
del empty_module

# some convenience alias for complex functions
_lapack_alias = {
    'corghr': 'cunghr', 'zorghr': 'zunghr',
    'corghr_lwork': 'cunghr_lwork', 'zorghr_lwork': 'zunghr_lwork',
    'corgqr': 'cungqr', 'zorgqr': 'zungqr',
    'cormqr': 'cunmqr', 'zormqr': 'zunmqr',
    'corgrq': 'cungrq', 'zorgrq': 'zungrq',
}


def get_lapack_funcs(names, arrays=(), dtype=None):
    """Return available LAPACK function objects from names.

    Arrays are used to determine the optimal prefix of LAPACK routines.

    Parameters
    ----------
    names : str or sequence of str
        Name(s) of LAPACK functions without type prefix.

    arrays : sequence of ndarrays, optional
        Arrays can be given to determine optimal prefix of LAPACK
        routines. If not given, double-precision routines will be
        used, otherwise the most generic type in arrays will be used.

    dtype : str or dtype, optional
        Data-type specifier. Not used if `arrays` is non-empty.


    Returns
    -------
    funcs : list
        List containing the found function(s).


    Notes
    -----
    This routine automatically chooses between Fortran/C
    interfaces. Fortran code is used whenever possible for arrays with
    column major order. In all other cases, C code is preferred.

    In LAPACK, the naming convention is that all functions start with a
    type prefix, which depends on the type of the principal
    matrix. These can be one of {'s', 'd', 'c', 'z'} for the numpy
    types {float32, float64, complex64, complex128} respectevely, and
    are stored in attribute `typecode` of the returned functions.
    """
    return _get_funcs(names, arrays, dtype,
                      "LAPACK", _flapack, _clapack,
                      "flapack", "clapack", _lapack_alias)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

#ifndef SPARSETOOLS_H
#define SPARSETOOLS_H

<<<<<<< HEAD
/*
 * sparsetools.h 
 *   A collection of routines for sparse matrix operations:
 *  
 * Original code by Nathan Bell ( http://www.wnbell.com/ ) 
 *
 * Files/formats are:
 *  csr.h - Compressed Sparse Row format
 *  csc.h - Compressed Sparse Column format
 *  coo.h - COOrdinate format
 *  bsr.h - Block Sparse Row format
 *  dia.h - DIAgonal format
 *
 */

#include "csr.h" 
#include "csc.h"
#include "coo.h"
#include "bsr.h"
#include "dia.h"
=======
#include <Python.h>
#include "numpy/ndarrayobject.h"

#include <stdexcept>

#include "bool_ops.h"
#include "complex_ops.h"

typedef Py_ssize_t thunk_t(int I_typenum, int T_typenum, void **args);

NPY_VISIBILITY_HIDDEN PyObject *
call_thunk(char ret_spec, const char *spec, thunk_t *thunk, PyObject *args);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

#endif

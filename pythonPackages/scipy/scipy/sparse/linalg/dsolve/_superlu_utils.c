<<<<<<< HEAD

#include "Python.h"
#include <setjmp.h>

jmp_buf _superlu_py_jmpbuf;
PyObject *_superlumodule_memory_dict=NULL;

/* Abort to be used inside the superlu module so that memory allocation 
   errors don't exit Python and memory allocated internal to SuperLU is freed.
   Calling program should deallocate (using SUPERLU_FREE) all memory that could have 
=======
/* Should be imported before Python.h */

#include <Python.h>

#define NO_IMPORT_ARRAY
#define PY_ARRAY_UNIQUE_SYMBOL _scipy_sparse_superlu_ARRAY_API

#include "_superluobject.h"
#include "numpy/npy_3kcompat.h"
#include <setjmp.h>

jmp_buf _superlu_py_jmpbuf;
PyObject *_superlumodule_memory_dict = NULL;

/* Abort to be used inside the superlu module so that memory allocation
   errors don't exit Python and memory allocated internal to SuperLU is freed.
   Calling program should deallocate (using SUPERLU_FREE) all memory that could have
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
   been allocated.  (It's ok to FREE unallocated memory)---will be ignored.
*/

void superlu_python_module_abort(char *msg)
{
<<<<<<< HEAD
  PyErr_SetString(PyExc_RuntimeError, msg);
  longjmp(_superlu_py_jmpbuf, -1);
=======
    PyErr_SetString(PyExc_RuntimeError, msg);
    longjmp(_superlu_py_jmpbuf, -1);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

void *superlu_python_module_malloc(size_t size)
{
<<<<<<< HEAD
  PyObject *key=NULL;
  long keyval;
  void *mem_ptr; 

  if (_superlumodule_memory_dict == NULL) {
    _superlumodule_memory_dict = PyDict_New();
  }
  mem_ptr = malloc(size);
  if (mem_ptr == NULL) return NULL;
  keyval = (long) mem_ptr;
  key = PyInt_FromLong(keyval);
  if (key == NULL) goto fail;
  if (PyDict_SetItem(_superlumodule_memory_dict, key, Py_None)) goto fail;
  Py_DECREF(key);
  return mem_ptr;

 fail:
  Py_XDECREF(key);
  free(mem_ptr);
  superlu_python_module_abort("superlu_malloc: Cannot set dictionary key value in malloc.");
  return NULL;
 
=======
    PyObject *key = NULL;
    void *mem_ptr;

    if (_superlumodule_memory_dict == NULL) {
	_superlumodule_memory_dict = PyDict_New();
    }
    mem_ptr = malloc(size);
    if (mem_ptr == NULL)
	return NULL;
    key = PyLong_FromVoidPtr(mem_ptr);
    if (key == NULL)
	goto fail;
    if (PyDict_SetItem(_superlumodule_memory_dict, key, Py_None))
	goto fail;
    Py_DECREF(key);
    return mem_ptr;

  fail:
    Py_XDECREF(key);
    free(mem_ptr);
    superlu_python_module_abort
	("superlu_malloc: Cannot set dictionary key value in malloc.");
    return NULL;

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

void superlu_python_module_free(void *ptr)
{
<<<<<<< HEAD
  PyObject *key;
  long keyval;
  PyObject *ptype, *pvalue, *ptraceback;

  if (ptr == NULL) return;
  PyErr_Fetch(&ptype, &pvalue, &ptraceback);
  keyval = (long )ptr;
  key = PyInt_FromLong(keyval);
  /* This will only free the pointer if it could find it in the dictionary
     of already allocated pointers --- thus after abort, the module can free all
     the memory that "might" have been allocated to avoid memory leaks on abort 
     calls.
   */ 
  if (_superlumodule_memory_dict && \
      !(PyDict_DelItem(_superlumodule_memory_dict, key))) {
    free(ptr);
  }
  Py_DECREF(key);
  PyErr_Restore(ptype, pvalue, ptraceback);
  return; 
=======
    PyObject *key;
    PyObject *ptype, *pvalue, *ptraceback;

    if (ptr == NULL)
	return;
    PyErr_Fetch(&ptype, &pvalue, &ptraceback);
    key = PyLong_FromVoidPtr(ptr);
    /* This will only free the pointer if it could find it in the dictionary
     * of already allocated pointers --- thus after abort, the module can free all
     * the memory that "might" have been allocated to avoid memory leaks on abort
     * calls.
     */
    if (_superlumodule_memory_dict &&
	!(PyDict_DelItem(_superlumodule_memory_dict, key))) {
	free(ptr);
    }
    Py_DECREF(key);
    PyErr_Restore(ptype, pvalue, ptraceback);
    return;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

/*
 * Stubs for Harwell Subroutine Library functions that SuperLU tries to call.
 */

void mc64id_(int *a)
{
    superlu_python_module_abort("chosen functionality not available");
}

void mc64ad_(int *a, int *b, int *c, int d[], int e[], double f[],
<<<<<<< HEAD
             int *g, int h[], int *i, int j[], int *k, double l[],
             int m[], int n[])
=======
	     int *g, int h[], int *i, int j[], int *k, double l[],
	     int m[], int n[])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
{
    superlu_python_module_abort("chosen functionality not available");
}

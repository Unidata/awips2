/* Python TNC module */

/*
 * Copyright (c) 2004-2005, Jean-Sebastien Roy (js@jeannot.org)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static char const rcsid[] =
  "@(#) $Jeannot: moduleTNC.c,v 1.12 2005/01/28 18:27:31 js Exp $";

#include "Python.h"
<<<<<<< HEAD
=======
#include "numpy/arrayobject.h"
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "tnc.h"

<<<<<<< HEAD
typedef struct _pytnc_state
{
  PyObject *py_function;
  int n;
=======
/* NPY_ARRAY_IN_ARRAY was introduced in numpy 1.7.0 */
#if NPY_API_VERSION < 0x00000007
    #define NPY_ARRAY_C_CONTIGUOUS    0x0001
    #define NPY_ARRAY_ALIGNED         0x0100
    #define NPY_ARRAY_IN_ARRAY (NPY_ARRAY_C_CONTIGUOUS | NPY_ARRAY_ALIGNED)
#endif

typedef struct _pytnc_state
{
  PyObject *py_function;
  PyObject *py_callback;
  npy_intp n;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  int failed;
} pytnc_state;

static tnc_function function;
static PyObject *moduleTNC_minimize(PyObject *self, PyObject *args);
<<<<<<< HEAD
static int PyObject_AsDouble(PyObject *py_obj, double *x);
static double *PyList_AsDoubleArray(PyObject *py_list, int *size);
static PyObject *PyDoubleArray_AsList(int size, double *x);
static int PyList_IntoDoubleArray(PyObject *py_list, double *x, int size);

int PyObject_AsDouble(PyObject *py_obj, double *x)
{
  PyObject *py_float;

  py_float = PyNumber_Float(py_obj);

  if (py_float == NULL) return -1;

  *x = PyFloat_AsDouble(py_float);

  Py_DECREF(py_float);
  return 0;
}

double *PyList_AsDoubleArray(PyObject *py_list, int *size)
{
  int i;
  double *x;

  if (!PyList_Check(py_list))
  {
    *size = -1;
    return NULL;
  }

  *size = PyList_Size(py_list);
  if (*size <= 0) return NULL;
  x = malloc((*size)*sizeof(*x));
  if (x == NULL) return NULL;

  for (i=0; i<(*size); i++)
  {
    PyObject *py_float = PyList_GetItem(py_list, i);
    if (py_float == NULL || PyObject_AsDouble(py_float, &(x[i])))
    {
      free(x);
      return NULL;
    }
  }

  return x;
}

int PyList_IntoDoubleArray(PyObject *py_list, double *x, int size)
{
  int i;

  if (py_list == NULL) return 1;

  if (!PyList_Check(py_list)) return 1;

  if (size != PyList_Size(py_list)) return 1;

  for (i=0; i<size; i++)
  {
    PyObject *py_float = PyList_GetItem(py_list, i);
    if (py_float == NULL || PyObject_AsDouble(py_float, &(x[i])))
      return 1;
  }

  return 0;
}

PyObject *PyDoubleArray_AsList(int size, double *x)
{
  int i;
  PyObject *py_list;

  py_list = PyList_New(size);
  if (py_list == NULL) return NULL;

  for (i=0; i<size; i++)
  {
    PyObject *py_float;
    py_float = PyFloat_FromDouble(x[i]);
    if (py_float == NULL || PyList_SetItem(py_list, i, py_float))
    {
      Py_DECREF(py_list);
      return NULL;
    }
  }

  return py_list;
}

static int function(double x[], double *f, double g[], void *state)
{
  PyObject *py_list, *arglist, *py_grad, *result = NULL;
  pytnc_state *py_state = (pytnc_state *)state;

  py_list = PyDoubleArray_AsList(py_state->n, x);
  if (py_list == NULL)
=======

static int function(double x[], double *f, double g[], void *state)
{
  PyArrayObject *py_x, *arr_grad=NULL;
  PyObject *arglist, *result = NULL, *py_grad;
  pytnc_state *py_state = (pytnc_state *)state;

  py_x = (PyArrayObject *)PyArray_SimpleNew(1, &py_state->n, NPY_DOUBLE);
  if (py_x == NULL)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  {
    PyErr_SetString(PyExc_MemoryError, "tnc: memory allocation failed.");
    goto failure;
  }
<<<<<<< HEAD

  arglist = Py_BuildValue("(N)", py_list);
=======
  memcpy(PyArray_DATA(py_x), x, (py_state->n)*sizeof(double));

  arglist = Py_BuildValue("(N)", py_x);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  result = PyEval_CallObject(py_state->py_function, arglist);
  Py_DECREF(arglist);

  if (result == NULL)
    goto failure;

  if (result == Py_None)
  {
    Py_DECREF(result);
    return 1;
  }

<<<<<<< HEAD
  if (!PyArg_ParseTuple(result, "dO!", f, &PyList_Type, &py_grad))
=======
  if (!PyArg_ParseTuple(result, "dO", f, &py_grad))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  {
    PyErr_SetString(PyExc_ValueError,
      "tnc: invalid return value from minimized function.");
    goto failure;
  }
<<<<<<< HEAD

  if (PyList_IntoDoubleArray(py_grad, g, py_state->n))
    goto failure;

=======
  arr_grad = (PyArrayObject *)PyArray_FROM_OTF((PyObject *)py_grad,
                                               NPY_DOUBLE, NPY_ARRAY_IN_ARRAY);
  if (arr_grad == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid gradient vector.");
    goto failure;
  }

  if (PyArray_SIZE(arr_grad) != py_state->n)
  {
    PyErr_SetString(PyExc_ValueError,
      "tnc: invalid gradient vector from minimized function.");
    goto failure;
  }
  memcpy(g, PyArray_DATA(arr_grad), (py_state->n)*sizeof(double));

  Py_DECREF(arr_grad);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  Py_DECREF(result);

  return 0;

failure:
  py_state->failed = 1;
<<<<<<< HEAD
=======
  Py_XDECREF(arr_grad);
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  Py_XDECREF(result);
  return 1;
}

<<<<<<< HEAD
PyObject *moduleTNC_minimize(PyObject *self, PyObject *args)
{
  PyObject *py_x0, *py_low, *py_up, *py_list, *py_scale, *py_offset;
  PyObject *py_function = NULL;
  pytnc_state py_state;
  int n, n1, n2, n3, n4;

  int rc, msg, maxCGit, maxnfeval, nfeval = 0;
  double *x, *low, *up, *scale = NULL, *offset = NULL;
  double f, eta, stepmx, accuracy, fmin, ftol, xtol, pgtol, rescale;

  if (!PyArg_ParseTuple(args, "OO!O!O!O!O!iiidddddddd",
    &py_function,
    &PyList_Type, &py_x0,
    &PyList_Type, &py_low,
    &PyList_Type, &py_up,
    &PyList_Type, &py_scale,
    &PyList_Type, &py_offset,
    &msg, &maxCGit, &maxnfeval, &eta, &stepmx, &accuracy, &fmin, &ftol,
    &xtol, &pgtol,
    &rescale
    ))
    return NULL;
=======
static void callback(double x[], void *state)
{
  PyArrayObject *py_x;
  PyObject *arglist, *result = NULL;
  pytnc_state *py_state = (pytnc_state *)state;

  py_x = (PyArrayObject *)PyArray_SimpleNew(1, &py_state->n, NPY_DOUBLE);
  if (py_x == NULL)
  {
    PyErr_SetString(PyExc_MemoryError, "tnc: memory allocation failed.");
    return;
  }
  memcpy(PyArray_DATA(py_x), x, (py_state->n)*sizeof(double));

  arglist = Py_BuildValue("(N)", py_x);
  result = PyEval_CallObject(py_state->py_callback, arglist);
  Py_DECREF(arglist);
  Py_XDECREF(result);
}

PyObject *moduleTNC_minimize(PyObject *self, PyObject *args)
{
  PyArrayObject *arr_x=NULL, *arr_low=NULL, *arr_up=NULL, *arr_scale=NULL,
                *arr_offset=NULL;
  PyObject *py_x0, *py_low, *py_up, *py_scale, *py_offset;
  PyObject *py_function = NULL;
  PyObject *py_callback = NULL;
  pytnc_state py_state;
  int n, n1, n2, n3, n4;
  tnc_callback *callback_function = NULL;

  int rc, msg, maxCGit, maxnfeval, nfeval = 0, niter = 0;
  double *x = NULL, *low = NULL, *up = NULL, *scale = NULL, *offset = NULL;
  double f, eta, stepmx, accuracy, fmin, ftol, xtol, pgtol, rescale;

  if (!PyArg_ParseTuple(args, "OOOOOOiiiddddddddO",
      &py_function, &py_x0, &py_low, &py_up, &py_scale, &py_offset,
      &msg, &maxCGit, &maxnfeval, &eta, &stepmx, &accuracy, &fmin, &ftol,
      &xtol, &pgtol, &rescale, &py_callback)) return NULL;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (!PyCallable_Check(py_function))
  {
    PyErr_SetString(PyExc_TypeError, "tnc: function must be callable");
    return NULL;
  }

<<<<<<< HEAD
  scale = PyList_AsDoubleArray(py_scale, &n3);
  if (n3 != 0 && scale == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid scaling parameters.");
    return NULL;
  }

  offset = PyList_AsDoubleArray(py_offset, &n4);
  if (n4 != 0 && offset == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid offset parameters.");
    return NULL;
  }

  x = PyList_AsDoubleArray(py_x0, &n);
  if (n != 0 && x == NULL)
  {
    if (scale) free(scale);

    PyErr_SetString(PyExc_ValueError, "tnc: invalid initial vector.");
    return NULL;
  }

  low = PyList_AsDoubleArray(py_low, &n1);
  up = PyList_AsDoubleArray(py_up, &n2);

  if ((n1 != 0 && low == NULL) || (n2 != 0 && up == NULL))
  {
    if (scale) free(scale);
    if (x) free(x);
    if (low) free(low);
    if (up) free(up);

    PyErr_SetString(PyExc_ValueError, "tnc: invalid bounds.");
    return NULL;
=======
  arr_scale = (PyArrayObject *)PyArray_FROM_OTF((PyObject *)py_scale,
                                                NPY_DOUBLE, NPY_ARRAY_IN_ARRAY);
  if (arr_scale == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid scaling parameters.");
    goto failure;
  }
  if ((n3 = PyArray_Size((PyObject *)arr_scale)) != 0)
  {
    scale = (double *)PyArray_GETPTR1(arr_scale, 0);
    if (scale == NULL)
    {
      PyErr_SetString(PyExc_ValueError, "tnc: invalid scaling parameters.");
      goto failure;
    }
  }

  arr_offset = (PyArrayObject *)PyArray_FROM_OTF((PyObject *)py_offset,
                                                 NPY_DOUBLE, NPY_ARRAY_IN_ARRAY);
  if (arr_offset == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid offset parameters.");
    goto failure;
  }
  if ((n4 = PyArray_Size((PyObject *)arr_offset)) != 0)
  {
    offset = (double *)PyArray_GETPTR1(arr_offset, 0);
    if (offset == NULL)
    {
      PyErr_SetString(PyExc_ValueError, "tnc: invalid offset parameters.");
      goto failure;
    }
  }

  arr_x = (PyArrayObject *)PyArray_FROM_OTF((PyObject *)py_x0,
                                            NPY_DOUBLE, NPY_ARRAY_IN_ARRAY);
  if (arr_x == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid initial vector.");
    goto failure;
  }
  if ((n = PyArray_Size((PyObject *)arr_x)) != 0)
  {
    x = (double *)PyArray_GETPTR1(arr_x, 0);
    if (x == NULL)
    {
      PyErr_SetString(PyExc_ValueError, "tnc: invalid initial vector.");
      goto failure;
    }
  }

  arr_low = (PyArrayObject *)PyArray_FROM_OTF((PyObject *)py_low,
                                              NPY_DOUBLE, NPY_ARRAY_IN_ARRAY);
  if (arr_low == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid lower bound.");
    goto failure;
  }
  if ((n1 = PyArray_Size((PyObject *)arr_low)) != 0)
  {
    low = (double *)PyArray_GETPTR1(arr_low, 0);
    if (low == NULL)
    {
      PyErr_SetString(PyExc_ValueError, "tnc: invalid lower bound.");
      goto failure;
    }
  }
  arr_up = (PyArrayObject *)PyArray_FROM_OTF((PyObject *)py_up,
                                             NPY_DOUBLE, NPY_ARRAY_IN_ARRAY);
  if (arr_up == NULL)
  {
    PyErr_SetString(PyExc_ValueError, "tnc: invalid upper bound.");
    goto failure;
  }
  if ((n2 = PyArray_Size((PyObject *)arr_up)) != 0)
  {
    up = (double *)PyArray_GETPTR1(arr_up, 0);
    if (up == NULL)
    {
      PyErr_SetString(PyExc_ValueError, "tnc: invalid upper bound.");
      goto failure;
    }
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  }

  if (n1 != n2 || n != n1 || (scale != NULL && n != n3)
    || (offset != NULL && n != n4))
  {
<<<<<<< HEAD
    if (scale) free(scale);
    if (offset) free(offset);
    if (x) free(x);
    if (low) free(low);
    if (up) free(up);

    PyErr_SetString(PyExc_ValueError, "tnc: vector sizes must be equal.");
    return NULL;
  }

=======
    PyErr_SetString(PyExc_ValueError, "tnc: vector sizes must be equal.");
    goto failure;
  }

  if (py_callback != Py_None)
  {
      if (!PyCallable_Check(py_callback))
      {
        PyErr_SetString(PyExc_TypeError,
                        "tnc: callback must be callable or None.");
        goto failure;
      }
      py_state.py_callback = py_callback;
      Py_INCREF(py_callback);
      callback_function = callback;
  }

  Py_INCREF(py_function);

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  py_state.py_function = py_function;
  py_state.n = n;
  py_state.failed = 0;

<<<<<<< HEAD
  Py_INCREF(py_function);

  rc = tnc(n, x, &f, NULL, function, &py_state, low, up, scale, offset, msg,
    maxCGit, maxnfeval, eta, stepmx, accuracy, fmin, ftol, xtol, pgtol, rescale,
    &nfeval);

  Py_DECREF(py_function);

  if (low) free(low);
  if (up) free(up);
  if (scale) free(scale);
  if (offset) free(offset);

  if (py_state.failed)
  {
    if (x) free(x);
    return NULL;
  }
=======
  rc = tnc(n, x, &f, NULL, function, &py_state, low, up, scale, offset, msg,
    maxCGit, maxnfeval, eta, stepmx, accuracy, fmin, ftol, xtol, pgtol, rescale,
    &nfeval, &niter, callback_function);

  Py_DECREF(py_function);

  if (py_callback != Py_None) Py_DECREF(py_callback);

  if (py_state.failed) goto failure;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

  if (rc == TNC_ENOMEM)
  {
    PyErr_SetString(PyExc_MemoryError, "tnc: memory allocation failed.");
<<<<<<< HEAD
    if (x) free(x);
    return NULL;
  }

  py_list = PyDoubleArray_AsList(n, x);
  if (x) free(x);
  if (py_list == NULL)
  {
    PyErr_SetString(PyExc_MemoryError, "tnc: memory allocation failed.");
    return NULL;
  }

  return Py_BuildValue("(iiN)", rc, nfeval, py_list);;
=======
    goto failure;
  }

  Py_DECREF(arr_scale);
  Py_DECREF(arr_offset);
  Py_DECREF(arr_low);
  Py_DECREF(arr_up);

  return Py_BuildValue("(iiiN)", rc, nfeval, niter, PyArray_Return(arr_x));

failure:
  Py_XDECREF(arr_scale);
  Py_XDECREF(arr_offset);
  Py_XDECREF(arr_low);
  Py_XDECREF(arr_up);
  Py_XDECREF(arr_x);
  return NULL;
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
}

static PyMethodDef moduleTNC_methods[] =
{
  {"minimize", moduleTNC_minimize, METH_VARARGS},
  {NULL, NULL}
};

<<<<<<< HEAD
PyMODINIT_FUNC initmoduleTNC(void)
{
  (void) Py_InitModule("moduleTNC", moduleTNC_methods);
}
=======
#if PY_VERSION_HEX >= 0x03000000
static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT,
    "moduleTNC",
    NULL,
    -1,
    moduleTNC_methods,
    NULL,
    NULL,
    NULL,
    NULL
};

PyObject *PyInit_moduleTNC(void)
{
    import_array();
    return PyModule_Create(&moduledef);
}
#else
PyMODINIT_FUNC initmoduleTNC(void)
{
  (void) Py_InitModule("moduleTNC", moduleTNC_methods);
  import_array();
}
#endif
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

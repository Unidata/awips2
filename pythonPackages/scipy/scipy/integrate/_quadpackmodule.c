/*
  From Multipack project
 */
#include "quadpack.h"
<<<<<<< HEAD
static PyObject *quadpack_error;
=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
#include "__quadpack.h"
static struct PyMethodDef quadpack_module_methods[] = {
{"_qagse", quadpack_qagse, METH_VARARGS, doc_qagse},
{"_qagie", quadpack_qagie, METH_VARARGS, doc_qagie},
{"_qagpe", quadpack_qagpe, METH_VARARGS, doc_qagpe},
{"_qawoe", quadpack_qawoe, METH_VARARGS, doc_qawoe},
{"_qawfe", quadpack_qawfe, METH_VARARGS, doc_qawfe},
{"_qawse", quadpack_qawse, METH_VARARGS, doc_qawse},
{"_qawce", quadpack_qawce, METH_VARARGS, doc_qawce},
{NULL,		NULL, 0, NULL}
};
<<<<<<< HEAD
=======

#if PY_VERSION_HEX >= 0x03000000
static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT,
    "_quadpack",
    NULL,
    -1,
    quadpack_module_methods,
    NULL,
    NULL,
    NULL,
    NULL
};

PyObject *PyInit__quadpack(void)
{
    PyObject *m, *d, *s;

    m = PyModule_Create(&moduledef);
    import_array();
    d = PyModule_GetDict(m);

    s = PyUString_FromString(" 1.13 ");
    PyDict_SetItemString(d, "__version__", s);
    quadpack_error = PyErr_NewException ("quadpack.error", NULL, NULL);
    Py_DECREF(s);
    PyDict_SetItemString(d, "error", quadpack_error);
    if (PyErr_Occurred()) {
        Py_FatalError("can't initialize module quadpack");
    }
    return m;
}
#else
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
PyMODINIT_FUNC init_quadpack(void) {
  PyObject *m, *d, *s;
  m = Py_InitModule("_quadpack", quadpack_module_methods);
  import_array();
  d = PyModule_GetDict(m);

<<<<<<< HEAD
  s = PyString_FromString(" 1.13 ");
=======
  s = PyUString_FromString(" 1.13 ");
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
  PyDict_SetItemString(d, "__version__", s);
  quadpack_error = PyErr_NewException ("quadpack.error", NULL, NULL);
  Py_DECREF(s);
  PyDict_SetItemString(d, "error", quadpack_error);
  if (PyErr_Occurred())
    Py_FatalError("can't initialize module quadpack");
}
<<<<<<< HEAD
        
=======
#endif
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

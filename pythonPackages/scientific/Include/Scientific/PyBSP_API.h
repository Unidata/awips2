/*
 * C API functions
 */

#define PyBSP_Sync_RET void
#define PyBSP_Sync_PROTO Py_PROTO((void))
#define PyBSP_Sync_NUM 0

#define PyBSP_SetTagSize_RET void
#define PyBSP_SetTagSize_PROTO Py_PROTO((int *tag_nbytes))
#define PyBSP_SetTagSize_NUM 1

#define PyBSP_Send_RET void
#define PyBSP_Send_PROTO Py_PROTO((int pid, const void *tag, const void *payload, int payload_nbytes))
#define PyBSP_Send_NUM 2

#define PyBSP_QSize_RET void
#define PyBSP_QSize_PROTO Py_PROTO((int *nmessages, int *accum_nbytes))
#define PyBSP_QSize_NUM 3

#define PyBSP_GetTag_RET void
#define PyBSP_GetTag_PROTO Py_PROTO((int *status, void *tag))
#define PyBSP_GetTag_NUM 4

#define PyBSP_Move_RET void
#define PyBSP_Move_PROTO Py_PROTO((void *payload, int reception_nbytes))
#define PyBSP_Move_NUM 5

#define PyBSP_HPMove_RET int
#define PyBSP_HPMove_PROTO Py_PROTO((void **tag_ptr, void **payload_ptr))
#define PyBSP_HPMove_NUM 6

#define PyBSP_SendString_RET int
#define PyBSP_SendString_PROTO Py_PROTO((PyStringObject *string, int dest_pid))
#define PyBSP_SendString_NUM 7

#define PyBSP_SendArray_RET int
#define PyBSP_SendArray_PROTO Py_PROTO((PyArrayObject *array, int dest_pid))
#define PyBSP_SendArray_NUM 8

#define PyBSP_NumberOfObjects_RET int
#define PyBSP_NumberOfObjects_PROTO Py_PROTO((void))
#define PyBSP_NumberOfObjects_NUM 9

#define PyBSP_ReceiveObject_RET PyObject *
#define PyBSP_ReceiveObject_PROTO Py_PROTO((void))
#define PyBSP_ReceiveObject_NUM 10

#define PyBSP_API_pointers 11

#ifdef _BSP_MODULE

static PyBSP_Sync_RET PyBSP_Sync PyBSP_Sync_PROTO;

static PyBSP_SetTagSize_RET PyBSP_SetTagSize PyBSP_SetTagSize_PROTO;

static PyBSP_Send_RET PyBSP_Send PyBSP_Send_PROTO;

static PyBSP_QSize_RET PyBSP_QSize PyBSP_QSize_PROTO;

static PyBSP_GetTag_RET PyBSP_GetTag PyBSP_GetTag_PROTO;

static PyBSP_Move_RET PyBSP_Move PyBSP_Move_PROTO;

static PyBSP_HPMove_RET PyBSP_HPMove PyBSP_HPMove_PROTO;

static PyBSP_SendString_RET PyBSP_SendString PyBSP_SendString_PROTO;

static PyBSP_SendArray_RET PyBSP_SendArray PyBSP_SendArray_PROTO;

static PyBSP_NumberOfObjects_RET PyBSP_NumberOfObjects PyBSP_NumberOfObjects_PROTO;

static PyBSP_ReceiveObject_RET PyBSP_ReceiveObject PyBSP_ReceiveObject_PROTO;

#define set_PyBSP_API_pointers(){ \
   PyBSP_API[PyBSP_Sync_NUM] = (void *)&PyBSP_Sync; \
   PyBSP_API[PyBSP_SetTagSize_NUM] = (void *)&PyBSP_SetTagSize; \
   PyBSP_API[PyBSP_Send_NUM] = (void *)&PyBSP_Send; \
   PyBSP_API[PyBSP_QSize_NUM] = (void *)&PyBSP_QSize; \
   PyBSP_API[PyBSP_GetTag_NUM] = (void *)&PyBSP_GetTag; \
   PyBSP_API[PyBSP_Move_NUM] = (void *)&PyBSP_Move; \
   PyBSP_API[PyBSP_HPMove_NUM] = (void *)&PyBSP_HPMove; \
   PyBSP_API[PyBSP_SendString_NUM] = (void *)&PyBSP_SendString; \
   PyBSP_API[PyBSP_SendArray_NUM] = (void *)&PyBSP_SendArray; \
   PyBSP_API[PyBSP_NumberOfObjects_NUM] = (void *)&PyBSP_NumberOfObjects; \
   PyBSP_API[PyBSP_ReceiveObject_NUM] = (void *)&PyBSP_ReceiveObject; \
}

#else

static void **PyBSP_API;

#define PyBSP_Sync \
  (*(PyBSP_Sync_RET (*)PyBSP_Sync_PROTO) \
   PyBSP_API[PyBSP_Sync_NUM])

#define PyBSP_SetTagSize \
  (*(PyBSP_SetTagSize_RET (*)PyBSP_SetTagSize_PROTO) \
   PyBSP_API[PyBSP_SetTagSize_NUM])

#define PyBSP_Send \
  (*(PyBSP_Send_RET (*)PyBSP_Send_PROTO) \
   PyBSP_API[PyBSP_Send_NUM])

#define PyBSP_QSize \
  (*(PyBSP_QSize_RET (*)PyBSP_QSize_PROTO) \
   PyBSP_API[PyBSP_QSize_NUM])

#define PyBSP_GetTag \
  (*(PyBSP_GetTag_RET (*)PyBSP_GetTag_PROTO) \
   PyBSP_API[PyBSP_GetTag_NUM])

#define PyBSP_Move \
  (*(PyBSP_Move_RET (*)PyBSP_Move_PROTO) \
   PyBSP_API[PyBSP_Move_NUM])

#define PyBSP_HPMove \
  (*(PyBSP_HPMove_RET (*)PyBSP_HPMove_PROTO) \
   PyBSP_API[PyBSP_HPMove_NUM])

#define PyBSP_SendString \
  (*(PyBSP_SendString_RET (*)PyBSP_SendString_PROTO) \
   PyBSP_API[PyBSP_SendString_NUM])

#define PyBSP_SendArray \
  (*(PyBSP_SendArray_RET (*)PyBSP_SendArray_PROTO) \
   PyBSP_API[PyBSP_SendArray_NUM])

#define PyBSP_NumberOfObjects \
  (*(PyBSP_NumberOfObjects_RET (*)PyBSP_NumberOfObjects_PROTO) \
   PyBSP_API[PyBSP_NumberOfObjects_NUM])

#define PyBSP_ReceiveObject \
  (*(PyBSP_ReceiveObject_RET (*)PyBSP_ReceiveObject_PROTO) \
   PyBSP_API[PyBSP_ReceiveObject_NUM])



#define import_bsplib() \
{ \
  PyObject *module = PyImport_ImportModule("Scientific.BSPlib"); \
  if (module != NULL) { \
    PyObject *module_dict = PyModule_GetDict(module); \
    PyObject *c_api_object = PyDict_GetItemString(module_dict, "_C_API"); \
    if (PyCObject_Check(c_api_object)) { \
      PyBSP_API = (void **)PyCObject_AsVoidPtr(c_api_object); \
    } \
  } \
}

#endif

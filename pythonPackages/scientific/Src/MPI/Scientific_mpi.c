/*
 * Low-level MPI interface routines
 *
 * Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
 *        and Jakob Schiotz <schiotz@fysik.dtu.dk>
 *        and Ciro Cattuto <ciro@prosa.it>
 * last revision: 2004-3-29
 */


#include "Python.h"
#include "assert.h"

#define _MPI_MODULE
#include "Scientific/mpimodule.h"

#define PyMPI_TEST(rc, funcname) if ((rc) != MPI_SUCCESS) \
	return PyMPI_SetError(rc, funcname)

/* Global variables */

PyObject *PyExc_MPIError;
const int max_tag = 32767;  /* Minimal value required by MPI norm */

/* We need a few forward declarations */
staticforward PyMPIRequestObject *
newPyMPIRequestObject(MPI_Request rq, PyObject *buffer, int operation,
		      MPI_Datatype mpi_type);

staticforward PyMPIOperationObject *
newPyMPIOperationObject(MPI_Op mpi_op, char *op_name);


/* Utility functions */

static MPI_Datatype
mpi_type(int py_type)
{
  switch(py_type) {
  case PyArray_CHAR:
    return MPI_CHAR;
  case PyArray_UBYTE:
  case PyArray_SBYTE:
    return MPI_BYTE;
  case PyArray_SHORT:
    return MPI_SHORT;
  case PyArray_INT:
    return MPI_INT;
  case PyArray_LONG:
    return MPI_LONG;
  case PyArray_FLOAT:
  case PyArray_CFLOAT:
    return MPI_FLOAT;
  case PyArray_DOUBLE:
  case PyArray_CDOUBLE:
    return MPI_DOUBLE;
  }
  return 0;
}

/* ...should probably be a macro */
static MPI_Op
mpi_op(PyMPIOperationObject *op)
{
  return op->mpi_op;
}

/* Doubles the count for sending/receiving complex numbers. */
static int
mpi_count_factor(int py_type)
{
  switch(py_type) {
  case PyArray_CFLOAT:
  case PyArray_CDOUBLE:
    return 2;
  default:
    return 1;
  }
}

static PyObject *
PyMPI_SetError(int errcode, char *funcname)
{
  char mpierr[MPI_MAX_ERROR_STRING];
  int errlen;
  
  MPI_Error_string(errcode, mpierr, &errlen);
  PyErr_Format(PyExc_MPIError, "%s failed: %s", funcname, mpierr);
  return NULL;
}


/***************************************/
/* MPI Operation object implementation */
/***************************************/

staticforward PyTypeObject PyMPIOperation_Type;

static PyMPIOperationObject *
newPyMPIOperationObject(MPI_Op mpi_op, char *op_name)
{
  PyMPIOperationObject *self;

  self = PyObject_NEW(PyMPIOperationObject, &PyMPIOperation_Type);
  if (self == NULL)
    return NULL;
  self->mpi_op = mpi_op;
	strcpy(self->op_name, op_name);
  return self;
}

/* Deallocate the object */
static void
PyMPIOperation_dealloc(PyMPIOperationObject *self)
{
  PyObject_FREE(self);
}

/* __repr__ */
/* should check for rbuffer overflow */
static PyObject *
PyMPIOperation_repr(PyMPIOperationObject *self)
{
  char rbuffer[256];

  sprintf(rbuffer,
	  "<PyMPIOperation at %lx: operation = %s>",
	  (long)self, self->op_name);
  return PyString_FromString(rbuffer);
}

statichere PyTypeObject PyMPIOperation_Type = {
	PyObject_HEAD_INIT(NULL)
	0,				/*ob_size*/
	"PyMPIOperation",		/*tp_name*/
	sizeof(PyMPIOperationObject),	/*tp_basicsize*/
	0,				/*tp_itemsize*/
	/* methods */
	(destructor) PyMPIOperation_dealloc, /*tp_dealloc*/
	0,				/*tp_print*/
	0, 				/*tp_getattr*/
	0,                      	/*tp_setattr*/
	0,				/*tp_compare*/
	(reprfunc) PyMPIOperation_repr,	/*tp_repr*/
	0,				/*tp_as_number*/
	0,				/*tp_as_sequence*/
	0,				/*tp_as_mapping*/
	0,				/*tp_hash*/
	0,				/*tp_call*/
	0				  /*tp_str*/
};

/******************************************/
/* MPI communicator object implementation */
/******************************************/

staticforward PyTypeObject PyMPICommunicator_Type;

static PyMPICommunicatorObject *
newPyMPICommunicatorObject(MPI_Comm handle)
{
  PyMPICommunicatorObject *self;

  self = PyObject_NEW(PyMPICommunicatorObject, &PyMPICommunicator_Type);
  if (self == NULL)
    return NULL;
  self->handle = handle;
  if (MPI_Comm_rank(handle, &self->rank) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "couldn't obtain communicator rank");
    PyObject_FREE(self);
    return NULL;
  }
  if (MPI_Comm_size(handle, &self->size) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "couldn't obtain communicator size");
    PyObject_FREE(self);
    return NULL;
  }
  return self;
}

/*
 * PyMPICommunicator methods
 */

static void
PyMPICommunicator_dealloc(PyMPICommunicatorObject *self)
{
  if (self->handle != MPI_COMM_WORLD)
    MPI_Comm_free(&self->handle);
  PyObject_FREE(self);
}


/* Duplicate */

static PyObject *
PyMPI_DuplicateCommunicator(PyMPICommunicatorObject *comm)
{
  MPI_Comm new;
  if (MPI_Comm_dup(comm->handle, &new) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Comm_dup failed");
    return NULL;
  }
  return (PyObject *)newPyMPICommunicatorObject(new);
}

static PyObject *
PyMPICommunicator_duplicate(PyMPICommunicatorObject *self, PyObject *args)
{
  if (!PyArg_ParseTuple(args, ""))
    return NULL;
  return PyMPI_DuplicateCommunicator(self);
}


/* Subset */

static PyObject *
PyMPI_SubsetCommunicator(PyMPICommunicatorObject *comm, PyArrayObject *array)
{
  MPI_Group group;
  MPI_Group newgroup;
  MPI_Comm new;
  int *ranks;
  int dimension, i1, i2;

  if (MPI_Comm_group(comm->handle, &group) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Comm_group failed");
    return NULL;
  }
  dimension = array->dimensions[0];
  ranks = (int*)(array->data);
  for (i1 = 0; i1 < dimension; i1++) {
    if (ranks[i1] < 0 || ranks[i1] >= comm->size) {
      PyErr_SetString(PyExc_MPIError, "invalid MPI rank");
      return NULL;
    }
    for (i2 = 0; i2 < i1; i2++)
      if (ranks[i1] == ranks[i2]) {
	PyErr_SetString(PyExc_MPIError, "duplicated MPI rank");
	return NULL;
      }
  }
  if (MPI_Group_incl(group, dimension, ranks, &newgroup) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Group_incl failed");
    return NULL;
  }
  if (MPI_Comm_create(comm->handle, newgroup, &new) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Comm_create failed");
    return NULL;
  }
  if (MPI_Group_free(&newgroup) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Group_free failed");
    return NULL;
  }
  if (MPI_Group_free(&group) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Group_free failed");
    return NULL;
  }
  if (new == MPI_COMM_NULL) {
    Py_INCREF(Py_None);
    return Py_None;
  }
  else
    return (PyObject *)newPyMPICommunicatorObject(new);
}

static PyObject *
PyMPICommunicator_subset(PyMPICommunicatorObject *self, PyObject *args)
{
  PyObject *ranks;
  PyObject *new;
  if (!PyArg_ParseTuple(args, "O", &ranks))
    return NULL;
  ranks = PyArray_ContiguousFromObject(ranks, PyArray_INT, 1, 1);
  if (ranks == NULL)
    return NULL;
  new = PyMPI_SubsetCommunicator(self, (PyArrayObject *)ranks);
  Py_DECREF(ranks);
  return new;
}


/* Barrier */

static int
PyMPI_Barrier(PyMPICommunicatorObject *comm)
{
  return MPI_Barrier(comm->handle);
}

static PyObject *
PyMPICommunicator_barrier(PyMPICommunicatorObject *self, PyObject *args)
{
  if (!PyArg_ParseTuple(args, ""))
    return NULL;
  if (PyMPI_Barrier(self) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Barrier failed");
    return NULL;
  }
  Py_INCREF(Py_None);
  return Py_None;
}

/* Send data */

static int
PyMPI_Send(PyMPICommunicatorObject *comm, void *data, int type, int len,
	   int dest, int tag)
{
  return MPI_Send(data, len, mpi_type(type), dest, tag, comm->handle);
}

static int
PyMPI_SendArray(PyMPICommunicatorObject *comm, PyArrayObject *array,
		int dest, int tag)
{
  int count;
  int error;
  int i;

  if (tag < 0 || tag > max_tag) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI tag");
    return -1;
  }
  if (dest < 0 || dest >= comm->size) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI destination");
    return -1;
  }
  if (PyArray_ISCONTIGUOUS(array))
    Py_INCREF(array);
  else {
    array = (PyArrayObject *)PyArray_ContiguousFromObject((PyObject *)array,
							  PyArray_NOTYPE,
							  0, 0);
    if (array == NULL)
      return -1;
  }
  count = 1;
  for (i = 0; i < array->nd; i++)
    count *= array->dimensions[i];
  count *= mpi_count_factor(array->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Send(comm, array->data, array->descr->type_num,
		     count, dest, tag);
  Py_END_ALLOW_THREADS;
  Py_DECREF(array);
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Send failed");
    return -1;
  }
  return 0;
}

static int
PyMPI_SendString(PyMPICommunicatorObject *comm, PyStringObject *string,
		int dest, int tag)
{
  char *data;
  int error, count;

  if (tag < 0 || tag > max_tag) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI tag");
    return -1;
  }
  if (dest < 0 || dest >= comm->size) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI destination");
    return -1;
  }
  data = PyString_AsString((PyObject *)string);
  count = PyString_GET_SIZE(string);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Send(comm, data, PyArray_CHAR, count, dest, tag);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Send failed");
    return -1;
  }
  return 0;
}

static PyObject *
PyMPICommunicator_send(PyMPICommunicatorObject *self, PyObject *args)
{
  PyObject *data;
  int dest, tag;

  if (!PyArg_ParseTuple(args, "Oii", &data, &dest, &tag))
    return NULL;
  /* #define PyArray_Check(op) PyObject_TypeCheck(op, &PyArray_Type) */
  /*  if (PyArray_Check(data)) { */
  if (PyObject_TypeCheck(data, &PyArray_Type)) {
    if (PyMPI_SendArray(self, (PyArrayObject *)data, dest, tag) != 0)
      return NULL;
  }
  else if (PyString_Check(data)) {
    if (PyMPI_SendString(self, (PyStringObject *)data, dest, tag) != 0)
      return NULL;
  }
  else {
    PyErr_SetString(PyExc_MPIError, "can send only array or string");
    return NULL;
  }
  Py_INCREF(Py_None);
  return Py_None;
}

/* Nonblocking send data */

static PyObject *
PyMPI_SendNonBlocking(PyMPICommunicatorObject *comm, void *buffer,
		      PyObject* buffer_owner, int type, int len, int dest,
		      int tag)
{
  MPI_Request rq;
  int rc;
  
  if (tag < 0 || tag > max_tag) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI tag");
    return NULL;
  }
  if (dest < 0 || dest >= comm->size) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI destination");
    return NULL;
  }
  Py_BEGIN_ALLOW_THREADS;
  rc = MPI_Isend(buffer, len, mpi_type(type), dest, tag, comm->handle, &rq);
  Py_END_ALLOW_THREADS;
  PyMPI_TEST(rc, "MPI_Isend");
  return (PyObject *) newPyMPIRequestObject(rq, buffer_owner,
					    PyMPIRequestSend, mpi_type(type));
}

static PyObject *
PyMPI_SendArrayNonBlocking(PyMPICommunicatorObject *comm, PyArrayObject *array,
			   int dest, int tag)
{
  PyObject *request;
  int count;
  int i;
  
  /* Make sure array is contiguous.  Just increase refcount if it is */
  array = (PyArrayObject *) PyArray_ContiguousFromObject((PyObject *) array,
							 PyArray_NOTYPE, 0, 0);
  if (array == NULL)
    return NULL;
  count = 1;
  for (i = 0; i < array->nd; i++)
    count *= array->dimensions[i];
  count *= mpi_count_factor(array->descr->type_num);
  request = PyMPI_SendNonBlocking(comm, array->data, (PyObject *) array,
				  array->descr->type_num, count,
				  dest, tag);
  Py_DECREF(array);
  return request;
}

static PyObject *
PyMPI_SendStringNonBlocking(PyMPICommunicatorObject *comm,
			    PyStringObject *string, int dest, int tag)
{
  PyObject *request;
  char *data;
  int count;
  
  data = PyString_AS_STRING(string);
  count = PyString_GET_SIZE(string);
  request = PyMPI_SendNonBlocking(comm, data, (PyObject *) string,
			 	  PyArray_CHAR, count, dest, tag);
  return request;
}

static PyObject *
PyMPICommunicator_nonblocking_send(PyMPICommunicatorObject *self,
				   PyObject *args)
{
  PyObject *data;
  int dest, tag;

  if (!PyArg_ParseTuple(args, "Oii", &data, &dest, &tag))
    return NULL;
  if (PyArray_Check(data))
    return PyMPI_SendArrayNonBlocking(self, (PyArrayObject *) data, dest, tag);
  if (PyString_Check(data))
    return PyMPI_SendStringNonBlocking(self, (PyStringObject *) data, dest,
				       tag);
  PyErr_SetString(PyExc_MPIError, "can only send an array or a string");
  return NULL;
}

/* Receive data */

static int
PyMPI_Receive(PyMPICommunicatorObject *comm, void *buffer,
	      int type, int len,
	      int source, int tag, int *sourcep, int *tagp, int *lenp)
{
  MPI_Status status;
  int error;

  error = MPI_Recv(buffer, len, mpi_type(type), source, tag,
		   comm->handle, &status);
  if (error == MPI_SUCCESS) {
    if (sourcep != NULL)
      *sourcep = status.MPI_SOURCE;
    if (tagp != NULL)
      *tagp = status.MPI_TAG;
    if (lenp != NULL)
      error = MPI_Get_count(&status, mpi_type(type), lenp);
  }
  return error;
}

static int
PyMPI_ReceiveArray(PyMPICommunicatorObject *comm, PyArrayObject *array,
		   int source, int tag, int *sourcep, int *tagp, int *lenp)
{
  int count, i;
  int error;

  count = 1;
  for (i = 0; i < array->nd; i++)
    count *= array->dimensions[i];
  count *= mpi_count_factor(array->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Receive(comm, array->data, array->descr->type_num,
			count, source, tag, sourcep, tagp, lenp);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Recv failed");
    return -1;
  }
  return 0;
}

static PyObject *
PyMPI_ReceiveString(PyMPICommunicatorObject *comm,
		    int source, int tag, int *sourcep, int *tagp)
{
  MPI_Status status;
  PyStringObject *string;
  int count;
  int error;

  Py_BEGIN_ALLOW_THREADS;
  error = MPI_Probe(source, tag, comm->handle, &status);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Probe failed");
    return NULL;
  }
  source = status.MPI_SOURCE;
  tag = status.MPI_TAG;
  if (MPI_Get_count(&status, MPI_CHAR, &count) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Get_count failed");
    return NULL;
  }
  string = (PyStringObject *)PyString_FromStringAndSize(NULL, count);
  if (string == NULL)
    return NULL;
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Receive(comm, PyString_AsString((PyObject *)string),
			PyArray_CHAR, count,
			source, tag, sourcep, tagp, NULL);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Recv failed");
    Py_DECREF(string);
    return NULL;
  }
  return (PyObject *)string;
}

static PyObject *
PyMPICommunicator_receive(PyMPICommunicatorObject *self, PyObject *args)
{
  PyArrayObject *array;
  PyObject *buffer_ob, *source_ob, *tag_ob, *return_ob;
  int source, tag, count;
  int error;

  source_ob = Py_None;
  tag_ob = Py_None;
  if (!PyArg_ParseTuple(args, "O|OO", &buffer_ob, &source_ob, &tag_ob))
    return NULL;
  if (source_ob == Py_None)
    source = MPI_ANY_SOURCE;
  else if (!PyInt_Check(source_ob)
	   || (source = PyInt_AsLong(source_ob)) < 0
	   || source >= self->size) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI source");
    return NULL;
  }
  if (tag_ob == Py_None)
    tag = MPI_ANY_TAG;
  else if (!PyInt_Check(tag_ob)
	   || (tag = PyInt_AsLong(tag_ob)) < 0
	   || tag >= max_tag) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI tag");
    return NULL;
  }

  if (PyArray_Check(buffer_ob)) {
    array = (PyArrayObject *)buffer_ob;
    if (!PyArray_ISCONTIGUOUS(array)) {
      PyErr_SetString(PyExc_ValueError, "buffer must be contiguous");
      return NULL;
    }
    Py_INCREF(array);
  }
  else if (PyString_Check(buffer_ob)) {
    MPI_Status status;
    char type_code = PyString_AsString(buffer_ob)[0];
    int type = PyArray_DescrFromType(type_code)->type_num;
    int factor;
    Py_BEGIN_ALLOW_THREADS;
    error = MPI_Probe(source, tag, self->handle, &status);
    Py_END_ALLOW_THREADS;
    if (error != MPI_SUCCESS) {
      PyErr_SetString(PyExc_MPIError, "MPI_Probe failed");
      return NULL;
    }
    source = status.MPI_SOURCE;
    tag = status.MPI_TAG;
    if (MPI_Get_count(&status, mpi_type(type), &count) != MPI_SUCCESS) {
      PyErr_SetString(PyExc_MPIError, "MPI_Get_count failed");
      return NULL;
    }
    factor = mpi_count_factor(type);
    if (count == MPI_UNDEFINED || count % factor != 0) {
      PyErr_SetString(PyExc_MPIError,
		      "buffer data type incompatible with message");
      return NULL;
    }
    count /= factor;
    array = (PyArrayObject *)PyArray_FromDims(1, &count, type);
    if (array == NULL)
      return NULL;
  }

  if (PyMPI_ReceiveArray(self, array, source, tag,
			 &source, &tag, &count) != 0) {
    Py_DECREF(array);
    return NULL;
  }

  return_ob = Py_BuildValue("Oiii", array, source, tag, count);
  Py_DECREF(array);
  return return_ob;
}

static PyObject *
PyMPICommunicator_receiveString(PyMPICommunicatorObject *self, PyObject *args)
{
  PyObject *source_ob = Py_None, *tag_ob = Py_None, *return_ob;
  PyObject *string;
  int source, tag;

  if (!PyArg_ParseTuple(args, "|OO", &source_ob, &tag_ob))
    return NULL;
  if (source_ob == Py_None)
    source = MPI_ANY_SOURCE;
  else if (!PyInt_Check(source_ob)
	   || (source = PyInt_AsLong(source_ob)) < 0
	   || source >= self->size) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI source");
    return NULL;
  }
  if (tag_ob == Py_None)
    tag = MPI_ANY_TAG;
  else if (!PyInt_Check(tag_ob)
	   || (tag = PyInt_AsLong(tag_ob)) < 0
	   || tag >= max_tag) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI tag");
    return NULL;
  }

  string = PyMPI_ReceiveString(self, source, tag, &source, &tag);
  if (string == NULL)
    return NULL;

  return_ob = Py_BuildValue("Oii", string, source, tag);
  Py_DECREF(string);
  return return_ob;

}

/* Nonblocking receive */

static PyObject *
PyMPI_ReceiveArrayNonBlocking(PyMPICommunicatorObject *comm,
			      PyArrayObject *array, int source, int tag)
{
  int count, i;
  MPI_Request rq;
  int error;
  MPI_Datatype type;

  count = 1;
  for (i = 0; i < array->nd; i++)
    count *= array->dimensions[i];
  count *= mpi_count_factor(array->descr->type_num);
  type = mpi_type(array->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = MPI_Irecv(array->data, count, type, source, tag, comm->handle, &rq);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Irecv failed");
    return NULL;
  }
  return (PyObject *) newPyMPIRequestObject(rq, (PyObject *) array,
					    PyMPIRequestReceive, type);
}

static PyObject *
PyMPICommunicator_nonblocking_receive(PyMPICommunicatorObject *self,
				      PyObject *args)
{
  PyArrayObject *array;
  PyObject *source_ob, *tag_ob;
  int source, tag;

  source_ob = Py_None;
  tag_ob = Py_None;
  if (!PyArg_ParseTuple(args, "O!|OO", &PyArray_Type, &array,
			&source_ob, &tag_ob))
    return NULL;
  if (source_ob == Py_None)
    source = MPI_ANY_SOURCE;
  else if (!PyInt_Check(source_ob)
	   || (source = PyInt_AsLong(source_ob)) < 0
	   || source >= self->size) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI source");
    return NULL;
  }
  if (tag_ob == Py_None)
    tag = MPI_ANY_TAG;
  else if (!PyInt_Check(tag_ob)
	   || (tag = PyInt_AsLong(tag_ob)) < 0
	   || tag >= max_tag) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI tag");
    return NULL;
  }

  if (!PyArray_ISCONTIGUOUS(array)) {
    PyErr_SetString(PyExc_ValueError, "buffer must be contiguous");
    return NULL;
  }

  return PyMPI_ReceiveArrayNonBlocking(self, array, source, tag);
}

/* Nonblocking Probe */

static int
PyMPI_ProbeNonBlocking(PyMPICommunicatorObject *comm, int source, int tag,
		       int *flagp, int *sourcep, int *tagp)
{
  MPI_Status status;
  int error;

  error = MPI_Iprobe(source, tag, comm->handle, flagp, &status);
  if (error == MPI_SUCCESS) {
    if (sourcep != NULL)
      *sourcep = status.MPI_SOURCE;
    if (tagp != NULL)
      *tagp = status.MPI_TAG;
  }
  return error;
}

static PyObject *
PyMPICommunicator_nonblocking_probe(PyMPICommunicatorObject *self,
				    PyObject *args)
{
  PyObject *source_ob, *tag_ob;
  int source, tag, available;
  int rc;

  source_ob = Py_None;
  tag_ob = Py_None;
  if (!PyArg_ParseTuple(args, "|OO", &source_ob, &tag_ob))
    return NULL;
  if (source_ob == Py_None)
    source = MPI_ANY_SOURCE;
  else if (!PyInt_Check(source_ob)
	   || (source = PyInt_AsLong(source_ob)) < 0
	   || source >= self->size) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI source");
    return NULL;
  }
  if (tag_ob == Py_None)
    tag = MPI_ANY_TAG;
  else if (!PyInt_Check(tag_ob)
	   || (tag = PyInt_AsLong(tag_ob)) < 0
	   || tag >= max_tag) {
    PyErr_SetString(PyExc_TypeError, "invalid MPI tag");
    return NULL;
  }

  Py_BEGIN_ALLOW_THREADS;
  rc = PyMPI_ProbeNonBlocking(self, source, tag, &available,
			      &source, &tag);
  Py_END_ALLOW_THREADS;
  PyMPI_TEST(rc, "MPI_Iprobe");

  if (available)
    return Py_BuildValue("ii", source, tag);
  else {
    Py_INCREF(Py_None);
    return Py_None;
  }
}


/* Broadcast */

static int
PyMPI_Broadcast(PyMPICommunicatorObject *comm, void *buffer,
		int type, int count, int root)
{
  return MPI_Bcast(buffer, count, mpi_type(type), root, comm->handle);
}

static int
PyMPI_BroadcastArray(PyMPICommunicatorObject *comm,
		     PyArrayObject *array, int root)
{
  int count;
  int error;
  int i;

  if (root < 0 || root >= comm->size) {
    PyErr_SetString(PyExc_MPIError, "invalid MPI rank");
    return -1;
  }
  if (!PyArray_ISCONTIGUOUS(array)) {
    PyErr_SetString(PyExc_ValueError, "array must be contiguous");
    return -1;
  }
  count = 1;
  for (i = 0; i < array->nd; i++)
    count *= array->dimensions[i];
  count *= mpi_count_factor(array->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Broadcast(comm, array->data, array->descr->type_num,
			  count, root);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Bcast failed");
    return -1;
  }
  return 0;
}

static PyObject *
PyMPICommunicator_broadcast(PyMPICommunicatorObject *self, PyObject *args)
{
  PyArrayObject *array;
  int root;
  if (!PyArg_ParseTuple(args, "O!i",
			&PyArray_Type, &array, &root))
    return NULL;
  if (PyMPI_BroadcastArray(self, array, root) != 0)
    return NULL;
  Py_INCREF(Py_None);
  return Py_None;
}


/* Share data between all processes (Allgather) */

static int
PyMPI_Share(PyMPICommunicatorObject *comm, void *send, void *receive,
	    int type, int count)
{
  return MPI_Allgather(send, count, mpi_type(type), receive, count,
		       mpi_type(type), comm->handle);
}

static int
PyMPI_ShareArray(PyMPICommunicatorObject *comm, PyArrayObject *send,
		 PyArrayObject *receive)
{
  int count, error, compatible;
  int i;

  compatible = (receive->nd == send->nd+1);
  compatible = compatible && (receive->dimensions[0] == comm->size);
  if (compatible)
    for (i = 0; i < send->nd; i++)
      if (send->dimensions[i] != receive->dimensions[i+1])
	compatible = 0;
  compatible = compatible &&
               (send->descr->type_num == receive->descr->type_num);
  if (!compatible) {
    PyErr_SetString(PyExc_MPIError,
		    "send and receive arrays are not compatible");
    return -1;
  }

  count = 1;
  for (i = 0; i < send->nd; i++)
    count *= send->dimensions[i];
  count *= mpi_count_factor(send->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Share(comm, send->data, receive->data, send->descr->type_num, count);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Allgather failed");
    return -1;
  }
  return 0;
}

static PyObject *
PyMPICommunicator_share(PyMPICommunicatorObject *self, PyObject *args)
{
  PyArrayObject *send, *receive;
  if (!PyArg_ParseTuple(args, "O!O!",
			&PyArray_Type, &send,
			&PyArray_Type, &receive))
    return NULL;
  if (PyMPI_ShareArray(self, send, receive) != 0)
    return NULL;
  Py_INCREF(Py_None);
  return Py_None;
}

/* Abort - for emergency use only */

static int
PyMPI_Abort(PyMPICommunicatorObject *comm, int err)
{
  return MPI_Abort(comm->handle, err);
}

static PyObject *
PyMPICommunicator_abort(PyMPICommunicatorObject *self, PyObject *args)
{
  int errcode;
    
  if (!PyArg_ParseTuple(args, "i", &errcode))
    return NULL;
  if (PyMPI_Abort(self, errcode) != MPI_SUCCESS) 
  {
    PyErr_SetString(PyExc_MPIError, "MPI_Abort failed");
    return NULL;
  }
    
  Py_INCREF(Py_None);
  return Py_None;
}

/* Reduce and Allreduce*/

static int
PyMPI_Reduce(PyMPICommunicatorObject *comm, void *sendbuf, void *recvbuf,
	     int count, int datatype, PyMPIOperationObject *op, int root)
{
  return MPI_Reduce(sendbuf, recvbuf, count, mpi_type(datatype), mpi_op(op), root, comm->handle);
}

static int
PyMPI_Allreduce(PyMPICommunicatorObject *comm, void *sendbuf, void *recvbuf,
		int count, int datatype, PyMPIOperationObject *op)
{
  return MPI_Allreduce(sendbuf, recvbuf, count, mpi_type(datatype), mpi_op(op), comm->handle);
}

static int
PyMPI_ReduceArray(PyMPICommunicatorObject *comm, PyArrayObject *send,
		  PyArrayObject *receive, PyMPIOperationObject *op, int root)
{
  int compatible, count, error;
  int i;

  compatible = (receive->nd == send->nd);
  count = 1;
  if (compatible) {    
    for (i = 0; i < send->nd; i++) {
      compatible = compatible && (receive->dimensions[i] ==
				  send->dimensions[i]);
      count *= send->dimensions[i];
    }
  }
  compatible = compatible && (receive->descr->type_num ==
			      send->descr->type_num);
  
  if (!compatible) {
    PyErr_SetString(PyExc_MPIError,
		    "send and receive arrays are not compatible.");
    return -1;
  }

  count *= mpi_count_factor(send->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Reduce(comm, send->data, receive->data, count, send->descr->type_num, op, root);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Reduce failed");
    return -1;
  }
  return 0;
}

static int
PyMPI_AllreduceArray(PyMPICommunicatorObject *comm, PyArrayObject *send,
		     PyArrayObject *receive, PyMPIOperationObject *op)
{
  int compatible, count, error;
  int i;

  compatible = (receive->nd == send->nd);
  count = 1;
  if (compatible) {    
    for (i = 0; i < send->nd; i++) {
      compatible = compatible && (receive->dimensions[i] ==
				  send->dimensions[i]);
      count *= send->dimensions[i];
    }
  }
  compatible = compatible && (receive->descr->type_num ==
			      send->descr->type_num);
  
  if (!compatible) {
    PyErr_SetString(PyExc_MPIError,
		    "send and receive arrays are not compatible.");
    return -1;
  }

  count *= mpi_count_factor(send->descr->type_num);
  Py_BEGIN_ALLOW_THREADS;
  error = PyMPI_Allreduce(comm, send->data, receive->data, count, send->descr->type_num, op);
  Py_END_ALLOW_THREADS;
  if (error != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Allreduce failed");
    return -1;
  }
  return 0;
}

static PyObject *
PyMPICommunicator_reduce(PyMPICommunicatorObject *self, PyObject *args)
{
  PyArrayObject *send, *receive;
  PyMPIOperationObject *operation;
  int root;

  if (!PyArg_ParseTuple(args, "O!O!O!i",
			&PyArray_Type, &send,
			&PyArray_Type, &receive,
			&PyMPIOperation_Type, &operation,
			&root))
    return NULL;
  if (PyMPI_ReduceArray(self, send, receive, operation, root) != 0)
    return NULL;
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
PyMPICommunicator_allreduce(PyMPICommunicatorObject *self, PyObject *args)
{
  PyArrayObject *send, *receive;
  PyMPIOperationObject *operation;

  if (!PyArg_ParseTuple(args, "O!O!O!",
			&PyArray_Type, &send,
			&PyArray_Type, &receive,
			&PyMPIOperation_Type, &operation))
    return NULL;
  if (PyMPI_AllreduceArray(self, send, receive, operation) != 0)
    return NULL;
  Py_INCREF(Py_None);
  return Py_None;
}

/* Attribute access */

static PyMethodDef PyMPICommunicator_methods[] = {
  {"duplicate", (PyCFunction)PyMPICommunicator_duplicate, 1},
  {"subset", (PyCFunction)PyMPICommunicator_subset, 1},
  {"send", (PyCFunction)PyMPICommunicator_send, 1},
  {"nonblocking_send", (PyCFunction)PyMPICommunicator_nonblocking_send, 1},
  {"nonblockingSend", (PyCFunction)PyMPICommunicator_nonblocking_send, 1},
  {"receive", (PyCFunction)PyMPICommunicator_receive, 1},
  {"receiveString", (PyCFunction)PyMPICommunicator_receiveString, 1},
  {"nonblocking_receive", (PyCFunction)PyMPICommunicator_nonblocking_receive,
   1},
  {"nonblockingReceive", (PyCFunction)PyMPICommunicator_nonblocking_receive,
   1},
  {"nonblockingProbe", (PyCFunction)PyMPICommunicator_nonblocking_probe,
   1},
  {"broadcast", (PyCFunction)PyMPICommunicator_broadcast, 1},
  {"share", (PyCFunction)PyMPICommunicator_share, 1},
  {"barrier", (PyCFunction)PyMPICommunicator_barrier, 1},
  {"abort", (PyCFunction)PyMPICommunicator_abort, 1},
  {"reduce", (PyCFunction)PyMPICommunicator_reduce, 1},
  {"allreduce", (PyCFunction)PyMPICommunicator_allreduce, 1},
  {NULL, NULL}
};

static PyObject *
PyMPICommunicator_getattr(PyMPICommunicatorObject *self, char *name)
{
  if (strcmp(name, "rank") == 0) {
    return PyInt_FromLong((long)self->rank);
  }
  else if (strcmp(name, "size") == 0) {
    return PyInt_FromLong((long)self->size);
  }
  else
    return Py_FindMethod(PyMPICommunicator_methods, (PyObject *)self, name);
}

statichere PyTypeObject PyMPICommunicator_Type = {
	/* The ob_type field must be initialized in the module init function
	 * to be portable to Windows without using C++. */
	PyObject_HEAD_INIT(NULL)
	0,			/*ob_size*/
	"PyMPICommunicator",			/*tp_name*/
	sizeof(PyMPICommunicatorObject),	/*tp_basicsize*/
	0,			/*tp_itemsize*/
	/* methods */
	(destructor)PyMPICommunicator_dealloc, /*tp_dealloc*/
	0,			/*tp_print*/
	(getattrfunc)PyMPICommunicator_getattr, /*tp_getattr*/
	0,                      /*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};

/*************************************/
/* MPI Request object implementation */
/*************************************/

staticforward PyTypeObject PyMPIRequest_Type;

static PyMPIRequestObject *
newPyMPIRequestObject(MPI_Request rq, PyObject *buffer, int operation,
		      MPI_Datatype mpi_type)
{
  PyMPIRequestObject *self;

  self = PyObject_NEW(PyMPIRequestObject, &PyMPIRequest_Type);
  if (self == NULL)
    return NULL;
  self->handle[0] = rq;
  self->operation = operation;
  Py_INCREF(buffer);
  self->buffer = buffer;
  self->mpi_type = mpi_type;
  self->active = 1;
  return self;
}

/* Deallocate the object */
static void
PyMPIRequest_dealloc(PyMPIRequestObject *self)
{
  Py_XDECREF(self->buffer);  /* Release an eventual reference to the buffer */
  PyObject_FREE(self);
}

/* __repr__ */

static PyObject *
PyMPIRequest_repr(PyMPIRequestObject *self)
{
  char rbuffer[256];
  char opbuffer[50];

  switch (self->operation) {
  case PyMPIRequestSend:
    strcpy(opbuffer, "send");
    break;
  case PyMPIRequestReceive:
    strcpy(opbuffer, "receive");
    break;
  default:
    sprintf(opbuffer, "*** INVALID (%d) ***", self->operation);
  }
  
  sprintf(rbuffer,
	  "<PyMPIRequest at %lx: handle = %ld, operation = %s, status = %s>",
	  (long)self, (long)self->handle, opbuffer,
	  (self->active ? "active" : "expired"));
  return PyString_FromString(rbuffer);
}

/* Wait for a nonblocking operation */

/* The argument is void * to keep MPI_Status out of C API, where it caused
   trouble */
static int
PyMPI_Wait(PyMPIRequestObject *self, void *s)
{
  return MPI_Wait(self->handle, (MPI_Status *) s);
}

static PyObject *
PyMPIRequest_wait(PyMPIRequestObject *self, PyObject *args)
{
  PyObject *return_ob;
  MPI_Status stat;
  int source, tag, count;
  
  if (!PyArg_ParseTuple(args, ""))
    return NULL;
  if (!self->active) {
    PyErr_SetString(PyExc_MPIError, "Cannot wait on expired request");
    return NULL;
  }

  if (PyMPI_Wait(self, &stat) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Wait failed");
    return NULL;
  }

  assert(self->buffer->ob_refcnt > 0);
  /* fprintf(stderr, "REFCOUNT: %d\n", (int) self->buffer->ob_refcnt); */
  switch (self->operation) {

  case PyMPIRequestSend:
    self->active = 0;      /* The operation has completed */
    Py_DECREF(self->buffer);  /* Discard the buffer */
    self->buffer = NULL;
    
    Py_INCREF(Py_None);
    return Py_None;

  case PyMPIRequestReceive:
    self->active = 0;      /* The operation has completed */
    source = stat.MPI_SOURCE;
    tag = stat.MPI_TAG;
    if (MPI_Get_count(&stat, self->mpi_type, &count) != MPI_SUCCESS) {
      PyErr_SetString(PyExc_MPIError, "MPI_Get_count failed");
      return NULL;
    }
    return_ob = Py_BuildValue("Oiii", self->buffer, source, tag, count);
    Py_DECREF(self->buffer);
    self->buffer = NULL;

    return return_ob;
  }
  PyErr_SetString(PyExc_MPIError, "Invalid operation field in request object");
  return NULL;
}

/* Test a nonblocking operation for completion */

/* The argument is void * to keep MPI_Status out of C API, where it caused
   trouble */
static int
PyMPI_Test(PyMPIRequestObject *self, int *flag, void *s)
{
  return MPI_Test(self->handle, flag, (MPI_Status *) s);
}

static PyObject *
PyMPIRequest_test(PyMPIRequestObject *self, PyObject *args)
{
  PyObject *return_ob;
  MPI_Status stat;
  int flag, source, tag, count;

  if (!PyArg_ParseTuple(args, ""))
    return NULL;
  if (!self->active) {
    PyErr_SetString(PyExc_MPIError, "Cannot test expired request.");
    return NULL;
  }

  if (PyMPI_Test(self, &flag, &stat) != MPI_SUCCESS) {
    PyErr_SetString(PyExc_MPIError, "MPI_Test failed.");
    return NULL;
  }

  if (!flag) {
    /* The operation has not completed.  Return None */
    Py_INCREF(Py_False);
    return Py_False;
  }

  /* The operation has completed.  In case of a send operation, return true.
     In case of a receive operation, return the buffer, the source, the tag,
     and the count (from the MPI_Status object).  In both cases, remove the
     buffer. */
  switch (self->operation) {

  case PyMPIRequestSend:
    self->active = 0;      /* The operation has completed */
    Py_DECREF(self->buffer);  /* Discard the buffer */
    self->buffer = NULL;
    Py_INCREF(Py_True);
    return Py_True;

  case PyMPIRequestReceive:
    self->active = 0;      /* The operation has completed */
    source = stat.MPI_SOURCE;
    tag = stat.MPI_TAG;
    if (MPI_Get_count(&stat, self->mpi_type, &count) != MPI_SUCCESS) {
      PyErr_SetString(PyExc_MPIError, "MPI_Get_count failed");
      return NULL;
    }
    return_ob = Py_BuildValue("Oiii", self->buffer, source, tag, count);
    Py_DECREF(self->buffer);
    self->buffer = NULL;

    return return_ob;
  }
  PyErr_SetString(PyExc_MPIError, "Invalid operation field in request object");
  return NULL;
}
  

/* Attribute access */

static PyMethodDef PyMPIRequest_methods[] = {
  {"wait", (PyCFunction)PyMPIRequest_wait, METH_VARARGS},
  {"test", (PyCFunction)PyMPIRequest_test, METH_VARARGS},
  {NULL, NULL}
};

static PyObject *
PyMPIRequest_getattr(PyMPICommunicatorObject *self, char *name)
{
  return Py_FindMethod(PyMPIRequest_methods, (PyObject *)self, name);
}

statichere PyTypeObject PyMPIRequest_Type = {
	PyObject_HEAD_INIT(NULL)
	0,				/*ob_size*/
	"PyMPIRequest",			/*tp_name*/
	sizeof(PyMPIRequestObject),	/*tp_basicsize*/
	0,				/*tp_itemsize*/
	/* methods */
	(destructor) PyMPIRequest_dealloc, /*tp_dealloc*/
	0,				/*tp_print*/
	(getattrfunc) PyMPIRequest_getattr, /*tp_getattr*/
	0,                      	/*tp_setattr*/
	0,				/*tp_compare*/
	(reprfunc) PyMPIRequest_repr,	/*tp_repr*/
	0,				/*tp_as_number*/
	0,				/*tp_as_sequence*/
	0,				/*tp_as_mapping*/
	0,				/*tp_hash*/
	0,				/*tp_call*/
	0				/*tp_str*/
};

/*****************************/
/* Error object registration */
/*****************************/

static PyObject *
register_error_object(PyObject *dummy, PyObject *args)
{
  if (!PyArg_ParseTuple(args, "O", &PyExc_MPIError))
    return NULL;
  Py_INCREF(PyExc_MPIError);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************************/
/* Table of functions defined in the module */
/********************************************/

static PyMethodDef mpi_methods[] = {
  {"_registerErrorObject", register_error_object, 1},
  {NULL, NULL}		/* sentinel */
};

/*************************/
/* Module initialization */
/*************************/

/* a handy macro to create/register PyMPIOperation objects */
#define NEW_OP(mpi_op,op_name) do { \
	PyObject *op = (PyObject *) newPyMPIOperationObject(mpi_op,op_name); \
	PyDict_SetItemString(d, op_name, op); \
	} while (0)

DL_EXPORT(void)
initScientific_mpi(void)
{
  PyObject *m, *d, *world;
  static void *PyMPI_API[PyMPI_API_pointers];
  int mpi_init_flag;

  /* Set type of locally created static objects. */
  PyMPIOperation_Type.ob_type    = &PyType_Type;
  PyMPICommunicator_Type.ob_type = &PyType_Type;
  PyMPIRequest_Type.ob_type      = &PyType_Type;

  /* Create the module */
  m = Py_InitModule("Scientific_mpi", mpi_methods);
  d = PyModule_GetDict(m);

  /* Initialize C API pointer array and store in module */
  set_PyMPI_API_pointers();
  PyDict_SetItemString(d, "_C_API", PyCObject_FromVoidPtr(PyMPI_API, NULL));

  /* Import the array module */
  import_array();
  if (PyErr_Occurred()) {
    PyErr_SetString(PyExc_ImportError, "Can\'t import Numeric.");
    return;
  }

  /* Check that MPI has been initialized */
  if (MPI_Initialized(&mpi_init_flag) != MPI_SUCCESS || !mpi_init_flag) {
    Py_INCREF(Py_None);
    PyDict_SetItemString(d, "world", Py_None);
#if 0
    fprintf(stderr, "Use mpipython to run this program!\n");
    _exit(1);
#endif
  }
  else {
    /* Create the world communicator object */
    world = (PyObject *)newPyMPICommunicatorObject(MPI_COMM_WORLD);
    PyDict_SetItemString(d, "world", world);

    NEW_OP(MPI_MAX,     "max");
    NEW_OP(MPI_MIN,     "min");
    NEW_OP(MPI_SUM,     "sum");
    NEW_OP(MPI_PROD,    "prod");
    NEW_OP(MPI_LAND,    "land");
    NEW_OP(MPI_BAND,    "band");
    NEW_OP(MPI_LOR,     "lor");
    NEW_OP(MPI_BOR,     "bor");
    NEW_OP(MPI_LXOR,    "lxor");
    NEW_OP(MPI_BXOR,    "bxor");
    NEW_OP(MPI_MAXLOC,  "maxloc");
    NEW_OP(MPI_MINLOC,  "minloc");
#if MPI_VERSION > 1
    NEW_OP(MPI_REPLACE, "replace");
#endif
  }

  /* Check for errors */
  if (PyErr_Occurred())
    PyErr_SetString(PyExc_ImportError, "Can\'t initialize module.");
}


/* Keep Konrad Hinsens indentation style when using cc mode in (x)emacs. */
/* Local Variables: */
/* c-basic-offset: 2 */
/* c-hanging-braces-alist: ((brace-list-open) (substatement-open after) (class-open after) (class-close before) (block-close . c-snug-do-while)) */
/* End: */

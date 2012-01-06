/*
 * Include file for MPI interface.
 *
 * Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
 *        and Jakob Schiotz <schiotz@fysik.dtu.dk>
 *        and Ciro Cattuto <ciro@prosa.it>
 * last revision: 2001-3-27
 */

#ifndef Py_MPIMODULE_H
#define Py_MPIMODULE_H

/* INSTRUCTIONS:

   This header file is used to create Python extension modules that
   use MPI (either manually or using SWIG).  It exports the necessary
   functions from the Python executable using the standard CObject
   method.

   If possible, use the PyMPI functions defined in
   Scientific/PyMPI_API.h .  They take a Python communicator object as
   their first argument.

   In some cases, e.g. when using source code shared between a Python
   module and a stand-alone executable, it is more practical to use
   ordinary MPI calls (MPI_Send() etc).  This header files also
   exports many MPI_XXXX symbols from the MPI library linked into the
   mpipython executable.

   WARNING: If you use an MPI call that is not exported through this
   header file, you risk to either get undefined symbols in your
   module, or to load two copies of the MPI library.  See the
   "troubleshooting" section below.

   INSTRUCTIONS FOR SINGLE FILE MODULES:
   If you are building an extension module in a single source file,
   just #include this header file, and remember to call the macro
   import_mpi() in the module initialization code.

   INSTRUCTIONS FOR MULTIPLE FILE MODULES:
   The PyMPI_API array of pointers must be shared and cannot therefore
   be declared static. In all file except one, write
       #define PYMPI_API_LINKAGE extern
       #include "Scientific/mpimodule.h"
   and in one file (e.g. where the module initialization is), write
       #define PYMPI_API_LINKAGE
       #include "Scientific/mpimodule.h"
   Again, remember to call import_mpi() in the module initialization
   code.
   
   Having an externally linked PyMPI_API array creates the risk of
   colliding symbols if you load multiple MPI-aware modules into
   Python.  This is probably harmless, but can be avoided by changing
   the name of the array, e.g. by writing
       #define PyMPI_API PyMPI_API_MYMODULENAME
   before Scientific/mpimodule.h is included (this must of course be
   done in _all_ .c files in your extension module).

   TROUBLESHOOTING:
   This header file has to include the original mpi.h in order to get
   the definitions of the MPI data types (in particular MPI_Status and
   MPI_Request).  This means that the compiler will not warn you, if
   you use an MPI_XXXX function that is not exported through the usual
   Python mechanism.  Instead, you will probably get linkage problems
   when linking the module or when loading it into Python (or, on some
   platforms, two copies of the MPI library loaded into Python).  The
   symptoms may vary from platform to platform (errors when importing
   the module, weird coredumps, errors from MPI, ...).  To check for
   this problem, use nm on your module or on the .o files:
       nm mymodule.so | grep MPI
       nm *.o | grep MPI
   The only output from this should be PyMPI_API.  If there are other
   MPI symbols, you should probably extend the list of functions
   exported through this header file.

   EXTENDING THE LIST OF EXPORTED MPI FUNCTIONS:  
   Add the missing functions to the end of Src/Scientific_mpi.export,
   (the syntax should be obvious).  Then run makeheader.py, and copy
   the resulting PyMPI_API.h file to Scientific/Includes/Scientific .
   IMPORTANT: Remember to rebuild the python executable containing
   Scientific.MPI (mpipython) and ALL your own modules using MPI!
*/

#ifdef __cplusplus
extern "C" {
#endif

#include "Scientific/arrayobject.h"
#include "mpi.h"

#ifndef MPI_VERSION
#define MPI_VERSION 1
#endif
#ifndef MPI_SUBVERSION
#define MPI_SUBVERSION 0
#endif


#ifdef MS_WINDOWS
#ifndef snprintf
#define snprintf _snprintf
#endif
#endif

/* MPI communicator object */

typedef struct {
  PyObject_HEAD
  MPI_Comm handle;
  int rank;
  int size;
} PyMPICommunicatorObject;

#define PyMPICommunicatorObject_Check(v) \
        ((v)->ob_type == &PyMPICommunicator_Type)
 
/* MPI request object */
 
typedef struct {
  PyObject_HEAD
  MPI_Request handle[1];
  int active;       /* Is it still valid, or has the request been processed? */
  int operation;    /* 0 = receive, 1 = send */
  PyObject *buffer; /* The buffer being used */
  MPI_Datatype mpi_type;  /* The type of data send */
} PyMPIRequestObject;

#define PyMPIRequestObject_Check(v) \
	((v)->ob_type == &PyMPIRequest_Type)

#define PyMPIRequestReceive 0
#define PyMPIRequestSend 1

/* MPI operation object */

#define OP_NAME_LEN 16
typedef struct {
  PyObject_HEAD
  MPI_Op mpi_op; /* operation type */
  char op_name[OP_NAME_LEN]; /* operation name */
} PyMPIOperationObject;

#define PyMPIOperationObject_Check(v) \
	((v)->ob_type == &PyMPIOperation_Type)

/* Include the automatically generated API definitions */
#include "Scientific/PyMPI_API.h"

#ifdef __cplusplus
}
#endif
#endif /* Py_MPIMODULE_H */

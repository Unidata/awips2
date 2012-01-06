/* MPI version of python.c. MPI initialization must occur before
   Python starts up. */

#include "Python.h"
#include "mpi.h"

extern DL_EXPORT(int) Py_Main(int, char **);
extern DL_EXPORT(void) initScientific_mpi(void);

int
main(int argc, char **argv)
{
  int return_code;
  MPI_Init(&argc, &argv);
  MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

  Py_Initialize();
  initScientific_mpi();

  return_code = Py_Main(argc, argv);

  MPI_Finalize();
  return return_code;
}

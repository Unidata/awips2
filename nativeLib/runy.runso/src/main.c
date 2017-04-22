/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

/* SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2008             jelkins     Initial creation
 * Apr 3, 2009			   jelkins     portland fortran support
 *
 * @author: jelkins
 */

/*
 * Load a library and execute a function within the library with the given arguments.
 *
 * The library function is expected to have the same signature as a main routine.
 *
 */
int main(int argc, char ** argv) {

    /* --- Parse the arguments -------------------------------------------- */

    if ( argc <= 2 ) {
        puts("Usage: library function [args]");
        exit(1);
    }

    char * library  = argv[1];
    char * function = argv[2];
    char * function_args[argc - 2];

    function_args[0] = function;

    int i;
    for (i=1; i < argc - 2; i++) {

        function_args[i] = argv[i+2];

    }

    /* --- add lib and so to the library name --- */

    char library_name [50];

    sprintf(library_name, "lib%s.so", library);

    /* --- Load the library ------------------------------------------------ */

    void* library_handle = dlopen(library_name,RTLD_LAZY);

    if (!library_handle) {
        fputs(dlerror(), stderr);
        fputs("\n",stderr);
        return 1;
    }

    int (*function_handle) (int,char**) = dlsym(library_handle, function);


    char * error = dlerror();
    if (error != NULL) {
        fputs(error, stderr);
        fputs("\n",stderr);
        return 1;
    }

    int (*__pgio_set_argc) (int) = dlsym(library_handle,"__pgio_set_argc");
    int (*__pgio_set_argv) (char **) = dlsym(library_handle,"__pgio_set_argv");

    if (dlerror() == NULL ) {

    	(*__pgio_set_argc) (argc - 2);
    	(*__pgio_set_argv) (function_args);

    }

    void (*_gfortran_set_args) (int, char**) = dlsym(library_handle,"_gfortran_set_args");

    if (dlerror() == NULL ) {

    	(*_gfortran_set_args) (argc - 2,function_args);

    }



    int rVal = 0;

    rVal = (*function_handle) (argc - 2, function_args);

    dlclose(library_handle);

    return rVal;

}

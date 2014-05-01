/*********************************************************
   
   Exits the geo utility program and closes the database
   and the text file.
   
   ********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsAccess.h"
#include "DbmsDefs.h"


void exit_geoutil_file(FILE *filePtr)
{
   
   /* close the file */
   
   fclose(filePtr);
   
   exit(1);
   
}


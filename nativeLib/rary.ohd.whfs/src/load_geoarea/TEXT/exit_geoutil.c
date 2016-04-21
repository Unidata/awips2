/*********************************************************
   
   Exits the geo utility program and closes the database
   and the text file.
   
   ********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsAccess.h"
#include "DbmsDefs.h"


#include "geo_dbutil.h"


void exit_geoutil(FILE *filePtr)
{
   int status;
   
   
   /* close the file and database */
   
   fclose(filePtr);
   
   status = CloseDbms();
   
   exit(-1);
   
}


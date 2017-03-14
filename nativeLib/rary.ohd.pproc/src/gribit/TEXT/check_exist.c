/* Routine to check if a directory or file exists.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "create_fortran_link.h"

create_fortran_link( void, check_exist, (char *name, char *type, int *iexist, int iprint), (name, type, iexist, iprint)) {

struct stat buf;
int ldebug, istat;

   ldebug=0;

   if (ldebug > 0) printf ("enter check_exist\n");

   if (ldebug > 0) printf ("name=%s type=%s\n",name,type);

   *iexist=1;

   if (strcmp(type,"file") == 0 && name[strlen(name)-1] == '/') {
      if (iprint == 1) printf ("ERROR: file %s should be a %s but is a directory.\n",name,type);
      *iexist=-1;
      return;
      }

   istat=stat(name,&buf);

   if (ldebug > 0) printf ("istat=%i\n",istat);

/* check if found */
   if (istat == -1) {
      if (iprint == 1) printf ("ERROR: %s %s not found.\n",type,name);
      *iexist=0;
      return;
      }

   if (ldebug > 0) printf ("buf.st_blocks=%i\n",buf.st_blocks);

/* check size */
   if (strcmp(type,"file") == 0 && buf.st_blocks == 0) {
      if (iprint == 1) printf ("ERROR: %s %s found but has a size of zero.\n",type,name);
      *iexist=-2;
      return;
      }



}

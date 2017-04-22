#include <stdlib.h>
#include <string.h>

#include "save_grib.h"
#include "stage3.h"
#include "version.h"

/****************************************************************************/
/*   FUNCTION NAME:  save_grib()                                            */
/*       FUNCTION:   generate grib encoded file using the gribit executable */
/*                   and copy file for display through D2D                  */
/***************************************************************************

Function type:
   char *  

Called by function:
   save_rfcwide 

gribfile = name of grib encoded file (without qualifier) (not full pathname)
lenfn = length of fname string
xmrgfile = input file name in xmrg file format
leninf = length of infile string

leninf and lenfn are necessary to be passed into this routine because
 when this routine is called from FORTAN (by mpe_fieldgen), the strings
 are not NULL terminated

gribit is normally called via the gribits script

This function returns a dynamically allocated pointer to a copy of
the command used to invoke the process_grib_files script.  This can
be used by the calling application for logging.  The caller should first
test this pointer for NULLness.  The caller is responsible for deallocating
this character string when done with it.

Modification History

Bryon Lawrence      May 11, 2006      Set up to return command string 
                                      used to invoke the process_grib_files
                                      script.  The calling application
                                      can use this for logging purposes.
*******************************************************************/
char * save_grib ( char xmrgfile[XMRG_FILE_LENGTH],
	           int *leninf, 
                   char gribfile[XMRG_FILE_LENGTH], 
                   int *lenfn)
{
   char * pCommandString = NULL;
   int len;
   char bindir[100], command[512];
   char xmf[30], grf[30];

   memset (xmf, '\0', XMRG_FILE_LENGTH);
   memset (grf, '\0', XMRG_FILE_LENGTH);

   strncpy (xmf, xmrgfile, *leninf);
   strncpy (grf, gribfile, *lenfn);

 /*--------------------------------------------------------------*/
 /*   construct command and execute                              */
 /*   process_grib_files is a new script which calls gribit and  */
 /*     also copies and renames the file for display through D2D */
 /*--------------------------------------------------------------*/

   len = strlen ("pproc_bin");
   get_apps_defaults ("pproc_bin", &len, bindir, &len);

   sprintf (command, "%s/process_grib_files %s %s", bindir, xmf, grf);
   system (command);

   pCommandString = strdup (command);
   return pCommandString;
}

/*------------------------------------------------------------------------------
  getuno - get the unit number to use for a NWSRFS file
  ------------------------------------------------------------------------------
  notes: (1) It is assumed that this program will be called from a
             Fortran routine.  Consequently, all arguments to it are
             pointers.  It is further assumed that any character
             strings passed to this routine are terminated with a
             space character which can be replaced by a NULL
             character in this routine.
         (2) All data is initially treated as character strings to
             allow flexible input.
  ------------------------------------------------------------------------------
  variables:
   anyflag      .... flag indicating whether a file can be assigned to "any
                     unit number"
   dchars       .... delimiting characters
   fileunit     .... array which indicates status of file unit numbers
   ierr         .... error flag if the unit number cannot be assigned
   key0         .... keyword indicating file to open (with space)
   key          .... keyword indicating file to open (with NULL)
   keylen       .... length of "key"
   maxfiles     .... maximum number of files that can be open
   program0     .... program that is requesting unit number (with space)
   program      .... program that is requesting unit number (with NULL)
   pt           .... pointer to a string token
   rkey         .... key read from data file
   rprogram     .... program read from data file
   runit1       .... lower limit on unit number , read from data file, as string
   runit2       .... upper limit on unit number, read from data file, as string
   string       .... string used for line read from file
   pathname     .... full path to file containing file unit information
   in_file      .... pointer to FILEUNIT file
   unit         .... unit number that is set if it is found
   unitfound    .... indicates whether a unit number has been found
   unit1        .... lower limit on unit, as integer
   unit2        .... upper limit on unit, as integer
  ------------------------------------------------------------------------------
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common/unitno.h"

#define MAXC 256

extern int  get_apps_defaults ( char *, int *, char *, int * );

int getuno ( int *maxfiles, int *fileunit, char *program0, char *key0,
             int *unit, int *ierr )
{
FILE  *in_file;
int   anyflag, i, keylen, programlen, unitfound, unit1, unit2;
int   len, len2;
char  apps_var[25];
char  dchars[3], key[MAXC], program[MAXC], *pt, rkey[MAXC],
      rprogram[MAXC], runit1[MAXC], runit2[MAXC],
      string[MAXC], pathname[MAXC];

   *ierr = 0;
   strcpy ( dchars, " \t" );
   *unit = 0;
   unitfound = 0;

/* Get the path to the FILEUNIT file */
   strcpy (apps_var,"rfs_sys_dir");
   len=strlen(apps_var);
   if ( get_apps_defaults(apps_var, &len, pathname, &len2) ) {
      fprintf ( stderr, "**ERROR** apps_defaults %s not specified.\n",apps_var);
      *ierr = 1;
      return *ierr;
      }
      
   strcat ( pathname, "/FILEUNIT" );

/* Add NULL to input strings */

   strcpy ( program, program0 );
   pt = program;
   while ( *pt != ' ' ) ++pt;
   *pt = '\0';
   programlen = strlen ( program );

   strcpy ( key, key0 );
   pt = key;
   while ( *pt != ' ' ) ++pt;
   *pt = '\0';
   keylen = strlen ( key );

/* Read through the units file */
   in_file = fopen ( pathname, "r" );
   if ( ! in_file ) {
      fprintf ( stderr, "**ERROR** Unable to open file %s.\n",pathname );
      *ierr = 1;
      return *ierr;
      }
   while ( fgets(string,MAXC,in_file) ) {
      anyflag     = 0;
      rkey[0]     = '\0';
      rprogram[0] = '\0';
      runit1[0]   = '\0';
      runit2[0]   = '\0';
   /* get program name */
      pt = string;
      pt = strtok ( pt, dchars );
      if ( pt ) strcpy ( rprogram, pt );
      if ( rprogram[0] == '\n' || rprogram[0] == '#' )
          continue;
   /* get key */
      pt = strtok ( NULL, dchars );
      if ( pt ) strcpy ( rkey, pt );
   /* get starting unit number in range */
      pt = strtok ( NULL, dchars );
      if ( pt ) strcpy ( runit1, pt );
      if ( ! runit1[0] ) {
         fprintf ( stderr, "**ERROR** No unit number specified for program '%s' and key '%s'.\n",
                   rprogram, rkey );
         fclose ( in_file );
         *ierr = 1;
         return *ierr;
         }
       if ( runit1[0] == '?' )
          anyflag = 1;
          else
             unit1 = atoi ( runit1 );
          /* get ending unit number in range */
             if ( ! anyflag ) {
                pt = strtok ( NULL, dchars );
                if ( pt && ((*pt != '\n') && (*pt != '#')) )
                   strcpy ( runit2, pt );
                 if ( runit2[0] )
                    unit2 = atoi ( runit2 );
                    else
                       unit2 = unit1;
          }
   /* check if the requested program and key match the values read from the file 
      (case must match) */
      if ( ! strcmp("DEFAULT",rprogram) && ! strcmp(key,rkey) ) {
      /* this is the default value for the keyword -
         set the unit number to the default */
          *unit = unit1;
          unitfound = 1;
          }
          else 
             if ( ! strcmp(program,rprogram) && ! strcmp(key,rkey) ) {
             /* this is a match - try to determine a free unit number */
                if ( anyflag ) {
                /* search units 1 through the maximum number of unit numbers */
                   unit1 = 1; 
                   unit2 = *maxfiles;
                   }
            /* use a free unit number in the specified range of unit numbers */
               for ( i = unit1; i <= unit2; i++ ) {
                  if ( fileunit[i - 1] & NWSRFS_FILE_RESERVED ) {
                  /* in the future allow a reserved unit number */
                  }
                  else 
                     if ( ! fileunit[i - 1] ) {
                     /* have found an unused unit number */
                        *unit = i;
                        unitfound = 1;
                        break;
                        }
                    }
                }
             if ( unitfound ) 
                break;
        }

    fclose ( in_file );

    if ( ! unitfound )
       *ierr = 1;

    return *ierr;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/getuno.c,v $";
 static char rcs_id2[] = "$Id: getuno.c,v 1.3 2001/06/12 18:50:39 dws Exp $";}
/*  ===================================================  */

}

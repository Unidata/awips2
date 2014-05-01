/* Routine to check the access mode of a directory or file.

*/

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <stdio.h>      /* needed for fopen */
#include "create_fortran_link.h"


create_fortran_link( void, check_access_mode, (char *pathname, char *mode, int *ireturn, int *iprint), (pathname, mode, ireturn, iprint)) {

struct stat buf;
struct tm *t_local;
struct tm *t_gmt;
int ldebug, istat;
int iokay;
int imode1, imode2, imode3;
char string[6];
char cmode1[2], cmode2[2], cmode3[2];
char appsvar[25]; int lappsvar;
char timezone[7]; int ltimezone;
char datetime[20]; int ldatetime=20;
char unixcmd[150];

   ldebug=0;

   if (ldebug > 0) printf ("enter check_mode\n");

   if (ldebug > 0) printf ("pathname=%s mode=%s\n",pathname,mode);

   *ireturn=0;

/* get status information */
   istat=stat(pathname,&buf);

   if (ldebug > 0) printf ("stat called : istat=%i\n",istat);

/* check if found */
   if (istat == -1) {
      if (ldebug > 0) printf ("errno=%i\n",errno);
      if (*iprint == 1) printf ("ERROR in check_mode: pathname %s not found.\n",pathname);
      *ireturn=1;
      return;
      }

   sprintf (unixcmd,"ls -l %s",pathname);
   if (ldebug > 0) system(unixcmd);

/* set file access modes */
   sprintf (string,"%o",buf.st_mode);
   if (ldebug > 0) printf ("buf.st_mode=%o (in octal format) \n",buf.st_mode);
   /* st_basemode not on Linux ; printf ("buf.st_basemode=%o (in octal format) \n",buf.st_basemode);*/
   if (ldebug > 0) printf ("string=%s \n",string);
   strncpy (cmode1,&string[3],1); cmode1[1] = 0;
   if (ldebug > 0) printf ("cmode1=%s \n",cmode1);
   imode1=atoi(cmode1);
   if (ldebug > 0) printf ("imode1=%i \n",imode1);
   strncpy (cmode2,&string[4],1); cmode2[1] = 0;
   if (ldebug > 0) printf ("cmode2=%s \n",cmode2);
   imode2=atoi(cmode2);
   if (ldebug > 0) printf ("imode2=%i \n",imode2);
   strncpy (cmode3,&string[5],1); cmode3[1] = 0;
   if (ldebug > 0) printf ("cmode3=%s \n",cmode3);
   imode3=atoi(cmode3);
   if (ldebug > 0) printf ("imode3=%i \n",imode3);

/* check file access mode */
   iokay=0;
   if (strcmp(mode,"r" ) == 0) iokay=1;
   if (strcmp(mode,"rw") == 0) iokay=1;
   if (iokay == 0) {
      if (*iprint == 1) {
         printf ("ERROR in check_mode: pathname %s cannot be checked for mode '%s'.\n",
             pathname,mode);
         }
      *ireturn=2;
      return;
      }
   iokay=0;
   if (strcmp(mode,"r" ) == 0 && imode1 == 4) iokay=1;
   if (strcmp(mode,"r" ) == 0 && imode1 == 6) iokay=1;
   if (strcmp(mode,"r" ) == 0 && imode1 == 7) iokay=1;
   if (strcmp(mode,"rw") == 0 && imode1 == 6) iokay=1;
   if (strcmp(mode,"rw") == 0 && imode1 == 7) iokay=1;
   if (iokay == 0) {
      if (*iprint == 1) {
         printf ("ERROR in check_mode: pathname %s cannot be opened for mode '%s'.\n",
                 pathname,mode);
         printf ("NOTE: output from UNIX command '%s':\n",unixcmd);
         system(unixcmd);
         }
      *ireturn=3;
      return;
      }

   if (ldebug > 1) {
      printf ("buf.st_nlink=%o (in octal format) \n",buf.st_nlink);
      printf ("buf.st_uid=%s \n",buf.st_uid);
      printf ("buf.st_gid=%s \n",buf.st_gid);
      printf ("buf.st_size=%i \n",buf.st_size);
      strcpy (appsvar,"TZ");
      lappsvar=strlen(appsvar);
      get_apps_defaults (&appsvar,&lappsvar,timezone,&ltimezone);
      printf ("TZ=%s \n",timezone);
      t_local = localtime(&buf.st_atime);
      strftime(datetime, ldatetime, "%Y/%m/%d@%H:%M", t_local);
      printf ("buf.st_atime=%s \n",datetime);
      t_local = localtime(&buf.st_mtime);
      strftime(datetime, ldatetime, "%Y/%m/%d@%H:%M", t_local);
      printf ("buf.st_mtime=%s \n",datetime);
      t_local = localtime(&buf.st_ctime);
      strftime(datetime, ldatetime, "%Y/%m/%d@%H:%M", t_local);
      printf ("buf.st_ctime=%s \n",datetime);
      t_gmt = gmtime(&buf.st_atime);
      strftime(datetime, ldatetime, "%Y/%m/%d@%H:%MZ", t_local);
      printf ("buf.st_atime=%s \n",datetime);
      t_gmt = gmtime(&buf.st_mtime);
      strftime(datetime, ldatetime, "%Y/%m/%d@%H:%MZ", t_local);
      printf ("buf.st_mtime=%s \n",datetime);
      t_gmt = gmtime(&buf.st_ctime);
      strftime(datetime, ldatetime, "%Y/%m/%d@%H:%MZ", t_local);
      printf ("buf.st_ctime=%s \n",datetime);
      printf ("buf.st_blksize=%i \n",buf.st_blksize);
      }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/check_access_mode.c,v $";
 static char rcs_id2[] = "$Id: check_access_mode.c,v 1.1 2004/01/30 17:43:57 scv Exp $";}
/*  ===================================================  */

}

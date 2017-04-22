/* ============================================================================
** pgm: cln_file .. removes one file
**
** use:     status = cln_file(fullpath, ful_len,  &istat)
**
**  in: fullpath .... full pathname of file to remove - const char *
**  in: ful_len ..... number of chars in "fullpath" - const size_t *
** out: istat ....... exit status same as below (for FORTRAN calls) - int *
** out: status ...... exit status:  0 if file was removed
** out:                             1 if file does not exist
** out:                             2 if file has no write access 
** out:                             3 if file could otherwise not be removed
** out:                             4 if error (pathname too long, > 253)
** out: (stderr) .... error messages if any
**
** cmt: Prototype:      int cln_file(char *, int *, int *)
** cmt: FORTRAN call:   CALL CLN_FILE(fullpath, ful_len, istat)
** ============================================================================
*/
/*
#include <errno.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
*/
#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>

#define  MAX_PATH  256
#define  FD_ERR    stderr

int cln_file(const char fullpath[], const int *ful_len, int *istat)
{
 int  exit_status=0;
 size_t  pathlen;
 char  ent[MAX_PATH];
 const char  *out_err=" Error in \"cln_file\":";
 const char  *out_war=" Warning in \"cln_file\":";
 FILE  *fd1=FD_ERR;

  if ( *ful_len >= (size_t) (MAX_PATH - 3) )
   {
     (void) fprintf(fd1,out_err);
     (void) fprintf(fd1," path name too long!\n");
     exit_status=4;
   }
  else
   {
     pathlen = *ful_len;
     (void) strncpy(ent,fullpath,pathlen);
     ent[pathlen++] = '/';
     ent[pathlen]   = '\0';

     if ( access(ent,F_OK) != 0 )
	  exit_status = 1;
     else if ( access(ent,W_OK) != 0 )
      {
        (void) fprintf(fd1,out_war);
        (void) fprintf(fd1," file \"%s\":",&ent[pathlen]);
        (void) fprintf(fd1," no write access\n");
        exit_status = 2;
      }
     else if (unlink(ent) != 0 )
      {
        (void) fprintf(fd1,out_war);
        (void) fprintf(fd1," file \"%s\":",&ent[pathlen]);
        (void) fprintf(fd1," cannot unlink!\n");
        exit_status = 3;
      }
   }

  *istat    = exit_status;

  return(exit_status);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/cln_file.c,v $";
 static char rcs_id2[] = "$Id: cln_file.c,v 1.2 1999/01/20 14:00:09 page Exp $";}
/*  ===================================================  */

}

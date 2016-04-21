/* MEMBER UPFSIZ
** =====================================================================
** pgm: UPFSIZ(PATHNM,PX,BYTES) .. Get num of bytes in file
**
**  in: PATHNM ..... packed string containing file pathname - CHAR*128
**  in: PX ......... number of characters in the file pathname - INT
**  in:              (if 0 then finds length if PATHNM is blank
**  in:              terminated)
** out: BYTES ...... size of file in 8-bit bytes, 0 no file,
** out:              -1 err - INT*4
** =====================================================================
*/
#include <sys/types.h>
#include <sys/stat.h>

void upfsiz(char *pathnm, int *pathln, int *bytes)
{
  int          len, ii, retval;
  struct stat  iistat;

    len = *pathln;
    *bytes = 0;

    if ( len <= 0 || len > 128 ) len = 128;
    ii = 0;
    while ( ii < len )
    {
      if ( pathnm[ii] == ' ' ) len = ii;
      ii++;
    }
    pathnm[len] = '\0';

    retval = stat(pathnm, &iistat);
    *bytes = iistat.st_size;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upfsiz.c,v $";
 static char rcs_id2[] = "$Id: upfsiz.c,v 1.1 2000/12/18 21:25:42 dws Exp $";}
/*  ===================================================  */

}

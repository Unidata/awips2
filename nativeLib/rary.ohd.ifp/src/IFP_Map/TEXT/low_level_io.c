/* File: low_level_io.c
 *
 * Gets source files in GetSource() and the file length in GetFileLen()
 *
 */
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

long    GetFileLen ();
char    *GetSource ();

/* *********************************************************

	GetSource ()

   ********************************************************* */
char *GetSource (char *fileptr)
{

	static char     *retbuff;  /* return buffer */
	int             fd;        /* file pointer value */
	int             flen;      /* file length */


if ((fd = open (fileptr, O_RDONLY)) < 0)
	{
/*        printf ("Cannot open file %s\n", fileptr);  */
	return ((char *) NULL);
	}

flen = GetFileLen(fd);
retbuff = (char*) calloc (1, flen + 1);
if (read (fd, retbuff, flen) <= 0)
	{
/*        printf ("Error reading file %s\n", fileptr);  */
	return ((char *) NULL);
	}


close (fd);
return (retbuff);

}


/* *********************************************************

	GetFileLen ()

   ********************************************************* */
long GetFileLen (int fd)
        /* int fd  -  file pointer value */
{
  static int retval;

  lseek (fd, 0, SEEK_SET);              /* Set file pointer to 'offset': the file beginning     */
  retval = lseek (fd, 0, SEEK_END);     /* Set file pointer to ens + 'offset'                   */
  lseek (fd, 0, SEEK_SET);              /* Set file pointer to 'offset': the file beginning     */

  return (retval);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/IFP_Map/RCS/low_level_io.c,v $";
 static char rcs_id2[] = "$Id: low_level_io.c,v 1.2 2007/05/15 16:26:36 aivo Exp $";}
/*  ===================================================  */

}


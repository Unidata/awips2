/* ----------------------------------------------------------------------------
** Dirname - get directory of file
** ----------------------------------------------------------------------------
** Notes:	(1)	This routine returns the directory name (path name
**			without file name).
** ----------------------------------------------------------------------------
** History:
**
** 04 Oct 1995	Steven A. Malers	Created function.  Basically just call
**					GetFileParts.
** ----------------------------------------------------------------------------
** Variables	I/O	Description
**
** base		O	Filename base (filename before extension).
** file		O	Name of file.
** lead		O	Leading path to file.
** path		I	Full path to file.
** ----------------------------------------------------------------------------
*/

#include "ResJ.h"

int Dirname ( char *path, char *lead )
{	char	base[256], extension[256], file[256];

	GetFileParts ( path, "", lead, file, base, extension );
	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/Dirname.c,v $";
 static char rcs_id2[] = "$Id: Dirname.c,v 1.1 1999/02/18 15:16:38 dws Exp $";}
/*  ===================================================  */

}

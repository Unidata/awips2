#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void cfl_scnt ( const char *path, const char *tmplt, int isort,
                struct dirent ***dnlist, int *nfile, int *iret )
/************************************************************************
 * cfl_scnt								*
 *                                                                      *
 * This subroutine scans a directory for the files that match a given 	*
 * file name template.                                                  *
 *                                                                      *
 * cfl_scnt ( path, tmplt, isort, dnlist, nfile, iret )            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *path           const char      Directory path                  *
 *      *tmplt          const char      Filename template               *
 *      isort           int		Sorting order                   *
 *                                         1 = Alphabetical             *
 *                                        -1 = Reverse alphabetical     *
 *                                                                      *
 * Output parameters:                                                   *
 *      ***dnlist	char	        Pointer to dirent struct array	*
 *      *nfile          int		Number of file names            *
 *      *iret           int		Return code                     *
 *                                          0 = normal return           *
 *                                        -12 = cannot scan directory   *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 1/06	From FL_SCND				*
 * T. Piper/SAIC	04/07	Modified to use cfl_scandir		*
 ***********************************************************************/
{
    int ier;
    char templ[MXTMPL*5];

    char     diagMessage[720];
    int      ierm;
/*----------------------------------------------------------------------*/
/*
 * Convert the GEMPAK template to use metacharacters for the
 * directory scan.
 */
    strcpy ( templ, tmplt );
    cst_rpst ( templ, "YYYY", "[0-9][0-9][0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "YY", "[0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "MMM", "[A-Za-z][A-Za-z][A-Za-z]", templ, &ier );
    cst_rpst ( templ, "MM", "[0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "DD", "[0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "HH", "[0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "NN", "[0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "DWK", "[A-Za-z][A-Za-z][A-Za-z]", templ, &ier );
    cst_rpst ( templ, "FFF", "[0-9][0-9][0-9]", templ, &ier );
    cst_rpst ( templ, "FF", "[0-9][0-9]", templ, &ier );

/*
 *  Call cfl_scnd to perform the directory scan.
 */
    /*
    * If the directory name contains A2DB get the directory content from
    * db functions
    */
    if ( strstr (path, "A2DB") != NULL ) {
         sprintf (diagMessage, "%s%s%s%d", "calling db_gFileNames with templ=", templ, " isort=",isort);
         db_msgcave ("cfl_scnt", "debug", diagMessage, &ierm);
         *nfile = db_gFileNames ( templ, isort, dnlist);
         sprintf (diagMessage, "%s%d", "after db_gFileNames nfile=", *nfile);
         db_msgcave ("cfl_scnt", "debug", diagMessage, &ierm);
    }
    else {
       if ( isort == -1 )
	  *nfile = cfl_scandir(path, templ, _selecttmplt,
					_ralphasort, dnlist);
       else
	  *nfile = cfl_scandir(path, templ, _selecttmplt,
					_alphasort, dnlist);
    
    }
    *iret = G_NORMAL;
}

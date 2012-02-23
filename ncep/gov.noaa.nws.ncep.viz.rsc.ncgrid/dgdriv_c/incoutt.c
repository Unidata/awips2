#include "geminc.h"
#include "gemprm.h"

void inc_outt ( const char *output, const char *def, int *termflg,
                int *fileflg, char *filnam, int *iret )
/************************************************************************
 * inc_outt                                                             *
 *                                                                      *
 * This subroutine processes the OUTPUT variable. The requested output	*
 * types are determined and appropriate flags are set. Output may be	*
 * directed to the terminal and/or a file. OUTPUT will be searched for	*
 * 'T' and 'F' to determine the output devices.  If the output devices	*
 * are followed by a slash and a string, the string will be used as the	*
 * name of the output file. If file output is requested and no file	*
 * name is specified, the default will be used. If no valid devices are	*
 * specified, output will be sent to the terminal. If the output	*
 * request contains an 'N' before the slash, no output will be written.	*
 *                                                                      *
 * inc_outt ( output, default, termflg, fileflg, filnam, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *output		const char	Output variable                 *
 *      *def		const char	Default file name		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*termflg	int		Flag for terminal output	*
 *	*fileflg	int		Flag for file output		*
 *	*filnam		char		Output file name		*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -17 = invald input		*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 8/06						*
 * R. Tian/SAIC		 1/07		Fixed bug file name has '/'	*
 ************************************************************************/
{
    char carr[2][LLMXLN], *cp;
    int len, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *termflg = G_FALSE;
    *fileflg = G_FALSE;
    carr[0][0] = '\0';
    carr[1][0] = '\0';

    /*
     * Break output into two strings separated by /.  The string
     * before the / contains devices; after the / is the file name.
     */
    cp = strchr ( output, '/' );
    if ( cp ) {
        len = (int)( cp - output );
	strncpy ( carr[0], output, len );
	carr[0][len] = '\0';
	strcpy ( carr[1], &output[len+1] );
	cst_rmbl ( carr[1], carr[1], &len, &ier );
    } else {
        strcpy ( carr[0], output );
    }
    cst_lcuc ( carr[0], carr[0], &ier );

    /*
     * Check for no output first.
     */
    if ( strchr ( carr[0], 'N' ) ) {
    	return;
    }

    /*
     * Check for terminal output.
     */
    if ( strchr ( carr[0], 'T' ) ) {
	*termflg = G_TRUE;
    }

    /*
     * Check for output file requested.
     */
    if ( strchr ( carr[0], 'F' ) ) {
	*fileflg = G_TRUE;
	if ( carr[1][0] != '\0' ) {
	    strcpy ( filnam, carr[1] );
	} else if ( def[0] != '\0' ) {
	    strcpy ( filnam, def );
	} else {
	    *iret = -17;
	    return;
	}
    }


    /*
     * If no valid devices were selected, send output to terminal.
     */
    if ( *termflg == G_FALSE && *fileflg == G_FALSE ) {
    	*termflg = G_TRUE;
    }

    return;
}

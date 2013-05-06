#include <stdio.h>
#include <string.h>
/*
 * C prototype (see fortran_wrapper.h)
 */
void tgc_dual (const char *time1,  const char *time2, char *time, int *iret);

/*
 * underscore issue (uscore.h)
 */
#define tg_dual 	tg_dual_

void tgc_dual (const char *time1,  const char *time2, char *time, int *iret)
/************************************************************************
 * tgc_dual								*
 * 									*
 * This subroutine is a wrapper function for in_scal.  It is used by 	*
 * the Java JNA to resolve string size issues.				*
 * 									*
 * inc_scal ( scale, iscals, iscalv, iret )				*
 * 									*
 * Input parameters:                                                    *
 *      *scale		const char	Scaling factor			*
 * 									*
 * Output parameters:                                                   *
 *	*iscals		int		Scalar scaling factor		*
 *	*iscalv		int		Vector scaling factor		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/NCEP		3/10   C wrapper for IN_SCAL			*
 ************************************************************************/
{
    char t[2][20];
    *iret = 0;
    strcpy (t[0], time1);
    strcpy (t[1], time2);
    /*
     * Call Fortran funtion IN_SCAL.
     */
    if ( strlen(t[1]) != 0 ) {
	strcpy(time, strcat(t[0],t[1]));
    }
    else {
	strcpy ( time, t[0] );
    }
    return;
}

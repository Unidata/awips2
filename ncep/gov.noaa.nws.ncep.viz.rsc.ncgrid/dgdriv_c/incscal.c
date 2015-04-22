#include <stdio.h>
#include <string.h>
/* 
 * Fortran prototype (see proto_gemlib.h)
 */
void in_scal_ (char *scale, int *iscals, int *iscalv, int *iret, size_t);

/*
 * C prototype (see fortran_wrapper.h)
 */
void inc_scal (const char *scale, int *iscals, int *iscalv, int *iret);

/*
 * underscore issue (uscore.h)
 */
#define in_scal 	in_scal_

void inc_scal (const char *scale, int *iscals, int *iscalv, int *iret)
/************************************************************************
 * inc_scal								*
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
    *iret = 0;
    *iscals = -9999;
    *iscalv = -9999;

    /*
     * Call Fortran funtion IN_SCAL.
     */
    in_scal ( (char *)scale, iscals, iscalv, iret, strlen(scale));
    return;
}

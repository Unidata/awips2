#include "geminc.h"
#include "gemprm.h"
#include "ercmn.h"
/* 
 * Fortran prototype (see proto_gemlib.h)
 */

/*
 * C prototype (see fortran_wrapper.h)
 */
void erc_wmsg  (const char *errgrp, int *errnum, const char *errstr, int *iret);

/*
 * underscore issue (uscore.h)
 */
#define er_wmsg 	er_wmsg_

void erc_wmsg (const char *errgrp, int *errnum, const char *errstr, int *iret)
/************************************************************************
 * erc_wmsg								*
 * 									*
 * This subroutine is a wrapper function for gg_maps.  It is used by 	*
 * the Java JNA to resolve string size issues.  The error message is	*
 * written to a buffer.  This option is set in IN_BDTA.			*
 * 									*
 * ggc_maps ( proj, garea, imgfil, idrpfl, iret )			*
 * 									*
 * Input parameters:                                                    *
 *      *proj		const char	Map projection			*
 *      *garea		const char	Graphic area			*
 *      *imgfil		const char	Image file name			*
 * 									*
 * Output parameters:                                                   *
 *	*idrpfl		int		Image drop flag			*
 *					  0 = No input from user	*
 *					  1 = Drop image		*
 *					  2 = Do not drop image		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/NCEP		4/10   C wrapper for ER_WMSG			*
 ************************************************************************/
{
    *iret = 0;

    /*
     * Call Fortran funtion ER_WMSG
     */
//printf ( " In er_cwmsg xxxxxxxxxxxxxxxxxx error number %d \n", *errnum);
    er_wmsg ( (char *)errgrp, errnum, (char *)errstr, iret, 
		strlen(errgrp), strlen(errstr));
//printf ( " In er_cwmsg yyyyyyyyyyyyyyyyyy number of err %d \n", nermsg);
    return;
}

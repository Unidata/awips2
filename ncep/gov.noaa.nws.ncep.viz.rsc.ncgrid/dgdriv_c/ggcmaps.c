#include <stdio.h>
#include <string.h>

/* 
 * Fortran prototype (see proto_gemlib.h)
 */
void gg_maps_ (char *proj, char *garea, char *imgfil, int *idrpfl, 
		int *iret, size_t, size_t, size_t);

/*
 * C prototype (see fortran_wrapper.h)
 */
void ggc_maps  (const char *proj, const char *garea, const char *imgfil,
		int *idrpfl, int *iret);

/*
 * underscore issue (uscore.h)
 */
#define gg_maps 	gg_maps_

void ggc_maps (const char *proj, const char *garea, const char *imgfil,
		int *idrpfl, int *iret)
/************************************************************************
 * ggc_maps								*
 * 									*
 * This subroutine is a wrapper function for gg_maps.  It is used by 	*
 * the Java JNA to resolve string size issues.				*
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
 * T. Lee/NCEP		3/10   C wrapper for GG_MAPS			*
 ************************************************************************/
{
    *iret = 0;
    *idrpfl = 0;

    /*
     * Call Fortran funtion GG_MAPS
     */
    gg_maps ( (char *)proj, (char *)garea, (char *)imgfil, idrpfl, iret, 
		strlen(proj), strlen(garea), strlen(imgfil));
/*printf ( " strlen(proj) ---> %d\n ", strlen(proj) );
printf ( " strlen(garea) ---> %d\n ", strlen(garea) );
printf ( " strlen(imgfil) ---> %d\n ", strlen(imgfil) );
*/

    return;
}

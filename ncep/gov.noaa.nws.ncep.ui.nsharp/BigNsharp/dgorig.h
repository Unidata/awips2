/************************************************************************
 * dgorig.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGORIG_H
#define _DGORIG_H

#include	"gemprm.h"

/*
 * This structure contains the latitude/longitude position of the   
 * origin with respect to which to compute psuedo angular momentum.
 */
struct dgorig {
    /*
     * latitude of origin.
     */
    float	orglat;

    /*
     * longitude of origin.
     */
    float	orglon;

    /*
     * grid x coord of origin.
     */
    float	orgxpt;

    /*
     * grid y coord of origin.
     */
    float	orgypt;
};

#endif

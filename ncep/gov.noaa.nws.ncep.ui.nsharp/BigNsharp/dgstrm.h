/************************************************************************
 * dgstrm.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGSTRM_H
#define _DGSTRM_H

#include	"gemprm.h"

/*
 * These values are used in the HORIZON projection for polar coords.
 */
struct dgstrm {
    /*
     * rotation.
     */
    float	rot;

    /*
     * sin of center latitude.
     */
    float	sinclt;

    /*
     * cos of center longitude.
     */
    float	cosclt;

    /*
     * center longitude.
     */
    float	clon;
};

#endif

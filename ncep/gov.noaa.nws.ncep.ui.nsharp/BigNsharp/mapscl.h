/************************************************************************
 * mapscl.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _MAPSCL_H
#define _MAPSCL_H

#include	"gemprm.h"

/*
 * This structure contains map scale factor information for the current
 * grid file.
 */
struct mapscl {
    /*
     * internal grid numbers for scale factors in x, y.
     */
    int		ixmscl;
    int		iymscl;

    /*
     * y derivative of x scl fac internal grid number.
     */
    int		ixmsdy;

    /*
     * x derivative of y scl fac internal grid number.
     */
    int		iymsdx;

    /*
     * grid spacing in x, y.
     */
    float	gddx;
    float	gddy;
};

#endif

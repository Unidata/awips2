/************************************************************************
 * dgsubg.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 * D.W.Plummer/NCEP	10/06	Rm transfer grid from dgsubg structure	*
 ************************************************************************/

#ifndef _DGSUBG_H
#define _DGSUBG_H

#include	"gemprm.h"

/*
 * This structure contains information needed for the placement of
 * data on the subset internal grid.                                   
 */
struct dgsubg {
    /*
     * reference navigation.
     */
    float	refnav[LLNNAV];

    /*
     * globe wrapping grid flg.
     */
    int		gwrapg;

    /*
     * flag for DG_SUBG called.
     */
    int		dgsubg;

    /*
     * shift position for grid re-arrangement.
     */
    int		ishift;

    /*
     * subset grid index bound on re-arranged transfer grid.
     */
    int		jsgxmn;
    int		jsgxmx;
    int		jsgymn;
    int		jsgymx;

    /*
     * subset stride throug transfer grid in x & y.
     */
    int		jsgxsk;
    int		jsgysk;
};

#endif

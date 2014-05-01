/************************************************************************
 * dggrid.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGGRID_H
#define _DGGRID_H

#include	"gemprm.h"

/*
 * This structure contains the internal grids of the DG package.
 */
#define		NDGRD		80

typedef struct { float *grid; int size; } grid_t;

struct dggrid {
    /*
     * internal grids.
     */
    grid_t	dgg[NDGRD];

    /*
     * last grid used.
     */
    int		idglst;

    /*
     * date/times of grids.
     */
    char	dttimd1[NDGRD][21];
    char	dttimd2[NDGRD][21];

    /*
     * levels of grids.
     */
    int		leveld1[NDGRD];
    int		leveld2[NDGRD];

    /*
     * vertical coordinates.
     */
    int		ivcrdd[NDGRD];

    /*
     * name of grids.
     */
    char	gparmd[NDGRD][14];

    /*
     * file #.
     */
    int		ifiled[NDGRD];

    /*
     * flag for grid in use.
     */
    int		iusesv[NDGRD];

    /*
     * flag for grid rename.
     */
    int		savflg[NDGRD];

    /*
     * Ensemble member numbe associated with this grid.
     */
    int		iensmb[NDGRD];

    /*
     * max # of internal grids.
     */
    int		maxdgg;

    /*
     * subroutine ID number.
     */
    int		isubid;
};

#endif

/************************************************************************
 * hintrp.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _HINTRP_H
#define _HINTRP_H

#include	"gemprm.h"

/*
 * This structure contains horizontal interporlation information.
 */
struct hintrp {
    /*
     * last transfer navigation read.
     */
    float	tfrnav[LLNNAV];

    /*
     * internal grd# of transfer grid relative i index positions on
     * internal grid.
     */
    int		igrxig;

    /*
     * internal grd# of transfer grid relative j index positions on
     * internal grid.
     */
    int		igryig;

    /*
     * internal grd # of SINs of rotation angle from transfer grid
     * to north relative orientation.
     */
    int		isnrot;

    /*
     * internal grd # of COS of rotation angle from transfer grid
     * to north relative orientation.
     */
    int		icsrot;

    /*
     * flag that wind rotation is required.
     */
    int		wndrot;

    /*
     * flag to add column to transfer grid.
     */
    int		adcltg;

    /*
     * flag to indicate globe wrapping transfer grid.
     */
    int		gwrptg;

    /*
     * flag to indicate horz intrp was done.
     */
    int		didint;
};

#endif

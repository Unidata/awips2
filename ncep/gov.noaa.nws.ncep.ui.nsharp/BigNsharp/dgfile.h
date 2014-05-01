/************************************************************************
 * dgfile.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGFILE_H
#define _DGFILE_H

#include	"gemprm.h"

/*
 * This structure contains information from the current grid file.
 */
struct dgfile {
    /*
     * flag indicating file set.
     */
    int		dgset;

    /*
     * grid file number.
     */
    int		idlun;

    /*
     * other grid file numbers.
     */
    int		idflnm[MMFILE];

    /*
     * grid file names.
     */
    char	gdcur[MMFILE][73];

    /*
     * template flag.
     */
    int		tmpflg[MMFILE];

    /*
     * template name.
     */
    char	templt[MMFILE][MXFLSZ+1];

    /*
     * template date/time.
     */
    char	tdattm[MMFILE][21];

    /*
     * raw navigation data.
     */
    float	snav[LLNNAV];

    /*
     * # x grid points.
     */
    int		kxd;

    /*
     * # y grid points.
     */
    int		kyd;

    /*
     * total # of grid points.
     */
    int		kxyd;

    /*
     * latitude internal grd #.
     */
    int		idglat;

    /*
     * longitude internal grd #.
     */
    int		idglon;

    /*
     * projection type.
     */
    char	cprj[5];

    /*
     * grid header.
     */
    int		ighdr[LLGDHD];

    /*
     * projection angles (rad.).
     */
    float	anglr1;
    float	anglr2;
    float	anglr3;

    /*
     * constant of cone.
     */
    float	concon;

    /*
     * first time in file.
     */
    char	tfirst[MMFILE][21];

    /*
     * last time in file.
     */
    char	tlast[MMFILE][21];

    /*
     * flag to add col of data.
     */
    int		addcol;
};

#endif

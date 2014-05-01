/************************************************************************
 * dgtabl.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGTABL_H
#define _DGTABL_H

#include	"gemprm.h"

/*
 * This structure contains the table generated for the user input of
 * GFUNC or GVECT.
 */
#define		LENTC		50

struct dgtabl {
    /*
     * character function table.
     */
    char	ctabl[LENTC][17];

    /*
     * length of table.
     */
    int		ltabl;

    /*
     * level from @.
     */
    char	clevel[LENTC][25];

    /*
     * vcord from %.
     */
    char	cvcord[LENTC][5];

    /*
     * time from ^.
     */
    char	cgdttm[LENTC][49];

    /*
     * file number from +.
     */
    int		icflnm[LENTC];

    /*
     * function number of args.
     */
    int		nfargs[LENTC];
};

#endif

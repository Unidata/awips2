/************************************************************************
 * dginpt.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGINPT_H
#define _DGINPT_H

#include	"gemprm.h"

/* 
 * This structure contains the user input for the grid diagnostics.
 */
struct dginpt {
    /*
     * user date/time.
     */
    char	ddttim1[21];
    char	ddttim2[21];

    /*
     * user levels.
     */
    int		ldlevl1;
    int		ldlevl2;

    /*
     * user vertical coord.
     */
    int		lvcord;

    /*
     * input for grid time.
     */
    char	ingdtm[41];

    /*
     * input for grid level.
     */
    char	inglev[LLMXLN];

    /*
     * input for vert coord.
     */
    char	invcrd[5];
};

#endif

/************************************************************************
 * dgstck.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGSTCK_H
#define _DGSTCK_H

#include	"gemprm.h"

/*
 * This structure contains the stack used in computing diagnostics.
 */
#define		LNSTK		50

struct dgstck {
    /*
     * top of stack.
     */
    int		itop;

    /*
     * character stack.
     */
    char	stack[LNSTK][13];

    /*
     * integer stack.
     */
    int		istack[LNSTK];

    /*
     * ptr from stack to table.
     */
    int		icpntr[LNSTK];
};

#endif

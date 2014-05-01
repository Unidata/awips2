/************************************************************************
 * dgerr.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGERR_H
#define _DGERR_H

#include	"gemprm.h"

/*
 * This structure contains the diagnostic error string.
 */
struct dgerr {

    char	errst[61];

};

#endif

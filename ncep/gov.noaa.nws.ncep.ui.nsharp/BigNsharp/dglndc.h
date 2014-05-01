/************************************************************************
 * dglndc.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ***********************************************************************/

#ifndef _DGLNDC_H
#define _DGLNDC_H

#include	"gemprm.h"

/*
 * This structure contains the information for the land-sea mask.
 */
struct dglndc {
    /*
     * land-sea mask raw array.
     */
    int		ls[8136];

    /*
     * internal grid number of land-sea mask grid array.
     */
    int		lndsea;
};

#endif

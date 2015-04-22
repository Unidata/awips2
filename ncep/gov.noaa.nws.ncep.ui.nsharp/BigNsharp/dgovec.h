/************************************************************************
 * dgovec.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGOVEC_H
#define _DGOVEC_H

#include	"gemprm.h"

/*
 * This common area contains the direction of an orientation vector, 
 * for example,the tangent vector to a cross section.  This is like 
 * a conventional wind direction (grid relative) with the y axis   
 * pointing north.                                                
 */
struct dgovec {
    /*
     * direction angle (rad).
     */
    float	ornang;
};

#endif

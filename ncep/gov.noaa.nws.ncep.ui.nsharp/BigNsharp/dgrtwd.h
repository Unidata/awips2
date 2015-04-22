/************************************************************************
 * dgrtwd.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGRTWD_H
#define _DGRTWD_H

#include	"gemprm.h"

/*
 * This structure contains the cosines and sines of the rotation    
 * angle which are the elements of the matrix for the rotation       
 * transformation between grid relative and north relative and its  
 * inverse.                                                            
 */
struct dgrtwd {
    /*
     * intrnl grd # of cosines.
     */
    int		irtcos;

    /*
     * intrnl grd # of sines.
     */
    int		irtsin;
};

#endif

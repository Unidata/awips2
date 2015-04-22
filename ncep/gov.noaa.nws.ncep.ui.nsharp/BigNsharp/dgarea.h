/************************************************************************
 * dgarea.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _DGAREA_H
#define _DGAREA_H

#include	"gemprm.h"

/*
 * This structure contains information about the points that should 
 * be included in any diagnostics computation.                         
 */
struct dgarea {
    /*
     * user input area.
     */
    int		kgxmin;
    int		kgxmax;
    int		kgymin;
    int		kgymax;

    /*
     * number of additional pts.
     */
    int		kextnd;

    /*
     * full area.
     */
    int		jgxmin;
    int		jgxmax;
    int		jgymin;
    int		jgymax;

    /*
     * index range for sub area.
     */
    int		ksub1;
    int		ksub2;
};

#endif

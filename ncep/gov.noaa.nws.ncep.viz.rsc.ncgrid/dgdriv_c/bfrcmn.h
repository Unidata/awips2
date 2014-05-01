/************************************************************************
 * BFRCMN.H								*
 *									*
 * This header file defines the structures needed in FXY.		*
 *									*
 *									*
 * M. Li/SAIC		10/03						*
 * M. Li/SAIC		05/04	Added vgftype				*
 ***********************************************************************/

typedef	struct	{
	char	*alias;    	/* Pointer to alias values		*/
	char	*fxyfils;	/* Pointer to FXY file names		*/
	char	*bufrout;	/* Pointer to BUFR output file names	*/
	char	*vgftype;	/* Pointer to VGF type name		*/
} bfrtbl_t;

#include "proto_bfr.h"

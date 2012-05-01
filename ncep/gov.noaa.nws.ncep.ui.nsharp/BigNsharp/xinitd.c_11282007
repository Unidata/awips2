#define GEM_GLOB

#include "gui.h"

#include "xwcmn.h"

#ifdef UNDERSCORE
#define xsattbl xsattbl_
#define xdwtbl xdwtbl_
#define xinitd xinitd_
#endif /* UNDERSCORE */

void
/* xinitd ( win_name, lenw, xsize, ysize, iwdth, ihght, iret )
	char	win_name[];
	int	*lenw;
	float	*xsize;
	float	*ysize;
	int	*iwdth;
	int	*ihght;
	int	*iret; */

xinitd ( iwdth, ihght, iret )
	int	*iwdth;
	int	*ihght;
	int	*iret;
/************************************************************************
 * xinitd								*
 *									*
 * This subroutine opens the graphics window and sets the initial	*
 * graphics context along with basic window attributes.			*
 *									*
 * xinitd  ( win_name, lenw, xsize, ysize, iwdth, ihght, iret )		*
 *									*
 * Input and Output parameters:						*
 *	win_name	char[]		Window name			*
 *	*lenw		int		Window name length		*
 *	*xsize		float		Window X size			*
 *	*ysize		float		Window Y size			*
 *									*
 * Output parameters:							*
 *	*iwdth		int		Right edge of window		*
 *	*ihght		int		Bottom edge of window		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = window not opened		*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91						*
 * J. Whistler/SSAI	10/91	Added call to XSetFillRule		*
 * M. desJardins/NMC	12/91	GEMPAK 5.1 version			*
 * M. desJardins/NMC	 1/92	Changed definition of colors		*
 * S. Jacobs/NMC	 3/94	Added call to xsattbl			*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window & Multi-pixmap		*
 * J. Cowie/COMET	 9/95	Added win_name, x,ysizes, ( consistent	*
 *				with XW driver but not used here )	*
 * C. Lin/EAI	         1/96	clrsalloc -> allocflag[]		*
 ***********************************************************************/

{
int	i;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Set hardware text variables in case they are used.
 */
	txfont_req = 1;
	txfont_set = 0;
	txsize_req = 1.;
	txsize_set = 0.;
/*
 *	Set satellite color allocation flag to true.
 */
	for (i = 0; i < 3; i++)
		allocflag[i] = G_TRUE;

/*
 *	Initialize foreground and background colors.
 *
 *	Note that the background color index is 0 and not NNCOLR.
 *	A pixel index of 0 is used for the X windows color
 *	allocation routines. The user will still access the
 *	background color as color number 101.
 */
	ibkcol = 0;
	ifrcol = 1;
}

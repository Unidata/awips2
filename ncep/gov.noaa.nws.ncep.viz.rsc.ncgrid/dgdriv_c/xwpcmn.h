/************************************************************************
 * XWPCMN.H								*
 *									*
 * This header file declares the variables used in the XWP device 	*
 * device driver.							*
 *									*
 **									*
 * Log:									*
 * T. Piper/GSC		03/01	Created					*
 ***********************************************************************/
#ifndef XWPCMN_H
#define XWPCMN_H

#ifdef XWPCMN_GLOBAL

	float		txszx, txszy;
				/* Text font size in the
				   X and Y directions */

	int		kctype;
				/* Color scheme type
				   0 = Monochrome
				   1 = Grayscale
				   2 = Color */

	int		kunit;
				/* Type of output (Used for XW)
				   1 = GEMPAK
				   2 = MOTIF */

	int		kjust;
				/* Text justification */

	float		tsfill;

	int		kfillt;
				/* Fill size and type */

#else

	extern float		txszx;
	extern float            txszy;
	extern int		kctype;
	extern int		kunit;
	extern int		kjust;
	extern float		tsfill;
	extern int		kfillt;

#endif
#endif

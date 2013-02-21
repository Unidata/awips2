/************************************************************************
 * xwgui.h								*
 *									*
 * This include file saves global variables for the XW device driver	*
 * that are specific to the various GUI applications.			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		10/99	initial coding				*
 * E. Safford/GSC	01/00	add _loopSet flag			*
 * E. Safford/GSC	05/00	add _allFrmsBad flag			*
 * H. Zeng/EAI          04/01   replaced _lpIdx with _lp                *
 * E. Safford/SAIC	01/02	add _viewedLoop				*
 ***********************************************************************/

#ifndef XWGUI_H
#define XWGUI_H

#include "xwcmn.h"

#ifdef XWGUI_GLOBAL

int	_lp;			/* current loop */
int	_viewedLoop;		/* currently viewed loop */

int	_numPxm[MAX_LOOP];	/* number of pixmaps in loop */
int	_fstPxm[MAX_LOOP];	/* first pixmap in loop */
int	_lstPxm[MAX_LOOP];	/* last pixmap in loop */
int	_blankPxm[MAX_LOOP];	/* blank pixmap for loop */
Boolean	_loopSet[MAX_LOOP];	/* loop set flag */
Boolean _allFrmsBad[MAX_LOOP];  /* flag for all frames bad in loop */

#else

extern int	_lp;			/* current loop */
extern int	_viewedLoop;		/* currently viewed loop */

extern int	_numPxm[MAX_LOOP];	/* number of pixmaps in loop */
extern int	_fstPxm[MAX_LOOP];	/* first pixmap in loop */
extern int	_lstPxm[MAX_LOOP];	/* last pixmap in loop */
extern int	_blankPxm[MAX_LOOP];	/* blank pixmap for loop */
extern Boolean  _loopSet[MAX_LOOP];     /* loop set flag */
extern Boolean  _allFrmsBad[MAX_LOOP];  /* flag for all frames bad in loop */

#endif /* XWGUI_GLOBAL */

#endif  /* XWGUI_H */

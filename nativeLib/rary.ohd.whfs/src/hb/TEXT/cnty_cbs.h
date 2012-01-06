/*
	File:		cnty_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the popup County/State DS.
	
*/

#ifndef cnty_cbs_h
#define cnty_cbs_h

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Counties.h"

#define FROM_LOC 0
#define FROM_NWR 1

void		ShowCntyDs(Widget w, int called_from);
Counties**	getCountiesBase(void);
void		load_cntysts(void);

void	close_cnty(Widget w, XtPointer ptr, XtPointer cbs);
void	clear_cnty(Widget w, XtPointer ptr, XtPointer cbs);
void	copy_loc_cntyst(Widget w, XtPointer ptr, XtPointer cbs);

char*	ChosenCnty(void);
char*	ChosenState(void);


#endif

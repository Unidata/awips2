/*
	File:		grp_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Groups DS.
	
*/

#ifndef grp_cbs_h
#define grp_cbs_h


void	ShowGrpDs(Widget w, XtPointer ptr, XtPointer cbs);
int	load_grps(void);
void	close_grp();
void	clear_grp();

void    copy_grp();
char * ChosenGroup(void);


#endif

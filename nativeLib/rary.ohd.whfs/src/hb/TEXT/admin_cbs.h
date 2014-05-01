/*
	File:		admin_cbs.h
	Date:		11/09/1994
	Author:		Dale Shelton
	
*/

#ifndef admin_cbs_h
#define admin_cbs_h


/*
	Function prototypes.
*/
void	ShowAdmDs(Widget w);
void	admin_load(void);


/*
	Callback prototypes.
*/
void	admin_save(Widget w, XtPointer ptr, XtPointer cbs);
void	admin_close(Widget w, XtPointer ptr, XtPointer cbs);


#endif

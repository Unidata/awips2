/*
	File:		stachar_show.h
	Date:		2/13/96
	Author:		Paul Taylor
	
	Purpose:	Provides support for Station Characteristics DS.
	
*/

#ifndef stachar_show_h
#define stachar_show_h

#include "Observer.h"

void    ShowStaCharDs(Widget w, char *lid, Boolean editable);

void	stachar_addTextFilterCallbacks(void);
void	stachar_removeTextFilterCallbacks(void);

void	stachar_ok(Widget w, Widget om, XtPointer cbs);
void    stachar_apply(Widget w, Widget om, XtPointer cbs);
void    stachar_checkPB(Widget w, XtPointer ptr, XtPointer cbs);
void    stachar_close(Widget w, XtPointer ptr, XtPointer cbs);
void    stachar_del_conf(Widget w, Widget om, XtPointer cbs);

void    stachar_delete(Widget w, Widget om, XtPointer cbs);
void	stachar_help(Widget w, XtPointer ptr, XtPointer cbs);


void    obs_btns(void);
void    obs_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	obs_load(char *lid);
void    obs_loaddate(Widget w, XtPointer ptr, XtPointer cbs);
int     obs_unload(Observer *obs);

void	obs_set_3line_address(char *address,
			      char *line1, char *line2, char *line3);


void    dcp_btns(void);
void    dcp_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	dcp_load(char *lid);


void    telm_btns(void);
void    telm_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	telm_load(char *lid);

XmStringTable freelist(XmStringTable xmStr, int cnt);


#endif


   

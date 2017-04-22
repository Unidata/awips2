/*
	File:		setup_cbs.h
	Date:		12/13/1994
	Author:		Dale Shelton
	
*/

#ifndef setup_cbs_h
#define setup_cbs_h


void	setup_show(Widget w);
void	setup_load(XmStringTable *xmStr, int count);
void	setup_callbacks(void);
char *	setup_value(void);
void	setup_del_conf();
void	setup_delete();
void	setup_insert();
void	setup_update();
void	setup_close();
void	setup_reset();


#endif

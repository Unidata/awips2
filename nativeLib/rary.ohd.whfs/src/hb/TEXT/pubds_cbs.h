/*
	File:		pubds_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef pubds_cbs_h
#define pubds_cbs_h


/*
	Function prototypes.
*/
void	pubds_show(Widget w, const char *lid);
void	pubds_key(char *key, int *pos);
void	pubds_callbacks(void);
void	pubds_clear(void);
int	pubds_load(const char *lid);
int	pubds_save(void);


/*
	Callback prototypes.
*/
void	pubds_import();
void	pubds_ok(Widget w, XtPointer ptr, XtPointer cbs);
void	pubds_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	pubds_close();
void	pubds_new();
void	pubds_delete();
void	pubds_del_conf();


#endif

/*
	File:		refer_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef refer_cbs_h
#define refer_cbs_h


void	refer_show(Widget w, const char *lid);
int	refer_load(const char *lid);
void	refer_key(char *key, int *pos);
void	refer_callbacks(void);
void	refer_clear(void);
int	refer_save(void);


/*
	Callback prototypes.
*/
void	refer_import();
void	refer_ok(Widget w,    XtPointer ptr, XtPointer cbs);
void	refer_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	refer_close();
void	refer_new();
void	refer_delete();
void	refer_del_conf();


#endif

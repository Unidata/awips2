/*
	File:		bench_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef bench_cbs_h
#define bench_cbs_h


void	bench_show(Widget w, const char *lid);
int	bench_load(const char *lid);
void	bench_key(char *key, int *pos);
void	bench_callbacks(void);
int 	bench_save(void);


/*
	Callback prototypes.
*/
void	bench_import();
void	bench_ok(Widget w,    XtPointer ptr, XtPointer cbs);
void	bench_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	bench_close();
void	bench_new();
void	bench_delete();
void	bench_del_conf();


#endif

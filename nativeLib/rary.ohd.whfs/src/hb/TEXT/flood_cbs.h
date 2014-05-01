/*
	File:		flood_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef flood_cbs_h
#define flood_cbs_h


/*
	Function prototypes.
*/
void	flood_show(Widget w, const char *lid);
int	flood_load(const char *lid);
void	flood_key(char *key, int *pos);
void	flood_callbacks(void);
int	flood_save(void);


/*
	Callback prototypes.
*/
void	flood_import();
void	flood_ok(Widget w, XtPointer ptr, XtPointer cbs);
void	flood_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	flood_close(Widget w, XtPointer ptr, XtPointer cbs);
void	flood_new();
void	flood_delete();
void	flood_del_conf();


#endif

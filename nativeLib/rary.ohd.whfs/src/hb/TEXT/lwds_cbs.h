/*
	File:		lwds_cbs.h
	Date:		11/30/1994
	Author:		Dale Shelton, Paul Taylor
	
	Purpose:	Provide support for the LowWater DS.
*/


#ifndef lwds_cbs_h
#define lwds_cbs_h


/*
	Function prototypes.
*/
void	lwds_show(Widget w, const char *lid);
int	lwds_load(const char *lid);
void	lwds_key(char *key, int *pos);

void	lwds_callbacks(void);
void	lwds_add_textfilter_callbacks(void);
void	lwds_remove_textfilter_callbacks(void);

void	lwds_clear(void);
int	lwds_save(void);


/*
	Callback prototypes.
*/
void	lwds_import();
void	lwds_ok(Widget w, XtPointer ptr, XtPointer cbs);
void	lwds_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	lwds_close();
void	lwds_new();
void	lwds_delete();
void	lwds_del_conf();


#endif




/*
	File:		mgage_cbs.h
	Author:		Dale Shelton
	
	Purpose:	
	
*/


#ifndef mgage_cbs_h
#define mgage_cbs_h


/*
	Function prototypes.
*/
void	ShowMgageDs(Widget w, char *lid);
int	mgage_load(const char *lid);
void	mgage_key(char *key, int *pos);
void	mgage_btns(void);
void	mgage_callbacks(void);
int	mgage_save(void);


/*
	Callback prototypes.
*/
void	mgage_import();
void	mgage_ok(Widget w,    XtPointer ptr, XtPointer cbs);
void	mgage_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	mgage_new();
void	mgage_del_conf();
void	mgage_clear();
void	delete_mgage();
void	close_mgage();
void    create_gage_where(char *where, char const *lid);

#endif

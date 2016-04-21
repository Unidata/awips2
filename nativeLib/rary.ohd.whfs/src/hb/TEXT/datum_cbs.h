/*
	File:		datum_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef datum_cbs_h
#define datum_cbs_h

#include "Datum.h"

/*
	Function prototypes.
*/
void	datum_show(Widget w, const char *lid);
void	datum_load(const char *lid);
Datum * datum_selected ( int *pos );
void	datum_callbacks(void);
int	datum_save(void);


/*
	Callback prototypes.
*/
void	datum_import();
void	datum_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	datum_close();
void	datum_delete();
void	datum_del_conf();


#endif

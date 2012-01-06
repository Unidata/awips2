/*
	File:		hgstation_show.h
	Date:		03/16/2006
	Author:		Jingtao Deng

*/


#ifndef hgstation_show_h
#define hgstation_show_h
#include "hgstation_struct.h"

/*
	Function prototypes.
*/
void	hgs_show(Widget w);
void	load_hgs();
void	hgs_key(char *key, int *pos);
void	hgs_callbacks(void);
void    create_shefpe();
void    create_shefts();
void    create_sheffcstts();
void    malloc_hgstation_struct(int	cnt,
		                hgstation_struct **hgs_struct);
/*
	Callback prototypes.
*/
void	hgs_import();
void	hgs_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	hgs_close(Widget w, XtPointer ptr, XtPointer cbs );
void	hgs_delete(Widget w, XtPointer ptr, XtPointer cbs);
void	hgs_del_conf(Widget w, XtPointer ptr, XtPointer cbs);


#endif




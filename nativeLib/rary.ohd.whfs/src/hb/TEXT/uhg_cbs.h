/*
	File:		uhg_cbs.h
	Date:		May 2001
	Author:		Russell Erb
	
	Purpose:
*/


#ifndef uhg_cbs_h
#define uhg_cbs_h


#include "UnitGraph.h"

void	ShowUhgDs(Widget w, char *lid);
void	uhg_callbacks(void);
void	import_file_uhg();
void	select_file_uhg();
void	close_uhg();
void	load_list_uhg(UnitGraph *unitgraph);
void	clear_list_uhg(void);
void	current_ordinal_uhg();
void	close_file_uhg();
void	uhg_ok();
void	uhg_save();
void	uhg_clear();
void	uhg_clear_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	uhg_delete();
void	uhg_del_conf(Widget w, XtPointer ptr, XtPointer cbs);
void 	uhg_add();
void 	uhg_remove();
void 	uhg_remove_conf(Widget w, XtPointer ptr, XtPointer cbs);
void 	uhg_add();
void 	uhg_modify();
void	uhg_sensitize();
void	uhg_find(UnitGraph *uhgPtr);

#endif

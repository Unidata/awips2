/*
	File:		loc_cbs.h
	Date:		March 1996
	Author:		Dale Shelton, Chip Gobs, Paul Taylor
	
	Purpose:	Provides support for creating callbacks for locTLS.
	
*/

#ifndef loc_cbs_h
#define loc_cbs_h


#include "agencyoffice_typedefs.h"


void	loc_callbacks(void);
void	loc_addTextFilterCallbacks(void);
void	loc_removeTextFilterCallbacks(void);

void	loccopy_callbacks(void);

void	ShowLocCopyDs(Widget w);

void    show_agencyoffice_callback(Widget w, XtPointer ptr, XtPointer cbs);
void	show_cntys	(Widget w, XtPointer ptr, XtPointer cbs);
void	show_loccopy	(Widget w, XtPointer ptr, XtPointer cbs);
void	set_date();

void	ok_loc(Widget w,    XtPointer ptr, XtPointer cbs);
void	apply_loc(Widget w, XtPointer ptr, XtPointer cbs);
void	close_loc();
void 	delete_loc();
void	help_loc();

void 	locSetSensitivity(Boolean set);
void	loc_create_btns(void);
void    init_locPBs();
void	lid_filter();
void    load_locdate();
int	save_loc(void);

void    copy_loc_cnty();
void 	loc_del_conf();
void	create_where_loc();
void	clear_loc();

void	loc_checkPB(Widget w, XtPointer ptr, XtPointer cbs);

char * ChosenCnty(void);
char * CurrentLid();


void	LocImport(char *lid);

void	Loc_LoadCooperators(char *lid);
int	Loc_LoadSelectedList(AgencyOfficeRec *aor);

void	Loc_UpdateStationClassFields(char *lid);



void	ok_loccopy	(Widget w, XtPointer ptr, XtPointer cbs);
void	close_loccopy	(Widget w, XtPointer ptr, XtPointer cbs);

void ShowLocDs ( Widget w , char * id ) ;

#endif



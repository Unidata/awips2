/*
	File:		callbacks.h
	Date:		7/21/94
	Author:		Dale Shelton, Paul Taylor
	
	Purpose:	Sets up and implements the callback 
			functions for the hydrobase main
			window widgets.  Subsidiary windows
			implement callbacks in their own
			source files.
	
        Modified:       Bryon Lawrence
        Revision Date:  2/25/2004
        Notes:          Removed the show_damcat callback routine.  The option
                        to view the dam catalog will no longer be available
                        in Build Ob4.
			
        Modified:       Jingtao
	Revision Date:  03/16/2006
	Notes:          Add "River Station Location Info" in the "Setup" menu
	                to show information from HgStation table.			
                        
*/

#ifndef callbacks_h
#define callbacks_h


/*
	Search text box management function(s).
*/
void	clear_search_window(void);


/*
	Callback prototypes.
*/

void	show_prefs(Widget w, XtPointer ptr, XtPointer cbs);
void	close_hb(Widget w, XtPointer ptr, XtPointer cbs);
void	abort_hb(char *msg);

void	hbmain_setFocus(Widget w, XtPointer ptr, XEvent *event);

void	check_password(Widget w, XtPointer ptr, XtPointer cbs);
void	MotionCallback(Widget w, XtPointer ptr, XtPointer cbc);
void	TextModifiedCallback(Widget w, XtPointer ptr, XtPointer cbs);

void	show_loc(Widget w, XtPointer ptr, XtPointer cbs);
void	show_schar(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rparam(Widget w, XtPointer ptr, XtPointer cbs);
void	show_czone(Widget w, XtPointer ptr, XtPointer cbs);
void	show_contact(Widget w, XtPointer ptr, XtPointer cbs);


void	show_river(Widget w, XtPointer ptr, XtPointer cbs);
void	show_fcat(Widget w, XtPointer ptr, XtPointer cbs);
void	show_fstmt(Widget w, XtPointer ptr, XtPointer cbs);
void	show_lwstmt(Widget w, XtPointer ptr, XtPointer cbs);
void	show_fdam(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rate(Widget w, XtPointer ptr, XtPointer cbs);
void    show_shef(Widget w, XtPointer ptr, XtPointer cbs);
void	show_uhg(Widget w, XtPointer ptr, XtPointer cbs);
void	show_crest(Widget w, XtPointer ptr, XtPointer cbs);
void	show_lowwater(Widget w, XtPointer ptr, XtPointer cbs);
void	show_benchmark(Widget w, XtPointer ptr, XtPointer cbs);
void	show_datum(Widget w, XtPointer ptr, XtPointer cbs);
void	show_descr(Widget w, XtPointer ptr, XtPointer cbs);
void	show_gage(Widget w, XtPointer ptr, XtPointer cbs);
void	show_pub(Widget w, XtPointer ptr, XtPointer cbs);
void	show_refer(Widget w, XtPointer ptr, XtPointer cbs);

void	show_res(Widget w, XtPointer ptr, XtPointer cbs);

void	show_filter(Widget w, XtPointer ptr, XtPointer cbs);
void	show_adjust(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rcheck(Widget w, XtPointer ptr, XtPointer cbs);
void	show_purge(Widget w, XtPointer ptr, XtPointer cbs);


void	show_fsrept(Widget w, XtPointer ptr, XtPointer cbs);
void	show_textrept(Widget w, XtPointer ptr, XtPointer cbs);


void	show_admin(Widget w, XtPointer ptr, XtPointer cbs);
void	show_cities(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rfields(Widget w, XtPointer ptr, XtPointer cbs);
void	show_stcozo(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rpfparams(Widget w, XtPointer ptr, XtPointer cbs);
void	show_fgrp(Widget w, XtPointer ptr, XtPointer cbs);
void	show_radarloc(Widget w, XtPointer ptr, XtPointer cbs);
void	show_geoarea(Widget w, XtPointer ptr, XtPointer cbs);
void	show_geoline(Widget w, XtPointer ptr, XtPointer cbs);
void	show_nwrtower(Widget w, XtPointer ptr, XtPointer cbs);
void	show_tscfg(Widget w, XtPointer ptr, XtPointer cbs);
void	show_hgs(Widget w, XtPointer ptr, XtPointer cbs);

void	show_techinfo(Widget w, XtPointer ptr, XtPointer cbs);

void	show_filter_options(Widget w, XtPointer ptr, XtPointer cbs);
void	filter_option_cancel(Widget w, XtPointer ptr, XtPointer cbs);
void	filter_option_save(Widget w, XtPointer ptr, XtPointer cbs);
void	serv_bkup_toggles(Widget w, XtPointer ptr, XtPointer cbs);

/*
	Main list callbacks.
*/
void	list_state();
void	set_stat_list();
void	set_stat_pos();
void	lookup_lid();


/*
	Other function prototypes.
*/
void	AddCallbacks(void);
void	CreateWidgets(Widget parent);
void	setup_wfo_selector(void);
void	load_wfo_btns(void);
void	list_lid_match(const char *lid);
int	load_stats(const char *where, int pos);
void	set_list_label(const long code);
int	search_stats(const char *str);
char*	CurrentLid(void);

void	create_password_dialog( Widget w , XtPointer client_data ,
                                XtPointer call_data );

#endif




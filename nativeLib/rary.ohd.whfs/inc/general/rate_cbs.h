/*
	File:		rate_cbs.h
	Date:	
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef rate_cbs_h
#define rate_cbs_h


#include "Rating.h"
#include "RatingShift.h"

void	ShowRateDs(Widget w, char *lid, Boolean editable);
void	rate_callbacks(void);
void	draw_rating(Widget w);
void	import_file();
void	select_file();
void	draw_curve();
void	close_rate();
void	load_list(Rating *rating);
void	load_shift_list(RatingShift *ratingShift);
void	clear_list(void);
void	current_stage();
void	current_shift();
void	set_curve();
void	close_file();
void	rate_ok();
void	rate_save();
void	rate_clear();
void	rate_clear_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	rate_delete();
void	rate_del_conf(Widget w, XtPointer ptr, XtPointer cbs);
void 	rate_remove();
void 	rate_remove_conf(Widget w, XtPointer ptr, XtPointer cbs);
void 	shift_remove();
void 	shift_remove_conf(Widget w, XtPointer ptr, XtPointer cbs);
void 	rate_modify();
void 	shift_modify();
void	rate_sensitize();
void	shift_sensitize();
void 	set_stages(float *maxstage,float *minstage, Rating *rating, float shiftValue);

#endif

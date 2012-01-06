#ifndef river_cbs_h
#define river_cbs_h

#include "ShefPe.h"

/*
	Data buffers.
*/
long	ratedat;
long	uhgdur;


/*
	Function prototypes.
*/
void	river_callbacks(void);
void	river_addTextFilterCallbacks(void);
void	river_removeTextFilterCallbacks(void);

void	river_load(const char *lid);
void 	river_load_group(const char *lid);
void	river_save_group(const char *lid);


/*
	Callback prototypes.
*/
void	rvr_checkPB(Widget w, XtPointer ptr, XtPointer cbs);
void 	riverSetSensitivity(Boolean set);
void	show_rmcrest(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rmgage(Widget w, XtPointer ptr, XtPointer cbs);
void	show_rvrsub(Widget w, XtPointer ptr, XtPointer cbs);
void	show_fcstsvc(Widget w, XtPointer ptr, XtPointer cbs);
void	show_hwsvc(Widget w, XtPointer ptr, XtPointer cbs);
void	delete_river(Widget w, XtPointer ptr, XtPointer cbs);
void	river_del_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	apply_river(Widget w, XtPointer ptr, XtPointer cbs);
void	close_river(Widget w, XtPointer ptr, XtPointer cbs);
void	ok_river(Widget w, XtPointer ptr, XtPointer cbs);
void	help_river(Widget w, XtPointer ptr, XtPointer cbs);
int	save_river(void);
void	river_settime();
void	lookup_group();

ShefPe*	grpinfo_getshefpes(Boolean reset_statics);
void   	grpinfo_freeshefpes(void);
char*   grpinfo_getpe_from_peLI(Widget peLI);
void    grpinfo_loadpeLI(Widget peLI, ShefPe* shefpePtr);
char*   grpinfo_get_shefpe_name(char *pe);
void    grpinfo_loadpe_into_peLI(Widget peLI, char *pe);
void 	grpinfo_highlightpeLI(Widget riverinfo_peLI, char *rv_lid);
void 	river_fcst_TB ( Widget w, XtPointer ptr, XtPointer cbs );



#endif

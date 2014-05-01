/*
	File:		fcst_cbs.h
	Date:		11/07/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/

#ifndef fcst_cbs_h
#define fcst_cbs_h


#include "DbmsDefs.h"


/*
	Data buffers to provide permanent storage
	for data elements displayed through the
	section options.
*/
char		ctrl_buf[LONG_LEN + 1];
char		ice_buf[ICE_INFO_LEN + 1];
char		topo_buf[LONG_LEN + 1];
char		rmk_buf[LONG_LEN + 1];
char		res_buf[LONG_LEN + 1];


/*
	Function prototypes.
*/
void	ShowFcstDs(Widget w, const char *lid);
int	fcst_load(const char *lid);
void	fcst_btns();
void	fcst_callbacks();
void	fcst_clear();
void	fcst_benchmark();
void	fcst_datum();
void	fcst_flood();
void	fcst_lowwater();
void	fcst_publication();
void	fcst_reference();
void	fcst_section();
void	fcst_delete();
void	fcst_del_conf();
void	fcst_close();
void	fcst_save();
void	fcst_delete2();
void	fcst_del_conf2();
void	fcst_save2();


#endif

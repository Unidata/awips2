/*
	File:		nwrtower_show.h
	Date:		October 1998
	Author:		Russ Erb
*/

#ifndef nwrtower_show_h
#define nwrtower_show_h

#include "NWRTransmitter.h"

/*
	Function prototypes.
*/
void	nwrtower_show(Widget w);
int	nwrtower_load();
void	nwrtower_key(char *key, int *pos);

void	nwrtower_callbacks(void);
void	nwrtower_add_callbacks(void);
void	nwrtower_remove_callbacks(void);

int	nwrtower_save(void);
void	find_nwrtower_pos(NWRTransmitter *cPtr, int *pos);

void	nwravailcty_load(void);
void	nwrcounty_load(const char *call_sign);
void	nwrcounty_key(char *key, int *pos);

/*
	Callback prototypes.
*/
void	nwrtower_import();
void    nwrtower_show_cntyst(Widget w, XtPointer ptr, XtPointer cbs);
void	nwrtower_apply(Widget w, XtPointer ptr, XtPointer cbs);
void	nwrtower_clear(Widget w, XtPointer ptr, XtPointer cbs);
void	nwrtower_del_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	nwrtower_delete(Widget w, XtPointer ptr, XtPointer cbs);

void	nwrcounty_del_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	nwrcounty_delete(Widget w, XtPointer ptr, XtPointer cbs);
void	nwrcounty_add(Widget w, XtPointer ptr, XtPointer cbs);

void	nwr_close(Widget w, XtPointer ptr, XtPointer cbs);

#endif

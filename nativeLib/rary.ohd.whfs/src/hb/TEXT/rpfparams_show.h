/*
	File:		rpfparams_show.h
	Date:		1/8/1998
	Author:		Paul Taylor
	
	Purpose:	Provide support for the RiverPro General Params DS.
*/


#ifndef RPFPARAMS_SHOW_H
#define RPFPARAMS_SHOW_H

/*
	Includes.
*/
#include "RpfParams.h"


/*
	Prototypes.
*/
void	rpfparams_show(Widget w);

void	add_rpfparams_cbs(void);

void	add_rpfparams_textfilter_cbs(void);
void	remove_rpfparams_textfilter_cbs(void);

void	rpfparams_updateCB(Widget w, XtPointer ptr, XtPointer cbs);
void	read_rpfparams_info(RpfParams *paraminfo);
void	write_rpfparams_info(void);

void 	free_rpfparams(void);
void 	rpfparams_closeCB(Widget w, XtPointer ptr, XtPointer cbs);


#endif 



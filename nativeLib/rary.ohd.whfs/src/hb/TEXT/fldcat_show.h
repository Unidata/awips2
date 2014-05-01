/*
	File:		fldcat_show.h
	Date:		August 1995
	Author:		Dale Shelton
	
	Purpose:	Defines the function prototypes employed
			by the Flood Category user interface
			display.
			
*/


#ifndef fldcat_show_h
#define fldcat_show_h

#include <Xm/Xm.h>


void	ShowFdatDs(Widget w, char *lid);	
int	fdat_load(const char *lid);
void	fdat_callbacks(void);
void	save_fdat();
void	close_fdat();
void	fdat_delete();
void	fdat_del_conf();
void    fldcat_show ( Widget w , char * lid ) ;


#endif

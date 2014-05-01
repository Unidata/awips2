#ifndef GEOLOG_SHOW_H
#define GEOLOG_SHOW_H

#include <Xm/Xm.h>


/* prototypes */

void geolog_show(Widget w,
		 char *title,
		 char *labelstr,
		 char *text);

void geolog_close(void);


int load_log_text(char *logfilename,
		  char **textstr);

#endif 

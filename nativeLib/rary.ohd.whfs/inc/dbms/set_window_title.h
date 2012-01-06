#ifndef SET_WINDOW_TITLE_H
#define SET_WINDOW_TITLE_H 

#include <Xm/Xm.h>
#include "DbmsUtils.h"
#include "Xtools.h"

#ifndef MAX_BUF_LEN
#define MAX_BUF_LEN        255
#endif

#ifndef MAX_WHERE_LEN
#define MAX_WHERE_LEN      255
#endif

void set_title(Widget w, char *title, char *lid);

#endif

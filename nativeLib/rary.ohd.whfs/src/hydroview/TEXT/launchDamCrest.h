#ifndef LAUNCHDAMCREST_H
#define LAUNCHDAMCREST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <Xm/Xm.h>

#include "DbmsDefs.h"
#include "HvDisplayControlDefs.h"
#include "HvDisplayControlProto.h"
#include "HvColorList.h"
#include "mapBackgrounds.h"

#include "display_control_show.h"
#include "pointcontrol_show.h" 
#include "select.h"
#include "select_show.h"
#include "grid.h"

void	launch_damcrest_cb ( Widget map_widget, clicks * mouse_clicks ) ;
char * locateDam ( int x , int y );

#endif

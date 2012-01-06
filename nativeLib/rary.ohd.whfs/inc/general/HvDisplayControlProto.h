#ifndef HV_DISPLAY_PROTO_H
#define HV_DISPLAY_PROTO_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h>

#include "LatestObsValue.h"   /* database tables/views */
#include "HvStation.h"
#include "TelmType.h"

#include "MotifWidgets.h"
#include "DbmsUtils.h"
#include "HvDisplayControlDefs.h"

#include "cvt_latlon.h"
#include "time_convert.h"
#include "load_maxfcst.h"
#include "HvTime.h"

#include "pointcontrol_show.h"

#include "ArealDisplayControl.h"

HvDisplayControl * getHvDisplayControl();

void initHvDisplayControl ( Widget top_widget ,
                            HvDisplayControl *hdc ,
                            void display_routine ( ) ,
                            int add_referesh_timer ) ;

void initBackgroundDisplay(BackgroundDisplaySettings *bds);

void initHvProdlist(HvDisplayControl *hdc);

void initHvOptions(HvDisplayControl *hdc);

void transfer_pc_options(HvDisplayControl *hdc);

void load_latest_river_data(HvDisplayControl *hdc);
void set_pc_options_river(pc_options_struct *pc_options);

int check_if_rivertype();

#endif

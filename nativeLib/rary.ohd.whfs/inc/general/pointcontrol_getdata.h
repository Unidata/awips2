#ifndef POINTCONTROL_GETDATA_H
#define POINTCONTROL_GETDATA_H

#include "CurPC.h"
#include "CurPP.h"
#include "LatestObsValue.h"
#include "Observation.h"
#include "pointcontrol_options.h"
#include "RiverStatus.h"
#include "time_convert.h"

void    build_riverstatus_where(const pc_options_struct *pc_options,
                                char                    *where);

void    build_river_where(const pc_options_struct * pc_options,
                          char                  *pe,
                          char                  *where);

void    build_SnowTempOther_where(const pc_options_struct  *  pc_options,
                                  char  *where) ;

char durhours_to_shefcode(const pc_options_struct * pc_options);

void getRainData(const pc_options_struct   *  pc_options,
                 CurPC                  **pcHead,
                 CurPP                  **ppHead);

void getRiverData(const pc_options_struct * pc_options,
                  Observation           **obshHead,
                  Observation           **obsdHead,
                  RiverStatus           **rsHead);

void getSnowTempOtherData(pc_options_struct   *  pc_options,
                          Observation           **obsHead,
                          LatestObsValue        **latHead);

void build_type_source_where_filter(char *wherestr,
                                    const pc_options_struct * pc_options_custom);

#endif /* #ifndef POINTCONTROL_GETDATA_H */

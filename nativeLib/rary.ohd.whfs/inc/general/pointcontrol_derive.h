#ifndef POINTCONTROL_DERIVE_H
#define POINTCONTROL_DERIVE_H

#include <time.h>

#include "CurPC.h"
#include "CurPP.h"
#include "DbmsDefs.h"
#include "IngestFilter.h"
#include "get_total_precip.h"
#include "LatestObsValue.h"
#include "Observation.h"
#include "pointcontrol_options.h"
#include "pointcontrol_report.h"
#include "RiverStatus.h"

/* structure for saving small portion of RiverStat record */

typedef struct
{
   char         lid [ LOC_ID_LEN + 1 ] ;
   char         pe [ SHEF_PE_LEN + 1 ] ;
   double       fq ;
   double       fs ;
   double       aq ;
   double       as ;
} rs_info_struct ;


int compare_tsrank(char         *ts1,
                   char         *ts2,
                   IngestFilter *ingestHead);

Observation * derive_report_obs(const pc_options_struct * pc_options,
                                Observation             *startPtr,
                                Observation             *endPtr,
                                int                     change_hour,
                                int                     lid_cnt,
                                double                  *change ) ;

Observation * derive_report_obsriv(const pc_options_struct * pc_options,
                                   Observation          *startPtr,
                                   Observation          *endPtr,
                                   int                  lid_count,
                                   int                  change_window,
                                   double              *change ) ;

LatestObsValue * derive_report_lat(const pc_options_struct * pc_options,
                                   LatestObsValue       *startPtr,
                                   LatestObsValue       *endPtr,
                                   int                  lid_count);

RiverStatus * derive_report_rs(const pc_options_struct  *pc_options,
                               char                     *use_ts,
                               RiverStatus              *startPtr,
                               RiverStatus              *endPtr,
                               int                      lid_count ) ;

/* derive functions for "other" data */
ReportList * derive_reportsOther(const pc_options_struct * pc_options,
                                 Observation            *obsHead,
                                 LatestObsValue         *lHead);

/* derive functions for precip data */
ReportList * derive_reportsRain(const pc_options_struct * pc_options,
                                CurPC                   * pcHead,
                                CurPP                   * ppHead);

/*derive functions for river data */
void derive_reportsRiver(const pc_options_struct *pc_options,
                         Observation            *obshHead,
                         Observation            *obsdHead,
                         RiverStatus            *rsHead,
                         ReportList             **obsrepHeadDPtr,
                         ReportList             **fcstrepHead);



int get_change_hour_window ( ) ;

rs_info_struct * get_rs_info ( char * lid ) ;

ReportList * load_precip_report ( const struct total_precip * pTotalPrecip ,
	                          time_t end_timet , ReportList * inputPtr ) ;

ReportList * process_lid_lat(const pc_options_struct * pc_options,
                             LatestObsValue     *startPtr,
                             LatestObsValue     *endPtr,
                             int                lid_count,
                             ReportList         *inputPtr);

ReportList * process_lid_obs(const pc_options_struct * pc_options,
                             Observation        *startPtr,
                             Observation        *endPtr,
                             int                lid_count,
                             int                change_window,
                             ReportList         *inputPtr);


ReportList * process_lid_obsriv(const pc_options_struct *pc_options,
                                Observation             *startPtr,
                                Observation             *endPtr,
                                int                     lid_count,
                                int                     change_window,
                                ReportList              *inputPtr ) ;

ReportList * process_lid_rs ( const pc_options_struct * pc_options ,
                              char                * use_ts ,
                              RiverStatus         * startPtr ,
                              RiverStatus         * endPtr ,
                              int                 lid_count ,
                              ReportList          * inputPtr ) ;

void FreeReportList(ReportList *sp);


#endif /*#ifndef POINTCONTROL_DERIVE_H */

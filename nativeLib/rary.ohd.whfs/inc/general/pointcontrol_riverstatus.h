/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#ifndef POINTCONTROL_RIVERSTATUS_H 
#define POINTCONTROL_RIVERSTATUS_H

#include "DbmsDefs.h"
#include "pointcontrol_report.h"

void FreeRiverStatusList ( ) ;
void get_floodinfo ( char         *lid,
                     char         *pe,
                     float        *flood_level,
                     float        *action_level ) ;
void process_river_threat_index ( ReportList * repHead ,
                                  int retrieval_required ) ;
pc_options_struct * pc_get_river_options_struct ( );
                                  

#endif /* #ifndef POINTCONTROL_RIVERSTATUS */


/*******************************************************************************
* FILENAME:            pointcontrol_print.h
* DESCRIPTION:         Contains the prototypes for the 
*                      printPCPrecipData and printPPPrecipData routines.
* 
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       September 2, 2004
* ORGANIZATION:        OHD-11 HSEB
* MACHINE:             Redhat Linux Enterprise
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    Sept 2, 2004 Bryon Lawrence    Original Coding  
********************************************************************************
*/

#ifndef POINTCONTROL_PRINT_H
#define POINTCONTROL_PRINT_H

#include "CurPC.h"
#include "CurPP.h"
#include "LatestObsValue.h"
#include "Observation.h"
#include "pointcontrol_riverstatus.h"
#include "RiverStatus.h"


void    printHeightData(Observation *obshHead);
void    printDischargeData(Observation *obsdHead);
void    printLatestData(LatestObsValue   *lHead);
void    printPCPrecipData ( CurPC * pcHead ) ;
void    printPPPrecipData ( CurPP * ppHead ) ;
void    printReports(ReportList *repHead);
void    printRiverStatusData(RiverStatus *rsHead);
void    printSnowTempOtherData(Observation *obsHead);

#endif /* #ifndef POINTCONTROL_PRINT_H */

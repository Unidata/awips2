/*******************************************************************************
* FILENAME:            pointcontrol_addmissing.h
* GENERAL INFORMATION:  
* DESCRIPTION:         Contains the prototype information for
*                      the add_missing_reports and add_missing_report routines.
*
* ORIGINAL AUTHOR:  Unknown
* CREATION DATE:    Unknown
* ORGANIZATION:     OHD-11 / HSEB
* MACHINE:          HP-9000
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    12/31/2003   Bryon Lawrence    Added documentation.  Split the
*                                   Pointcontrol library into an engine
*                                   and a gui library.
********************************************************************************
*/

#ifndef POINTCONTROL_ADDMISSING_H
#define POINTCONTROL_ADDMISSING_H

#include "pointcontrol_report.h"
#include "pointcontrol_options.h"

void add_missing_reports ( pc_options_struct * pc_options ,
                           ReportList           ** obsrepHeadDPtr ) ;

void add_missing_report ( pc_options_struct     * pc_options ,
                          char                  * lid ,
                          ReportList            ** listHeadPtr ,
                          ReportList            * listPtr ) ;


#endif /* #ifndef POINTCONTROL_ADDMISSING_H */

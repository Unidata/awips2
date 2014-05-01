
/*******************************************************************************
* FILENAME:
* GENERAL INFORMATION:
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

#ifndef POINTCONTROL_VALUE_H
#define POINTCONTROL_VALUE_H

#include "pointcontrol_options.h" 
#include "pointcontrol_report.h"

enum PeType { HG , QR , PRIMARY } ;

void process_value_option_menu ( ReportList * reportHead ,
                                 const pc_options_struct * pc_options ,
                                 int retrieval_required ) ;

#endif  /* ifndef POINTCONTROL_VALUE_H */

/*******************************************************************************
* FILENAME:           gage_pp_init.h
* DESCRIPTION:        Contains the prototype for the gage_pp_init routine.
*
* ORIGINAL AUTHOR:    Bryon Lawrence
* CREATION DATE:      December 17, 2004 
* ORGANIZATION:       HSEB/OHD
* MACHINE:            Linux
* MODIFICATION HISTORY:
*      DATE         PROGRAMMER        DESCRIPTION/REASON
* Dec 17, 2004      Bryon Lawrence    Original Coding
********************************************************************************
*/
#ifndef GAGE_PP_INIT_H
#define GAGE_PP_INIT_H

#include "DbmsDefs.h"
#include "gage_pp_write_rec.h"
#include "HourlyPP.h"
#include "time_convert.h"

int gage_pp_init ( HourlyPP * pHourlyPP,
                   const char datetime [ ANSI_YEARSEC_TIME_LEN + 1 ],
                   const char lid [ LOC_ID_LEN + 1 ],
                   const char ts [ SHEF_TS_LEN + 1 ],
                   short int value,
                   char obsdate [ OBSYTD + 1 ],
                   char minute_offset ,
                   char qc ) ;

#endif /* #ifndef GAGE_PP_INIT_H */

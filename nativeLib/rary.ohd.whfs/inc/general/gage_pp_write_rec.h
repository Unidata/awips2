/*******************************************************************************
* FILENAME:            gage_pp_write_rec.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*         DESCRIPTION: This file contains constant declarations and prototype 
*                      of the process_file routine
*                      defined within the gage_pp_write_rec.c file.
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       July 7, 2004 
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   7/4/2001     Moria Shebsovich  Original Coding
*   10/20/2004   Bryon Lawrence    Added the prototype for the 
*                                  get_hour_slot_value function.
*   01/04/2006   Bryon Lawrence    Added intpc, intlppp, intuppp, and intppq members
*                                  to the GagePPoptions structure.
*********************************************************************************/

#ifndef GAGE_PP_WRITE_REC_H
#define GAGE_PP_WRITE_REC_H

#include "DailyPP.h"
#include "GeneralUtil.h"
#include "HourlyPC.h"
#include "HourlyPP.h"


typedef struct GagePPoptions
{
   int shef_duplicate ;     /* The shef_duplicate token. */
   int intpc;               /* The intpc token. */
   int intlppp;             /* The intlppp token. */
   int intuppp;             /* The intuppp token. */
   float intppq;              /* The intppq token. */
} GagePPoptions ;


#define GPP_BADRECORD 3
#define GPP_ERROR   1
#define GPP_OK      0
#define GPP_STOP    2
#define GPP_SKIP_RECORD 4

#define DAYSIZ 2
#define HOURSIZ 2
#define KEYSIZ 100
#define MINSIZ 2
#define OBSYTD 10

#define SECONDS_IN_6HOUR_PERIOD  (6 * SECONDS_PER_HOUR)

enum WriteRecDBaction { WriteRecInsert , WriteRecUpdate , WriteRecNone } ;

typedef struct WriteInfoStruct
{
   enum WriteRecDBaction db_action ;
   int num_hr_inserts ;
   int num_hr_updates ;
   int num_hr_ignored ;
}  WriteInfo ;

/* Function prototype. */
int compare_offset_codes ( char minute_offset1 , char minute_offset2 ) ;

char get_offset_code ( int minute ) ;

int gage_pp_write_rec ( const HourlyPP * pHourlyPP_new ,  WriteInfo * pWriteInfo ,
                        char * msgstr , const char * pe , 
                        const char * obsdate , const GagePPoptions * pOptions , 
                        const short * rev_code , 
                        const short * rev_6hour_code,
                        int value_count ) ;

int gage_pp_write_daily_rec ( const DailyPP * pDailyPP,
                              const GagePPoptions * pOptions,
                              const char * obstime,
                              WriteInfo * pWriteInfo,
                              char msgstr [ ],
                              int revision_flag ) ;

inline short int get_hour_slot_value ( const HourlyPP * pHourlyPP , 
		                               short int hour ) ;

inline void set_hour_slot_value ( HourlyPP * pHourlyPP , short int hour ,
		                          short int value ) ; 
		                          
inline short int get_6hour_slot_value ( const HourlyPP * pHourlyPP , 
		                                short int hour );
		                                
inline void set_6hour_slot_value ( HourlyPP * pHourlyPP , short int hour ,
                                   short int precip_value );

inline int use_precip_value ( short value_new,
                              short value_old,
                              char value_new_qc,
                              char value_old_qc,
                              char value_new_offset,
                              char value_old_offset,
                              int shef_duplicate,
                              int revision_code
                            );
                                   
#endif /* #ifndef GAGE_PP_WRITE_REC_H */



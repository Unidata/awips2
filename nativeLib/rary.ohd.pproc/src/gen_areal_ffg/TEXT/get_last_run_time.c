#include <stdio.h>
#include <string.h>
#include "sqlca.h"
#include "time_convert.h"
#include "PerfLog.h"
#include "current_GMT_dt.h"

/*
extern long int SQLCODE;
extern struct sqlca_s sqlca;
*/

void get_last_run_time(char *process, char strdt[22], long int *irc)

/*

   this function finds the last run time for the process name from the
    PerfLog table
  
   if no records are found for the process, then return 00z and today's date

*/

{
   char process_name[11], where[100], strcurr[22];
   dtime_t current_time;
   PerfLog *perfHead, *perfPtr;

   process_name[10] = '\0';
   strcpy(process_name,process);

   /*-----------------------------------*/
   /*  set up Linked List               */
   /*  if problem, then return today's date 00z */
   /*-----------------------------------*/

   sprintf(where,"WHERE process= '%s' ORDER BY start_time desc",process_name );

   perfHead = GetPerfLog(where);
   if(SQLCODE != 0 )
   {
     printf("PostgreSQL error %ld attempting select from PerfLog table\n",
        SQLCODE);
     printf("last run time = %s z assumed\n",strdt);

     *irc = 0;
     current_GMT_dt(&current_time);
     *irc = yearsec_dt_to_ansi(current_time, strcurr);
     sprintf(strdt,"%c%c%c%c-%c%c-%c%c 00:00:00",
             strcurr[0],strcurr[1],strcurr[2],strcurr[3],
             strcurr[5],strcurr[6],strcurr[8],strcurr[9]);

     return;
   }

   if(perfHead)
   {

      perfPtr = (PerfLog *) ListFirst(&perfHead->list);
      
      *irc = yearsec_dt_to_ansi(perfPtr->start_time, strdt);
      if(*irc != 0) printf("error = %ld from yearsec_dt_to_ansi\n",*irc);

   }
   else
   {

      /*----------------------------------------------------------*/
      /*  if no records for process, then return today's date 00z */
      /*----------------------------------------------------------*/

      *irc = 0;

      current_GMT_dt(&current_time);
      *irc = yearsec_dt_to_ansi(current_time, strcurr);
      sprintf(strdt,"%c%c%c%c-%c%c-%c%c 00:00:00",
          strcurr[0],strcurr[1],strcurr[2],strcurr[3],
          strcurr[5],strcurr[6],strcurr[8],strcurr[9]);

      printf("no records found for process = %s",process_name);
      printf(" -- last run time = %s assumed\n",strdt);
   }

}  /*  end get_last_run_time function  */

#include <time.h>
#include <stdio.h>
#include "siii_shared.h"
#include "RejectedData.h"
#include "time_convert.h"

void insert_reject ( char id[9], char odt[22], char ts[3], short int dur, 
                     float *orig_gval )

/*
this function inserts records into the RejectedData table
previously, this function inserted records into the ManRejectObs table

calling function: update_gval

*/

{
   extern char LOGNAME [ ] ;
   dtime_t otime,ctime;
   int ret;
   RejectedData fppp;
   time_t current_time;

   current_time = time ( NULL );
   timet_to_yearsec_dt ( current_time, &ctime );

   strcpy(fppp.lid,id);
   fppp.dur = dur;
   strcpy(fppp.ts,ts);
   strcpy(fppp.extremum,"Z");
   fppp.probability = 0.0;

   fppp.revision = 0;
   strcpy(fppp.shef_qual_code,"Z");
   strcpy(fppp.product_id,"XXXXXXXXXX");
   fppp.quality_code = 0;
   strcpy(fppp.reject_type,"M");
   strcpy(fppp.userid,LOGNAME);

   ret = yearsec_ansi_to_dt ( odt, &otime);

   if ( ret != 0 )
   {
      printf("PostgreSQL error %d converting to datetime -- record\n"
             "not written to RejectedData table",ret);
      return;
   }

   fppp.validtime = otime;
   fppp.basistime = otime;
   fppp.postingtime = ctime;
   fppp.producttime = ctime;

   /*------------------------------------*/
   /*   PP data records                  */
   /*------------------------------------*/

   strcpy ( fppp.pe, "PP" );
   fppp.value = * orig_gval;

   ret = PutRejectedData(&fppp);

   if ( ret != 0 )
   {
      printf("PostgreSQL error %d attempting insert into RejectedData\n"
              "table\n",ret);
   }
}

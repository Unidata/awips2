#include <stdio.h>
#include <string.h>
#include "sqlca.h"
#include "gen_areal_ffg.h"  
#include "ContingencyValue.h"
#include "current_GMT_dt.h"
#include "time_convert.h"
#include "QualityCode.h"

/* extern long int SQLCODE;
   extern struct sqlca_s sqlca; */

      void writeContingency(char *area_id, int duration_hrs, float value,
                            char *valtime)
{
/*

   this subroutine writes records to the ContingencyValue table
   SiteSpecific process reads these records

   duration_hrs = duration in units of hours
                  possible values = 1,3,6,12,24

   calling subroutine: process_areas

*/
      ContingencyValue fppp;
      int ret;
      char validtime[22], where[200], where1[100], where2[100];
      static int ncall=0;
      static long int qualcode;
      static dtime_t begin_post_time;
      static char post_time[22];
      dtime_t cur_post_time;

/*----------------------------------------------*/
/*   set up variables in structure              */
/*   validtime = timestamp from selected gridded FFG file  */
/*----------------------------------------------*/

if(ncall == 0)
{
   current_GMT_dt(&begin_post_time);
   yearsec_dt_to_ansi(begin_post_time, post_time);

   set_qccode(QC_DEFAULT, &qualcode);

   ncall++;
}

   strcpy(fppp.pe,"PP");
   strcpy(fppp.ts,"CP");
   strcpy(fppp.extremum,"Z");
   fppp.probability = -1.000;
   fppp.revision = 0;
   strcpy(fppp.product_id,"GRIDFFG");
   strcpy(fppp.shef_qual_code,"Z");
   fppp.quality_code = qualcode;

   strcpy(validtime, valtime);
   yearsec_ansi_to_dt(validtime, &fppp.validtime);

   current_GMT_dt(&cur_post_time);

      if(duration_hrs == 1)
         fppp.dur = 1001;
      else if(duration_hrs == 3)
         fppp.dur = 1003;
      else if(duration_hrs == 6)
         fppp.dur = 1006;        
      else if(duration_hrs == 12)
         fppp.dur = 1012;
      else if(duration_hrs == 24)
         fppp.dur = 2001;

      strcpy(fppp.lid,area_id);
      fppp.value = value;

      fppp.basistime = begin_post_time;
      fppp.producttime = begin_post_time;
      fppp.postingtime = cur_post_time;

      /*---------------------------------*/
      /*  print ContingencyValue record  */
      /*---------------------------------*/

/*
      yearsec_dt_to_ansi(fppp.basistime, strbtime);

      printf("record = %s %s %d %s %s %s %s %f %s %ld %s %s\n",
                    fppp.lid,
                    fppp.pe,
                    fppp.dur,
                    fppp.ts,
                    fppp.extremum,
                    strbtime,
                    validtime, 
                    fppp.value,                                  
                    fppp.shef_qual_code,
                    fppp.quality_code, 
                    post_time,      
                    validtime);   
*/

      /*------------------------------------------------------*/
      /* attempt to insert record into ContingencyValue table */
      /*------------------------------------------------------*/

      ret = PutContingencyValue(&fppp);
      if(ret == -400)
      {

           /*------------------------------------------------*/
           /* insert failed - attempt update                 */
           /*------------------------------------------------*/

sprintf(where1,"WHERE lid = '%s' AND pe = '%s' AND dur = '%d' AND ts = '%s' AND extremum = '%s' ",
        fppp.lid,fppp.pe,fppp.dur,fppp.ts,fppp.extremum);
sprintf(where2," AND probability = '%f' AND validtime = '%s' AND basistime = '%s' ",
        fppp.probability,valtime,post_time);
sprintf(where,"%s%s",where1,where2);
           ret = UpdateContingencyValue(&fppp,where);
           if(ret != 0)
             printf("PostgreSQL error %d attempting update to ContingencyValue table\n",
                     ret);

      }

}  /*  end writeContingency function  */

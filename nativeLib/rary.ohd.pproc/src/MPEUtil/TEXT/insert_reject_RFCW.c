#include "rfcwide.h"
#include "RejectedData.h"
#include "time_convert.h"

void insert_reject_RFCW ( char id[9], char ts[3], short int dur, char ope[3], 
		          char odt[22], float *orig_gval)

/*
this function inserts records into the RejectedData table

calling function: update_gval_RFCW

*/

{

extern char LOGNAME [ ] ;
dtime_t otime,ctime;
int ret;
RejectedData fppp;
time_t current_time;

/* ctime.dt_qual = TU_DTENCODE(TU_YEAR,TU_SECOND);*/
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

if((ret = yearsec_ansi_to_dt(odt,&otime)))
{
   printf("PostgreSQL error %d converting to datetime -- record not written to RejectedData table",ret);
   return;
}

fppp.validtime = otime;
fppp.basistime = otime;
fppp.postingtime = ctime;
fppp.producttime = ctime;

fppp.value = *orig_gval;

if(strcmp(ope,"PP") == 0)
{

   /*------------------------------------*/
   /*   PP data records                  */
   /*------------------------------------*/

   strcpy(fppp.pe,"PP");

   ret = PutRejectedData(&fppp);
   if(ret != 0)
     printf("PostgreSQL error %d attempting insert into RejectedData table (PP record)\n",ret);
}
else
{

   /*------------------------------------*/
   /*   PC data records                  */
   /*------------------------------------*/

   strcpy(fppp.pe,"PC");

   ret = PutRejectedData(&fppp);
   if(ret != 0)
     printf("PostgreSQL error %d attempting insert into RejectedData table (PC record)\n",ret);
}

}

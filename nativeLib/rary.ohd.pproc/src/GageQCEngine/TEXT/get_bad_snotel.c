#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

extern char message[150];

void get_bad_snotel( const char * snow_file )

{


   FILE *fp;
   int i,j,k;
   int zday,reason;
   char *p;
   char buf[1000];
   char hb5[100];
   int ier;
   int num_qc_days;

   /* Retrieve the number of days to QC data for. */
   num_qc_days = get_num_days_to_qc ( );

   extern int max_stations;
   extern struct pdata pdata[];
   extern struct station station[ ];

   for ( j=0; j<num_qc_days; j++) {

      for(i=0;i<max_stations;i++) {

         for(k=0;k<5;k++)
            pdata[j].stn[i].snoflag[k]=0;

      }

   }

   fp=fopen(snow_file,"r");

   if(fp==NULL)
   {
      bzero(message,150);
      sprintf(message,"%s%s\n","could not open file:",snow_file);
      logMessage(message);
      return;
   }
   else
   {
      bzero(message,150);
      sprintf(message,"%s%s\n","Opened file",snow_file);
      logMessage(message);
   }

   for(;;) {

      p=fgets(buf,100,fp);

      if(p==NULL)
         break;


      ier=sscanf(buf,"%s",hb5);

      for(i=0;i<max_stations;i++) {

         if(strcasecmp(hb5,station[i].hb5)==0) {


            for(;;) {

               p=fgets(buf,100,fp);

               if(p==NULL)
                  break;

               if(strstr(buf,"END")!=NULL)
                  break;

               ier=sscanf(buf,"%d %d",&zday,&reason);

               for(j=0;j<num_qc_days;j++) {

                  if(pdata[j].ztime==zday) {

                     for(k=0;k<5;k++){

                        pdata[j].stn[i].snoflag[k]=reason;

                    } 

                  }

               }


            }

            break;

         }


      }

   }



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

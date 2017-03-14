#include <stdlib.h>

#include "S3PostAnalPrefs.h"
#include "postX.h"
#include "GeneralUtil.h"
#include "read_papref.h" 

void read_papref()
/*
   this function reads the S3PostAnalPrefs table
   if no record is found for the user, then the default hard coded 
     values are used
  
   calling function: ReadParam
*/
{
   char where[100];
   char stemp1[4],stemp2[4],stemp3[4],stemp4[4],stemp5[4];


   S3PostAnalPrefs *s3prefHead = NULL;
   S3PostAnalPrefs *s3prefPtr = NULL;

   memset(stemp1,'\0',4);
   memset(stemp2,'\0',4);
   memset(stemp3,'\0',4);
   memset(stemp4,'\0',4);
   memset(stemp5,'\0',4);

/*--------------------------------------*/
/*  get record from S3PostAnalPrefs table  */
/*--------------------------------------*/
      
   sprintf(where,"WHERE userid = '%s' ",LOGNAME);   
   s3prefHead = GetS3PostAnalPrefs(where);
   
   if(s3prefHead != NULL)
   {
      s3prefPtr = (S3PostAnalPrefs*) ListFirst(&s3prefHead->list);

      if(s3prefPtr)
      {
          strcpy(stemp1,s3prefPtr->state_overlay);
          strcpy(stemp2,s3prefPtr->city_overlay);
          strcpy(stemp3,s3prefPtr->river_overlay);
          strcpy(stemp4,s3prefPtr->basin_overlay);
          strcpy(stemp5,s3prefPtr->radar_overlay);

          if (strcmp(stemp1, "off")==0 || strcmp(stemp1, "OFF")==0) istate = 0;
          if (strcmp(stemp2, "off")==0 || strcmp(stemp2, "OFF")==0) icity = 0;
          if (strcmp(stemp3, "on ")==0 || strcmp(stemp3, "ON ")==0) iriver = 1;
          if (strcmp(stemp4, "on ")==0 || strcmp(stemp4, "ON ")==0) ibound = 1;
          if (strcmp(stemp5, "on ")==0 || strcmp(stemp5, "ON ")==0) iring = 1;

          NUMHRS = s3prefPtr->num_hours_wind;
     }      
   }
   else
   {
      printf("no record for user in S3PostAnalPrefs table\n");
      return;
   }
     
   if (s3prefHead != NULL)
   {
       FreeS3PostAnalPrefs(s3prefHead);
       s3prefHead = NULL;
   }       
   
   return;
   
}  /*  end read_papref function  */

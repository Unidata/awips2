#include <stdlib.h>
#include <stdio.h>

#include "S3PostAnalParams.h"
#include "post_stage3.h"
#include "GeneralUtil.h"
#include "read_postparam.h"
#include "postX.h"

/*-------------------------------------------------
   this function reads the S3PostAnalparams table
   calling function: ReadParam
---------------------------------------------------*/
int read_postparam()
{
   char where[100];
   S3PostAnalParams *s3paramHead = NULL;
   S3PostAnalParams *s3paramPtr = NULL;
 
/*--------------------------------------*/
/*  get record from S3PostAnalParams table  */
/*--------------------------------------*/
      
   sprintf(where," ");   
   s3paramHead = GetS3PostAnalParams(where);
   
   if(s3paramHead != NULL)
   {
      s3paramPtr = (S3PostAnalParams*) ListFirst(&s3paramHead->list);

      if(s3paramPtr)
      {
          MINVAL = s3paramPtr->gg_min_gage_val;	  
	  MINDIST = s3paramPtr->gg_min_dist;
	  IWIND = s3paramPtr->gg_weighting;
	  SC = s3paramPtr->kernel_est_scale;
	  rhat = s3paramPtr->rhat;	            
     }      
     return(0);
   }
   else
      return(-1);
     
   if (s3paramHead != NULL)
   {
       FreeS3PostAnalParams(s3paramHead);
       s3paramHead = NULL;
   }       
      
}  /*  end read_postparam() function  */

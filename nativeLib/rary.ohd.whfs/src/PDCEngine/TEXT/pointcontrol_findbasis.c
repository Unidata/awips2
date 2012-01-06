/****************************************************************************
   File:           pointcontrol_findbasis.c 


   History:
   03/10/02     Bryon Lawrence    Gave this file a "once over" to try
                                  to clean up as many possible occurrences
                                  of NULL pointer reads as possible.
   
   **************************************************************************/

#include <stdio.h>

#include "pointcontrol_findbasis.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_report.h"

/*****************************************************************************
   determine_rs_mofo()
   
   **************************************************************************/
ReportList * determine_rs_mofo(ReportList *obsHead,
			       ReportList *fcstHead)
{
   ReportList 	* reportHead = NULL ;
   ReportList 	* obsPtr = NULL , * fcstPtr = NULL ;
   char		curlid[LOC_ID_LEN + 1];
   double	obsval, fcstval ;
   
   
   /* loop on the two lists and where there are cases where there
      is an entry in both obs and fcst lists for the same station,
      then use the higher of the two values.  if only one list
      has a value, use it. */
   
    
   /* if neither list has data, return now */
   
   if (!obsHead && !fcstHead)
      return(reportHead);
     
   
   /* initialize */
   
   obsPtr  = obsHead;
   fcstPtr = fcstHead; 
         
   
   /* loop until we are at the end of both lists. this is an interesting
      block of logic because it requires simultaneously marching thru
      two independent sets of data, while tracking the common lid info 
      in them at the same time. */
   
   while (obsPtr != (ReportList *) NULL || fcstPtr != (ReportList *)NULL)
   {
      
      /* determine which is the current id to work on.  this is 
	 the id that is first alphanumerically when looking
	 at both of the lists */
      
      if (obsPtr != (ReportList *)NULL && fcstPtr != (ReportList *)NULL)
      {
	 if (strcmp(obsPtr->lid, fcstPtr->lid) <= 0)
	    strcpy(curlid, obsPtr->lid);
	 
	 else
	    strcpy(curlid, fcstPtr->lid);
      }
      
      else if (obsPtr != (ReportList *)NULL)
	 strcpy(curlid, obsPtr->lid);
      
      else
	 strcpy(curlid, fcstPtr->lid);
      
      
      /* initialize for this lid */
      
      obsval = fcstval = MISSING_VAL;
           

      /* load the temporary value if the id matches the current id */   
      
      if ( ( obsPtr != NULL ) && strcmp(obsPtr->lid, curlid) == 0)
      {	
	 obsval = obsPtr->value;
      }  
      
      
      /* ditto for fcst */
      
      if ( ( fcstPtr != NULL ) && strcmp(fcstPtr->lid, curlid) == 0)
      {	 
	 fcstval = fcstPtr->value;
      }   
      
      
      /* load in the record if a valid value was found */
      
      /* if (DEBUG) printf("%s:%.2f:%.2f\n", curlid, obsval, fcstval); */
      
      if (obsval != MISSING_VAL || fcstval != MISSING_VAL)
      {
	 if (obsval >= fcstval)
	    reportHead = load_obsfcst_report(obsPtr, reportHead);
	 else
	    reportHead = load_obsfcst_report(fcstPtr, reportHead);	 
      }
      
      
      /* get the next record from the respective lists */ 
      
      if ( ( obsPtr != NULL ) && strcmp(obsPtr->lid, curlid) == 0)
	 obsPtr = (ReportList *) ListNext(&obsPtr->node);
            
      if ( ( fcstPtr != NULL ) && strcmp(fcstPtr->lid, curlid) == 0)	 
	 fcstPtr = (ReportList *) ListNext(&fcstPtr->node);
   
              
   }  /* end of while loop of ptrs != NULL */

   
   return(reportHead);
}


/*****************************************************************************
   load_obsfcst_report()
   This function appends the given report to the input list.
   The input list is passed back as an argument to handle
   the initial case where the list is empty.
   This function appends to a lists, which it creates if it
   is initially empty.
   
   ***************************************************************************/
ReportList * load_obsfcst_report(ReportList	 	*repPtr, 
				 ReportList		*inputHead)
{
   ReportList	*onereportPtr = NULL;
   ReportList	*outputPtr = NULL;
         
   if ( repPtr == NULL )
   {
      fprintf ( stderr , "In routine \"load_obsfcst_report\":\n"
                         "the \"repPtr\" input argument is NULL.\n"
                         "Exiting the routine with a NULL return value.\n" ) ;
      return NULL ;
   }

   /* copy the report into the linked list of Reports */
   
   if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
   {      
      /* copy fields */
      strcpy(onereportPtr->lid,      repPtr->lid);
      strcpy(onereportPtr->pe,       repPtr->pe);
      onereportPtr->dur =            repPtr->dur;
      strcpy(onereportPtr->ts,       repPtr->ts);
      strcpy(onereportPtr->extremum, repPtr->extremum);
      onereportPtr->probability =    repPtr->probability;
      
      strcpy(onereportPtr->shef_qual_code, repPtr->shef_qual_code);
      onereportPtr->quality_code =         repPtr->quality_code;
      
      onereportPtr->value     = repPtr->value;
      onereportPtr->value2    = MISSING_VAL ;
      onereportPtr->validtime = repPtr->validtime;
      onereportPtr->basistime = repPtr->basistime;

      onereportPtr->threat_index = THREAT_MISSING_DATA ;
      
      /* initialize the list; else set to Head pointer since
	 the outputPtr gets set to null each time this 
	 function is eccessed. */
      
      if (inputHead == NULL)
      {
	 outputPtr = onereportPtr;
	 ListInit(&outputPtr->list);
      }
            
      else
	 outputPtr = inputHead;
      
      
      /* add the data to the list */
      
      ListAdd(&outputPtr->list, &onereportPtr->node);
   }
   
   else
   {
      fprintf(stderr, "Error allocating for ReportList obsfcst report.\n");
   }
   
   
   return(outputPtr);
}


/*****************************************************************************
   copy_replist()
   Simply copy the contents of a list to another list.
   This creates a new list.
   
   ***************************************************************************/
ReportList * copy_replist(ReportList *inputHead)
{   
   ReportList	*inputPtr = NULL ;
   ReportList	*outputPtr = NULL;
   ReportList	*onereportPtr = NULL;
  
   /* have special check in the event of an empty input list */
   
   if (inputHead == NULL)
      return(NULL);
   
   
   /* set pointer to first in list */
   
   inputPtr = (ReportList *) ListFirst(&inputHead->list);            
      
   
   /* copy the report */
   
   while(inputPtr)
   {
      if ((onereportPtr = (ReportList *)malloc(sizeof(ReportList))) != NULL)
      {      
	 /* copy fields */
	 
	 strcpy(onereportPtr->lid,      inputPtr->lid);
	 strcpy(onereportPtr->pe,       inputPtr->pe);
	 onereportPtr->dur =            inputPtr->dur;
	 strcpy(onereportPtr->ts,       inputPtr->ts);
	 strcpy(onereportPtr->extremum, inputPtr->extremum);
	 onereportPtr->probability =    inputPtr->probability;
	 
	 strcpy(onereportPtr->shef_qual_code, inputPtr->shef_qual_code);
	 onereportPtr->quality_code =         inputPtr->quality_code;
	 
	 onereportPtr->value     = inputPtr->value;
         onereportPtr->value2    = MISSING_VAL ; 
	 onereportPtr->validtime = inputPtr->validtime;
	 onereportPtr->basistime = inputPtr->basistime;
	 
         onereportPtr->threat_index = THREAT_MISSING_DATA ;
	 
	 /* initialize the list */
	 
	 if (outputPtr == NULL)
	 {
	    outputPtr = onereportPtr;
	    ListInit(&outputPtr->list);
	 }
	 
	 /* add the data to the list */
	 
	 ListAdd(&outputPtr->list, &onereportPtr->node);
      }
      
      else
      {
	 fprintf(stderr, "Error allocating for ReportList copy report.\n");
      }
      
      
      /* get the next item in the input list */
      
      inputPtr = (ReportList *) ListNext(&inputPtr->node);
   }
   
   
   return(outputPtr);
}

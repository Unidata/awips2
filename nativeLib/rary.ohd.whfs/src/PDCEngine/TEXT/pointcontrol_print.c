/****************************************************************************
   File:           pointcontrol_print.c   

   History:
           Bryon Lawrence   03/10/02     Tried to minimize the occurrence
                                         of NULL pointer reads due to 
                                         missing database data. 

   **************************************************************************/

#include <stdio.h>

#include "CurPC.h"
#include "CurPP.h"
#include "LatestObsValue.h"
#include "List.h"
#include "Observation.h"
#include "pointcontrol_report.h"
#include "RiverStatus.h"
#include "time_convert.h"


/****************************************************************************
   printHeightData()
   *************************************************************************/

void printHeightData(Observation *obshHead)
{
   Observation	*oPtr = NULL ;   
   char 	obstime_ansi[ANSI_TIME_LEN+1];   
   int		n, count;
   
   
   if (obshHead)
      count = ListCount(&obshHead->list);
   
   else
   {
      printf("No Height data...\n");
      return;
      
   }
   
   printf("  Height count = %d\n", count);
   
   oPtr = (Observation *) ListFirst(&obshHead->list);
   n = 0;
   while (oPtr)
   {
      if (n++ < 10)
      {
	 yearsec_dt_to_ansi(oPtr->obstime,obstime_ansi);
	 printf("%s %s %s %.2f %s\n", oPtr->lid, oPtr->pe, oPtr->ts,
		oPtr->value, obstime_ansi);
      }
      oPtr = (Observation *) ListNext(&oPtr->node);
      
   }
   
   
   return;   
}


/******************************************************************************
   printDischargeData()
   ***************************************************************************/

void printDischargeData(Observation *obsdHead)
{
   Observation	*oPtr = NULL ;   
   char 	obstime_ansi[ANSI_TIME_LEN+1];   
   int		n, count;
   
   if (obsdHead)
      count = ListCount(&obsdHead->list);
   
   else
   {
      printf("No Discharge data...\n");
      return;      
   }
   
   printf("  Discharge count = %d\n", count);
   
   oPtr = (Observation *) ListFirst(&obsdHead->list);
   n = 0;
   while (oPtr)
   {
      if (n++ < 10)
      {
	 yearsec_dt_to_ansi(oPtr->obstime, obstime_ansi);
	 printf("%s %s %s %.2f %s\n", oPtr->lid, oPtr->pe, oPtr->ts,
		oPtr->value, obstime_ansi);
      }
      
      oPtr = (Observation *) ListNext(&oPtr->node);      
   }
   
   
   return;
}


/*****************************************************************************
   printLatestData()
   **************************************************************************/
void  printLatestData(LatestObsValue   *lHead)
{
   LatestObsValue  	*lPtr = NULL ;   
   char 		obstime_ansi[ANSI_TIME_LEN+1];   
   int              	n, count;
   
   if (lHead)
      count = ListCount(&lHead->list);
   
   else
   {
      printf("No Latest data...\n");
      return;
   }
   
   printf(" Latest count = %d\n", count);
   
   lPtr = (LatestObsValue *) ListFirst(&lHead->list);
   n = 0;
   while (lPtr)
   {
      if (n++ < 10) 
      {
	 yearsec_dt_to_ansi(lPtr->obstime, obstime_ansi);
	 printf("%s %s %s %.2f %s\n", lPtr->lid, lPtr->pe, lPtr->ts,
		lPtr->value, obstime_ansi);
      }
      
      lPtr = (LatestObsValue *) ListNext(&lPtr->node);      
   }
   
   
   return;
}


/*****************************************************************************
   printRiverStatusData()
   ***************************************************************************/

void printRiverStatusData(RiverStatus *rsHead)
{
   RiverStatus	*rPtr = NULL ;   
   char 	validtime_ansi[ANSI_TIME_LEN+1];   
   int		n, count;
   
   if (rsHead)
      count = ListCount(&rsHead->list);
   
   else
   {
      printf("No RiverStatus data...\n");
      return;
      
   }
   
   printf("  RiverStatus count = %d\n", count);
   
   rPtr = (RiverStatus *) ListFirst(&rsHead->list);
   n = 0;
   while (rPtr)
   {
      if (n++ < 10)
      {
	 yearsec_dt_to_ansi(rPtr->validtime, validtime_ansi);
	 printf("%s %s %s %10.2f %s\n", rPtr->lid, rPtr->pe, rPtr->ts,
		rPtr->value, validtime_ansi);
      }
      
      rPtr = (RiverStatus*) ListNext(&rPtr->node);      
   }
   
   
   return;
}


/*****************************************************************************
   printPCPrecipData()
   **************************************************************************/
void printPCPrecipData(CurPC     	 *pcHead)
{
   CurPC  	*tPtr = NULL ;   
   char		obstime_ansi[ANSI_TIME_LEN+1];   
   int		n, count;
   
   if (pcHead)
      count = ListCount(&pcHead->list);
   
   else
   {
      printf("No PC data.\n");
      return;
   }
   
   printf("  PC count = %d\n", count);
   
   tPtr = (CurPC *) ListFirst(&pcHead->list);
   n = 0;
   while (tPtr)
   {
      if (n++ < 10) 
      {
	 yearsec_dt_to_ansi(tPtr->obstime, obstime_ansi);
	 printf("%s %s %s %.2f %s\n", tPtr->lid, tPtr->pe, tPtr->ts,
		tPtr->value, obstime_ansi);
      }
      
      tPtr = (CurPC *) ListNext(&tPtr->node);      
   }
      
   
   return;
}



/******************************************************************************
   printPPPrecipData()
   ***************************************************************************/
void printPPPrecipData(CurPP *ppHead)
{
   CurPP  	*tPtr = NULL ;   
   char 	obstime_ansi[ANSI_TIME_LEN+1];    
   int	 	n, count;
   
   if (ppHead)
      count = ListCount(&ppHead->list);
   
   else
   {
      printf("No PP data.\n");
      return;
   }
   
   printf("  PP count = %d\n", count);
   
   tPtr = (CurPP *) ListFirst(&ppHead->list);
   n = 0;
   while (tPtr )
   {
      if (n++ < 10)
      {
	 yearsec_dt_to_ansi(tPtr->obstime, obstime_ansi);
	 printf("%s %s %s %.2f %s\n", tPtr->lid, tPtr->pe, tPtr->ts,
		tPtr->value, obstime_ansi);
      }
      tPtr = (CurPP *) ListNext(&tPtr->node);
      
   }
   
   
   return;
}


/*****************************************************************************
   printSnowTempOtherData()
   **************************************************************************/
void  printSnowTempOtherData(Observation	 *obsHead)
{
   Observation	*oPtr = NULL ;   
   char 	obstime_ansi[ANSI_TIME_LEN+1];   
   int		n, count;
   
   if (obsHead)
      count = ListCount(&obsHead->list);
   
   else
   {
      printf("No Generic data.\n");
      return;
   }
   
   printf("  Generic count = %d\n", count);
   
   oPtr = (Observation *) ListFirst(&obsHead->list);
   n = 0;
   while (oPtr)
   {
      if (n++ < 10)
      {
	 yearsec_dt_to_ansi(oPtr->obstime, obstime_ansi);
	 printf("%s %s %s %.2f %s\n", oPtr->lid, oPtr->pe, oPtr->ts,
		oPtr->value, obstime_ansi);
      }
      
      oPtr = (Observation *) ListNext(&oPtr->node);      
   }
   
   
   return;
}


/******************************************************************************
   printReports()
   ***************************************************************************/
void printReports(ReportList *genericrepHead)
{
   ReportList  	*rPtr = NULL ;   
   char 	obstime_ansi[ANSI_TIME_LEN+1];    
   int	 	n, count;
   
   if (genericrepHead)
      count = ListCount(&genericrepHead->list);
   
   else
   {
      printf("No report data.\n");
      return;
   }
   
   printf("  report count = %d\n", count);
   
   rPtr = (ReportList *) ListFirst(&genericrepHead->list);
   n = 0;
   while (rPtr )
   {
      if (n++ < 1000)
      {
	 if (rPtr->use)
	 {
	    timet_to_yearsec_ansi(rPtr->validtime, obstime_ansi);
	    printf("%s %s %ld %s %.2f %s\n",
		   rPtr->lid, rPtr->pe, rPtr->dur, rPtr->ts,
		   rPtr->value, obstime_ansi);
	 }
	 
	 else
	 {
	    timet_to_yearsec_ansi(rPtr->validtime, obstime_ansi);
	    printf("%s %s %ld %s %.2f %s *FILTERED_OUT\n",
		   rPtr->lid, rPtr->pe, rPtr->dur, rPtr->ts,
		   rPtr->value, obstime_ansi);
	 }
      }
      rPtr = (ReportList *) ListNext(&rPtr->node);      
   }
   
   
   return;
}

/*
 	Procedure:	copy_lid.c
	Date:		May 1997
	Author:		Paul Taylor

	Purpose:	Procedure to copy all lid-related information
			from one lid to another.
			
	WARNING:  This is a "perl-generated" function.
	          DO NOT modify this "c" file directly !!
*/

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include <datetime.h>
#include <time.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "ParamDefs.h"
#include "user_prefs.h"
#include "GeneralUtil.h"

#include "copy_lid.h"

#include "Location.h"
#include "Riverstat.h"
#include "RiverMonLocation.h"
#include "FcstPtService.h"
#include "FcstPtWatSup.h"
#include "FcstPtDeterm.h"
#include "FcstPtESP.h"
#include "AdjustFactor.h"
#include "Agricultural.h"
#include "AlertAlarmVal.h"
#include "ArealFcst.h"
#include "ArealObs.h"
#include "Benchmark.h"
#include "CommentValue.h"
#include "Contacts.h"
#include "ContingencyValue.h"
#include "Countynum.h"
#include "Crest.h"
#include "CurPC.h"
#include "CurPP.h"
#include "DailyPP.h"
#include "Datum.h"
#include "Dcp.h"
#include "Descrip.h"
#include "Discharge.h"
#include "Evaporation.h"
#include "FcstDischarge.h"
#include "FcstHeight.h"
#include "FcstPrecip.h"
#include "FcstTemperature.h"
#include "FcstOther.h"
#include "FishCount.h"
#include "Flood.h"
#include "Floodcat.h"
#include "Floodstmt.h"
#include "FloodTs.h"
#include "FpPrevProd.h"
#include "Gage.h"
#include "GateDam.h"
#include "Ground.h"
#include "Height.h"
#include "HgStation.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "Ice.h"
#include "IngestFilter.h"
#include "Lake.h"
#include "LatestObsValue.h"
#include "LocArea.h"
#include "LocExtAgency.h"
#include "LocImage.h"
#include "LocDataLimits.h"
#include "Lowwater.h"
#include "Moisture.h"
#include "MonthlyValues.h"
#include "OFSStnTrans.h"
#include "Observer.h"
#include "PairedValue.h"
#include "Power.h"
#include "Pressure.h"
#include "ProcValue.h"
#include "ProductLink.h"
#include "Pub.h"
#include "Radiation.h"
#include "Rating.h"
#include "RatingShift.h"
#include "RawPC.h"
#include "RawPP.h"
#include "RawPother.h"
#include "Refer.h"
#include "RejectedData.h"
#include "Reservoir.h"
#include "Rescap.h"
#include "RpfFcstPoint.h"
#include "Snow.h"
#include "SshpConfig.h"
#include "StnClass.h"
#include "Telem.h"
#include "Temperature.h"
#include "UnitGraph.h"
#include "UnkStn.h"
#include "UnkStnValue.h"
#include "WaterQuality.h"
#include "Weather.h"
#include "Wind.h"
#include "YUnique.h"
#include "Zonenum.h"


void	CopyLid_All (char *fromlid, char *tolid)
{
   Location	*locFrom = NULL;
   Location	*locTo = NULL;
   
   char		where[MAX_WHERE_LEN];
   

   /*
	Perform checks.
   */
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);
   locFrom = (Location *) GetLocation(where);
   if (locFrom == NULL)
   {
      return;  /* no source lid specified */
   }
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", tolid);
   locTo = (Location *) GetLocation(where);
   if (locTo != NULL)
   {
      FreeLocation(locTo);
      return;  /* destination lid must not exist in the database! */
   }

   
   
   /*
   	Copy all necessary information.
   */
   Copy_Location(fromlid, tolid);
   Copy_Riverstat(fromlid, tolid);
   Copy_RiverMonLocation(fromlid, tolid);
   Copy_FcstPtService(fromlid, tolid);
   Copy_FcstPtWatSup(fromlid, tolid);
   Copy_FcstPtDeterm(fromlid, tolid);
   Copy_FcstPtESP(fromlid, tolid);
   Copy_AdjustFactor(fromlid, tolid);
   Copy_Agricultural(fromlid, tolid);
   Copy_AlertAlarmVal(fromlid, tolid);
   Copy_ArealFcst(fromlid, tolid);
   Copy_ArealObs(fromlid, tolid);
   Copy_Benchmark(fromlid, tolid);
   Copy_CommentValue(fromlid, tolid);
   Copy_Contacts(fromlid, tolid);
   Copy_ContingencyValue(fromlid, tolid);
   Copy_Countynum(fromlid, tolid);
   Copy_Crest(fromlid, tolid);
   Copy_CurPC(fromlid, tolid);
   Copy_CurPP(fromlid, tolid);
   Copy_DailyPP(fromlid, tolid);
   Copy_Datum(fromlid, tolid);
   Copy_Dcp(fromlid, tolid);
   Copy_Descrip(fromlid, tolid);
   Copy_Discharge(fromlid, tolid);
   Copy_Evaporation(fromlid, tolid);
   Copy_FcstDischarge(fromlid, tolid);
   Copy_FcstHeight(fromlid, tolid);
   Copy_FcstPrecip(fromlid, tolid);
   Copy_FcstTemperature(fromlid, tolid);
   Copy_FcstOther(fromlid, tolid);
   Copy_FishCount(fromlid, tolid);
   Copy_Flood(fromlid, tolid);
   Copy_Floodcat(fromlid, tolid);
   Copy_Floodstmt(fromlid, tolid);
   Copy_FloodTs(fromlid, tolid);
   Copy_FpPrevProd(fromlid, tolid);
   Copy_Gage(fromlid, tolid);
   Copy_GateDam(fromlid, tolid);
   Copy_Ground(fromlid, tolid);
   Copy_Height(fromlid, tolid);
   Copy_HgStation(fromlid, tolid);
   Copy_HourlyPC(fromlid, tolid);
   Copy_HourlyPP(fromlid, tolid);
   Copy_Ice(fromlid, tolid);
   Copy_IngestFilter(fromlid, tolid);
   Copy_Lake(fromlid, tolid);
   Copy_LatestObsValue(fromlid, tolid);
   Copy_LocArea(fromlid, tolid);
   Copy_LocExtAgency(fromlid, tolid);
   Copy_LocImage(fromlid, tolid);
   Copy_LocDataLimits(fromlid, tolid);
   Copy_Lowwater(fromlid, tolid);
   Copy_Moisture(fromlid, tolid);
   Copy_MonthlyValues(fromlid, tolid);
   Copy_OFSStnTrans(fromlid, tolid);
   Copy_Observer(fromlid, tolid);
   Copy_PairedValue(fromlid, tolid);
   Copy_Power(fromlid, tolid);
   Copy_Pressure(fromlid, tolid);
   Copy_ProcValue(fromlid, tolid);
   Copy_ProductLink(fromlid, tolid);
   Copy_Pub(fromlid, tolid);
   Copy_Radiation(fromlid, tolid);
   Copy_Rating(fromlid, tolid);
   Copy_RatingShift(fromlid, tolid);
   Copy_RawPC(fromlid, tolid);
   Copy_RawPP(fromlid, tolid);
   Copy_RawPother(fromlid, tolid);
   Copy_Refer(fromlid, tolid);
   Copy_RejectedData(fromlid, tolid);
   Copy_Reservoir(fromlid, tolid);
   Copy_Rescap(fromlid, tolid);
   Copy_RpfFcstPoint(fromlid, tolid);
   Copy_Snow(fromlid, tolid);
   Copy_SshpConfig(fromlid, tolid);
   Copy_StnClass(fromlid, tolid);
   Copy_Telem(fromlid, tolid);
   Copy_Temperature(fromlid, tolid);
   Copy_UnitGraph(fromlid, tolid);
   Copy_UnkStn(fromlid, tolid);
   Copy_UnkStnValue(fromlid, tolid);
   Copy_WaterQuality(fromlid, tolid);
   Copy_Weather(fromlid, tolid);
   Copy_Wind(fromlid, tolid);
   Copy_YUnique(fromlid, tolid);
   Copy_Zonenum(fromlid, tolid);

   return;
}







void	CopyLid_Reference (char *fromlid, char *tolid)
{
   Location	*locFrom = NULL;
   Location	*locTo = NULL;
   
   char		where[MAX_WHERE_LEN];
   

   /*
	Perform checks.
   */
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);
   locFrom = (Location *) GetLocation(where);
   if (locFrom == NULL)
   {
      return;  /* no source lid specified */
   }
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", tolid);
   locTo = (Location *) GetLocation(where);
   if (locTo != NULL)
   {
      FreeLocation(locTo);
      return;  /* destination lid must not exist in the database! */
   }

   
   
   /*
   	Copy all necessary information.
   */
   Copy_Location(fromlid, tolid);
   Copy_Riverstat(fromlid, tolid);
   Copy_RiverMonLocation(fromlid, tolid);
   Copy_FcstPtService(fromlid, tolid);
   Copy_FcstPtWatSup(fromlid, tolid);
   Copy_FcstPtDeterm(fromlid, tolid);
   Copy_FcstPtESP(fromlid, tolid);
   Copy_AdjustFactor(fromlid, tolid);
   Copy_Benchmark(fromlid, tolid);
   Copy_Contacts(fromlid, tolid);
   Copy_Countynum(fromlid, tolid);
   Copy_Crest(fromlid, tolid);
   Copy_Datum(fromlid, tolid);
   Copy_Dcp(fromlid, tolid);
   Copy_Descrip(fromlid, tolid);
   Copy_Flood(fromlid, tolid);
   Copy_Floodcat(fromlid, tolid);
   Copy_Floodstmt(fromlid, tolid);
   Copy_Gage(fromlid, tolid);
   Copy_IngestFilter(fromlid, tolid);
   Copy_LocArea(fromlid, tolid);
   Copy_LocDataLimits(fromlid, tolid);
   Copy_LocExtAgency(fromlid, tolid);
   Copy_LocImage(fromlid, tolid);
   Copy_Lowwater(fromlid, tolid);
   Copy_Observer(fromlid, tolid);
   Copy_OFSStnTrans(fromlid, tolid);
   Copy_Pub(fromlid, tolid);
   Copy_Rating(fromlid, tolid);
   Copy_RatingShift(fromlid, tolid);
   Copy_Refer(fromlid, tolid);
   Copy_Reservoir(fromlid, tolid);
   Copy_Rescap(fromlid, tolid);
   Copy_RpfFcstPoint(fromlid, tolid);
   Copy_SshpConfig(fromlid, tolid);
   Copy_StnClass(fromlid, tolid);
   Copy_Telem(fromlid, tolid);
   Copy_Zonenum(fromlid, tolid);

   return;
}





void	Copy_Location(char *fromlid, char *tolid)
{
   Location	*info = NULL, *infoPtr = NULL;

   Location	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Location *) GetLocation(where);
   if (info != NULL)
   {
      infoPtr = (Location *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLocation(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Location record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Location *) ListNext(&infoPtr->node);
      }
      
      FreeLocation(info);
   }
   
   return;
}




void	Copy_Riverstat(char *fromlid, char *tolid)
{
   Riverstat	*info = NULL, *infoPtr = NULL;

   Riverstat	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Riverstat *) GetRiverstat(where);
   if (info != NULL)
   {
      infoPtr = (Riverstat *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRiverstat(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Riverstat record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Riverstat *) ListNext(&infoPtr->node);
      }
      
      FreeRiverstat(info);
   }
   
   return;
}




void	Copy_RiverMonLocation(char *fromlid, char *tolid)
{
   RiverMonLocation	*info = NULL, *infoPtr = NULL;

   RiverMonLocation	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RiverMonLocation *) GetRiverMonLocation(where);
   if (info != NULL)
   {
      infoPtr = (RiverMonLocation *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRiverMonLocation(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RiverMonLocation record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RiverMonLocation *) ListNext(&infoPtr->node);
      }
      
      FreeRiverMonLocation(info);
   }
   
   return;
}




void	Copy_FcstPtService(char *fromlid, char *tolid)
{
   FcstPtService	*info = NULL, *infoPtr = NULL;

   FcstPtService	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstPtService *) GetFcstPtService(where);
   if (info != NULL)
   {
      infoPtr = (FcstPtService *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstPtService(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstPtService record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstPtService *) ListNext(&infoPtr->node);
      }
      
      FreeFcstPtService(info);
   }
   
   return;
}




void	Copy_FcstPtWatSup(char *fromlid, char *tolid)
{
   FcstPtWatSup	*info = NULL, *infoPtr = NULL;

   FcstPtWatSup	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstPtWatSup *) GetFcstPtWatSup(where);
   if (info != NULL)
   {
      infoPtr = (FcstPtWatSup *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstPtWatSup(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstPtWatSup record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstPtWatSup *) ListNext(&infoPtr->node);
      }
      
      FreeFcstPtWatSup(info);
   }
   
   return;
}




void	Copy_FcstPtDeterm(char *fromlid, char *tolid)
{
   FcstPtDeterm	*info = NULL, *infoPtr = NULL;

   FcstPtDeterm	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstPtDeterm *) GetFcstPtDeterm(where);
   if (info != NULL)
   {
      infoPtr = (FcstPtDeterm *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstPtDeterm(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstPtDeterm record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstPtDeterm *) ListNext(&infoPtr->node);
      }
      
      FreeFcstPtDeterm(info);
   }
   
   return;
}




void	Copy_FcstPtESP(char *fromlid, char *tolid)
{
   FcstPtESP	*info = NULL, *infoPtr = NULL;

   FcstPtESP	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstPtESP *) GetFcstPtESP(where);
   if (info != NULL)
   {
      infoPtr = (FcstPtESP *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstPtESP(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstPtESP record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstPtESP *) ListNext(&infoPtr->node);
      }
      
      FreeFcstPtESP(info);
   }
   
   return;
}




void	Copy_AdjustFactor(char *fromlid, char *tolid)
{
   AdjustFactor	*info = NULL, *infoPtr = NULL;

   AdjustFactor	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (AdjustFactor *) GetAdjustFactor(where);
   if (info != NULL)
   {
      infoPtr = (AdjustFactor *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutAdjustFactor(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert AdjustFactor record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (AdjustFactor *) ListNext(&infoPtr->node);
      }
      
      FreeAdjustFactor(info);
   }
   
   return;
}




void	Copy_Agricultural(char *fromlid, char *tolid)
{
   Agricultural	*info = NULL, *infoPtr = NULL;

   Agricultural	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Agricultural *) GetAgricultural(where);
   if (info != NULL)
   {
      infoPtr = (Agricultural *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutAgricultural(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Agricultural record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Agricultural *) ListNext(&infoPtr->node);
      }
      
      FreeAgricultural(info);
   }
   
   return;
}




void	Copy_AlertAlarmVal(char *fromlid, char *tolid)
{
   AlertAlarmVal	*info = NULL, *infoPtr = NULL;

   AlertAlarmVal	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (AlertAlarmVal *) GetAlertAlarmVal(where);
   if (info != NULL)
   {
      infoPtr = (AlertAlarmVal *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutAlertAlarmVal(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert AlertAlarmVal record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (AlertAlarmVal *) ListNext(&infoPtr->node);
      }
      
      FreeAlertAlarmVal(info);
   }
   
   return;
}




void	Copy_ArealFcst(char *fromlid, char *tolid)
{
   ArealFcst	*info = NULL, *infoPtr = NULL;

   ArealFcst	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (ArealFcst *) GetArealFcst(where);
   if (info != NULL)
   {
      infoPtr = (ArealFcst *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutArealFcst(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert ArealFcst record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (ArealFcst *) ListNext(&infoPtr->node);
      }
      
      FreeArealFcst(info);
   }
   
   return;
}




void	Copy_ArealObs(char *fromlid, char *tolid)
{
   ArealObs	*info = NULL, *infoPtr = NULL;

   ArealObs	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (ArealObs *) GetArealObs(where);
   if (info != NULL)
   {
      infoPtr = (ArealObs *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutArealObs(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert ArealObs record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (ArealObs *) ListNext(&infoPtr->node);
      }
      
      FreeArealObs(info);
   }
   
   return;
}




void	Copy_Benchmark(char *fromlid, char *tolid)
{
   Benchmark	*info = NULL, *infoPtr = NULL;

   Benchmark	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Benchmark *) GetBenchmark(where);
   if (info != NULL)
   {
      infoPtr = (Benchmark *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutBenchmark(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Benchmark record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Benchmark *) ListNext(&infoPtr->node);
      }
      
      FreeBenchmark(info);
   }
   
   return;
}




void	Copy_CommentValue(char *fromlid, char *tolid)
{
   CommentValue	*info = NULL, *infoPtr = NULL;

   CommentValue	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (CommentValue *) GetCommentValue(where);
   if (info != NULL)
   {
      infoPtr = (CommentValue *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutCommentValue(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert CommentValue record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (CommentValue *) ListNext(&infoPtr->node);
      }
      
      FreeCommentValue(info);
   }
   
   return;
}




void	Copy_Contacts(char *fromlid, char *tolid)
{
   Contacts	*info = NULL, *infoPtr = NULL;

   Contacts	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Contacts *) GetContacts(where);
   if (info != NULL)
   {
      infoPtr = (Contacts *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutContacts(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Contacts record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Contacts *) ListNext(&infoPtr->node);
      }
      
      FreeContacts(info);
   }
   
   return;
}




void	Copy_ContingencyValue(char *fromlid, char *tolid)
{
   ContingencyValue	*info = NULL, *infoPtr = NULL;

   ContingencyValue	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (ContingencyValue *) GetContingencyValue(where);
   if (info != NULL)
   {
      infoPtr = (ContingencyValue *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutContingencyValue(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert ContingencyValue record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (ContingencyValue *) ListNext(&infoPtr->node);
      }
      
      FreeContingencyValue(info);
   }
   
   return;
}




void	Copy_Countynum(char *fromlid, char *tolid)
{
   Countynum	*info = NULL, *infoPtr = NULL;

   Countynum	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Countynum *) GetCountynum(where);
   if (info != NULL)
   {
      infoPtr = (Countynum *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutCountynum(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Countynum record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Countynum *) ListNext(&infoPtr->node);
      }
      
      FreeCountynum(info);
   }
   
   return;
}




void	Copy_Crest(char *fromlid, char *tolid)
{
   Crest	*info = NULL, *infoPtr = NULL;

   Crest	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Crest *) GetCrest(where);
   if (info != NULL)
   {
      infoPtr = (Crest *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutCrest(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Crest record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Crest *) ListNext(&infoPtr->node);
      }
      
      FreeCrest(info);
   }
   
   return;
}




void	Copy_CurPC(char *fromlid, char *tolid)
{
   CurPC	*info = NULL, *infoPtr = NULL;

   CurPC	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (CurPC *) GetCurPC(where);
   if (info != NULL)
   {
      infoPtr = (CurPC *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutCurPC(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert CurPC record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (CurPC *) ListNext(&infoPtr->node);
      }
      
      FreeCurPC(info);
   }
   
   return;
}




void	Copy_CurPP(char *fromlid, char *tolid)
{
   CurPP	*info = NULL, *infoPtr = NULL;

   CurPP	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (CurPP *) GetCurPP(where);
   if (info != NULL)
   {
      infoPtr = (CurPP *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutCurPP(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert CurPP record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (CurPP *) ListNext(&infoPtr->node);
      }
      
      FreeCurPP(info);
   }
   
   return;
}




void	Copy_DailyPP(char *fromlid, char *tolid)
{
   DailyPP	*info = NULL, *infoPtr = NULL;

   DailyPP	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (DailyPP *) GetDailyPP(where);
   if (info != NULL)
   {
      infoPtr = (DailyPP *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutDailyPP(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert DailyPP record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (DailyPP *) ListNext(&infoPtr->node);
      }
      
      FreeDailyPP(info);
   }
   
   return;
}




void	Copy_Datum(char *fromlid, char *tolid)
{
   Datum	*info = NULL, *infoPtr = NULL;

   Datum	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Datum *) GetDatum(where);
   if (info != NULL)
   {
      infoPtr = (Datum *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutDatum(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Datum record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Datum *) ListNext(&infoPtr->node);
      }
      
      FreeDatum(info);
   }
   
   return;
}




void	Copy_Dcp(char *fromlid, char *tolid)
{
   Dcp	*info = NULL, *infoPtr = NULL;

   Dcp	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Dcp *) GetDcp(where);
   if (info != NULL)
   {
      infoPtr = (Dcp *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutDcp(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Dcp record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Dcp *) ListNext(&infoPtr->node);
      }
      
      FreeDcp(info);
   }
   
   return;
}




void	Copy_Descrip(char *fromlid, char *tolid)
{
   Descrip	*info = NULL, *infoPtr = NULL;

   Descrip	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Descrip *) GetDescrip(where);
   if (info != NULL)
   {
      infoPtr = (Descrip *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutDescrip(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Descrip record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Descrip *) ListNext(&infoPtr->node);
      }
      
      FreeDescrip(info);
   }
   
   return;
}




void	Copy_Discharge(char *fromlid, char *tolid)
{
   Discharge	*info = NULL, *infoPtr = NULL;

   Discharge	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Discharge *) GetDischarge(where);
   if (info != NULL)
   {
      infoPtr = (Discharge *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutDischarge(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Discharge record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Discharge *) ListNext(&infoPtr->node);
      }
      
      FreeDischarge(info);
   }
   
   return;
}




void	Copy_Evaporation(char *fromlid, char *tolid)
{
   Evaporation	*info = NULL, *infoPtr = NULL;

   Evaporation	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Evaporation *) GetEvaporation(where);
   if (info != NULL)
   {
      infoPtr = (Evaporation *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutEvaporation(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Evaporation record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Evaporation *) ListNext(&infoPtr->node);
      }
      
      FreeEvaporation(info);
   }
   
   return;
}




void	Copy_FcstDischarge(char *fromlid, char *tolid)
{
   FcstDischarge	*info = NULL, *infoPtr = NULL;

   FcstDischarge	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstDischarge *) GetFcstDischarge(where);
   if (info != NULL)
   {
      infoPtr = (FcstDischarge *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstDischarge(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstDischarge record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstDischarge *) ListNext(&infoPtr->node);
      }
      
      FreeFcstDischarge(info);
   }
   
   return;
}




void	Copy_FcstHeight(char *fromlid, char *tolid)
{
   FcstHeight	*info = NULL, *infoPtr = NULL;

   FcstHeight	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstHeight *) GetFcstHeight(where);
   if (info != NULL)
   {
      infoPtr = (FcstHeight *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstHeight(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstHeight record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstHeight *) ListNext(&infoPtr->node);
      }
      
      FreeFcstHeight(info);
   }
   
   return;
}




void	Copy_FcstPrecip(char *fromlid, char *tolid)
{
   FcstPrecip	*info = NULL, *infoPtr = NULL;

   FcstPrecip	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstPrecip *) GetFcstPrecip(where);
   if (info != NULL)
   {
      infoPtr = (FcstPrecip *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstPrecip(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstPrecip record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstPrecip *) ListNext(&infoPtr->node);
      }
      
      FreeFcstPrecip(info);
   }
   
   return;
}




void	Copy_FcstTemperature(char *fromlid, char *tolid)
{
   FcstTemperature	*info = NULL, *infoPtr = NULL;

   FcstTemperature	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstTemperature *) GetFcstTemperature(where);
   if (info != NULL)
   {
      infoPtr = (FcstTemperature *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstTemperature(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstTemperature record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstTemperature *) ListNext(&infoPtr->node);
      }
      
      FreeFcstTemperature(info);
   }
   
   return;
}




void	Copy_FcstOther(char *fromlid, char *tolid)
{
   FcstOther	*info = NULL, *infoPtr = NULL;

   FcstOther	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FcstOther *) GetFcstOther(where);
   if (info != NULL)
   {
      infoPtr = (FcstOther *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFcstOther(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FcstOther record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FcstOther *) ListNext(&infoPtr->node);
      }
      
      FreeFcstOther(info);
   }
   
   return;
}




void	Copy_FishCount(char *fromlid, char *tolid)
{
   FishCount	*info = NULL, *infoPtr = NULL;

   FishCount	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FishCount *) GetFishCount(where);
   if (info != NULL)
   {
      infoPtr = (FishCount *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFishCount(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FishCount record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FishCount *) ListNext(&infoPtr->node);
      }
      
      FreeFishCount(info);
   }
   
   return;
}




void	Copy_Flood(char *fromlid, char *tolid)
{
   Flood	*info = NULL, *infoPtr = NULL;

   Flood	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Flood *) GetFlood(where);
   if (info != NULL)
   {
      infoPtr = (Flood *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFlood(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Flood record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Flood *) ListNext(&infoPtr->node);
      }
      
      FreeFlood(info);
   }
   
   return;
}




void	Copy_Floodcat(char *fromlid, char *tolid)
{
   Floodcat	*info = NULL, *infoPtr = NULL;

   Floodcat	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Floodcat *) GetFloodcat(where);
   if (info != NULL)
   {
      infoPtr = (Floodcat *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFloodcat(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Floodcat record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Floodcat *) ListNext(&infoPtr->node);
      }
      
      FreeFloodcat(info);
   }
   
   return;
}




void	Copy_Floodstmt(char *fromlid, char *tolid)
{
   Floodstmt	*info = NULL, *infoPtr = NULL;

   Floodstmt	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Floodstmt *) GetFloodstmt(where);
   if (info != NULL)
   {
      infoPtr = (Floodstmt *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFloodstmt(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Floodstmt record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Floodstmt *) ListNext(&infoPtr->node);
      }
      
      FreeFloodstmt(info);
   }
   
   return;
}




void	Copy_FloodTs(char *fromlid, char *tolid)
{
   FloodTs	*info = NULL, *infoPtr = NULL;

   FloodTs	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FloodTs *) GetFloodTs(where);
   if (info != NULL)
   {
      infoPtr = (FloodTs *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFloodTs(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FloodTs record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FloodTs *) ListNext(&infoPtr->node);
      }
      
      FreeFloodTs(info);
   }
   
   return;
}




void	Copy_FpPrevProd(char *fromlid, char *tolid)
{
   FpPrevProd	*info = NULL, *infoPtr = NULL;

   FpPrevProd	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (FpPrevProd *) GetFpPrevProd(where);
   if (info != NULL)
   {
      infoPtr = (FpPrevProd *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutFpPrevProd(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert FpPrevProd record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (FpPrevProd *) ListNext(&infoPtr->node);
      }
      
      FreeFpPrevProd(info);
   }
   
   return;
}




void	Copy_Gage(char *fromlid, char *tolid)
{
   Gage	*info = NULL, *infoPtr = NULL;

   Gage	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Gage *) GetGage(where);
   if (info != NULL)
   {
      infoPtr = (Gage *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutGage(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Gage record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Gage *) ListNext(&infoPtr->node);
      }
      
      FreeGage(info);
   }
   
   return;
}




void	Copy_GateDam(char *fromlid, char *tolid)
{
   GateDam	*info = NULL, *infoPtr = NULL;

   GateDam	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (GateDam *) GetGateDam(where);
   if (info != NULL)
   {
      infoPtr = (GateDam *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutGateDam(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert GateDam record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (GateDam *) ListNext(&infoPtr->node);
      }
      
      FreeGateDam(info);
   }
   
   return;
}




void	Copy_Ground(char *fromlid, char *tolid)
{
   Ground	*info = NULL, *infoPtr = NULL;

   Ground	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Ground *) GetGround(where);
   if (info != NULL)
   {
      infoPtr = (Ground *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutGround(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Ground record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Ground *) ListNext(&infoPtr->node);
      }
      
      FreeGround(info);
   }
   
   return;
}




void	Copy_Height(char *fromlid, char *tolid)
{
   Height	*info = NULL, *infoPtr = NULL;

   Height	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Height *) GetHeight(where);
   if (info != NULL)
   {
      infoPtr = (Height *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutHeight(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Height record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Height *) ListNext(&infoPtr->node);
      }
      
      FreeHeight(info);
   }
   
   return;
}




void	Copy_HgStation(char *fromlid, char *tolid)
{
   HgStation	*info = NULL, *infoPtr = NULL;

   HgStation	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (HgStation *) GetHgStation(where);
   if (info != NULL)
   {
      infoPtr = (HgStation *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutHgStation(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert HgStation record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (HgStation *) ListNext(&infoPtr->node);
      }
      
      FreeHgStation(info);
   }
   
   return;
}




void	Copy_HourlyPC(char *fromlid, char *tolid)
{
   HourlyPC	*info = NULL, *infoPtr = NULL;

   HourlyPC	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (HourlyPC *) GetHourlyPC(where);
   if (info != NULL)
   {
      infoPtr = (HourlyPC *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutHourlyPC(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert HourlyPC record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (HourlyPC *) ListNext(&infoPtr->node);
      }
      
      FreeHourlyPC(info);
   }
   
   return;
}




void	Copy_HourlyPP(char *fromlid, char *tolid)
{
   HourlyPP	*info = NULL, *infoPtr = NULL;

   HourlyPP	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (HourlyPP *) GetHourlyPP(where);
   if (info != NULL)
   {
      infoPtr = (HourlyPP *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutHourlyPP(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert HourlyPP record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (HourlyPP *) ListNext(&infoPtr->node);
      }
      
      FreeHourlyPP(info);
   }
   
   return;
}




void	Copy_Ice(char *fromlid, char *tolid)
{
   Ice	*info = NULL, *infoPtr = NULL;

   Ice	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Ice *) GetIce(where);
   if (info != NULL)
   {
      infoPtr = (Ice *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutIce(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Ice record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Ice *) ListNext(&infoPtr->node);
      }
      
      FreeIce(info);
   }
   
   return;
}




void	Copy_IngestFilter(char *fromlid, char *tolid)
{
   IngestFilter	*info = NULL, *infoPtr = NULL;

   IngestFilter	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (IngestFilter *) GetIngestFilter(where);
   if (info != NULL)
   {
      infoPtr = (IngestFilter *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutIngestFilter(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert IngestFilter record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (IngestFilter *) ListNext(&infoPtr->node);
      }
      
      FreeIngestFilter(info);
   }
   
   return;
}




void	Copy_Lake(char *fromlid, char *tolid)
{
   Lake	*info = NULL, *infoPtr = NULL;

   Lake	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Lake *) GetLake(where);
   if (info != NULL)
   {
      infoPtr = (Lake *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLake(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Lake record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Lake *) ListNext(&infoPtr->node);
      }
      
      FreeLake(info);
   }
   
   return;
}




void	Copy_LatestObsValue(char *fromlid, char *tolid)
{
   LatestObsValue	*info = NULL, *infoPtr = NULL;

   LatestObsValue	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (LatestObsValue *) GetLatestObsValue(where);
   if (info != NULL)
   {
      infoPtr = (LatestObsValue *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLatestObsValue(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert LatestObsValue record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (LatestObsValue *) ListNext(&infoPtr->node);
      }
      
      FreeLatestObsValue(info);
   }
   
   return;
}




void	Copy_LocArea(char *fromlid, char *tolid)
{
   LocArea	*info = NULL, *infoPtr = NULL;

   LocArea	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (LocArea *) GetLocArea(where);
   if (info != NULL)
   {
      infoPtr = (LocArea *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLocArea(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert LocArea record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (LocArea *) ListNext(&infoPtr->node);
      }
      
      FreeLocArea(info);
   }
   
   return;
}




void	Copy_LocExtAgency(char *fromlid, char *tolid)
{
   LocExtAgency	*info = NULL, *infoPtr = NULL;

   LocExtAgency	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (LocExtAgency *) GetLocExtAgency(where);
   if (info != NULL)
   {
      infoPtr = (LocExtAgency *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLocExtAgency(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert LocExtAgency record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (LocExtAgency *) ListNext(&infoPtr->node);
      }
      
      FreeLocExtAgency(info);
   }
   
   return;
}




void	Copy_LocImage(char *fromlid, char *tolid)
{
   LocImage	*info = NULL, *infoPtr = NULL;

   LocImage	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (LocImage *) GetLocImage(where);
   if (info != NULL)
   {
      infoPtr = (LocImage *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLocImage(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert LocImage record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (LocImage *) ListNext(&infoPtr->node);
      }
      
      FreeLocImage(info);
   }
   
   return;
}




void	Copy_LocDataLimits(char *fromlid, char *tolid)
{
   LocDataLimits	*info = NULL, *infoPtr = NULL;

   LocDataLimits	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (LocDataLimits *) GetLocDataLimits(where);
   if (info != NULL)
   {
      infoPtr = (LocDataLimits *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLocDataLimits(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert LocDataLimits record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (LocDataLimits *) ListNext(&infoPtr->node);
      }
      
      FreeLocDataLimits(info);
   }
   
   return;
}




void	Copy_Lowwater(char *fromlid, char *tolid)
{
   Lowwater	*info = NULL, *infoPtr = NULL;

   Lowwater	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Lowwater *) GetLowwater(where);
   if (info != NULL)
   {
      infoPtr = (Lowwater *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutLowwater(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Lowwater record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Lowwater *) ListNext(&infoPtr->node);
      }
      
      FreeLowwater(info);
   }
   
   return;
}




void	Copy_Moisture(char *fromlid, char *tolid)
{
   Moisture	*info = NULL, *infoPtr = NULL;

   Moisture	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Moisture *) GetMoisture(where);
   if (info != NULL)
   {
      infoPtr = (Moisture *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutMoisture(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Moisture record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Moisture *) ListNext(&infoPtr->node);
      }
      
      FreeMoisture(info);
   }
   
   return;
}




void	Copy_MonthlyValues(char *fromlid, char *tolid)
{
   MonthlyValues	*info = NULL, *infoPtr = NULL;

   MonthlyValues	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (MonthlyValues *) GetMonthlyValues(where);
   if (info != NULL)
   {
      infoPtr = (MonthlyValues *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutMonthlyValues(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert MonthlyValues record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (MonthlyValues *) ListNext(&infoPtr->node);
      }
      
      FreeMonthlyValues(info);
   }
   
   return;
}




void	Copy_OFSStnTrans(char *fromlid, char *tolid)
{
   OFSStnTrans	*info = NULL, *infoPtr = NULL;

   OFSStnTrans	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (OFSStnTrans *) GetOFSStnTrans(where);
   if (info != NULL)
   {
      infoPtr = (OFSStnTrans *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutOFSStnTrans(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert OFSStnTrans record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (OFSStnTrans *) ListNext(&infoPtr->node);
      }
      
      FreeOFSStnTrans(info);
   }
   
   return;
}




void	Copy_Observer(char *fromlid, char *tolid)
{
   Observer	*info = NULL, *infoPtr = NULL;

   Observer	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Observer *) GetObserver(where);
   if (info != NULL)
   {
      infoPtr = (Observer *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutObserver(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Observer record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Observer *) ListNext(&infoPtr->node);
      }
      
      FreeObserver(info);
   }
   
   return;
}




void	Copy_PairedValue(char *fromlid, char *tolid)
{
   PairedValue	*info = NULL, *infoPtr = NULL;

   PairedValue	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (PairedValue *) GetPairedValue(where);
   if (info != NULL)
   {
      infoPtr = (PairedValue *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutPairedValue(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert PairedValue record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (PairedValue *) ListNext(&infoPtr->node);
      }
      
      FreePairedValue(info);
   }
   
   return;
}




void	Copy_Power(char *fromlid, char *tolid)
{
   Power	*info = NULL, *infoPtr = NULL;

   Power	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Power *) GetPower(where);
   if (info != NULL)
   {
      infoPtr = (Power *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutPower(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Power record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Power *) ListNext(&infoPtr->node);
      }
      
      FreePower(info);
   }
   
   return;
}




void	Copy_Pressure(char *fromlid, char *tolid)
{
   Pressure	*info = NULL, *infoPtr = NULL;

   Pressure	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Pressure *) GetPressure(where);
   if (info != NULL)
   {
      infoPtr = (Pressure *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutPressure(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Pressure record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Pressure *) ListNext(&infoPtr->node);
      }
      
      FreePressure(info);
   }
   
   return;
}




void	Copy_ProcValue(char *fromlid, char *tolid)
{
   ProcValue	*info = NULL, *infoPtr = NULL;

   ProcValue	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (ProcValue *) GetProcValue(where);
   if (info != NULL)
   {
      infoPtr = (ProcValue *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutProcValue(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert ProcValue record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (ProcValue *) ListNext(&infoPtr->node);
      }
      
      FreeProcValue(info);
   }
   
   return;
}




void	Copy_ProductLink(char *fromlid, char *tolid)
{
   ProductLink	*info = NULL, *infoPtr = NULL;

   ProductLink	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (ProductLink *) GetProductLink(where);
   if (info != NULL)
   {
      infoPtr = (ProductLink *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutProductLink(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert ProductLink record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (ProductLink *) ListNext(&infoPtr->node);
      }
      
      FreeProductLink(info);
   }
   
   return;
}




void	Copy_Pub(char *fromlid, char *tolid)
{
   Pub	*info = NULL, *infoPtr = NULL;

   Pub	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Pub *) GetPub(where);
   if (info != NULL)
   {
      infoPtr = (Pub *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutPub(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Pub record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Pub *) ListNext(&infoPtr->node);
      }
      
      FreePub(info);
   }
   
   return;
}




void	Copy_Radiation(char *fromlid, char *tolid)
{
   Radiation	*info = NULL, *infoPtr = NULL;

   Radiation	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Radiation *) GetRadiation(where);
   if (info != NULL)
   {
      infoPtr = (Radiation *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRadiation(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Radiation record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Radiation *) ListNext(&infoPtr->node);
      }
      
      FreeRadiation(info);
   }
   
   return;
}




void	Copy_Rating(char *fromlid, char *tolid)
{
   Rating	*info = NULL, *infoPtr = NULL;

   Rating	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Rating *) GetRating(where);
   if (info != NULL)
   {
      infoPtr = (Rating *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRating(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Rating record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Rating *) ListNext(&infoPtr->node);
      }
      
      FreeRating(info);
   }
   
   return;
}




void	Copy_RatingShift(char *fromlid, char *tolid)
{
   RatingShift	*info = NULL, *infoPtr = NULL;

   RatingShift	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RatingShift *) GetRatingShift(where);
   if (info != NULL)
   {
      infoPtr = (RatingShift *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRatingShift(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RatingShift record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RatingShift *) ListNext(&infoPtr->node);
      }
      
      FreeRatingShift(info);
   }
   
   return;
}




void	Copy_RawPC(char *fromlid, char *tolid)
{
   RawPC	*info = NULL, *infoPtr = NULL;

   RawPC	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RawPC *) GetRawPC(where);
   if (info != NULL)
   {
      infoPtr = (RawPC *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRawPC(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RawPC record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RawPC *) ListNext(&infoPtr->node);
      }
      
      FreeRawPC(info);
   }
   
   return;
}




void	Copy_RawPP(char *fromlid, char *tolid)
{
   RawPP	*info = NULL, *infoPtr = NULL;

   RawPP	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RawPP *) GetRawPP(where);
   if (info != NULL)
   {
      infoPtr = (RawPP *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRawPP(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RawPP record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RawPP *) ListNext(&infoPtr->node);
      }
      
      FreeRawPP(info);
   }
   
   return;
}




void	Copy_RawPother(char *fromlid, char *tolid)
{
   RawPother	*info = NULL, *infoPtr = NULL;

   RawPother	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RawPother *) GetRawPother(where);
   if (info != NULL)
   {
      infoPtr = (RawPother *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRawPother(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RawPother record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RawPother *) ListNext(&infoPtr->node);
      }
      
      FreeRawPother(info);
   }
   
   return;
}




void	Copy_Refer(char *fromlid, char *tolid)
{
   Refer	*info = NULL, *infoPtr = NULL;

   Refer	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Refer *) GetRefer(where);
   if (info != NULL)
   {
      infoPtr = (Refer *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRefer(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Refer record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Refer *) ListNext(&infoPtr->node);
      }
      
      FreeRefer(info);
   }
   
   return;
}




void	Copy_RejectedData(char *fromlid, char *tolid)
{
   RejectedData	*info = NULL, *infoPtr = NULL;

   RejectedData	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RejectedData *) GetRejectedData(where);
   if (info != NULL)
   {
      infoPtr = (RejectedData *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRejectedData(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RejectedData record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RejectedData *) ListNext(&infoPtr->node);
      }
      
      FreeRejectedData(info);
   }
   
   return;
}




void	Copy_Reservoir(char *fromlid, char *tolid)
{
   Reservoir	*info = NULL, *infoPtr = NULL;

   Reservoir	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Reservoir *) GetReservoir(where);
   if (info != NULL)
   {
      infoPtr = (Reservoir *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutReservoir(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Reservoir record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Reservoir *) ListNext(&infoPtr->node);
      }
      
      FreeReservoir(info);
   }
   
   return;
}




void	Copy_Rescap(char *fromlid, char *tolid)
{
   Rescap	*info = NULL, *infoPtr = NULL;

   Rescap	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Rescap *) GetRescap(where);
   if (info != NULL)
   {
      infoPtr = (Rescap *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRescap(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Rescap record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Rescap *) ListNext(&infoPtr->node);
      }
      
      FreeRescap(info);
   }
   
   return;
}




void	Copy_RpfFcstPoint(char *fromlid, char *tolid)
{
   RpfFcstPoint	*info = NULL, *infoPtr = NULL;

   RpfFcstPoint	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (RpfFcstPoint *) GetRpfFcstPoint(where);
   if (info != NULL)
   {
      infoPtr = (RpfFcstPoint *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutRpfFcstPoint(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert RpfFcstPoint record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (RpfFcstPoint *) ListNext(&infoPtr->node);
      }
      
      FreeRpfFcstPoint(info);
   }
   
   return;
}




void	Copy_Snow(char *fromlid, char *tolid)
{
   Snow	*info = NULL, *infoPtr = NULL;

   Snow	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Snow *) GetSnow(where);
   if (info != NULL)
   {
      infoPtr = (Snow *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutSnow(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Snow record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Snow *) ListNext(&infoPtr->node);
      }
      
      FreeSnow(info);
   }
   
   return;
}




void	Copy_SshpConfig(char *fromlid, char *tolid)
{
   SshpConfig	*info = NULL, *infoPtr = NULL;

   SshpConfig	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (SshpConfig *) GetSshpConfig(where);
   if (info != NULL)
   {
      infoPtr = (SshpConfig *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutSshpConfig(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert SshpConfig record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (SshpConfig *) ListNext(&infoPtr->node);
      }
      
      FreeSshpConfig(info);
   }
   
   return;
}




void	Copy_StnClass(char *fromlid, char *tolid)
{
   StnClass	*info = NULL, *infoPtr = NULL;

   StnClass	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (StnClass *) GetStnClass(where);
   if (info != NULL)
   {
      infoPtr = (StnClass *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutStnClass(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert StnClass record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (StnClass *) ListNext(&infoPtr->node);
      }
      
      FreeStnClass(info);
   }
   
   return;
}




void	Copy_Telem(char *fromlid, char *tolid)
{
   Telem	*info = NULL, *infoPtr = NULL;

   Telem	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Telem *) GetTelem(where);
   if (info != NULL)
   {
      infoPtr = (Telem *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutTelem(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Telem record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Telem *) ListNext(&infoPtr->node);
      }
      
      FreeTelem(info);
   }
   
   return;
}




void	Copy_Temperature(char *fromlid, char *tolid)
{
   Temperature	*info = NULL, *infoPtr = NULL;

   Temperature	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Temperature *) GetTemperature(where);
   if (info != NULL)
   {
      infoPtr = (Temperature *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutTemperature(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Temperature record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Temperature *) ListNext(&infoPtr->node);
      }
      
      FreeTemperature(info);
   }
   
   return;
}




void	Copy_UnitGraph(char *fromlid, char *tolid)
{
   UnitGraph	*info = NULL, *infoPtr = NULL;

   UnitGraph	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (UnitGraph *) GetUnitGraph(where);
   if (info != NULL)
   {
      infoPtr = (UnitGraph *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutUnitGraph(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert UnitGraph record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (UnitGraph *) ListNext(&infoPtr->node);
      }
      
      FreeUnitGraph(info);
   }
   
   return;
}




void	Copy_UnkStn(char *fromlid, char *tolid)
{
   UnkStn	*info = NULL, *infoPtr = NULL;

   UnkStn	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (UnkStn *) GetUnkStn(where);
   if (info != NULL)
   {
      infoPtr = (UnkStn *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutUnkStn(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert UnkStn record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (UnkStn *) ListNext(&infoPtr->node);
      }
      
      FreeUnkStn(info);
   }
   
   return;
}




void	Copy_UnkStnValue(char *fromlid, char *tolid)
{
   UnkStnValue	*info = NULL, *infoPtr = NULL;

   UnkStnValue	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (UnkStnValue *) GetUnkStnValue(where);
   if (info != NULL)
   {
      infoPtr = (UnkStnValue *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutUnkStnValue(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert UnkStnValue record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (UnkStnValue *) ListNext(&infoPtr->node);
      }
      
      FreeUnkStnValue(info);
   }
   
   return;
}




void	Copy_WaterQuality(char *fromlid, char *tolid)
{
   WaterQuality	*info = NULL, *infoPtr = NULL;

   WaterQuality	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (WaterQuality *) GetWaterQuality(where);
   if (info != NULL)
   {
      infoPtr = (WaterQuality *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutWaterQuality(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert WaterQuality record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (WaterQuality *) ListNext(&infoPtr->node);
      }
      
      FreeWaterQuality(info);
   }
   
   return;
}




void	Copy_Weather(char *fromlid, char *tolid)
{
   Weather	*info = NULL, *infoPtr = NULL;

   Weather	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Weather *) GetWeather(where);
   if (info != NULL)
   {
      infoPtr = (Weather *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutWeather(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Weather record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Weather *) ListNext(&infoPtr->node);
      }
      
      FreeWeather(info);
   }
   
   return;
}




void	Copy_Wind(char *fromlid, char *tolid)
{
   Wind	*info = NULL, *infoPtr = NULL;

   Wind	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Wind *) GetWind(where);
   if (info != NULL)
   {
      infoPtr = (Wind *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutWind(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Wind record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Wind *) ListNext(&infoPtr->node);
      }
      
      FreeWind(info);
   }
   
   return;
}




void	Copy_YUnique(char *fromlid, char *tolid)
{
   YUnique	*info = NULL, *infoPtr = NULL;

   YUnique	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (YUnique *) GetYUnique(where);
   if (info != NULL)
   {
      infoPtr = (YUnique *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutYUnique(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert YUnique record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (YUnique *) ListNext(&infoPtr->node);
      }
      
      FreeYUnique(info);
   }
   
   return;
}




void	Copy_Zonenum(char *fromlid, char *tolid)
{
   Zonenum	*info = NULL, *infoPtr = NULL;

   Zonenum	newInfo;
   
   char		where[MAX_WHERE_LEN];

   int		error;
   
   
   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", fromlid);

   
   info = (Zonenum *) GetZonenum(where);
   if (info != NULL)
   {
      infoPtr = (Zonenum *) ListFirst(&info->list);
      while(infoPtr != NULL)
      {
         memcpy(&newInfo, infoPtr, sizeof(newInfo));
	 strcpy(newInfo.lid, tolid);

	 error = 0;
	 if ((error = PutZonenum(&newInfo)) != 0)
	 {
	    fprintf(stderr, "ERROR: Unable to insert Zonenum record: %d\n",
	            error);
	    return;
	 }
         infoPtr = (Zonenum *) ListNext(&infoPtr->node);
      }
      
      FreeZonenum(info);
   }
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}




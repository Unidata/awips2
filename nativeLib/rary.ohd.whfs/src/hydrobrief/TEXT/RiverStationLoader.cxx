#ifndef RIVER_STATION_HXX
#include "RiverStationLoader.H"
#endif

//*************************************************************************
// default constructor

RiverStationLoader::RiverStationLoader()
{
   return;   
}

//*************************************************************************
// constructor

RiverStationLoader::RiverStationLoader(const HvStation *station,
				       const long initRiverMile)
{
   struct Report	obsReport, fcstReport;
   int			obs_found, fcst_found;
   int			max_hours_old = 48;          /* 2 days  */
   int			fcst_hours_ahead = 240;      /* 10 days */
   int			fcst_basis_hours_ago = 72;   /* 3 days  */
   char			pe[SHEF_PE_LEN + 1];
   
   strcpy(lid, station->lid);
   strcpy(name, station->name);
   
   
   setRiverMile(initRiverMile);
   
   setFloodStage(missingIfNull(station->flood_stage));
   setActionStage(missingIfNull(station->action_stage));
   
   // load the cur observed and max forecast info.
   // use the (char *) typecasts to avoid compiler errors.
   // use the primary pe for this station, if it is missing or is not 
   // an H parameter, set it to HG, and try the retreival.  Q data
   // is not supported in the other aspects of this function. (for now)
      
   if (station->primary_pe[0] != 'H')
      strcpy(pe, "HG");
   else
      strcpy(pe, station->primary_pe);
   
   //printf("loading comf for %s:%s\n", station->lid, pe);
	  
   get_curobs_maxfcst((char *)station->lid, pe,
                      max_hours_old, fcst_hours_ahead, fcst_basis_hours_ago,
		      &obs_found, &fcst_found,
		      &obsReport, &fcstReport);
   
   if (obs_found)
   {
      curObsTime = obsReport.validtime;      
      setCurObsStage(obsReport.value);
   }
   else
   {
      curObsTime = 0;      
      setCurObsStage(MISSING);
   }
   
   
   if (fcst_found)
   {
      maxFcstTime = fcstReport.validtime;
      setMaxFcstStage(fcstReport.value); 
   }
   else
   {
      maxFcstTime = 0;      
      setMaxFcstStage(MISSING);
   }
   
   return;
} 


//*************************************************************************
// destructor

RiverStationLoader::~RiverStationLoader()
{
   
   return;
} 


//*************************************************************************

void RiverStationLoader::print()
{
   printf("inside RiverStationLoader::print()\n");
   
   printf("Station is (%s) at %s\n", lid, name);
   
   printf("Flood Stage: %f\n", getFloodStage());
   printf("Action Stage: %f\n", getActionStage());
   printf("Current Observed Stage: %f\n", getCurObsStage());
   printf("Max Forecast Stage: %f\n", getMaxFcstStage());
   
   if (isInFlood())
   {
      printf("%s is in flood\n", lid);	
      
   }
   else
   {
      printf("%s is below flood stage\n", lid);	
   }
   
   printf("*********************************\n");
   
   return;
}   


//*************************************************************************

int RiverStationLoader::isInFlood()
{
   int inFlood = 0;   
   
   if (getCurObsStage() > getFloodStage())
   {
      inFlood = 1;	
   }
   
   return inFlood;
}


//*************************************************************************

double RiverStationLoader::missingIfNull(double value)
{
   double rv = value;
   
   if ( IsNull(FLOAT, &value) == ISNULL)  
   {
      rv = MISSING;
   }
   
   return rv; 
}

//*************************************************************************

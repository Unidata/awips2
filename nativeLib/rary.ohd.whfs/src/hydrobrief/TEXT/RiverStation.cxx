#include "RiverStation.H"


//*************************************************************************
RiverStation::RiverStation()
{
     return;   
}

RiverStation::RiverStation(RiverStationLoader *loader)
{
      
     strcpy(lid, loader->getLid());
     strcpy(name, loader->getName());
     
     setRiverMile(loader->getRiverMile());
     
     setFloodStage(loader->getFloodStage());
     setActionStage(loader->getActionStage());
     
     curObsTime = loader->getCurObsTime();
     maxFcstTime = loader->getMaxFcstTime();
     
     setCurObsStage(loader->getCurObsStage());
     setMaxFcstStage(loader->getMaxFcstStage()); 
   
     return;
} 

//*************************************************************************

RiverStation::~RiverStation()
{
     
     return;
} 

//*************************************************************************

void RiverStation::print()
{
     printf("inside RiverStation::print()\n");
     
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

int RiverStation::isInFlood()
{
     int inFlood = 0;   
       
     if (getCurObsStage() > getFloodStage())
     {
         inFlood = 1;	
     }
     
     return inFlood;
}

//*************************************************************************

double RiverStation::missingIfNull(double value)
{
     double rv = value;
      
     if ( IsNull(FLOAT, &value) == ISNULL)  
     {
          rv = MISSING;
     }
     
     return rv; 
}

//*************************************************************************

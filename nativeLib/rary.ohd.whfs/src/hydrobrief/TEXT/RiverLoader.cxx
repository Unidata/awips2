#include "RiverLoader.H"



//*************************************************************************

RiverLoader::RiverLoader(Riverstat *rPtr, HvStation *hHead)
{
     
     char prevRiverName[BUFSIZ];
     HvStation *hPtr = NULL;
     RiverStationLoader *loader = NULL;
     int i = 0;
     
     setName(rPtr->stream);
     
     strcpy(prevRiverName, rPtr->stream);
     numRiverStations = getRiverStationCount(rPtr);
          
     riverStationArray = new RiverStation[getNumRiverStations()];
     
     
     i = 0;
     
     if (hHead)
     {
	  while(rPtr)
	  {
	       
	       /*
	            if it is a different River, then break
	       */
	       if (strcmp(prevRiverName, rPtr->stream) != 0)
	       {
	 	     break;    
	       }
	       
	       
	       /*
	            find the correct HvStation
	       */
	       hPtr = findHvStation(rPtr->lid,
				    hHead);
	       
	       
	       /*
	            if successful, construct a RiverStation from
		    the hPtr
	       */
	       if (hPtr)
	       {
		   loader = new RiverStationLoader(hPtr,
                                       static_cast < long > ( rPtr->mile ) );
                   riverStationArray[i] = RiverStation(loader);
		   delete loader;
		   loader = NULL;
		   
	   	   i++;

	       }
	       
	       rPtr = (Riverstat*) ListNext(&rPtr->node);  
	  }
	  
	 numRiverStations = i;
	  	  
     }
        

     return;   
}


//*************************************************************************

RiverLoader::~RiverLoader()
{

     return;   
}

//*************************************************************************

const char * RiverLoader::getName() const
{
     return name;
}

//*************************************************************************

RiverStation * RiverLoader::getRiverStationArray()
{
     return riverStationArray;   
}

//*************************************************************************

long  RiverLoader::getNumRiverStations()
{
     return numRiverStations; 
}

//*************************************************************************

long RiverLoader::getRiverStationCount(Riverstat *rPtr)
{
     long count = 0;
     char prevRiverName[BUFSIZ];
     
     
     // returns the count of riverstations along a particular river
     
     strcpy(prevRiverName, rPtr->stream);
     
     while (rPtr)
     {
	  if (strcmp(prevRiverName, rPtr->stream) == 0)
	  {
	      count++;	  
	  }
	  else
	  {
	      break;     
	  }
	  rPtr = (Riverstat *) ListNext(&rPtr->node);
     }
     
     return count;

}

/******************************************************************/

HvStation * RiverLoader::findHvStation(const char *lid, HvStation *hHead)
{
     
     HvStation *hPtr = NULL;
     HvStation *goalPtr = NULL;
     int result = 0;
     
     hPtr = (HvStation *) ListFirst(&hHead->list);
     
     
     while (hPtr)
     {
	  result = strcmp(lid, hPtr->lid);
	  
          if(result == 0)  
	  { 
	     //printf("found %s in HvStation list\n",lid);
	       goalPtr = hPtr;      	  
	  }
	  
	  else if (result < 0)
	  {
	       break;     
	  }
	  hPtr = (HvStation *) ListNext(&hPtr->node);
	  
     }
 
     return goalPtr;
}

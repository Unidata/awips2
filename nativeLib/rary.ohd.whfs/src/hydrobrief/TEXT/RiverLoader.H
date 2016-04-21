
#ifndef RIVER_LOADER_HXX
#define RIVER_LOADER_HXX

#include "RiverStation.H"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C"
{

#include "Riverstat.h"
#include "HvStation.h"      
}

class RiverLoader
{
   
public:
   RiverLoader(Riverstat *rPtr, HvStation *hvPtr);
   ~RiverLoader();
    
   const char * getName() const;
   RiverStation * getRiverStationArray();
   long getNumRiverStations();
   
   
private:   

   void setName(const char *initName);
   
   char name[BUFSIZ];
   
   RiverStation *riverStationArray;
   long numRiverStations;
   
 
   static long getRiverStationCount(Riverstat *rPtr);
   static HvStation * findHvStation(const char *lid, HvStation *hHead);
   
};

//************************************************************************

inline void RiverLoader::setName(const char *initName) 
{   
     strncpy(name, initName, sizeof(name));   
}

//************************************************************************

#endif

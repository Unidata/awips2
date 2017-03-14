#ifndef RIVER_HXX
#define RIVER_HXX

#include "RiverStation.H"
#include "RiverLoader.H"

extern "C"
{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Riverstat.h"
#include "HvStation.h"      
}


class River
{

 
public:
   River();
   River(RiverLoader *loader);
   virtual ~River();
   
   
   // River(Riverstat *rPtr, HvStation *hHead);
  
   void print();
   
   const char * getName() const;
   void setName(const char * initName);
   long getNumRiverStations();
   RiverStation*  getRiverStation(long i) const;
     
   
private:
   RiverStation *riverStationArray;
   
   long numRiverStations;
   char name[BUFSIZ];
   
   long getRiverStationCount(Riverstat *rPtr);
   HvStation * findHvStation(const char *lid, HvStation *hHead);
   
};

//************************************************************************

inline RiverStation * River::getRiverStation(long i) const
{
     return &(riverStationArray[i]);
}

//************************************************************************

inline const char * River::getName()  const
{
     return name;
}

//************************************************************************

inline void River::setName(const char *initName) 
{   
     strncpy(name, initName, sizeof(name));   
}

//************************************************************************

inline long River::getNumRiverStations() 
{   
     return numRiverStations;   
}

//************************************************************************



#endif

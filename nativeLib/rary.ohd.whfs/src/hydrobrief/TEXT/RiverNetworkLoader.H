#ifndef RIVER_NETWORK_LOADER_HXX
#define RIVER_NETWORK_LOADER_HXX


#include "River.H" 
#include "RiverLoader.H"

extern "C"
{
#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "Riverstat.h"
#include "HvStation.h"
#include "LoadUnique.h"
#include "DbmsAccess.h" 
#include "DbmsUtils.h"
}


class RiverNetworkLoader
{
   
public:
   
   RiverNetworkLoader();
   virtual ~RiverNetworkLoader();
   
   long getNumRivers();
   River *getRiverArray();
   
   
private:
   
   River *riverArray;
   long numRivers;
   
   int getRiverCount(Riverstat *rHead);
 
};


//************************************************************************

inline long RiverNetworkLoader::getNumRivers()
{
     return numRivers;      
}

//************************************************************************

inline River * RiverNetworkLoader::getRiverArray()
{
     return riverArray;      
}

//************************************************************************


#endif

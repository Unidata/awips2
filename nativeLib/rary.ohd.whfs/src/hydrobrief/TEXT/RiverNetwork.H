#ifndef RIVER_NETWORK_HXX
#define RIVER_NETWORK_HXX


#include "River.H" 
#include "RiverNetworkLoader.H"

#include "stdio.h"
#include "string.h"
#include "stdlib.h"

extern "C"
{

}


class RiverNetwork
{
   
public:
   
   RiverNetwork(RiverNetworkLoader *loader);
   virtual ~RiverNetwork();
   
   void print();
   
   River * getRiver(int riverIndex);
   long getNumRivers();
   
   long getMaxNumStations();
   
private:
   
   River *riverArray;
   long numRivers;
    
};


//************************************************************************

inline long RiverNetwork::getNumRivers()
{
     return numRivers;      
}

//************************************************************************


#endif

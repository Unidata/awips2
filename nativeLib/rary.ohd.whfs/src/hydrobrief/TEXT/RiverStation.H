#ifndef RIVER_STATION_HXX
#define RIVER_STATION_HXX

#include "RiverStationLoader.H"
#include "MiscDefs.H"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "time_convert.h"

extern "C"
{

}


//static const int MISSING = -9999;       

class RiverStation
{
   
public:
   RiverStation();
   RiverStation(RiverStationLoader *loader);
   virtual ~RiverStation(void);
    
   void print();
   
   const char * getLid() const;
   const char * getName() const;
   
   double missingIfNull(double value);
   
   
   
   //
   //  gets
   //
   double getFloodStage() const;
   double getActionStage() const;
   
   double getCurObsStage() const;
   double getMaxFcstStage() const;
   double getMaxObsFcstStage() const;
   
   time_t getCurObsTime() const;
   time_t getMaxFcstTime() const;
   time_t getMaxObsFcstTime() const;
   
   long getRiverMile() const;
   
   int getStageBasis() const;
   int isInFlood();
    
private:   
   
   //
   // sets
   //
   void setFloodStage(double initFloodStage);
   void setActionStage(double initActionStage);
   
   void setCurObsStage(double initCurObsStage);
   void setCurObsTime(time_t initCurObsTime);
   
   void setMaxFcstStage(double initMaxFcstStage);
   void setMaxFcstTime(time_t initMaxFcstTime);
 
   void setRiverMile(long riverMile);
     
   void setStageBasis(int initStageBasis);
   

   
   char lid[8+1];
   char name[50+1];
   
   double floodStage;
   double actionStage;
   
   double curObsStage;
   time_t curObsTime;
   
   double maxFcstStage;
   time_t maxFcstTime;
   
   long riverMile;
   
   int stageBasis;
    
};

//***********************************************************************

inline  const char * RiverStation::getLid() const
{
        return lid;
}

//***********************************************************************

inline  const char * RiverStation::getName() const
{
        return name;
}

//***********************************************************************


inline  double RiverStation::getFloodStage() const
{
        return floodStage;
}

//***********************************************************************


inline void RiverStation::setFloodStage(double initFloodStage)
{
        floodStage = initFloodStage;
	return;
}

//***********************************************************************


inline  double RiverStation::getActionStage() const
{
        return actionStage;
}

//***********************************************************************


inline void RiverStation::setActionStage(double initActionStage)
{
        actionStage = initActionStage;
	return;
}

//***********************************************************************

inline  time_t RiverStation::getCurObsTime() const
{
        return curObsTime;
}

//***********************************************************************

inline void RiverStation::setCurObsTime(time_t initCurObsTime)
{
        curObsTime = initCurObsTime;
	return;
}

//***********************************************************************


inline int RiverStation::getStageBasis() const
{
        return stageBasis;   
}


//***********************************************************************

//
//  gets
//

inline  double RiverStation::getCurObsStage() const
{
        return curObsStage;
}

//***********************************************************************

inline void RiverStation::setCurObsStage(double initCurObsStage)
{
        curObsStage = initCurObsStage;
	return;
}

//***********************************************************************


inline  double RiverStation::getMaxFcstStage() const
{
        return maxFcstStage;
}

//***********************************************************************


inline void RiverStation::setMaxFcstStage(double initMaxFcstStage)
{
        maxFcstStage = initMaxFcstStage;
	return;
}

//***********************************************************************

inline  time_t RiverStation::getMaxFcstTime() const
{
        return maxFcstTime;
}

//***********************************************************************


inline void RiverStation::setMaxFcstTime(time_t initMaxFcstTime)
{
        maxFcstTime = initMaxFcstTime;
	return;
}

//***********************************************************************


inline  long RiverStation::getRiverMile() const
{
        return riverMile;
}

//***********************************************************************


inline void RiverStation::setRiverMile(long initRiverMile)
{
        riverMile = initRiverMile;
	return;
}

//***********************************************************************


inline void RiverStation::setStageBasis(int initStageBasis)
{
        stageBasis = initStageBasis;   
        return;
}

//***********************************************************************

#endif

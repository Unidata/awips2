#ifndef RIVER_STATION_LOADER_HXX
#define RIVER_STATION_LOADER_HXX

#include "MiscDefs.H"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "time_convert.h"

extern "C"
{
#include "HvStation.h"
#include "DbmsUtils.h"
#include "Report.h"
#include "get_curobs_maxfcst.h"
}


class RiverStationLoader
{
   
public:
   RiverStationLoader();
   RiverStationLoader(const HvStation *station, const long initRiverMile);
   virtual ~RiverStationLoader(void);
    
   void load(const HvStation *station, const long initRiverMile);
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
   
   int isInFlood();

   
private:
   
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

inline  const char * RiverStationLoader::getLid() const
{
        return lid;
}

inline  const char * RiverStationLoader::getName() const
{
        return name;
}



inline  double RiverStationLoader::getFloodStage() const
{
        return floodStage;
}

inline void RiverStationLoader::setFloodStage(double initFloodStage)
{
        floodStage = initFloodStage;
	return;
}



inline  double RiverStationLoader::getActionStage() const
{
        return actionStage;
}

inline void RiverStationLoader::setActionStage(double initActionStage)
{
        actionStage = initActionStage;
	return;
}


inline  time_t RiverStationLoader::getCurObsTime() const
{
        return curObsTime;
}

inline void RiverStationLoader::setCurObsTime(time_t initCurObsTime)
{
        curObsTime = initCurObsTime;
	return;
}

inline int RiverStationLoader::getStageBasis() const
{
        return stageBasis;   
}



//
//  gets
//

inline  double RiverStationLoader::getCurObsStage() const
{
        return curObsStage;
}

inline void RiverStationLoader::setCurObsStage(double initCurObsStage)
{
        curObsStage = initCurObsStage;
	return;
}



inline  double RiverStationLoader::getMaxFcstStage() const
{
        return maxFcstStage;
}

inline void RiverStationLoader::setMaxFcstStage(double initMaxFcstStage)
{
        maxFcstStage = initMaxFcstStage;
	return;
}

inline  time_t RiverStationLoader::getMaxFcstTime() const
{
        return maxFcstTime;
}

inline void RiverStationLoader::setMaxFcstTime(time_t initMaxFcstTime)
{
        maxFcstTime = initMaxFcstTime;
	return;
}




inline  long RiverStationLoader::getRiverMile() const
{
        return riverMile;
}


inline void RiverStationLoader::setRiverMile(long initRiverMile)
{
        riverMile = initRiverMile;
	return;
}


inline void RiverStationLoader::setStageBasis(int initStageBasis)
{
        stageBasis = initStageBasis;   
        return;
}

#endif

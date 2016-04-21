#ifndef HYDRO_BRIEF_LOGIC_HXX
#define HYDRO_BRIEF_LOGIC_HXX 

#include <stdio.h>
#include <stdlib.h>

#include "MotifMaster.h"
#include "HydroBriefDLT.H"

#include "RiverNetwork.H"
#include "River.H"

enum StageBasis { OBS_ONLY,
                  FCST_ONLY,
		  MAX_OBS_FCST
	        };


class HydroBriefLogic
{
   
   
public:
   
     HydroBriefLogic(HydroBriefDLT *dlt);  
     ~HydroBriefLogic();
     
     RiverNetwork * getRiverNetwork();
     void selectRiver(int selectedPos);
     River * getSelectedRiver();
 
     StageBasis getStageBasis();
     void setStageBasis(StageBasis initStageBasis);
     
     
private:
   
     HydroBriefDLT *dlt;
        
     River * selectedRiver;
     
     RiverNetwork *riverNetwork;
     
     StageBasis stageBasis;
   
};

#endif

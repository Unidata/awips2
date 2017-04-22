#include "HydroBriefLogic.H"

//********************************************************************

HydroBriefLogic::HydroBriefLogic(HydroBriefDLT *initDLT)
{
   
   //printf("inside HydroBriefLogic constructor\n");
   
     
     //
     //  set DLT
     //
     dlt = initDLT;
     
     //
     //  load the river network and store the array of rivers
     //
     //printf("before HydroBriefLogic.loadRiverNetwork()"
     //	    "in HydroBriefLogic constructor\n");
     
     riverNetwork = dlt->loadRiverNetwork();    
    
     
     //
     //  init the stageBasis
     //
     stageBasis = MAX_OBS_FCST;
    
     
     return;   
}

//********************************************************************

HydroBriefLogic::~HydroBriefLogic()
{
   
   //printf("inside HydroBriefLogic destructor\n");
     

     return;   
}

//********************************************************************
   
void HydroBriefLogic::selectRiver(int selectedPos)
{
     selectedRiver = riverNetwork->getRiver(selectedPos);
              
     return;
}

//********************************************************************




//**************************************************************************

RiverNetwork * HydroBriefLogic::getRiverNetwork()
{
      return riverNetwork;
}

//**************************************************************************

void  HydroBriefLogic::setStageBasis(StageBasis initStageBasis)
{
      stageBasis = initStageBasis;   
   
      return;
}

//**************************************************************************

StageBasis HydroBriefLogic::getStageBasis()
{
      return stageBasis;
}

//**************************************************************************

River * HydroBriefLogic::getSelectedRiver()
{
      return selectedRiver;
}

//**************************************************************************




#include "HydroBriefULT.H"


//********************************************************************

HydroBriefULT::HydroBriefULT(HydroBriefLogic *initLogic)
{
     logic = initLogic;
     //printf("inside HydroBriefULT constructor\n");
     
  
     return;   
}

//********************************************************************

HydroBriefULT::~HydroBriefULT()
{
   
     //printf("inside HydroBriefULT destructor\n");
   
   
     return;   
}

//********************************************************************

void HydroBriefULT::updateBriefPicture(int XmListSelectedPos, Canvas *canvas)
{ 
     //printf("in  HydroBriefULT::updateBriefPicture\n");
 
     HydroBriefPainter painter(logic, canvas);
     
     
     // convert to the list position in logic
     
     int logicListSelectedPos = XmListSelectedPos - 1;
 
     // select a River in the logical layer
     
     logic->selectRiver(logicListSelectedPos);
    
     // updates canvas's drawing Area
     painter.draw();
     return; 
 
}
//********************************************************************

void HydroBriefULT::selectStageBasis(int buttonNumber, Canvas *canvas)
{ 
     //printf("in  HydroBriefULT::selectStageBasis()\n");
 
     HydroBriefPainter painter(logic, canvas);
     StageBasis stageBasis = MAX_OBS_FCST;
     
     
     //  Map the button number to a stage basis

     if (buttonNumber == 0)
          stageBasis = MAX_OBS_FCST;	
     else if (buttonNumber == 1)
	  stageBasis = OBS_ONLY;	
     else if (buttonNumber == 2)
          stageBasis = FCST_ONLY;	

     

     //  set the stage basis in the logic section

     logic->setStageBasis(stageBasis);
     
     
     // updates canvas's drawing Area
     painter.draw();
     return; 
 
}
//********************************************************************


void HydroBriefULT::loadRiverList(Widget riverList)
{
     LongText 		*text;
     int		i = 0;
     RiverNetwork 	*riverNetwork = logic->getRiverNetwork();
     long 		numRivers = riverNetwork->getNumRivers();
     
     
     text = (LongText *) malloc ( sizeof(LongText) * numRivers);
     
     if (text)
     {
	  for (i = 0; i < numRivers; i++)
	  {
	     if (riverNetwork->getRiver(i)->getNumRiverStations() > 1)
	     {
	       sprintf(text[i], "%-32s (%ld stations)",
		       riverNetwork->getRiver(i)->getName(),
		       riverNetwork->getRiver(i)->getNumRiverStations());
	     }
	     else
	     {
	       sprintf(text[i], "%-32s (%ld station)",
		       riverNetwork->getRiver(i)->getName(),
		       riverNetwork->getRiver(i)->getNumRiverStations());
	     }
	  }
	  
          loadXmList(riverList, text, numRivers);
	  
	  free(text);
     }
     
     
     return;  
}
   
//********************************************************************

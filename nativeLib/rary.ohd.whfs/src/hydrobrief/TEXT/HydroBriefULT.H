#ifndef HYDRO_BRIEF_ULT_HXX
#define HYDRO_BRIEF_ULT_HXX 

#include <stdio.h>
#include <stdlib.h>
extern "C"
{

}

#include "HydroBriefLogic.H"   
#include "Canvas.H"
#include "HydroBriefPainter.H"



class HydroBriefULT
{
    
public:

     HydroBriefULT(HydroBriefLogic *logic); 
     ~HydroBriefULT(); 
     
     void loadRiverList(Widget riverList);

     void updateBriefPicture(int selectedPos, Canvas *canvas);
     void selectStageBasis(int buttonNumber, Canvas *canvas);
     
      
private:
   
     HydroBriefLogic *logic;
   
};

//**************************************************************************



#endif

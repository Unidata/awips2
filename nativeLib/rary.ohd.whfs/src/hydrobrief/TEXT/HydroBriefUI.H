#ifndef HYDRO_BRIEF_UI_HXX
#define HYDRO_BRIEF_UI_HXX

#include <iostream.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
 
extern "C"
{
#include "MotifMaster.h"
#include "Xtools.h"      
}

#include "Application.H"
#include "HydroBriefULT.H"
#include "Canvas.H"

const Dimension sw_offset = 32;

class HydroBriefUI
{
   
public:
   HydroBriefUI();
   HydroBriefUI(HydroBriefULT *initULT,
			   Widget topShell,
			   Application *initApp,
			   int argc,
			   char *argv[]);
   
   
   virtual ~HydroBriefUI();
          
   virtual void show();   
   virtual void print();  
   virtual void close();


   // callbacks
   static void selectRiverCallback(Widget listWidget,
				    XtPointer ptr, XtPointer cbs);
   
   static void selectStageBasisCallback(Widget pushbutton,
					XtPointer ptr, XtPointer cbs);
   
   static void closeCallback(Widget w, XtPointer ptr, XtPointer cbs);


   
protected:
  
   HydroBriefULT *ult;
   
   virtual void createWidgets();
   virtual void setupEventHandling();
   virtual void setupCanvas();
   virtual void loadRiverList();  // is this method necessary?

   
   //  Data
   Application *app;
   Canvas *canvas; 
   
   
   Boolean isRunningEventLoop;
   
   
   // Widgets
   Widget parent;
    
   Widget shell;
   Widget form ;
   Widget streamSL;
   Widget streamHSB;
   Widget streamVSB;
   Widget streamLI;
   Widget streamListLA;
   Widget closePB;
   Widget optionFR;
   Widget optionFO;
   Widget stageBasisOM;
   Widget stageBasisOMLA;
   Widget stageBasisCB;
   Widget stageBasisPDM;
   Widget maxObsFcstPB;
   Widget obsPB;
   Widget fcstPB;
   Widget filterFrameLA;
   Widget drawAreaFrameLA;
   Widget drawingAreaSW;
   Widget drawingAreaHSB;
   Widget drawingAreaVSB;
   Widget briefDA;

};



#endif

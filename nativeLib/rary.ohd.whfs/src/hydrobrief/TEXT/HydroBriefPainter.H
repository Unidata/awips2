#ifndef HYDROBRIEF_PAINTER_HXX
#define HYDROBRIEF_PAINTER_HXX

#include "Painter.H"
#include "HydroBriefLogic.H"
#include "River.H"
#include "Canvas.H"

#include "MaxMin.H"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "MotifMaster.h"
#include "time_convert.h"
extern "C"
{

}
   

const int top_offset = 10;
const int bottom_offset = 40;
const int MAX_STAGE_DIFF = 100;

const Position gageWidth = 60;
const Position gageSpacing = 75;
const Position initialXPos = 130;
   

//
// vars for placement of drawn text
//
const int level1Text = -2;
const int level2Text = -15;
const int level3Text = -28;
const int level4Text = -41;
const int level5Text = -54;

class HydroBriefPainter : public Painter
{

public:
   
   HydroBriefPainter(HydroBriefLogic *logic, Canvas *canvas);
   ~HydroBriefPainter();
   
   void draw();
   void setRiver(River *initRiver);
   
 
protected:
   
   //Position getWinY(double ydata, double standardStage);
   //double getDataY(Position ywin, double standardStage);
 
   void adjustCanvasWidth();
   
   void drawAllStations();
   void drawStation(RiverStation *station,
			MaxMin stationMaxMin,
			Position x,
			Position gageWidth);
  
   void drawWaterLevel(char *fillColor,
		    double lowValue, double highValue,
		    Position x, Position fillWidth,
		    double gageMin, double gageMax,
		    Position topY, Position bottomY);
   
   
   void drawStageLabel(char *labelColor,
		       double stageValue,
		       Position yOffset,
		       Position x, 
		       double gageMin, double gageMax,
		       Position topY, Position bottomY);
   
   
   
   void drawStandardStageLine();
   
   void determineMaxMinStages();
   void determineStandardDiff();
 
   
   // data
   
   HydroBriefLogic *logic;
   
   River *river;
   long numRiverStations;
   
   double pixelsPerUnit;
   Position standardStageY;
   
   long ticInterval;
   
   MaxMin *stationStageMaxMin;
   MaxMin riverMaxMin;
   MaxMin standardDiffMaxMin;
   
 
   
};


 


#endif
 

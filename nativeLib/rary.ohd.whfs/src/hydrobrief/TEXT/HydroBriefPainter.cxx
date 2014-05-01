
#include "HydroBriefPainter.H"


//**********************************************************************

HydroBriefPainter::HydroBriefPainter(HydroBriefLogic *initLogic,
				     Canvas *initCanvas)
{
     //
     // init the object pointers
     //
     logic = initLogic;
     canvas = initCanvas;
 
     
     //
     // change the width of the canvas according to the max number of
     // stations on any of the streams
     //
     adjustCanvasWidth();
  

     // ticInterval is used to determine max 
     // and min stages to be
     // displayed by a particular station.
     ticInterval = 5;   
     
     return;
}

//**********************************************************************

HydroBriefPainter::~HydroBriefPainter()
{
     //printf("inside HydroBriefPainter destructor\n");
     
     delete [] stationStageMaxMin;
     
     return;
}


//**********************************************************************

void HydroBriefPainter::draw()
{
      clearBackground();
      
      SetColor(canvas->getGC(), canvas->getDrawingArea(), "white");
         
      drawAllStations();
      
   
      // tell canvas to update the drawing area
      canvas->update();
}

//**********************************************************************
void HydroBriefPainter::adjustCanvasWidth()
{
     Dimension width = 0;
     Widget drawingArea;
     long maxNumStations = 0;
     
     //
     //   get the drawingArea associated with the canvas
     //
     drawingArea = canvas->getDrawingArea();
  
     
     //
     //   calculate the necessary width
     //
     maxNumStations = logic->getRiverNetwork()->getMaxNumStations();
     width = initialXPos + (maxNumStations * (gageSpacing + gageWidth));
     
     
     //
     // set the widget heights
     //
     setWidgetWidth(drawingArea, width);
 
     
     //
     // tell the canvas to adjust itself to the new configuration of 
     // the drawingArea widget
     //
     canvas->reinit();
     
     return;   
}
//**********************************************************************


void HydroBriefPainter::drawAllStations()
{
     int i;
     Position x = 0;
     Position bottomHeight;
     Position topHeight;
     Position height;
  
     double tempDouble;
     double diff;
     RiverStation *curStation;
     
     // printf("inside drawAllStations() \n");
     
         
     setRiver(logic->getSelectedRiver());
 
     
     determineMaxMinStages();
 
     
     determineStandardDiff();
   
     
     //
     // find the Y coordinate of the standard stage and the 
     // number of pixels per unit of measure
     // 
     tempDouble = ((double)(canvas->getHeight() - 
				(top_offset + bottom_offset)));	
     
     
     //
     // position the stage location relative to the whole useable area
     //
     tempDouble /= 2.0; 
     standardStageY = (short)floor(tempDouble + 0.5);
 
     
     //
     //  determine the max possible heights in pixels for a gage
     //
     bottomHeight = (canvas->getHeight() - bottom_offset) - standardStageY;
     topHeight = (standardStageY - top_offset);
     
     
     //
     // set height to the min of topHeight and bottomHeight
     //
     height = topHeight;
     if (bottomHeight < topHeight)
     {
          height = bottomHeight; 	
     }
     
     
     //
     //  determine the number of pixels per unit of measure
     //  this will be used to scale the gages
     //
     pixelsPerUnit =  height / standardDiffMaxMin.getMaxValue() ;
 
     
     // printf("pixelsPerUnit %6.2lf height %ld / DiffMax %6.2lf \n",
     //	    pixelsPerUnit, height, standardDiffMaxMin.getMaxValue() );
     
     
     //
     // draw the standard stage line and all the station pictures
     //
     drawStandardStageLine();

     
     //
     //  draw the gage for each river station
     //
     x = initialXPos;
     for (i = 0; i < numRiverStations; i++)
     {  
	  curStation = river->getRiverStation(i);
	  
	  diff = stationStageMaxMin[i].getMaxValue()  -
	         stationStageMaxMin[i].getMinValue();
	
	  
	  drawStation(curStation, stationStageMaxMin[i], x, gageWidth);

	  x += gageWidth + gageSpacing;
	  
     }
	
	
     return;
}

//***********************************************************************

void HydroBriefPainter::drawStandardStageLine()
{
   	char text[BUFSIZ];
   
   
	/*
		Set the color and set the line attributes to a dashed line.
	*/
	SetColor(canvas->getGC(),
		 canvas->getDrawingArea(), "white");
	
        XSetLineAttributes(canvas->getDisplay(),
			   canvas->getGC(),
			   0, LineOnOffDash, 0, 0);
	
	
	/*
		Draw dashed horiz line
	*/

        XDrawLine(canvas->getDisplay(),
		  canvas->getPixmap(),
		  canvas->getGC(),
		  0, standardStageY,
		  canvas->getWidth(), standardStageY);
	
	
	/*
		Return the line attributes to solid.
	*/
        XSetLineAttributes(canvas->getDisplay(), canvas->getGC(),
			   0, LineSolid, 0, 0);
	
	
	//
	// Draw the label "Flood Stage"
	//
	sprintf(text,"Flood Stage");
	
	XDrawString(canvas->getDisplay(),
		    canvas->getPixmap(),
		    canvas->getGC(),
		    0, standardStageY,
		    text,
		    strlen(text));
	
	
	//
	// Draw the label "Name"
	//
	sprintf(text,"Name");
	
	XDrawString(canvas->getDisplay(),
		    canvas->getPixmap(),
		    canvas->getGC(),
		    0, canvas->getHeight() + level1Text,
		    text,
		    strlen(text));
	
	//
	// Draw the label "Id (river mile) "
	//
	sprintf(text,"Id (river mile)");
	
	XDrawString(canvas->getDisplay(),
		    canvas->getPixmap(),
		    canvas->getGC(),
		    0, canvas->getHeight() + level2Text,
		    text,
		    strlen(text));
	
	
	//
	// Draw the label "Date/Time "
	//
	sprintf(text,"Date/Time");
	
	XDrawString(canvas->getDisplay(),
		    canvas->getPixmap(),
		    canvas->getGC(),
		    0, canvas->getHeight() + level3Text,
		    text,
		    strlen(text));
	
	//
	// Draw the label "Stage (when no graph) "
	//
	sprintf(text,"Stage (if no graph)");
	
	XDrawString(canvas->getDisplay(),
		    canvas->getPixmap(),
		    canvas->getGC(),
		    0, canvas->getHeight() + level4Text,
		    text,
		    strlen(text));
	
	return;
}	

//**********************************************************************

void HydroBriefPainter::drawStation(RiverStation *station,
				    MaxMin stationMaxMin, Position x,
				    Position gageWidth)
{
     char text[80];
     char errorText[BUFSIZ];
     double gageMax;
     double gageMin;
     double gageDiff;
     double floodStage = station->getFloodStage();
     double actionStage;
     time_t stageTime;
     double stageValue = MISSING;
     double ticValue = 0;
     
     int topGageHeight = 0;
     int bottomGageHeight = 0;
     
     Position topY;
     Position bottomY;
     Position ticY;
     Position y;
     Position ticLength;
     Position labelOffset = 5;
     Position marginWidth;
     struct tm *tmPtr;
     
     StageBasis stageBasis = logic->getStageBasis();
     
     
     SetColor(canvas->getGC(), canvas->getDrawingArea(), "white");
     
     
     //
     //  get the stageValue and stageTime, if possible
     //
     if (stageBasis == OBS_ONLY)
     {
	  stageValue = station->getCurObsStage(); 
	  stageTime = station->getCurObsTime();
     }
     
     else if (stageBasis == FCST_ONLY)
     {
	  stageValue = station->getMaxFcstStage();
	  stageTime  = station->getMaxFcstTime();
     }
     
     else // MAX_OBS_FCST
     {
	  if ( station->getCurObsStage() > station->getMaxFcstStage() )
	  {
	       stageValue = station->getCurObsStage();
	       stageTime  = station->getCurObsTime();
	  }
	  else
	  {
	       stageValue = station->getMaxFcstStage();
	       stageTime  = station->getMaxFcstTime(); 
	       
	  }
     }

     
     //
     //   print the observed or forecast stage time
     //
     
     if (stageValue != MISSING)
     {
	  tmPtr = gmtime(&stageTime);
	  strftime(text, sizeof(text), "%m/%d %H:%M UTC", tmPtr);    
	  XDrawString(canvas->getDisplay(),
		      canvas->getPixmap(),
		      canvas->getGC(),
		      x, canvas->getHeight() + level3Text,
		      text,
		      strlen(text));
     }
     else //(stageValue == MISSING)
     {	  
	  sprintf(text,"MSG Stage Data");
	  XDrawString(canvas->getDisplay(),
		      canvas->getPixmap(),
		      canvas->getGC(),
		      x, canvas->getHeight() + level3Text,
		      text,
		      strlen(text));	
     }
     
     
     
     //
     // print the name of the station
     //
     // sprintf(text, "%s %6.2lf", station->getLid(), station->getRiverMile());
     sprintf(text, "%s (%ld)", station->getLid(), station->getRiverMile());
     
     XDrawString(canvas->getDisplay(),
		 canvas->getPixmap(),
		 canvas->getGC(),
		 x, canvas->getHeight() + level2Text,
		 text,
		 strlen(text));
     
     sprintf(text, "%s", station->getName());
     
     XDrawString(canvas->getDisplay(),
		 canvas->getPixmap(),
		 canvas->getGC(),
		 x, canvas->getHeight() + level1Text,
		 text,
		 strlen(text));
     
     
     //
     // get max and min values
     //
     gageMax = stationMaxMin.getMaxValue();
     gageMin = stationMaxMin.getMinValue();
     
     gageDiff = gageMax - gageMin;
     
     //
     //   check that station can be drawn
     //
     if ( (gageDiff < MAX_STAGE_DIFF) && (station->getFloodStage() > 0.0) )
     {
	  
	  //
	  // determine the size of the staff gage
	  //
	  bottomGageHeight =  (int) (pixelsPerUnit * ( station->getFloodStage() - gageMin));
	  bottomY = standardStageY + bottomGageHeight;
	  
	  topGageHeight = (int) (pixelsPerUnit * (gageMax - station->getFloodStage()));
	  topY = standardStageY - topGageHeight;
	  
	  
	  
	  //
	  //  fill green up to gageMax
	  //
	  drawWaterLevel("green",
			 gageMin, gageMax,
			 x, gageWidth,
			 gageMin, gageMax,
			 topY, bottomY);
	  
	  
	  
	  //
	  //  fill yellow from actionStage to gageMax
	  //
	  actionStage = station->getActionStage();
	  if ((actionStage != MISSING) && (actionStage > 0.0))
	  {
	       drawWaterLevel("yellow",
			      actionStage, gageMax,
			      x, gageWidth,
			      gageMin, gageMax,
			      topY, bottomY);
	       
	       //	  drawStageLabel("black",
	       //		 actionStage, 0,
	       //	         x + labelOffset, 
	       //	    	 gageMin, gageMax,
	       //	         topY, bottomY);
	  }
	  
	  
	  
	  //
	  //  fill red from floodStage to gageMax
	  //
	  floodStage = station->getFloodStage();
	  if ((floodStage != MISSING) && (floodStage > 0.0))
	  {
	       drawWaterLevel("red",
			      floodStage, gageMax,
			      x, gageWidth,
			      gageMin, gageMax,
			      topY, bottomY);
	       
	       // drawStageLabel("white",
	       //		 floodStage, 0,
	       //	         x + labelOffset, 
	       //	    	 gageMin, gageMax,
	       //	         topY, bottomY);
	  }
	  
	  
	  
	  //
	  //  draw the max fcst or observed stage level and label it
	  //
	  if (stageValue != MISSING)
	  {
	       
	       marginWidth = gageWidth/4;
	       
	       drawWaterLevel("blue",
			      gageMin, stageValue,
			      x + marginWidth, gageWidth - (2*marginWidth),
			      gageMin, gageMax,
			      topY, bottomY);
	       
	       
	       drawStageLabel("white",
			      stageValue, 13,
			      x + labelOffset, 
			      gageMin, gageMax,
			      topY, bottomY);
	  }
	 
	  
	  
	  //
	  //  get the y coord of the gageMax
	  //
	  y =  GetWinCoord(gageMax, gageMin, gageMax, bottomY, topY);
	  
	  
	  //
	  //  draw the outline of the gage
	  //
	  SetColor(canvas->getGC(), canvas->getDrawingArea(), "white");
	  XDrawRectangle(canvas->getDisplay(),
			 canvas->getPixmap(),
			 canvas->getGC(),
			 x, y,
			 gageWidth, bottomGageHeight + topGageHeight);
	  
	  
	  
	  //
	  //  draw tics on the rectangle
	  //
	  ticY = topY;
	  SetColor(canvas->getGC(), canvas->getDrawingArea(), "white"); 
	  
	  for (ticValue = gageMin; ticValue <= gageMax; ticValue ++)
	  {
	       ticY = GetWinCoord(ticValue, gageMin, gageMax, bottomY, topY);
	       
	       //
	       // determine length of the tic, major is 8,  minor is 4 
	       //
	       if ( (int)ticValue % (int)ticInterval == 0) 
	       {
		    ticLength = 8;
		    
		    sprintf(text,"%6.2f", ticValue);
		    XDrawString(canvas->getDisplay(),
				canvas->getPixmap(),
				canvas->getGC(),
				x + gageWidth + labelOffset, ticY,
				text,
				strlen(text));
		    
	       }
	       else
	       {
		    ticLength = 4;   
	       }
	       
	       XDrawLine(canvas->getDisplay(),
		         canvas->getPixmap(),
		         canvas->getGC(),
			 x - ticLength, ticY,
			 x,  ticY);
	       
	       XDrawLine(canvas->getDisplay(),
		         canvas->getPixmap(),
		         canvas->getGC(),
			 x + gageWidth , ticY,
			 x + gageWidth + ticLength, ticY);
	  }
	  
	  
	  //
	  // draw flood stage value
	  //
	  sprintf(text, "%6.2f", station->getFloodStage());
	  XDrawString(canvas->getDisplay(),
		      canvas->getPixmap(),
		      canvas->getGC(),
		      x + gageWidth + labelOffset, standardStageY,
		      text,
		      strlen(text));
	  
     }
     else // can't draw the staff gage
     { 	
	  if (station->getFloodStage() == 0.0)
	  {
	       sprintf(errorText, "MSG Flood Stage");
	  }
	  else if ( gageDiff >= MAX_STAGE_DIFF)
	  {
	       sprintf(errorText, "Stage diffs too big.");
	  }
	  
	  XDrawString(canvas->getDisplay(),
		      canvas->getPixmap(),
		      canvas->getGC(),
		      x, canvas->getHeight() + level5Text,
		      errorText,
		      strlen(errorText));
	  
	  //
	  // stageValue is drawn in a different place if
	  // there is no staff gage
	  // 
	  if (stageValue != MISSING)
	  {
	        sprintf(text,"Stage = %6.2f",stageValue);
	        XDrawString(canvas->getDisplay(),
		      canvas->getPixmap(),
		      canvas->getGC(),
		      x, canvas->getHeight() + level4Text,
		      text,
		      strlen(text));

	  }
	  
	  
     }
     
   
}

//**********************************************************************

void HydroBriefPainter::drawWaterLevel(char *fillColor,
		                       double lowValue, double highValue,
		                       Position x, Position fillWidth,
		                       double gageMin, double gageMax,
		                       Position topY, Position bottomY)
{
       
     Position lowY;
     Position highY;
     Position fillHeight;
    
     highY = GetWinCoord(highValue, gageMin, gageMax, bottomY, topY);
     lowY = GetWinCoord(lowValue, gageMin, gageMax, bottomY, topY);
     
     fillHeight = lowY - highY;
	  
     SetColor(canvas->getGC(), canvas->getDrawingArea(), fillColor);
     XFillRectangle(canvas->getDisplay(),
			 canvas->getPixmap(),
			 canvas->getGC(),
			 x, highY,
			 fillWidth, fillHeight);
	  
     return;   
}

//**********************************************************************

void HydroBriefPainter::drawStageLabel(
		    char *labelColor,
		    double stageValue,
		    Position yOffset,
		    Position x, 
		    double gageMin, double gageMax,
		    Position topY, Position bottomY)
{
   
      char text[BUFSIZ];
      Position y;
      
      
      y = GetWinCoord(stageValue, gageMin, gageMax, bottomY, topY);
      y += yOffset;
      
      SetColor(canvas->getGC(), canvas->getDrawingArea(), labelColor);
      
      sprintf(text,"%6.2f", stageValue);
      XDrawString(canvas->getDisplay(),
		      canvas->getPixmap(),
		      canvas->getGC(),
		      x, y,
		      text,
		      strlen(text));
   
   
     return;   
}

//**********************************************************************

void HydroBriefPainter::setRiver(River *initRiver)   
{
     river = initRiver;
     numRiverStations = river->getNumRiverStations();
     
     stationStageMaxMin = new MaxMin[numRiverStations];
	  
     return;   
}

//***********************************************************************
   
  
void HydroBriefPainter::determineMaxMinStages()
{
   
     int i;
     RiverStation *station;
     double minStage;
     double maxStage;
     long longStage;
      
     for (i = 0; i < numRiverStations ; i++ )
     {
	  station = river->getRiverStation(i);
	  
	  stationStageMaxMin[i].init(9999, -9999, MISSING);
	  
	  
	  //	  
	  // check these values against max and min
	  //
	  stationStageMaxMin[i].checkValue(station->getFloodStage());
	  
	  if (station->getActionStage() > 0.0)
	       stationStageMaxMin[i].checkValue(station->getActionStage());
	  stationStageMaxMin[i].checkValue(station->getCurObsStage());
	  stationStageMaxMin[i].checkValue(station->getMaxFcstStage());
	  
	  //
	  // adjust minStage down to nearest number divisible by ticInterval
	  //
	  minStage =  stationStageMaxMin[i].getMinValue();
	  if (station->getFloodStage() > 100)
	  {	       
	       
	       minStage -= ticInterval ;
	       longStage = static_cast < long > ( minStage / ticInterval ) ;
	       minStage = longStage * ticInterval;
	       
	  }
	  else
	  {
	       minStage = 0.0;    
	  }
	  
	  
          //
	  // adjust maxStage up to nearest number div by ticInterval
	  //
	  maxStage = stationStageMaxMin[i].getMaxValue();
	  maxStage += 2 * ticInterval ;
	  longStage  = static_cast < long > ( maxStage/ticInterval ) ;
	  maxStage = longStage*ticInterval;
	  
	       
	  stationStageMaxMin[i].checkValue(minStage);
	  stationStageMaxMin[i].checkValue(maxStage);
	  
	  // printf("stationStageMaxMin = ");
	  //stationStageMaxMin[i].print();
     }
     
     riverMaxMin.init(9999, -9999, MISSING);
     riverMaxMin.checkValue(stationStageMaxMin, numRiverStations);
	  
     // riverMaxMin.print();
     
     return;
}

//**********************************************************************
  
void HydroBriefPainter::determineStandardDiff()
{
   
     int i;
     RiverStation *station;
     double floodStage;
     double minStage;
     double maxStage;
     MaxMin bottomDiffMaxMin;
     MaxMin topDiffMaxMin;
     
     
     
     //
     //  init MaxMin variables
     //
     standardDiffMaxMin.init(9999, -9999, MISSING);
     bottomDiffMaxMin.init(9999, -9999, MISSING);
     topDiffMaxMin.init(9999, -9999, MISSING);
     
     
     //
     //  for each station, check the difference between its floodStage and minStage
     //  and its maxStage and floodStage, and keep track of the biggest and
     //  smallest (unused)  differences
     //
     for (i = 0; i < numRiverStations ; i++ )
     {
	  station = river->getRiverStation(i);

	  floodStage = station->getFloodStage();
	  
	  //
	  //  don't bother with stations in which the floodStage is set to 0.0
	  //
	  if (floodStage > 0.0)
	  {
	       
	       minStage = stationStageMaxMin[i].getMinValue();
	       maxStage = stationStageMaxMin[i].getMaxValue();
	       
	       if ((maxStage - minStage) > MAX_STAGE_DIFF )
	       {
		  //printf("station %s has too great a diff. Diff = %lf\n",
		  //	   station->getLid(), maxStage-minStage);	  
		  
	       }
	       
	       else
	       {
		    //
		    // check the value of the difference
		    // between floodStage and minStage
		    //
		    bottomDiffMaxMin.checkValue(floodStage - minStage);
		    
		    
		    //
		    // check value of the difference between
		    // maxStage and floodStage
		    //
		    topDiffMaxMin.checkValue(maxStage - floodStage);
		    
	       }
	  }
     }
     
     //
     //
     //
     standardDiffMaxMin.checkValue(bottomDiffMaxMin);
     standardDiffMaxMin.checkValue(topDiffMaxMin);
     
     //printf("standardDiffMaxMin = ");
     //standardDiffMaxMin.print();
     

     return;
}

//**********************************************************************


#include "cvt_latlon.h"
#include "staffgage_show.h"
/**** POSTGRES
#include <sqlhdr.h>
*****/


/**************************************************************************/

StaffGageDialog *  getStaffGageDialog(void)
{
      
       static StaffGageDialog dialog;

       return &dialog;
}


/**************************************************************************/

void showStaffGage(Widget parent, char *lid)
{
   
     StaffGageDialog *dialog = getStaffGageDialog();
     char title[BUFSIZ];
     
     
     /*
         create widgets and lay them out
     */
     create_staffGageDS(parent); 
     
     
     /*
         set dialog title
     */
     sprintf(title, "Staff Gage for (%s)", lid);
     SetTitle(staffGageDS, title);
     
  
     /*
          manage the form and dialog shell
     */
     XtManageChild(staffGageFO);
     /*XtManageChild(staffGageDS);*/
   
     
     /*
          make widgets non editable
     */
     setFormEditable(staffGageFO, FALSE);
   
     
     /*
          load the dialog structure
     */
     loadStaffGage(dialog, lid);
          
     
     /*
          load the widgets with the info in the dialog structure
     */
     loadStaffGageWidgets(dialog);
    
     
     /*
          add the callbacks
     */
     staffGageAddCallbacks(dialog);

     
     /*
          draw the staff gage
     */
     drawStaffGage(dialog);
  
   
     /*
          copy the pixmap to the drawing area 
     */
     staffGageRedrawCallback(NULL, dialog, NULL);
     
     return;
     
}

/**************************************************************************/ 

void staffGageAddCallbacks(StaffGageDialog *dialog)
{
   
      Atom	wmAtom;

     /*
          Window manager callbacks.
     */
     wmAtom = XmInternAtom(XtDisplay(staffGageDS),
			      "WM_DELETE_WINDOW", False);
	
     XmAddWMProtocolCallback(staffGageDS, wmAtom,
				staffGageCloseCallback, dialog);
   
     
     XtAddCallback(sgClosePB,  XmNactivateCallback,
		   staffGageCloseCallback, dialog); 
    
     XtAddCallback(staffDA,  XmNexposeCallback,
		   staffGageRedrawCallback, dialog); 
     
   
     return;   
}

/**************************************************************************/

void staffGageRedrawCallback(Widget w, XtPointer ptr, XtPointer cbs )
{
     StaffGageDialog *dialog = (StaffGageDialog *) ptr;
   
     CopyPixmapToDA(dialog->drawingArea, dialog->pixmap);
   
     return;   
}

/**************************************************************************/

void staffGageCloseCallback(Widget w, XtPointer ptr, XtPointer cbs )
{
     StaffGageDialog *dialog = (StaffGageDialog *) ptr;
   
     if (dialog->pixmap)
	   	XFreePixmap(dialog->display, dialog->pixmap);
     
     XtDestroyWidget(staffGageDS);
     staffGageDS = NULL;
   
     return;   
}

/**************************************************************************/

void loadStaffGage(StaffGageDialog *dialog, char *lid)
{

     Crest	*crestHead;
     Location	*lHead;
     Riverstat	*rsHead;
     Floodcat	*fcHead;
     char where[140];
     Dimension borderWidth = 10;
     
        
     
     strcpy(dialog->lid, lid);
     
     
     dialog->drawingArea = staffDA;
    	
     dialog->display = XtDisplay(dialog->drawingArea);
     dialog->screen = XtScreen(dialog->drawingArea);


     /*
     Set the specific graphics fields that are used
     by the Drawing Area.
     */
     dialog->window = XtWindow(dialog->drawingArea);
     dialog->gc = XCreateGC(dialog->display,
			    dialog->window, 0, NULL);

     /*
          Determine dialog->drawingArea's dimensions
     */
     dialog->daHeight = getWidgetHeight(dialog->drawingArea);
     dialog->daWidth = getWidgetWidth(dialog->drawingArea);
   
   
     /*
         create pixmap
     */
     dialog->pixmap = XCreatePixmap(dialog->display, dialog->window,
				    dialog->daWidth, dialog->daHeight,
				     DefaultDepthOfScreen(dialog->screen));
     
     /* 
     calculate the center coordinate of the drawing area 
     */
     dialog->centerHeight = dialog->daHeight / 2;
     dialog->centerWidth  = dialog->daWidth  / 2;
     
     
     /*
     calculate the upper and lower border offsets
     */
     dialog->upperBorder = borderWidth;
     dialog->lowerBorder = (dialog->daHeight - borderWidth);
     
     
     /*
     set the drawing font 
     */
     if ((dialog->finfo = XLoadQueryFont(dialog->display, "7x13")) != NULL)
	  XSetFont(dialog->display, dialog->gc, dialog->finfo->fid);
     
     
     /*
     	   load the data
     */
     
     sprintf(where, "WHERE lid = '%s' ", lid);
     if ( ( lHead = GetLocation ( where ) ) )

     {  	
	  strcpy(dialog->name, lHead->name);
	
	  strcpy(dialog->basin, lHead->rb);
	  
	  strcpy(dialog->county, lHead->county);
	  strcpy(dialog->state, lHead->state);
	  
	  dialog->lat = lHead->lat;
	  dialog->lon = lHead->lon;
	  
	  dialog->elevation = lHead->elev;
	  

	  FreeLocation(lHead);
     }
     
       
     if ( ( rsHead = GetRiverstat ( where ) ) )
     {
	
	  dialog->riverMile = rsHead->mile;
	  
	  strcpy(dialog->tidalEffects, rsHead->tide);
	 
	  strcpy(dialog->stream, rsHead->stream);
	  
	  dialog->floodStage    = rsHead->fs;
	  dialog->floodFlow     = rsHead->fq;
	  dialog->bankfullStage = rsHead->bf;
	  dialog->actionStage   = rsHead->wstg;
	  dialog->actionFlow    = rsHead->action_flow;
	  dialog->zeroDatum     = rsHead->zd;  
  
	  FreeRiverstat(rsHead);
     }
     
       
     if ( ( fcHead = GetFloodcat ( where ) ) )
     {
	
	  dialog->minorStage    = fcHead->minor_stage;
          dialog->moderateStage = fcHead->moderate_stage;
	  dialog->majorStage    = fcHead->major_stage;
	  dialog->minorFlow     = fcHead->minor_flow;
          dialog->moderateFlow  = fcHead->moderate_flow;
	  dialog->majorFlow     = fcHead->major_flow;
	
	  FreeFloodcat(fcHead);
     }
     
     sprintf(where, " WHERE lid='%s' AND stage = "
		 " (SELECT MAX(stage) FROM crest WHERE lid='%s' "
		 "  AND (suppress != 'X' OR suppress IS NULL) ) ", lid, lid);
     if ( ( crestHead = GetCrest ( where )) )
     {
          dialog->recordStage = crestHead->stage;

          /* Modified to use date_t_to_USA_date routine. BryonL - 
             1/25/2005*/ 
          date_t_to_USA_date ( crestHead->datcrst, 
                               dialog->recordCrestDate ) ;

          if (! IsNull(INT, (void*) &crestHead->q))
             dialog->recordFlow  = crestHead->q;
          else
             (void) SetNull(INT, (void *) &dialog->recordFlow); 

          FreeCrest(crestHead);
     }
     else
     {
          (void) SetNull(DOUBLE, (void *) &dialog->recordStage);     
          (void) SetNull(INT, (void *) &dialog->recordFlow);     
          strcpy(dialog->recordCrestDate, "");
     }
     
     return;   
}

/**************************************************************************/

void loadStaffGageWidgets(StaffGageDialog *dialog)
{
      
      char text[BUFSIZ];
      char text2[BUFSIZ];
      char stringLat[BUFSIZ];
      char stringLon[BUFSIZ];
   
      /*
              reference
      */
      sprintf(text, "%s", dialog->name);
      XmTextSetString(sgNameTE, text);
     
      sprintf(text, "%s", dialog->basin);
      XmTextSetString(sgBasinTE, text);
      
      sprintf(text, "%s", dialog->stream);
      XmTextSetString(streamTE, text);
      
      sprintf(text, "%s", dialog->county);
      XmTextSetString(countyTE, text);
      
      sprintf(text, "%s", dialog->state);
      XmTextSetString(stateTE, text);
     
        
      sprintf(text, "%s", dialog->tidalEffects);
      XmTextSetString(tidalTE, text);
      
      
      strcpy(stringLat, (char *)cvt_latlon_from_double(dialog->lat));
      strcpy(stringLon, (char *)cvt_latlon_from_double(dialog->lon));
      
      sprintf(text, "%s / %s", stringLat, stringLon); 
      XmTextSetString(latlonTE, text);
      
   	 
      
      DataToString(&dialog->elevation, DOUBLE, text, "%8.2lf", "MSG");
      XmTextSetString(elevationTE, text);
      
      
      
      /*
      		Stages 
      */
      DataToString(&dialog->floodStage, DOUBLE, text, "%8.2lf", "    MSG ");
      strcat(text, "   ");
      DataToString(&dialog->floodFlow, DOUBLE, text2, "%8.0lf", "    MSG ");
      strcat(text, text2);
      XmTextSetString(floodTE, text);
     
      DataToString(&dialog->actionStage, DOUBLE, text, "%8.2lf", "    MSG ");
      strcat(text, "   ");
      DataToString(&dialog->actionFlow, DOUBLE, text2, "%8.0lf", "    MSG ");
      strcat(text, text2);
      XmTextSetString(actionTE, text);
      
      DataToString(&dialog->bankfullStage, DOUBLE, text, "%8.2lf", "    MSG ");
      XmTextSetString(bankfullTE, text);
  
      DataToString(&dialog->zeroDatum, DOUBLE, text, "%8.2lf", "    MSG ");
      XmTextSetString(zeroDataTE, text); 
      
      DataToString(&dialog->minorStage, DOUBLE, text, "%8.2lf", "    MSG ");
      strcat(text, "   ");
      DataToString(&dialog->minorFlow, DOUBLE, text2, "%8.0lf", "    MSG ");
      strcat(text, text2);
      XmTextSetString(minorTE, text); 
      
      DataToString(&dialog->moderateStage, DOUBLE, text, "%8.2lf", "    MSG ");
      strcat(text, "   ");
      DataToString(&dialog->moderateFlow, DOUBLE, text2, "%8.0lf", "    MSG ");
      strcat(text, text2);
      XmTextSetString(moderateTE, text); 
       
      DataToString(&dialog->majorStage, DOUBLE, text, "%8.2lf", "    MSG ");
      strcat(text, "   ");
      DataToString(&dialog->majorFlow, DOUBLE, text2, "%8.0lf", "    MSG ");
      strcat(text, text2);
      XmTextSetString(majorTE, text); 
  
      DataToString(&dialog->recordStage, DOUBLE, text, "%8.2lf", "    MSG ");
      strcat(text, "   ");
      DataToString(&dialog->recordFlow, INT, text2, "%8d", "    MSG ");
      strcat(text, text2);
      XmTextSetString(recordTE, text); 
     
      sprintf(text, "Record (%s):", dialog->recordCrestDate); 
      SetLabel(recordLA, text); 

 
     return;   
}

/**************************************************************************/

void drawStaffGage(StaffGageDialog *dialog)
{
    
     draw_mainarea(dialog);
    
   
     return;   
}
/**************************************************************************/


void	draw_mainarea(StaffGageDialog *dialog)
{
   /* draw the background filler  */
   
   draw_background(dialog);
   
   
   /* calculate the max/min stages */
   
   if (calc_max_stage(dialog) != DEFAULT_MAX &&
       calc_min_stage(dialog) != DEFAULT_MIN)
   {
      /* draw the label strings */
      
      draw_labels(dialog);
      
      
      /* draw the staff marker */
      
      draw_staff(dialog);
      
      
      /* draw the stage markers */
      
      draw_stage(dialog, dialog->maxStage);
      draw_stage(dialog, dialog->minStage);
      draw_stage(dialog, dialog->recordStage);
      draw_stage(dialog, dialog->majorStage);
      draw_stage(dialog, dialog->moderateStage);
      draw_stage(dialog, dialog->minorStage);
      draw_stage(dialog, dialog->floodStage);
      draw_stage(dialog, dialog->actionStage);
      draw_stage(dialog, dialog->bankfullStage);
      
      
      /* draw some labels on the categorical stages in the left
	 elevation column; and some other labels on the right side */
      
      draw_stagelabels(dialog, dialog->minorStage,    "Minor",    'L');
      draw_stagelabels(dialog, dialog->moderateStage, "Moderate", 'L');
      draw_stagelabels(dialog, dialog->majorStage,    "Major",    'L');
      draw_stagelabels(dialog, dialog->recordStage,   "Record",   'L');
      
      draw_stagelabels(dialog, dialog->floodStage,  "Flood Stg",  'R');
      draw_stagelabels(dialog, dialog->actionStage, "Action Stg", 'R');
      
      
      /* draw the elevation markers */
      
      draw_elev(dialog, dialog->maxStage);
      draw_elev(dialog, dialog->minStage);
      draw_elev(dialog, dialog->recordStage);
      draw_elev(dialog, dialog->majorStage);
      draw_elev(dialog, dialog->moderateStage);
      draw_elev(dialog, dialog->minorStage);
      draw_elev(dialog, dialog->floodStage);
      draw_elev(dialog, dialog->actionStage);
      draw_elev(dialog, dialog->bankfullStage);
      
      
      /* draw categorical brackets */
      
      draw_brackets(dialog);
   }
   else
   {
      draw_missing(dialog);
   }
   
  
   return;
}


/**********************************************************************
   Set the background color, and fill.
   *******************************************************************/

void	draw_background(StaffGageDialog *dialog)
{
   SetColor(dialog->gc, dialog->drawingArea, "black");
   XFillRectangle(dialog->display, dialog->pixmap, dialog->gc,
		  0, 0, dialog->daWidth, dialog->daHeight);
   return;   
}


/**********************************************************************
   Draw the missing error message.
   ********************************************************************/

void	draw_missing(StaffGageDialog *dialog)
{
   char	*label = "STAGE DATA UNAVAILABLE FOR STAFF GAGE DISPLAY.";
   
   
   SetColor(dialog->gc, dialog->drawingArea, "white");
   XDrawString(dialog->display, dialog->pixmap, dialog->gc,
	       dialog->centerWidth - 200, dialog->centerHeight - 10, 
	       label, strlen(label));	
   return;
}


/**********************************************************************
   Draw the staff gage.
   
   if action and flood stage, then colors are green-yellow-red;
   if no action stage then colors are green-red;
   if no flood stage then colors are green, yellow;
   if neither action nor flood stage, then green.
   
   ********************************************************************/

void	draw_staff(StaffGageDialog *dialog)
{
     Position	upperpos, lowerpos;
     Position	height, xpos, ypos;
     Dimension	offset;
     float	stage;
     int	actionAvail, floodAvail;
     Dimension  staffWidth = 40;
     Dimension  halfStaffWidth = staffWidth/2;
     Dimension  lowerBorderOffset = 10; 
     
     
     /* check if the action stage and flood stage are defined */
     
     if (dialog->actionStage > 0.0)
	  actionAvail = 1;
     else
	  actionAvail = 0;
     
     if (dialog->floodStage > 0.0)
	  floodAvail = 1;
     else
	  floodAvail = 0;
     
     if (dialog->actionStage >= dialog->floodStage)
	  actionAvail = 0;
     
     
     /* draw the green portion of the staff gage;
	  this color is always drawn.  draw green starting at bottom of
	  display and draw to top determined by data availability */
     
     lowerpos = GetWinCoord(dialog->minStage,
			    dialog->minStage,    dialog->maxStage,
			    dialog->lowerBorder, dialog->upperBorder);
     
     if (actionAvail == 1)
	  stage = dialog->actionStage;
     else if (floodAvail == 1)
	  stage = dialog->floodStage;
     else
	  stage = dialog->maxStage;
     
     upperpos = GetWinCoord(stage,
			    dialog->minStage,    dialog->maxStage,
			    dialog->lowerBorder, dialog->upperBorder);
     
     height   = lowerpos - upperpos;
     
     SetColor(dialog->gc, dialog->drawingArea, "green");
     XFillRectangle(dialog->display, dialog->pixmap, dialog->gc,
		    dialog->centerWidth - halfStaffWidth, upperpos, 50, height);
     
     
     /* draw the yellow portion of the stage, but only if an action stage 
	  is defined */
     
     if (actionAvail == 1)
     {
	  lowerpos = GetWinCoord(dialog->actionStage,
				 dialog->minStage,    dialog->maxStage,
				 dialog->lowerBorder, dialog->upperBorder);
	  
	  if (floodAvail == 1)
	       stage = dialog->floodStage;
	  else
	       stage = dialog->maxStage;
	  
	  upperpos = GetWinCoord(stage,
				 dialog->minStage,    dialog->maxStage,
				 dialog->lowerBorder, dialog->upperBorder);
	  
	  height = lowerpos - upperpos;
	  
	  SetColor(dialog->gc, dialog->drawingArea, "yellow");
	  XFillRectangle(dialog->display, dialog->pixmap, dialog->gc,
			 dialog->centerWidth - halfStaffWidth, upperpos, 50, height);
     }
     
     
     /* draw the red portion of the staff gage section representing the area
	  above flood stage */
     
     if (floodAvail == 1)
     {
	  lowerpos = GetWinCoord(dialog->floodStage,
				 dialog->minStage,    dialog->maxStage,
				 dialog->lowerBorder, dialog->upperBorder);
	  upperpos = GetWinCoord(dialog->maxStage,
				 dialog->minStage,    dialog->maxStage,
				 dialog->lowerBorder, dialog->upperBorder);	
	  height = lowerpos - upperpos;
	  SetColor(dialog->gc, dialog->drawingArea, "red");
	  XFillRectangle(dialog->display, dialog->pixmap, dialog->gc,
			 dialog->centerWidth - halfStaffWidth, upperpos, 50, height);
     }
     
     
     /* draw staff border */
     
     SetColor(dialog->gc, dialog->drawingArea, "white");
     XDrawRectangle(dialog->display, dialog->pixmap, dialog->gc,
		    dialog->centerWidth - halfStaffWidth,
		    dialog->upperBorder,
		    50,
		    dialog->lowerBorder - lowerBorderOffset);
     
     
     /* draw the tick marks */
     
     for (stage = dialog->maxStage; stage >= dialog->minStage; stage -= 1.0)
     {
	  xpos = dialog->centerWidth + 30;
	  ypos = GetWinCoord(stage,
			     dialog->minStage,    dialog->maxStage,
			     dialog->lowerBorder, dialog->upperBorder);
	  
	  if ((long) stage % 5)
	       offset = 2;
	  else
	       offset = 8;
	  
	  
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, ypos, xpos + offset, ypos);
	  
	  
	  xpos = dialog->centerWidth - halfStaffWidth;
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, ypos, xpos - offset, ypos);
     }
     
     
     return;  
}

/*********************************************************************
 draw the stage value labels  
*********************************************************************/

void	draw_stage(StaffGageDialog *dialog, float stage)
{
     Position	xpos, ypos;
     char		buf[BUFSIZ];
     long		len;
     
     
     /* prevent invalid values, especially nulls */
     
     if (stage > 0.0)
     {      
	  SetColor(dialog->gc, dialog->drawingArea, "white");
	  
	  
	  /* convert the floating value to a character string */
	  
	  DataToString(&stage, FLOAT, buf, "%8.2lf", "N/A");
	  len = strlen(buf);
	  
	  
	  /* calculate the ypos, and draw the string */
	  
	  xpos = dialog->centerWidth + 35;
	  ypos = GetWinCoord(stage, dialog->minStage, dialog->maxStage,
			     dialog->lowerBorder, dialog->upperBorder);
	  
	  
	  /* draw the string if a valid value is available */
	  
	  if (len)
	  {
	       XDrawString(dialog->display, dialog->pixmap, dialog->gc,
			   xpos, ypos + 5, buf, len);
	  }
     }
     
     return;  
}

/*********************************************************************
 draw the elevation value labels  
*********************************************************************/

void	draw_elev(StaffGageDialog *dialog, float stage)
{
     Position	xpos, ypos;
     char		buf[BUFSIZ];
     long		len;
     
     /* prevent invalid values, especially nulls */
     
     if (stage > 0.0 && dialog->zeroDatum > 0.0)
     {
	  
	  SetColor(dialog->gc, dialog->drawingArea, "white");
	  
	  
	  /* calculate the ypos, and draw the string */
	  
	  xpos = dialog->centerWidth - 95;
	  ypos = GetWinCoord(stage, dialog->minStage, dialog->maxStage,
			     dialog->lowerBorder, dialog->upperBorder);
	  
	  
	  /* convert the floating value to a character string */
	  
	  stage += dialog->zeroDatum;
	  DataToString(&stage, FLOAT, buf, "%8.2lf", "N/A");
	  len = strlen(buf);
	  
	  
	  /* draw the string if a valid value is available */
	  
	  if (len)
	  {
	       XDrawString(dialog->display, dialog->pixmap, dialog->gc,
			   xpos, ypos + 5, buf, len);
	  }
     }
     return;   
}

/*********************************************************************
   
*********************************************************************/
void	draw_brackets(StaffGageDialog *dialog)
{
     Position	xpos, uprypos, lwrypos;
     int		ticlen = 7;
     
     
     /* draw bracket from minor to moderate */
     
     if ((dialog->minorStage > 0.0) && (dialog->moderateStage > 0.0))
     {
	  SetColor(dialog->gc, dialog->drawingArea, "white");
	  
	  
	  /* calculate bracket positions */
	  
	  xpos    = dialog->centerWidth - 105;
	  lwrypos = GetWinCoord(dialog->minorStage, 
				dialog->minStage, 
				dialog->maxStage,
				dialog->lowerBorder, 
				dialog->upperBorder);
	  
	  uprypos = GetWinCoord(dialog->moderateStage, 
				dialog->minStage, 
				dialog->maxStage,
				dialog->lowerBorder, 
				dialog->upperBorder);
	  
	  
	  /* draw bracket outlines */
	  
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, lwrypos, xpos + ticlen, lwrypos);
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, lwrypos, xpos, uprypos);
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, uprypos, xpos + ticlen, uprypos);
     }
     
     
     /* draw bracket from moderate to major */
     
     if ((dialog->moderateStage > 0.0) && (dialog->majorStage > 0.0))
     {
	  SetColor(dialog->gc, dialog->drawingArea, "white");
	  
	  
	  /* calculate bracket positions */
	  
	  xpos    = dialog->centerWidth - 105;
	  lwrypos = GetWinCoord(dialog->moderateStage, 
				dialog->minStage, 
				dialog->maxStage,
				dialog->lowerBorder, 
				dialog->upperBorder);
	  
	  uprypos = GetWinCoord(dialog->majorStage, 
				dialog->minStage, 
				dialog->maxStage,
				dialog->lowerBorder, 
				dialog->upperBorder);
	  
	  
	  /* draw bracket outlines */
	  
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, lwrypos, xpos + ticlen, lwrypos);
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, lwrypos, xpos, uprypos);
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, uprypos, xpos + ticlen, uprypos);
     }
     
     
     /* draw bracket from major to record */
     
     if ((dialog->majorStage > 0.0) && (dialog->recordStage > 0.0))
     {
	  SetColor(dialog->gc, dialog->drawingArea, "white");
	  
	  
	  /* calculate bracket positions */
	  
	  xpos    = dialog->centerWidth - 105;
	  lwrypos = GetWinCoord(dialog->majorStage, 
				dialog->minStage, 
				dialog->maxStage,
				dialog->lowerBorder, 
				dialog->upperBorder);
	  
	  uprypos = GetWinCoord(dialog->recordStage, 
				dialog->minStage, 
				dialog->maxStage,
				dialog->lowerBorder, 
				dialog->upperBorder);
	  
	  
	  /* draw bracket outlines */
	  
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, lwrypos, xpos + ticlen, lwrypos);
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, lwrypos, xpos, uprypos);
	  XDrawLine(dialog->display, dialog->pixmap, dialog->gc,
		    xpos, uprypos, xpos + ticlen, uprypos);
     }
     
     return;   
}

/*********************************************************************
   draw column heading labels  
*********************************************************************/
   
void	draw_labels(StaffGageDialog *dialog)
{
     Position	xpos, ypos;
     char		*elevation = "ELEVATION";
     char		*stages    = "STAGE";
     
     
     SetColor(dialog->gc, dialog->drawingArea, "yellow");
     
     
     /* get the position, and draw the elevation label */
     
     ypos = GetWinCoord(dialog->maxStage,
			dialog->minStage,    dialog->maxStage,
			dialog->lowerBorder, dialog->upperBorder);
     XDrawString(dialog->display, dialog->pixmap, dialog->gc,
		 75, ypos - 20, elevation, strlen(elevation));
     
     
     /* get the position and draw the stage label */
     
     xpos = dialog->centerWidth + 55;
     XDrawString(dialog->display, dialog->pixmap, dialog->gc,
		 xpos, ypos - 20, stages, strlen(stages));
     
     return;  
}


/*********************************************************************
   draw stage labels  
   ******************************************************************/

void	draw_stagelabels(StaffGageDialog *dialog,
			 float stage,
			 char *stagetype,
			 char leftright)
{
     Position	xpos, ypos;
     
     
     /* prevent invalid values, especially nulls,
	  from being processed */
     
     if (stage > 0.0)
     {
	  
	  SetColor(dialog->gc, dialog->drawingArea, "white");
	  
	  
	  /* get the position, and draw the elevation label */
	  
	  ypos = GetWinCoord(stage,
			     dialog->minStage,    dialog->maxStage,
			     dialog->lowerBorder, dialog->upperBorder);
	  
	  /* draw the label on the left or the right side */
	  
	  if (leftright == 'L')
	       xpos = 7;   
	  else
	       xpos = dialog->centerWidth + 100;
	  
	  XDrawString(dialog->display, dialog->pixmap, dialog->gc,
		      xpos, ypos + 5, stagetype, strlen(stagetype));
     }
     
     
     return;  
}


/*************************************************************************/
   

float	calc_max_stage(StaffGageDialog *dialog)
{
   	float		max,
	   		tmp;
	long		lmax;
	
	
	/*
		Initialize max value to zero.
	*/
	max = DEFAULT_MAX;
	
	
	/*
		Compare max to each of the significant stage
		values and determine the maximum.
	*/
	if ((dialog->minorStage > max) && (dialog->minorStage > 0.0))
	   	max = dialog->minorStage;
	
	if ((dialog->moderateStage > max) && (dialog->moderateStage > 0.0))
	   	max = dialog->moderateStage;
	
	if ((dialog->majorStage > max) && (dialog->majorStage > 0.0))
	   	max = dialog->majorStage;
	
	if ((dialog->recordStage > max) && (dialog->recordStage > 0.0))
	   	max = dialog->recordStage;
	
	if ((dialog->floodStage > max) && (dialog->floodStage > 0.0))
	   	max = dialog->floodStage;
	
	if ((dialog->actionStage > max) && (dialog->actionStage > 0.0))
	   	max = dialog->actionStage;
	
	if ((dialog->bankfullStage > max) && (dialog->bankfullStage > 0.0))
	   	max = dialog->bankfullStage;
	
	
	/*
		Round the maximum value to the nearest
		multiple of five.
	*/
	if (max > DEFAULT_MAX)
	{	
	   	tmp  = max;
		lmax = (long) max / SCALE_ROUNDING;
		max  = (lmax + 1) * SCALE_ROUNDING;
		
		
		/*
			If the max value and the nearest stage
			are within 1.0 ft of each other, extend
			the max value.
		*/
		if ((max - tmp) < 1.0)
		   	max += 2.0;
	}
	
	
	/*
		Assign the max value.
	*/
	dialog->maxStage = max;
	return(max);  
}

/**************************************************************************/

float	calc_min_stage(StaffGageDialog *dialog)
{
 	float		min;
	float		tmp;
	long		lmin;
	
	
	/*
		Initialize min value to zero.
	*/
	min = DEFAULT_MIN;
	
	
	/*
		Compare min to each of the significant stage
		values and determine the minimum.
	*/
	if ((dialog->minorStage < min) && (dialog->minorStage > 0.0))
		min = dialog->minorStage;
	
	if ((dialog->moderateStage < min) && (dialog->moderateStage > 0.0))
		min = dialog->moderateStage;
	
	if ((dialog->majorStage < min) && (dialog->majorStage > 0.0))
		min = dialog->majorStage;
	
	if ((dialog->recordStage < min) && (dialog->recordStage > 0.0))
	   	min = dialog->recordStage;
	
	if ((dialog->floodStage < min) && (dialog->floodStage > 0.0))
		min = dialog->floodStage;
	
	if ((dialog->actionStage < min) && (dialog->actionStage > 0.0))
		min = dialog->actionStage;
	
	if ((dialog->bankfullStage < min) && (dialog->bankfullStage > 0.0))
		min = dialog->bankfullStage;
	
	
	/*
		Round to the nearest multiple of five (5).
	*/
	if ((min > 0.0) && (min < DEFAULT_MIN))
	{
	        tmp = min;
		lmin = (long) min / SCALE_ROUNDING;
		min  = lmin * SCALE_ROUNDING;
		
		/*
			If the min value and the nearest stage
			are within 1.0 ft of each other, extend
			the max value.
		*/
		if ((tmp - min) < 1.0)
		   	min -= 2.0;
	}
	else
	{
	 	min = 0.0;  
	}
	
	
	/*
		Assign the minimum value.
	*/
	dialog->minStage = min;
	return(min);  	
}


/**********************************************************************/

/*
	File:		crest_draw.c
	Date:		August 1995
	Author:		Chip Gobs
	
	Purpose:	Provides support for the Crest History DS.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Floodcat.h"
#include "Riverstat.h"
#include "Crest.h"
#include "crest_show.h"
#include "crest.h"
#include "ToolDefs.h"
#include "ParamDefs.h"



void	crest_draw(Widget w, Crest *crest)
{
   	Riverstat		*rPtr;
	
   	XFontStruct		*finfo;
	GC			gc;
	Pixmap			pm;
	Display			*display;
	Window			window;
	Dimension		height,
				width,
				y, x;
	
	double			maxstage,
				minstage,
				flood_stages[CREST_CAT_NUM];
	
	long			maxyear,
	 			minyear,
				num_points[CREST_CAT_NUM];
	
	int			i, floodcat_flag;
				

	char			drawn_string[BUFSIZ],
	   
	   			where[MAX_WHERE_LEN],

				*colors[] =
	   				{	"green",
				 		"yellow",
						"red",
						"red",
				 		"blue",
				 		"purple"
					},
		
				*cat_names[] = {"NONFLOOD",
						"ACTION",
						"FLOOD",
						"MINOR",
						"MODERATE",
						"MAJOR"};
	

	/*
		Get display attributes.
	*/
	display = XtDisplay(w);
	window  = XtWindow(w);
	width   = getWidgetWidth(w);
	height  = getWidgetHeight(w);

	
	/*
		Create gc and pixmap.
	*/
	gc = XCreateGC(display, window, 0, NULL);	
	pm = XCreatePixmap(display, window,
			   width, height,
			   DefaultDepthOfScreen(XtScreen(w)));
	

	/*
		Load font for drawing.
	*/
	if ((finfo = XLoadQueryFont(display, "7x13")) != NULL)
		XSetFont(display, gc, finfo->fid);
	
	
	
	/*
		Fill in background.
	*/
	SetColor(gc, w, "black");
	XFillRectangle(display, pm, gc, 0, 0, width, height);
	
	
	/*
		Determine the stage/year ranges.
	*/
	crest_stage_range(crest, &maxstage, &minstage);
	crest_date_range(crest, &maxyear, &minyear);
	

	/*
		Get the stage associated with each flood category
	*/
	floodcat_flag = get_flood_cat_stages((const char*)crst_lid,
					     flood_stages);


	/*
		Draw the scatterplot of past crests
	*/
	draw_crest_points(w, display, pm, gc,
			  height, width,
			  maxstage, minstage, 
			  maxyear, minyear,
			  crest, flood_stages, num_points,
			  colors);
	

	/*
		Draw lines and print labels for each of the flood cats.
	*/
	for (i = CREST_MAJOR;  i >= CREST_ACTION;  i--)
	{
	   if ((i > CREST_FLOOD) ||
	       ((i == CREST_FLOOD) && (floodcat_flag == False)) ||
	       (i == CREST_ACTION))
	   {
	      y = GetWinCoord(flood_stages[i], minstage, maxstage,
			      height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
	      
	      SetColor(gc, w, colors[i]);
	      XDrawLine(display, pm, gc, 0, y, width, y);
	      
	      sprintf(drawn_string, "%6.1f %s (%ld)",
		      flood_stages[i], cat_names[i], num_points[i]);
	      XDrawString(display, pm, gc, 0, y-7, drawn_string,
			  strlen(drawn_string));
	   }
	}

	
	/*
		Draw lines and print label for Riverstat's "FLOOD".
	*/
	memset(&where, '\0', sizeof(where));
	sprintf(where, " WHERE lid = '%s' ", crst_lid);
	rPtr = (Riverstat *) GetRiverstat(where);
	if (rPtr)
	{
	   y = GetWinCoord(rPtr->fs, minstage, maxstage,
			   height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
	   
	   x = GetWinCoord(maxyear, minyear, maxyear,
			   CREST_LEFT_OFFSET,
			   width-CREST_RIGHT_OFFSET);
	   
	   SetColor(gc, w, "red");
	   XDrawLine(display, pm, gc, 0, y, width, y);
	   
	   strcpy(drawn_string, "FLOOD");
	   XDrawString(display, pm, gc, x, y-7, drawn_string,
		       strlen(drawn_string));
	   
	   FreeRiverstat(rPtr);
	}
	
	
	/*
		Draw the axes
	*/
	draw_stage_axis(crstsclDA, maxstage, minstage,
			flood_stages, num_points, colors, cat_names);
	draw_year_axis(w, display, pm, gc, height, width, maxyear, minyear);
	
		
	/*
		Highlight chosen event
	*/
	highlight_crest(w, display, pm, gc,
			height, width, maxstage, minstage,
			maxyear, minyear);
	
	
	/*
		Register the x and y coordinate boundaries, so can retrieve them from
		within event handler function.  It avoids the use of globals.
	*/
	

	
	/*
		Set X coordinates	
	*/
	set_x_coords(minyear, maxyear, CREST_LEFT_OFFSET, width-CREST_RIGHT_OFFSET);

	
	/*
		Set Y coordinates
	*/	
	set_y_coords(minstage, maxstage, height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);

	
	/*
		Copy the pixmap to the canvas, cleanup,
		and return.
	*/
	XCopyArea(display, pm, window, gc, 0, 0, width, height, 0, 0);
	XFreeGC(display, gc);
	XFreePixmap(display, pm);

	
	return;  
}


void	draw_crest_points(Widget drawDA, Display *display, Pixmap pm, GC gc,
			  int height, int width,
			  double maxstage, double minstage, 
			  long maxyear, long minyear,
			  Crest *crest,
			  double flood_stages[], long num_points[],
			  char *colors[])
{
	double		cur_stage;
	long		cur_year;
		
	Crest		*crestPtr;
	int		level, 
	   		i;
	
	Position	x,
			y;
	
	
	if (crest)
		crestPtr = (Crest *) ListFirst(&crest->list);
	else
	   	crestPtr = NULL;
	
	
	
	/*
		Init the count of all the flood events for each level
	*/
	for (i = 0;  i < CREST_CAT_NUM;  i++)
		num_points[i] = 0;
	
	
	/*
		Check each crest to see if it is above any of the categories
	*/
	while (crestPtr) 
	{
	   if (! IsNull(DOUBLE, (void*) &crestPtr->stage))
	   {
		cur_stage = crestPtr->stage;
		
		
		/*
			Find flood category of crest
		*/
		level = -1;
		for ( i = CREST_MAJOR; i >= CREST_NONFLOOD; i--)
		{
		   if ((i == CREST_NONFLOOD) ||
		       
		       ((cur_stage >= flood_stages[i]) &&
			(flood_stages[i] > MISSING)) )
		       
		   {
		      level = i;
		      break;	
		   }
		}   
		
		
		/*
			Plot the points and increment the counter
			for number of points in a particular category.
		*/
		if ((level >= 0) && (level < CREST_CAT_NUM))
		{
		   y = GetWinCoord(cur_stage, minstage, maxstage,
				   height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
		   
		   cur_year = get_year(crestPtr->datcrst);
		   x = GetWinCoord(cur_year, minyear, maxyear, CREST_LEFT_OFFSET,
				   width-CREST_RIGHT_OFFSET);
		   
		   SetColor(gc, drawDA, colors[level]);
		   XDrawString(display, pm, gc, x+CREST_PT_X_OFFSET,
			       y+CREST_PT_Y_OFFSET, "X", 1);
		   
		   (num_points[level])++;
		}
	   }

	   
	   crestPtr = (Crest *) ListNext(&crestPtr->node); 
	}

  	
	return;   
}   



void	draw_stage_axis(Widget scaleDA, double maxstage, double minstage, 
			double flood_stages[], long num_points[],
			char *colors[], char * cat_names[])
				 
{
   	XFontStruct		*finfo;
	GC			gc;
	Pixmap			pm;
	Display			*display;
	Window			window;
	Dimension		height,
				width,
				x,
				y;
	Pixel			pixel;
		
	int			min,
				max;
	
	char			drawn_string[BUFSIZ];
	   		
		
	double			cur_stage,
				stage_inc;

	
	/*
		Get display attributes.
	*/
	display = XtDisplay(scaleDA);
	window  = XtWindow(scaleDA);
	width   = getWidgetWidth(scaleDA);
	height  = getWidgetHeight(scaleDA);
	

	/*
		Create gc and pixmap.
	*/
	gc = XCreateGC(display, window, 0, NULL);	
	pm = XCreatePixmap(display, window,
				    width, height,
				    DefaultDepthOfScreen(XtScreen(scaleDA)));
	
	
	/*
		Load font for drawing.
	*/
	if ((finfo = XLoadQueryFont(display, "7x13")) != NULL)
		XSetFont(display, gc, finfo->fid);
	
	
	/*
		Fill in the rectangle of the scaleDA with the same color as the
		Background of the Parent
	*/
	pixel = GetBackground(XtParent(scaleDA));
     	XSetForeground(display, gc, pixel);
     	XFillRectangle(display, pm, gc, 0, 0, width, height);
	SetColor(gc, scaleDA, "white");

	
	/*
		Get max and min coords of the stage
	*/
	min = GetWinCoord(minstage, minstage, maxstage,
				 height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
	max = GetWinCoord(maxstage, minstage, maxstage,
				 height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);	
	x = width - LINE_OFFSET;

	
	/*
		Draw the vertical line
	*/
	SetColor(gc, scaleDA, "white");
	XDrawLine(display, pm, gc, x, min, x, max); 
	

	/*
		Draw the horizontal lines and labels for the regular intervals
	*/	
	stage_inc = (maxstage - minstage)/CREST_SCALE_INTERVALS; 
	x = width - LINE_OFFSET;
	
	
	if (stage_inc == 0)
	{
	   	stage_inc = 10;   
	}
	
	
	for (cur_stage = minstage; cur_stage <= maxstage;
	 			   cur_stage += stage_inc)
	{
	   
	   	y = GetWinCoord(cur_stage, minstage, maxstage,
			      height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
		
	
	 	XDrawLine(display, pm, gc, x-5, y, x+5, y);

		sprintf(drawn_string, "%6.1f", cur_stage);
		XDrawString(display, pm, gc, x - LABEL_OFFSET, y+2,
			    drawn_string, strlen(drawn_string));
		   
	}   
		
	drawCrestUnitLabel(display, pm, gc, "STAGE IN FEET");

	/*
		Copy the pixmap to the canvas, cleanup,
		and return.
	*/
	XCopyArea(display, pm, window, gc, 0, 0, width, height, 0, 0);
	XFreeGC(display, gc);
	XFreePixmap(display, pm);
	
	return;  
}


void	drawCrestUnitLabel(Display *display, Pixmap pm, GC gc,
			   const char *label)
{
 	Dimension	x,
			y;
	char		buf[BUFSIZ];
	int		vertical_aspect,
	   		len,
	   		i;
	
	
	/*
		Initialize local variables.
	*/
	vertical_aspect = 12;
	y = 120;
	x = 0;

	
	/*
		Iterate through string and draw label.
	*/
	len = strlen(label);
	for (i = 0; i < len; i++)
	{
	 	buf[0] = label[i];
		buf[1] = '\0';
		
		XDrawString(display, pm,
			    gc, x, y, buf, strlen(buf));
		
		y += vertical_aspect;
	}
	
	return;
}


void	draw_year_axis(Widget w, Display *display, Pixmap pm, GC gc,
				   int height, int width,
				   long maxyear, long minyear)
{
	
	int			i, 
				year_inc;
	char			drawn_string[BUFSIZ];
	Position		x,
				y;
	
	

	SetColor(gc, w, "white");
	y = height - BOTTOM_Y_OFFSET;

	
	/*
		Draw year axis horizontal line
	*/
	XDrawLine(display, pm, gc, 0, y, width, y);
	
	
	/*
		Draw vertical marks
	*/
	year_inc = 10;
	for (i = minyear; i <= maxyear; i += year_inc)
	{
		x = GetWinCoord(i, minyear, maxyear,
				 CREST_LEFT_OFFSET, width-CREST_RIGHT_OFFSET);
		
		XDrawLine(display, pm, gc, x, y-5, x, y+5);		
		sprintf(drawn_string, "%d", i);	
		XDrawString(display, pm, gc, x-10, y+ 15,
				    drawn_string, strlen(drawn_string));
	}	
	
	
	return;	
}


void	highlight_crest(Widget drawDA, Display *display, Pixmap pm, GC gc,
				   int height, int width,
				   double maxstage, double minstage, 
				   long maxyear, long minyear)
{
		int		*poslist = NULL,
				pos;
		int		num_pos = 0;
		
		long		year;
		Position	x,
				y;
		
		Crest	*crestPtr;
		
		
		/*
			Get the position of the item that was selected.
		*/
		pos = 0;
		
		XmListGetSelectedPos(crestLB, &poslist, &num_pos);
				
		if ( (num_pos > 0) && poslist)
		{	
			pos = poslist[0];
			free(poslist);
		}
	
		
		/*
			go to that position in the crest linked list,
			and draw a white X on that point.
		*/
		
		if (crest && (crestPtr = (Crest *) ListNth(&crest->list, pos)) )
		{
			
			year = get_year(crestPtr->datcrst);
			
			x = GetWinCoord(year, minyear, maxyear,
					 CREST_LEFT_OFFSET, width-CREST_RIGHT_OFFSET);
			
			if (! IsNull(DOUBLE, (void*) &crestPtr->stage))
			{
			   y = GetWinCoord(crestPtr->stage, minstage, maxstage,
					   height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
			   
			   SetColor(gc, drawDA, "white");
			   XDrawString(display, pm, gc, x+CREST_PT_X_OFFSET,
				       y+CREST_PT_Y_OFFSET, "X", 1);
			}
		}
		
		return;
}	



int	get_flood_cat_stages(const char *lid, double flood_stages[])
{
   char		where[BUFSIZ];
   Floodcat	*fHead;
   Riverstat 	*rHead;
   int		minorExists;
   char		primary_pe[SHEF_PE_LEN + 1];
   

   /* initialize the stages to Missing */   

   flood_stages[CREST_NONFLOOD] = (double) 0.0;
   flood_stages[CREST_ACTION] = (double) MISSING;
   flood_stages[CREST_FLOOD] = (double) MISSING;
   
   flood_stages[CREST_MINOR] = (double) MISSING;
   flood_stages[CREST_MODERATE] = (double) MISSING;
   flood_stages[CREST_MAJOR] = (double) MISSING;
   
   
   /* retrieve the action and flood stages, and also 
      the primary pe for use in getting the proper 
      flood categories defined below */

   memset(&where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", lid);

   if ( (rHead = GetRiverstat(where)) )
   {
	 if (! IsNull(DOUBLE, (void*) &rHead->wstg))
	 {
	    flood_stages[CREST_ACTION] = rHead->wstg;
	 }
	 if (! IsNull(DOUBLE, (void*) &rHead->fs))
	 {
	    flood_stages[CREST_FLOOD] = rHead->fs;
	 }
      
      strcpy(primary_pe, rHead->primary_pe);
   }   


   /* if no primary pe defined, then assume it is HG */
   
   else
      strcpy(primary_pe, "HG");


   /*  retrieve the flood categories from the database.
       There are two sets of values, one for stage and 
       one for discharge.  Get the proper set based on 
       the primary pe. */

   minorExists = False;
   sprintf(where, "WHERE lid = '%s' ", lid); 
   if ( (fHead = GetFloodcat(where)) )
   {
      if (primary_pe[0] == 'Q')
      {
	 if (! IsNull(DOUBLE, (void*) &fHead->minor_flow))
	 {
	    flood_stages[CREST_MINOR] = fHead->minor_flow;
	    minorExists = True;
	 }
	 if (! IsNull(DOUBLE, (void*) &fHead->moderate_flow))
	 {
	    flood_stages[CREST_MODERATE] = fHead->moderate_flow;
	 }
	 if (! IsNull(DOUBLE, (void*) &fHead->major_flow))
	 {
	    flood_stages[CREST_MAJOR] = fHead->major_flow;
	 }
      }
      
      else
      {
         if (! IsNull(DOUBLE, (void*) &fHead->minor_stage))
         {
	    flood_stages[CREST_MINOR] = fHead->minor_stage;
	    minorExists = True;
         }
         if (! IsNull(DOUBLE, (void*) &fHead->moderate_stage)) 
         {
	    flood_stages[CREST_MODERATE] = fHead->moderate_stage;
         }
         if (! IsNull(DOUBLE, (void*) &fHead->major_stage))
         {
	    flood_stages[CREST_MAJOR] = fHead->major_stage;
         }
      }
      
      FreeFloodcat(fHead);
   }   


   
   if (rHead != NULL)
   {   
      FreeRiverstat(rHead);
   }   
   
   
   return(minorExists);  /* return value */
}




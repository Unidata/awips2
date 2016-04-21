/**************************************************************************
   statprof.c
   
   Implements the display and widget management functionality for
   the station profile dialog.
   
*************************************************************************/

#include "statprof_show.h"


/* global data declarations */

StatProf	*stations = NULL;
Report 		*allreports = NULL;

static Pixmap	profile_pix = 0;
static GC       profile_gc = NULL;
static int      create_pixmap = 1;

static void get_text_dimensions ( Widget w, int * height, int * width )
{
   static int first = 1;
   static int textheight = PROFILE_TEXT_HEIGHT;
   static int textwidth = PROFILE_TEXT_WIDTH; 
   XFontStruct * finfo = NULL;

   if ( first == 1 )
   {
      first = 0;
 
      /* Determine the font width */
      if ((finfo = XLoadQueryFont(XtDisplay(w), "7x13")) != NULL)
      {
         textheight = finfo->max_bounds.ascent - finfo->max_bounds.descent;
         textwidth = finfo->max_bounds.rbearing - finfo->min_bounds.lbearing;
      }
   }

   * height = textheight;
   * width = textwidth;  
}

static int find_label_position ( int y, int da_height, int textheight, 
                                 int * pLabelPixels, int dir )
{
   int i;
   int j;

   if ( dir == UP && y <= textheight )
   {
       y = find_label_position ( textheight + 1, da_height, textheight, 
                                 pLabelPixels, DOWN );
   }
   else if ( dir == DOWN && y >= da_height )
   {
       y = find_label_position ( da_height - 1, da_height, textheight,                                           pLabelPixels, UP );
   }
   else
   {
      for ( i = y; i > y - textheight; --i )
      {
         if ( pLabelPixels [ i ] == PIXEL_USED )
         {
            /* Find next free pixel and start search from there. */
            if ( dir == UP )
            {
               j = i ;
            }
            else
            {
               j = i + textheight;
            }

            while ( j > textheight && j < da_height )
            {
               if ( pLabelPixels [ j ] == PIXEL_FREE )
               {
                  break ;
               }

               j = j + dir;
            }

            y = find_label_position ( j, da_height, textheight, 
                                       pLabelPixels, dir );

            break;
         }
      }          
   }

   return y;
}

static int test_label_spacing ( StatProf * statPtr,
                                int textwidth, 
                                char * pLabel,  
                                int * end_mile,
                                int pixels_per_mile )
{
   int collision_flag;
   int label_width;
   int separation_in_miles;
   int separation_in_pixels;

   StatProf * statPtrNext = NULL;

   collision_flag = 0 ;

   if ( statPtr != NULL )
   { 
      /* Get the next station. */
      statPtrNext = ( StatProf * ) ListNext (  & statPtr->node );
      
      if ( statPtrNext != NULL )
      {
         /* Determine the number of pixels separating this station from the
            next station. */
         label_width = textwidth * strlen ( pLabel );
         * end_mile = (statPtr->mile - label_width / pixels_per_mile) - 2; 
         separation_in_miles = statPtr->mile - statPtrNext->mile;
         separation_in_pixels = separation_in_miles * pixels_per_mile ;

         if ( separation_in_pixels < label_width )
         {
            collision_flag = 1;
         }
      }
   }

   return collision_flag;
}

/*************************************************************************/

void	statprof_show(Widget w, const char *lid)
{
   Riverstat	*riverstat=NULL;
   char		where[255], buf[255];
   int		station_count;
   
 
   /* create station profile dialog, if not created. */
   
   if (! profileDS)
   {
      create_profileDS(GetTopShell(w));
      statprof_callbacks();
   }	
   
   
   /* manage and realize the station profile dialog,
      if not already displayed. */
   
   if (! XtIsManaged(profileDS))
   {
      /* get the stream name for the selected station. */
      
      sprintf(where, " WHERE lid = '%s' ", lid);
      if ((riverstat = GetRiverstat(where)) == NULL)
      {
	 ErrorDialog(w, "Unable to retrieve station information.");
	 return;
      }
      
      
      /* get the static station data */
      
      sprintf(where, " WHERE stream = '%s' ORDER BY mile DESC ", 
	      riverstat->stream);
      FreeRiverstat(riverstat);
      if ((stations = GetStatProf(where)) == NULL)
      {
	 ErrorDialog(w, "Unable to retrieve station information");
	 return;
      }
      
      
      /* get the dynamic river station data */
      
      station_count = ListCount(&stations->list);
      allreports = (Report *) malloc(sizeof(Report) * station_count);
      if (allreports == NULL)
      {
	 fprintf(stderr, "Error mallocing allreports.");
	 return;
      }
      load_statprof_reports();
      
      
      /* set the stream label. */
      
      sprintf(buf, "Stream: %s", stations->stream);
      SetLabel(profstrmLbl, buf);
      
      
      create_station_buttons();	
      
      XtManageChild(profileFM);
      XtManageChild(profileDS);
      XtUnmanageChild(profhelpPB);      
   }
   
   return;
}


/*************************************************************************/

void	statprof_callbacks(void)
{
   Atom		atom;
   
   /* window manager callbacks. */
   
   atom = XmInternAtom(XtDisplay(profileDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(profileDS, atom, statprof_close, NULL);
   
   
   /* widget callbacks. */
   
   XtAddCallback(profileDA, XmNexposeCallback,   statprof_draw,  NULL);
   XtAddCallback(profsclDA, XmNexposeCallback,   statscale_draw, NULL);
   XtAddCallback(profokPB,  XmNactivateCallback, statprof_close, NULL);
   
   return;  
}


/*************************************************************************/

void	statprof_draw(Widget w, XtPointer ptr, XtPointer cbs)
{
   draw_station_profile(w);
   
   return;
}


/*************************************************************************/


void	statscale_draw(Widget w, XtPointer ptr, XtPointer cbs)
{
   draw_station_scale(w);
   
   return;   
}


/*************************************************************************/

void	create_station_buttons(void)
{
   StatProf 	*statPtr;
   Widget	stationBtn;
   
   
   /* iterate through the list of stations and create the
      associated widgets for each location. */
   
   statPtr = (StatProf *) ListFirst(&stations->list);
   while (statPtr)
   {
      stationBtn = XtVaCreateManagedWidget(statPtr->lid,
					   xmPushButtonWidgetClass,
					   profstatPDM,
					   NULL);
      
      XtAddCallback(stationBtn, XmNactivateCallback, statprof_load, NULL);
      statPtr = (StatProf *) ListNext(&statPtr->node);  
   }
   
   
   /* set the menu position to the first element in the list,
      and subsequently invoke the associated callback. */
   
   SetMenuPos(profstatOM, 0);
   stationBtn = GetMenuHistory(profstatOM);
   statprof_load(stationBtn, NULL, NULL);
   
   return;
}


/*************************************************************************/

void	statprof_load(Widget w, XtPointer ptr, XtPointer cbs)
{
   StatProf	*statPtr;
   char		localprox[PROXIMITY_LEN], buf[255];
   int		pos;
   
   
   /* get the current menu position as an offset into the list of stations. */
   
   pos = GetMenuPos(profstatOM);
   statPtr = (StatProf *) ListNth(&stations->list, (pos + 1));
   
   
   /* load the station name field in the form of stream - proximity - name. */
   
   if (statPtr->proximity[0])
      strcpy(localprox, statPtr->proximity);
   else
      strcpy(localprox, "AT");
   
   sprintf(buf, "%s %s %s", statPtr->stream, localprox, statPtr->name);
   XmTextSetString(profnameTxt, buf);
   
   
   /* load the text fields.  */
   
   XmTextSetString(profrchTxt, statPtr->reach);
   if (statPtr->primary_pe[0] == 'H')
   {
      DataToString(&statPtr->fs,   DOUBLE, buf, "%8.2lf", ""); 
      XmTextSetString(proffsTxt, buf);
      DataToString(&statPtr->wstg, DOUBLE, buf, "%8.2lf", "");
      XmTextSetString(profasTxt, buf);
   }
   
   else
   {
      DataToString(&statPtr->fq,          DOUBLE, buf, "%8.0lf", "");
      XmTextSetString(proffsTxt, buf);
      DataToString(&statPtr->action_flow, DOUBLE, buf, "%8.0lf", ""); 
      XmTextSetString(profasTxt, buf);
   }
   
   return;  
}


/*************************************************************************/

float	get_max_elevation(StatProf *station)
{
   StatProf	*statPtr;
   float	max;
   long		lmax;
   
   /* initialize values. */
   statPtr = (StatProf *) ListFirst(&station->list);
   max = -10000.0;
   
   
   /* iterate through elevations and determine maximum value. */
   
   while (statPtr)
   {
      if ((statPtr->zd + statPtr->fs) > max)
	 max = statPtr->zd;
      
      statPtr = (StatProf *) ListNext(&statPtr->node);  
   }
   
   
   /* round off, and return. */
   
   lmax = (long) max / SCALE_ROUNDING;
   max  = (lmax + 100) * SCALE_ROUNDING;
   
   return(max);
}


/*************************************************************************/

float	get_min_elevation(StatProf *station)
{
   StatProf	*statPtr;
   float	min;
   long		lmin;
   
   
   /* initialize values.  */
   
   statPtr = (StatProf *) ListFirst(&station->list);
   min = 20000.0;
   
   
   /* iterate through elevations and determine maximum value. */
   
   while (statPtr)
   {
      if ((statPtr->zd + statPtr->fs) < min)
	 min = statPtr->zd;
      
      statPtr = (StatProf *) ListNext(&statPtr->node);  
   }
   
   
   /* round off to nice even values, and return. */
   
   lmin = (long) min / SCALE_ROUNDING;
   min  = (lmin - 100) * SCALE_ROUNDING;
   
   return(min);	
}


/*************************************************************************/

float	get_max_mile(StatProf *station)
{
   StatProf 	*statPtr;
   float	max, mod;
   long		lmax;
   
   
   /* initialize local variables. */
   
   statPtr = (StatProf *) ListFirst(&station->list);
   max  = 0.0;
   lmax = 0.0;
   
   
   /* iterate through the list of values, determining the maximum river mile. */
   
   while (statPtr)
   {
      if (statPtr->mile > max)
	 max = statPtr->mile;
      
      statPtr = (StatProf *) ListNext(&statPtr->node);
   }
   
   
   /* round off to nice even values, and return. */
   
   lmax = (long) max / SCALE_ROUNDING;
   max  = (lmax + 10) * SCALE_ROUNDING;
   if ( ( mod = ( ( long ) max % 10 ) ) )
      max += mod;
   
   return(max);
}


/*************************************************************************/

float	get_min_mile(StatProf *station)
{
   StatProf 	*statPtr;
   float	min, mod;
   long		lmin;
   
   
   /* initialize local variables. */
   
   statPtr = (StatProf *) ListFirst(&station->list);
   min  = 20000.0;
   lmin = 0.0;
   
   
   /* iterate through the list of values, determining the maximum river mile. */
   
   while (statPtr)
   {
      if (statPtr->mile < min)
	 min = statPtr->mile;
      
      statPtr = (StatProf *) ListNext(&statPtr->node);
   }
   
   
   /* round off to nice even values, and return. */
   
   if (min >= 0.0)
   {
      lmin = (long) min / SCALE_ROUNDING;
      min  = (lmin - 10) * SCALE_ROUNDING;
      if ( ( mod = ( (long) min % 10 ) ) )
	 min += mod;
   }
   else
   {
      min = 0.0;  
   }
   return(min);
}


/*************************************************************************/

void	draw_station_scale(Widget w)
{
   XFontStruct	*finfo;
   GC		gc;
   Pixmap	pm;
   Pixel	pixel;
   Display	*display;
   Window	window;
   Dimension	width, height, x, y;	
   
   float	maxelev, minelev;
   char		buf[255], tmp[255];
   int		textwidth, increment, len, i;
   
   
   
   /* get display attributes. */
   
   display = XtDisplay(w);
   window  = XtWindow(w);
   height  = getWidgetHeight(w);
   width   = getWidgetWidth(w);
   
   /* get the maximum and minimum elevations. */
   maxelev = get_max_elevation(stations);
   minelev = get_min_elevation(stations);
   
   /* create gc and pixmap. */
   gc = XCreateGC(display, window, 0, NULL);
   pm = XCreatePixmap(display, window, width, height, DefaultDepthOfScreen(XtScreen(w)));
   
   /* set the background color of the drawing area
      to match the background color of the parent. */
   pixel = GetBackground(XtParent(w));
   XSetForeground(display, gc, pixel);
   XFillRectangle(display, pm, gc, 0, 0, width, height);

   /* load font for drawing. */
   if ((finfo = XLoadQueryFont(XtDisplay(w), "7x13")) != NULL)
      XSetFont(display, gc, finfo->fid);
   
   /* determine the text width. */
   textwidth = (finfo->max_bounds.rbearing - finfo->min_bounds.lbearing) * 2;
   
   /* draw the scale. */
   x = width - ELEV_TOP_OFFSET;
   SetColor(gc, w, "white");
   XDrawLine(display, pm, gc, x, 5, x, height - ELEV_LOW_OFFSET);
   
   /* draw elevation foot markers. */
   x = width - 15;
   increment = (maxelev - minelev) / ELEV_INCREMENTS;
   for (i = maxelev; i >= minelev; i -= increment)
   {
      y = ((maxelev - i) / (maxelev - minelev)) * (height - ELEV_LOW_OFFSET);
      XDrawLine(display, pm, gc, x, y + 5, width - 5, y + 5);
      sprintf(buf, "%-6d", i);
      XDrawString(display, pm, gc, textwidth, y + finfo->ascent, 
		  buf, strlen(buf));
   }
   
   /* raw elevation legend. */
   strcpy(buf, "ELEVATION IN FEET");
   len = strlen(buf);
   y = (height  / 2) - ((len * finfo->ascent) / 2);

   for (i = 0; i < len; i++)
   {
      tmp[0] = buf[i];
      tmp[1] = '\0';
      XDrawString(display, pm, gc, 0, y, tmp, strlen(tmp));
      
      y += finfo->ascent;
   }
   
   
   /* copy the drawing to the pixmap, and cleanup any
      allocated data structures and/or memory. */
   XCopyArea(display, pm, window, gc, 0, 0, width, height, 0, 0);
   XFreeGC(display, gc);
   XFreePixmap(display, pm);
   
   return;  
}


/*************************************************************************/

void load_statprof_reports()
{
   StatProf	*statPtr;
   struct Report obsReport, fcstReport;
   int		obs_found, fcst_found;
   int		hours_back = 72;
   int		fcst_hours_ahead= 240, fcst_basis_hours_ago = 72;
   int		i, station_count;
   
   
   /* loop on the stations and get their data */
   
   station_count = ListCount(&stations->list);
   statPtr = (StatProf *) ListFirst(&stations->list);   
/*   printf("loading data for %d stations\n", station_count); */
   
   for (i = 0; i < station_count; i++)
   {
      
      /* get the river data for the current station */

      get_curobs_maxfcst(statPtr->lid, statPtr->primary_pe,
			 hours_back, fcst_hours_ahead,
			 fcst_basis_hours_ago,
			 &obs_found, &fcst_found,
			 &obsReport, &fcstReport);
     
      /* copy the max value and time into the global structure */
            
      if (obs_found && fcst_found)
      {
	 if (obsReport.value >= fcstReport.value)
	 {
	    allreports[i].value      = obsReport.value;
	    allreports[i].validtime  = obsReport.validtime;
	 }
	 else
	 {
	    allreports[i].value     = fcstReport.value;
	    allreports[i].validtime = fcstReport.validtime;	
	 }
      }
      
      else if (obs_found)
      {
	 allreports[i].value  = obsReport.value;
	 allreports[i].validtime = obsReport.validtime;
      }
      
      else if (fcst_found)
      {
	 allreports[i].value  = fcstReport.value;
	 allreports[i].validtime = fcstReport.validtime;
      }
      
      else
	allreports[i].value  = -9999.00;
      
      
      
      /* iterate to the next element in the list.*/
      
      statPtr = (StatProf *) ListNext(&statPtr->node);
   }
   
   return;
}


/*************************************************************************/
/* this function gets called at least  whenever the scroll bar is moved */

void	draw_station_profile(Widget w)
{
   int          end_mile;
   int          flag;
   int          j;
   int          opt_elev_y;
   int          opt_lid_y;
   int          *pLabelMiles = NULL;
   int          *pLabelPixels = NULL;
   int          *pStationPixels = NULL;
   int          station_count;
   int   textheight;
   int   textwidth;
   LabelPosition * pLabelPosition = NULL;
   XFontStruct	*finfo;
   GC           gc_labels = NULL;
   Display	*display;
   Window	window;
   Dimension	height, bottom, staff, oldx, oldy, x, y;
   static Dimension width = 0;
   XPoint	polygon[4];
   StatProf	*statPtr;
   
   char		buf[PROFILE_BUF_SIZE], buf2[PROFILE_BUF_SIZE], 
                tmp[255],lval[10]; 
   char         timeString[ANSI_TIME_LEN];
   float	maxelev, minelev, maxmile, minmile;
   int		total, i;
   time_t	obs_timet;
   struct tm	*obs_tm_ptr;
   
   double	max_value = -9999.00;
   void		*VoidPtr = &max_value;
   XGCValues    values;
   
   /* get display attributes. */
   
   display = XtDisplay(w);
   window  = XtWindow(w);
   height  = getWidgetHeight(w);
   
   /* If this is the first time that this routine is being called,
      get the text height and width in pixels. */
   if ( create_pixmap == 1 )
   {
      create_pixmap = 0;
      get_text_dimensions ( w, & textheight, & textwidth );

      /* Add any padding, */
      textheight += LABEL_Y_PAD;

      /* Create the array of label pixels. */
      pLabelPixels = ( int * ) malloc ( height * sizeof ( int ) );

      if ( pLabelPixels == NULL )
      {
         fprintf ( stderr, "Error mallocing pLabelPixels.\n" );
         return;
      }

      /* initialize the array of pixels. */
      for ( i = 0; i < height; ++i )
      {
         pLabelPixels [ i ] = PIXEL_FREE ;
      }

      /* Create the array of label miles. */
      pLabelMiles = ( int * ) malloc ( height * sizeof ( int ) );

      if ( pLabelMiles == NULL )
      {
         fprintf ( stderr, "Error mallocing pLabelMiles.\n" );
         return;
      }

      /* initialize the array of miles. */
      for ( i = 0; i < height; ++i )
      {
         pLabelMiles [ i ] = 0 ;
      }

      /* Create the array of station pixels. */
      pStationPixels = ( int * ) malloc ( height * sizeof ( int ) );
   
      if ( pStationPixels == NULL )
      {
         fprintf ( stderr, "Error mallocing pStationPixels.\n" );
         return;
      }

      /* Create the array of station label positions. */  
      station_count = ListCount ( & stations->list );

      if ( station_count > 0 )
      {
         pLabelPosition = ( LabelPosition * ) malloc ( station_count *
                                                    sizeof ( LabelPosition ) );

         if ( pLabelPosition == NULL )
         {
            fprintf ( stderr, "Error mallocing pLabelPosition.\n" );
            return;
         }
      }

      /* get the maximum and minimum elevations. */
      maxelev = get_max_elevation(stations);
      minelev = get_min_elevation(stations);
   
      /* get the maximum and minimum river miles. */
   
      maxmile = get_max_mile(stations);
      minmile = get_min_mile(stations);
   
      total = ( maxmile - minmile ) * 4; 

      if ( total < ( MIN_RIVER_MILES * 4 ) )
      {
         total = ( MIN_RIVER_MILES * 4 );
      }
   
      /* set the pixmap width. */
      setWidgetWidth(w, total);
      width = total;

      /* create gc and pixmap. */
      profile_gc = XCreateGC(display, window, 0, NULL);
      profile_pix = XCreatePixmap(display, window, width, height, 
                                  DefaultDepthOfScreen(XtScreen(w)));
   
   
      /* load font for drawing. */
      if ((finfo = XLoadQueryFont(XtDisplay(w), "7x13")) != NULL)
         XSetFont(display, profile_gc, finfo->fid);
   
      /* draw the window background. */
      SetColor(profile_gc, w, "black");
      XFillRectangle(display, profile_pix, profile_gc, 0, 0, width, height);
   
      /* set the lower edge of the display to allow space for */
      bottom = height - 40;
      staff  = 200 / ((maxelev - minelev) / bottom);
   
      /* draw the scale for river miles. */
      SetColor(profile_gc, w, "white");
      XDrawLine(display, profile_pix, profile_gc, 0, bottom, width, bottom);
      for (i = maxmile; i > minmile; i -= 10)
      {
         if ( (i % 50) == 0)
         {
	    x = ((maxmile - i) / (maxmile - minmile)) * width;
	    XDrawLine(display, profile_pix, profile_gc, x, bottom, x, 
                      height - 30);
	 
	    sprintf(buf, "%d", i);
	    XDrawString(display, profile_pix, profile_gc, x - 10, height - 20, 
                        buf, strlen(buf));
         }
      }
   
      /* draw river mile scale legend. */
      strcpy(buf, "RIVER MILES");
      x = (width / 2) - (strlen(buf) / 2);
      XDrawString(display, profile_pix, profile_gc, x, height - 5, buf, 
                  strlen(buf));
   
   
      /* loop on the stations and draw the station elevation points. */
      statPtr = (StatProf *) ListFirst(&stations->list);
      x = 0;
      y = ((maxelev - statPtr->zd) / (maxelev - minelev)) * bottom;
   
      i = 0;   
      while (statPtr)
      {
         oldx = x;
         oldy = y;
            
         x = ((maxmile - statPtr->mile) / (maxmile - minmile)) * width;		
         y = ((maxelev - statPtr->zd)   / (maxelev - minelev)) * bottom;

         SetColor(profile_gc, profileDA, "LightGoldenrod");
      
         polygon[0].x = oldx;
         polygon[0].y = oldy;
         polygon[1].x = x;
         polygon[1].y = y;
         polygon[2].x = x;
         polygon[2].y = bottom - 5;	
         polygon[3].x = oldx;
         polygon[3].y = bottom - 5;
      
         XFillPolygon(display, profile_pix, profile_gc, polygon, 4, Convex, 
                      CoordModeOrigin);		
         /* load the the river data for the current station */
         max_value  = allreports[i].value;
         obs_timet  = allreports[i].validtime;
      
         /* determine color of staff, based on proximity to action
	    or flood STAGE or DISCHARGE as appropriate. */
         if (statPtr->primary_pe[0] == 'H')
         {
	 
	    if ((max_value > statPtr->fs) && (statPtr->fs > 0.0))
	       SetColor(profile_gc, profileDA, "red");
	    
	    else if ((max_value > statPtr->wstg) && (statPtr->wstg > 0.0))
	       SetColor(profile_gc, profileDA, "yellow");
	 
	    else
	       SetColor(profile_gc, profileDA, "green");
         }
         else
         { 
	    if ((max_value > statPtr->fq) && (statPtr->fq > 0.0))
	       SetColor(profile_gc, profileDA, "red");
	 
	    else if ((max_value > statPtr->action_flow) && 
                     (statPtr->action_flow > 0.0))
	       SetColor(profile_gc, profileDA, "yellow");
	    
	    else
	       SetColor(profile_gc, profileDA, "green");
         }

         /* draw the staff gage. */
         XFillRectangle(display, profile_pix, profile_gc, x - 5, y - staff, 
                        10, staff);
      
         /* convert the value. */
         if (max_value != -9999.00)
         {
	    DataToString(VoidPtr, DOUBLE, lval, "%7.2lf", "MSG");
	    obs_tm_ptr = gmtime(&obs_timet);
	 
	    strftime(timeString, sizeof(timeString), "%H:%M %m/%d", obs_tm_ptr);	
	 
	    sprintf(tmp, "%s - %s", lval, timeString);
         }
      
         else
         {
	    strcpy(tmp, "MSG/MSG");
         }
      
         /* format the display string. */
         sprintf(buf, "%s (%s)", statPtr->lid, tmp);

         /* format the elevation string. */
         sprintf(buf2, "%7.2f", statPtr->zd);

         /* Copy these strings to the Label Postion structure. */
         memset ( pLabelPosition[ i ].lid_buf, '\0', PROFILE_BUF_SIZE + 1 ); 
         memset ( pLabelPosition[ i ].elev_buf, '\0', PROFILE_BUF_SIZE + 1 );
         strncpy ( pLabelPosition[ i ].lid_buf, buf, PROFILE_BUF_SIZE );
         strncpy ( pLabelPosition[ i ].elev_buf, buf2, PROFILE_BUF_SIZE );

         /* Zero out the station pixel array. */ 
         for ( j = 0; j < height; ++j ) pStationPixels [ j ] = 0; 

         /* Find the optimal y positions for the elevation and 
            lid strings. Fill in the Station Pixel array to represent these. */
         opt_lid_y = y - ( staff + 10 );
         opt_elev_y = y + 25 - textheight;

         /* Force the optimal label positions within the bounds of the
            station pixels and label miles arrays. */
         if ( opt_lid_y < 0 )
         { 
            opt_lid_y = 0;
         }
         else if ( opt_lid_y >= height )
         {
            opt_lid_y = height - 1;
         }
      
         if ( opt_elev_y < 0 )
         {
            opt_elev_y = 0;
         }
         else if ( opt_elev_y >= height )
         {
            opt_elev_y = height - 1; 
         }

         /* Set the pixels in the station pixel array corresponding between
            these optimal indexes to on. */
         for ( j = opt_lid_y; j <= opt_elev_y && j < height; ++j )
         {
            pStationPixels [ j ] = PIXEL_USED;
            pLabelMiles [ j ] = statPtr->mile;
         }

         /* Merge the label pixel array into the Station Pixel array. 
            Merge only those pixels which correspond to labels which 
            may conflict. */
         for ( j = 0; j < height; ++j )
         {
            if ( pLabelPixels [ j ] == PIXEL_USED )
            {
               if ( pLabelMiles [ j ] <= statPtr->mile )
               {
                  pStationPixels [ j ] |= pLabelPixels [ j ];
               }
            }
         }    
      
         /* find the best position for the elevation string */
         opt_elev_y = find_label_position ( opt_elev_y + 1, height, textheight, 
                                            pStationPixels, DOWN);

         /* Fill in the station pixels array to indicate the position of the
            elevation label. */
         for ( j = opt_elev_y; j > opt_elev_y - textheight; --j )
         {
            pStationPixels [ j ] = PIXEL_USED;
         }

         /* find the best position for the lid string. */
         opt_lid_y = find_label_position ( opt_lid_y - 1, height, 
                                           textheight, pStationPixels,
                                           UP );

         /* Record the label positions in the LabelPostion structure array. */
         pLabelPosition [ i ].lid_y = opt_lid_y + LABEL_Y_PAD;
         pLabelPosition [ i ].lid_x = x - 5;
         pLabelPosition [ i ].elev_y = opt_elev_y + LABEL_Y_PAD;
         pLabelPosition [ i ].elev_x = x - 25;

         /* Determine if the lid label of the next station will interfere 
            with this label. */
         flag = test_label_spacing ( statPtr, textwidth, buf, & end_mile, 4 );

         if ( flag == 1 )
         {
            /* A collision is possible.  Fill the Label Pixels array to
               to indicate the positions of the current labels. */
            for ( j = opt_lid_y; j > opt_lid_y - textheight; --j )
            {
               pLabelPixels [ j ] = PIXEL_USED;
               pLabelMiles [ j ] = end_mile;
            }

            /* Determine if the elevation label of this station will interfere 
               with the next. */
            flag = test_label_spacing ( statPtr, textwidth, buf2, 
                                        & end_mile, 4 );

            if ( flag == 1 )
            {
               /* A collision could occur.  Fill the Label Pixels array to
                  to indicate the positions of the current labels. */
               for ( j = opt_elev_y; j > opt_elev_y - textheight; --j )
               {
                  pLabelPixels [ j ] = PIXEL_USED;
                  pLabelMiles [ j ] = end_mile;
               }

            }

         }
         else
         {
            /* Clear the Pixels array. */
            for ( j = 0; j < height; ++j)
            {
               pLabelPixels [ j ] = PIXEL_FREE;
               pLabelMiles [ j ] = 0;
            }
         }

         /* move to the next element in the list. */
         statPtr = (StatProf *) ListNext(&statPtr->node); 
         i++;
      }
   
   
      /* terminate the drawing by extending the
         elevation contour to edge of the drawing area. */
   
      SetColor(profile_gc, profileDA, "LightGoldenrod");
   
      polygon[0].x = x;
      polygon[0].y = y;
      polygon[1].x = width;
      polygon[1].y = y;
      polygon[2].x = width;
      polygon[2].y = bottom - 5;	
      polygon[3].x = x;
      polygon[3].y = bottom - 5;
   
      XFillPolygon(display, profile_pix, profile_gc, polygon, 4, Convex, 
                   CoordModeOrigin);
   
      /* Create the graphics contect for the elevation and  lid labels.
         This will be an xor context which will ensure that the labels 
         are always visible no matter the color of the background they 
         are drawn on. */
      XtVaGetValues ( w, XmNbackground, &values.background,
                      XmNforeground, &values.foreground, NULL );
 
      values.foreground ^= values.background;
      values.function = GXxor;

      gc_labels = XCreateGC ( display, window, GCForeground | GCBackground |
                              GCFunction, &values );
      if ( finfo != NULL )
      {
         XSetFont(display, gc_labels, finfo->fid);
      }
   
      /* iterate through the list and draw the station lid and elevation
         labels. */
      i = 0;
      statPtr = (StatProf *) ListFirst(&stations->list);

      while (statPtr)
      {
         x = ((maxmile - statPtr->mile) / (maxmile - minmile)) * width;		
         y = ((maxelev - statPtr->zd)   / (maxelev - minelev)) * bottom;		
      
         /* draw a line from the elevation point. */
         SetColor ( profile_gc, w, "black" );
         XDrawLine(display, profile_pix, profile_gc, x, y, x, y + 10);
      
         /* draw the elevation in msl. */
         sprintf(buf, "%7.2f", statPtr->zd);
         XDrawString(display, profile_pix, gc_labels, 
                     pLabelPosition [ i ].elev_x, 
                     pLabelPosition [ i ].elev_y, 
                     pLabelPosition [ i ].elev_buf, 
                     strlen(pLabelPosition [ i ].elev_buf));

         /* Draw the station lid label. */
         XDrawString(display, profile_pix, gc_labels, 
                     pLabelPosition [ i ].lid_x, 
                     pLabelPosition [ i ].lid_y, pLabelPosition [ i ].lid_buf, 
                     strlen(pLabelPosition [ i ].lid_buf));
      
         /* iterate to the next element in the list.*/
         statPtr = (StatProf *) ListNext(&statPtr->node);
         i++;
      }

      /* Clean up any dynamically allocated memory. */
      if ( pStationPixels != NULL )
      {
         free ( pStationPixels );
         pStationPixels = NULL;
      }

      if ( pLabelPixels != NULL )
      {
         free ( pLabelPixels );
         pLabelPixels = NULL;
      }

      if ( pLabelPosition != NULL )
      {
         free ( pLabelPosition );
         pLabelPosition = NULL;
      }

      if ( pLabelMiles != NULL )
      {
         free ( pLabelMiles );
         pLabelMiles = NULL;
      }

      XFreeGC ( display, gc_labels );
      gc_labels = NULL;
   }
   
   /* copy the drawing to the pixmap */
   XCopyArea(display, profile_pix, window, profile_gc, 0, 0, width, height, 0, 
             0);

   return;  
}


/*************************************************************************/

void	statprof_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   Display * display = NULL;

   display = XtDisplay(w);

   if (stations)
   {
      FreeStatProf(stations);
      stations = NULL;
   }
      
   if (allreports)
   {
      free(allreports);
      allreports = NULL;
   }
   
   /* Free the pixmap and graphics context. */
   if ( profile_gc != NULL )
   {
      XFreeGC ( display, profile_gc );
      profile_gc = NULL;
   }

   if ( profile_pix != 0 )
   {
      XFreePixmap ( display, profile_pix ); 
      profile_pix = 0;
   }

   /* Set the flag indicating that upon the next expose event, 
      the pixmap needs to be recreated and redrawn to. */
   create_pixmap = 1;
   
   DestroyChildren(profstatPDM);

   if (XtIsManaged(profileDS))
   {
      XtUnmanageChild(profileDS);
   }
   
   return;
}


/*************************************************************************/

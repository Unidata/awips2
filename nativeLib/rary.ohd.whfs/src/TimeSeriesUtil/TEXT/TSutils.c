/* ************************************* */
/*    File:           TSutils.c        */
/*    Date:           April 1999       */
/*    Author:         Sung Vo          */
/*    Purpose:        Provide support  */
/*      Time Series Display plot's       */
/*      utility functions.               */
/* ************************************* */

/* ***************************************** */
/* ************* Header files ************** */
/* ***************************************** */

#include "TSGRAPH.h"
#include "GeneralUtil.h"
#include "TimeSeries_show.h"
#include "TimeSeries.h"
#include "TSedit.h"
#include "TSutils.h"
#include "TSControl_show.h"

/* ***************************************** */
/* external structures used in the Time      */
/* Series that defined in  TimeSeries_show.c */
/* ***************************************** */

extern  PAGE_DATA   TSpagedata[MAX_PAGE_DATA];
extern  PAGE_INFO   TSpageinfo[MAX_PAGE_INFO];
extern  COLORS  TScolors[MAX_COLORS];

int display_width;

const static char * plotButtonNames[ ] = {"TSPointsTB","TSLinesTB","TSBothTB"};
const static char * pcPpButtonNames[ ] = {"TSOffTB","TSInterpolateTB","TSAssignTB"}; 
const static char * scaleButtonNames [ ] = {"TSScaleDataOnlyTB","TSScaleDataShowCategTB",
                                            "TSScaleDataCategoriesTB"};
const static char * batchButtonNames [ ] = {"TSBatchDataOnlyTB","TSBatchShowCategTB",
                                            "TSBatchCategoriesTB"};  

Widget TSTracesDS, TSTracesClosePB, TSTracesW[40];

/* ---------------------------------------------------------------------- */
/* this function receives an input time series that contains
   only valid data. the output time series is returned
   in the same variable as the input time series. */

static int ts_accum_to_inc3 ( TRACE_DATA *TPtr ,
                      int distrib_mode )
{
   int         i, n;
   double    value;
   time_t    previous_time;
   double    previous_val;
   long        num_hrs_diff = ( long ) 0 ;
   int        npts;
   
   static     TRACE_DATA *tmpPtr;
   static     int first = 1;
   
   
   /* allocate the static memory the first time */
   
   if (first)
   {
      tmpPtr = malloc(sizeof(TRACE_DATA));
      first = 0;
   }
   
   memcpy((char *)tmpPtr, (char *)TPtr, sizeof(TRACE_DATA));
   
   
   /* save the first value as the previous before beginning the loop */
   
   previous_val  = TPtr->TSdata[0].y;
   previous_time = TPtr->TSdata[0].x;
   
   for (i = 1; i < TPtr->npts; i++)
   {
      /* always set the time */
      

      tmpPtr->TSdata[i - 1].x = TPtr->TSdata[i].x;
      
      
      /* set the value according to whether data available. */
      
      if (TPtr->TSdata[i].mode == DELETE) 
        TPtr->TSdata[i].y = MISSING;

      if (TPtr->TSdata[i].y != MISSING && previous_val != MISSING)
      {
     
     /* if distributing the amount equally over the multiple 
        periods, then find the value, then spread it to the
        previous hours that had unfilled data */
     
     if (distrib_mode)
     {
        num_hrs_diff = (TPtr->TSdata[i].x - previous_time) / 3600;
        value  =       (TPtr->TSdata[i].y - previous_val)  / num_hrs_diff;
        
        for (n = 0; n < num_hrs_diff; n++)
        {
           tmpPtr->TSdata[i - 1 - n].y = value;
        }
     }
     
     else
     {
        tmpPtr->TSdata[i - 1].y = TPtr->TSdata[i].y -  previous_val;
     }

     
     /* reject the value if a negative value results.
        if in distrib mode, then need to ensure that the previous
        time periods are also set to MISSING */
     
     if (tmpPtr->TSdata[i - 1].y < 0.0)
     {
        tmpPtr->TSdata[i - 1].y = MISSING;

        if (distrib_mode)
        {
            for (n = 0; n < num_hrs_diff; n++)
            {
                   tmpPtr->TSdata[i - 1 - (n + 1)].y = tmpPtr->TSdata[i - 1].y;
            }
        }
     }
      }
      
      else
     tmpPtr->TSdata[i - 1].y = MISSING;
            
      
      /* save any good values for the next pass */
      
      if (TPtr->TSdata[i].y != MISSING)
      {
     previous_val  = TPtr->TSdata[i].y;
     previous_time = TPtr->TSdata[i].x;
      }
   }
   
   memcpy((char *)TPtr, (char *)tmpPtr, sizeof(TRACE_DATA));
   
   
   /* return with the number of data points computed */
   
   if (TPtr->npts > 0)
      npts = TPtr->npts - 1;
   else
      npts = 0;
      
   return(npts);
   
}

/* ********************************** */
/* Create Graphic Context XOR for     */
/* Drawing crosshairs and rubber band */
/* ********************************** */
GC xs_create_xor_gc(Widget w)
{
   
   XGCValues     values;
   GC         gc;
   
   values.foreground = getMyColor ( XtDisplay( w ), "White");
   values.background = getMyColor ( XtDisplay( w ), "Black");

   values.foreground = (values.foreground ^ values.background);
   values.line_style = LineSolid;  
   values.function   = GXxor;
   
   gc = XtGetGC( w, GCForeground | GCBackground | GCFunction | GCLineStyle, &values);
   
   return ( gc );
   
}

/* ********************************************** */
/* Swap corner points when rubber band is actived */
/* ********************************************** */
void check_points(int *x1,int *y1,int *x2,int *y2)
{
   if( *x2 < *x1) { int tmp = *x1; *x1 = *x2; *x2 = tmp; }
   if( *y2 < *y1) { int tmp = *y1; *y1 = *y2; *y2 = tmp; }
   
}


/* ************************************************ */
/* get Observed data and fill up trace with points  */
/* ************************************************ */
int GetObserve_data( TRACE_DATA *tptr )
{
   
   extern PAGE_MGR    *PageMgr;
   
   Observation      *oHead, *oPtr;
   
   char                where[BUFSIZ];
   char            tablename[BUFSIZ];
   
   char ansiBeginTime[ANSI_TIME_LEN];

   char ansiEndTime[ANSI_TIME_LEN];
   
   int              count, 
                  n;
   float         ymin = 0. , 
                  ymax = 0. ;
   
   time_t              t, 
            BeginTime, 
            EndTime;
   
   BeginTime = PageMgr->BeginTime;
   EndTime   = PageMgr->EndTime;
   
   /* ****************************************** */
   /* convert times to to 19-character assi time */
   /* and build where cluase                     */
   /* ****************************************** */
   timet_to_yearsec_ansi(BeginTime, ansiBeginTime);
   timet_to_yearsec_ansi(EndTime, ansiEndTime);
   
   sprintf(where,
       " WHERE lid = '%s' "
       " AND pe = '%s' "
       " AND ts = '%s' "
       " AND dur = %d "
       " AND extremum = '%s' "
       " AND obstime >= '%s' "
       " AND obstime <= '%s' "
       " ORDER BY obstime ASC",
       tptr->trace_info.lid,
       tptr->trace_info.pe,
       tptr->trace_info.ts,
       tptr->trace_info.dur,
       tptr->trace_info.extremum,
       ansiBeginTime,
       ansiEndTime);
   
   /* ********************************************************* */
   /* Get data from database based on table name and trace info */
   /* ********************************************************* */
   getTableName(tptr->trace_info.pe,tptr->trace_info.ts, tablename);
   
   /* ***************************** */
   /* Initialize npts in trace = 0 */
   /* ***************************** */
   
   tptr->npts = 0;
   
   count = 0;

   oHead  = GetObservation(where, tablename);

   if ( oHead )
   {
      count = ListCount(&oHead->list);
   }
   else
   {
    return ( NO_DATA );
   }
   
   if ( count == 0) return (NO_DATA);
   
   
   oPtr = (Observation *) ListFirst(&oHead->list);
   n = 0;
   while ( oPtr )
   {
      yearsec_dt_to_timet(oPtr->obstime, &t);
      if (t >= BeginTime && t <= EndTime &&  oPtr->value != MISSING)
      {
     if ( n == 0 )
     {
           ymin = ymax = oPtr->value;
           tptr->value_xmin =  tptr->value_xmax =  t;

     }

     tptr->TSdata[n].x           = t;
     tptr->TSdata[n].y           = oPtr->value;
     tptr->TSdata[n].old_value   = oPtr->value;
     tptr->TSdata[n].revision    = oPtr->revision;
     tptr->TSdata[n].quality_code= oPtr->quality_code;
     tptr->TSdata[n].mode        = 0;

     if ( oPtr->value < ymin ) 
     {
        ymin = oPtr->value;
        tptr->value_xmin = t;
        
     }
     if ( oPtr->value > ymax ) 
     {
        ymax = oPtr->value;
        tptr->value_xmax = t;
     }
     
    n++;
       if ( n >= MAX_POINTS ) 
       {
              fprintf(stderr, "Maximum number of points (%d) per trace exceeded!\n", MAX_POINTS);
              n =  MAX_POINTS;
        break;
       }
      }
      
      oPtr = (Observation *) ListNext(&oPtr->node);
      
   }
    

   /* ****************************************************** */
   /* Store  real max and min from trace for display purpose */
   /* ****************************************************** */
   
   tptr->value_ymin = ymin;
   tptr->value_ymax = ymax;
   
   tptr->ymin = ymin;
   tptr->ymax = ymax;
   tptr->xmin = BeginTime; 
   tptr->xmax = EndTime;
   
   
   tptr->npts = n;
   
   if ( oHead )
      FreeObservation(oHead);
   
   if ( n > 0)
      return (1);
   else
      return (NO_DATA);


}



/* ************************************************ */
/* get Forecast data and fill up trace with points  */
/* ************************************************ */
int GetForecast_data( TRACE_DATA ftrace[] , TRACE_DATA *tptr)
{

    extern PAGE_MGR       *PageMgr;

    Forecast         *fHead, *fPtr;

    char             where[BUFSIZ];
    char        tablename[BUFSIZ];

    char ansiBeginTime[ANSI_TIME_LEN];
    char ansiEndTime[ANSI_TIME_LEN];

    int              count, 
            n, ntraces;
    float         ymin = 0. , 
            ymax = 0. ;

    time_t          t, 
            BeginTime, 
            EndTime;
    time_t        btime, prev_btime = 0.0;


        /* ****************************************** */
        /* convert times to to 19-character assi time */
        /* and build where cluase                     */
        /* ****************************************** */

    BeginTime = PageMgr->BeginTime;
    EndTime   = PageMgr->EndTime;
    timet_to_yearsec_ansi(BeginTime, ansiBeginTime);
    timet_to_yearsec_ansi(EndTime, ansiEndTime);

    sprintf(where," WHERE lid = '%s' "
            " AND pe = '%s'  "
            " AND ts = '%s'  "
            " AND dur = %d   "
            " AND extremum = '%s' "
            " AND validtime >= '%s' "
            " AND validtime <= '%s' "
            " ORDER BY ts, basistime DESC, validtime",
            tptr->trace_info.lid,
            tptr->trace_info.pe,
            tptr->trace_info.ts,
            tptr->trace_info.dur,
            tptr->trace_info.extremum,
            ansiBeginTime,
            ansiEndTime);

    /* ********************************************************* */
    /* Get data from database based on table name and trace info */
    /* ********************************************************* */
    getTableName(tptr->trace_info.pe,tptr->trace_info.ts, tablename);

    count = 0;

    fHead  = GetForecast(where, tablename);
    if( fHead )
          count = ListCount(&fHead->list);
    else
        return(NO_DATA);

    if ( count == 0) return (NO_DATA);


     fPtr = (Forecast *) ListFirst(&fHead->list);

    yearsec_dt_to_timet(fPtr->validtime, &t);
     yearsec_dt_to_timet(fPtr->basistime, &btime);

    /* ********************************** */
    /* Initialize n and ntraces to zeroes */
    /* ********************************** */
     n          = 0;
    ntraces    = 0;

    /* ********************************************* */
    /* Forecast data is different from observed data */
    /* Trace data is distingished by basis time      */
    /* so separated them when while reading them     */
    /* ********************************************* */
     while ( fPtr )
     {
         yearsec_dt_to_timet(fPtr->validtime, &t);

         if (t >= BeginTime && t <= EndTime && fPtr->value != MISSING)
         {
             yearsec_dt_to_timet(fPtr->basistime, &btime);

             if ( n == 0 )
             {
                prev_btime = btime;
                ymin   = ymax  =  fPtr->value;
                tptr->value_xmin =  tptr->value_xmax =  t;
            }


            if ( btime != prev_btime ) 
            {
               if (ntraces < MAX_FCST_TRACES)
               {
                n = 0;        /* Reset npts in new forecast trace */
                tptr->basistime =  prev_btime;
                memcpy((char *)&ftrace[ntraces],  (char *)tptr, sizeof(TRACE_DATA));
                ntraces++;
               }
               else
                   break; /* reached max fcst traces, break out of loop */
            }

            tptr->TSdata[n].y           = fPtr->value;
            tptr->TSdata[n].old_value   = fPtr->value;
            tptr->TSdata[n].x           = t;
            tptr->TSdata[n].mode        = 0;
            tptr->TSdata[n].revision    = fPtr->revision;
            tptr->TSdata[n].quality_code= fPtr->quality_code;
            tptr->TSdata[n].probability = fPtr->probability;

            if ( fPtr->value < ymin ) 
            {
                ymin = fPtr->value;
                tptr->value_xmin = t;

            }

            if ( fPtr->value > ymax ) 
            {
                ymax = fPtr->value;
                tptr->value_xmax = t;
            }

            n++;
            prev_btime = btime;
            tptr->npts = n;
            tptr->value_ymin = ymin;
            tptr->value_ymax = ymax;
            tptr->ymin = ymin;
            tptr->ymax = ymax;
            tptr->xmin = BeginTime; 
            tptr->xmax = EndTime;


        }

        fPtr = (Forecast *) ListNext(&fPtr->node);

     }


    /* *********************************** */
    /* Copy last trace into forecast trace */
    /* *********************************** */
    if (ntraces < MAX_FCST_TRACES)
    {
        tptr->basistime =  prev_btime;
        memcpy((char *)&ftrace[ntraces],  (char *)tptr, sizeof(TRACE_DATA));
        ntraces++;
    }

    if ( fHead )
         FreeForecast(fHead);

    return ( ntraces );

}


/* ************************************************ */
/* Show "NO DATA AVAILABLE" to the drawing area     */
/* When there is no data from the database for      */
/* selected Lid and PeTSed.                         */
/* ************************************************ */

void show_nodata_label( GRAPH_DATA *gptr , char *trace_lid, char *trace_name)
{

    extern PAGE_MGR       *PageMgr;
    
    char    buf[100];
    char     lid_buf[LOC_ID_LEN + 1];
    
 /*ms*/ char     stn_name[LOC_NAME_LEN + 1];
    char     river_name[STREAM_NAME_LEN + 1];
    
    char    stn_buf[SHOW_LOC_LEN +1];
    char    stream_buf[SHOW_RIVER_LEN + 1];
        
        char    tmpfs[5];
    
    
    sprintf(lid_buf, "%s", trace_lid);
    memset(buf, '\0', sizeof(buf));
    memset(tmpfs, '\0', sizeof(tmpfs));
    
    getStnRiverName(trace_lid, stn_name, river_name);

    memset(stn_buf,'\0',LOC_NAME_LEN + 1);
    strncat(stn_buf, stn_name, SHOW_LOC_LEN);  
    strcat(buf, stn_buf);
        
    if ( gptr->hdataPtr.flood_stage == -9999 )
            sprintf(tmpfs, "M");
    else
            sprintf(tmpfs, "%.1f", gptr->hdataPtr.flood_stage);
         
    if(strcmp(river_name,"UNDEFINED") == 0)
    {
        sprintf(buf, "%s (%s)", lid_buf, stn_buf); 
    }
    else
    {
        memset(stream_buf,'\0',SHOW_RIVER_LEN + 1); 
        strncat(stream_buf, river_name, SHOW_RIVER_LEN);
        sprintf(buf, "%s (%s - %s) fs=%s", 
            lid_buf, stn_buf, stream_buf, tmpfs);
    }
    
    /* Added by guoxian zhou 07-2004.
     * The gc_line color for no data graph is "randomly" changed.
     * Need initialize it's color to "white" as consistency.
     */
     
    SetColor(PageMgr->gc_line, TSDrawArea, "white");
    
    XDrawRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_line, 
            gptr->x, gptr->y,gptr->w,gptr->h);
       XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, 
            gptr->x, gptr->y-10, buf, strlen(buf));
       XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, 
            gptr->x + gptr->w/2 - 17, gptr->y + gptr->h/2, 
              "NO DATA AVAILABLE", 17);

    XFlush(PageMgr->display);

}

/* ******************************************* */
/* Draw y-axis in general - the scaling  based */
/* on min of the mins and max of the maxs      */
/* ******************************************* */
void draw_xaxis(GRAPH_DATA *graph)
{

    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

    int           x, 
            dy,
            ndays,
            days_cnt,
            days_skip,
                major_ticks,
            minor_ticks,
            ZHrDisplay;

        time_t        ltime,
            curtime;

    char        buf[BUFSIZ];

    struct tm       *tm_ptr;


    if ( graph->num_traces < 1 ) 
                   return;

    time (&curtime);

    SetColor(PageMgr->gc_line, PageMgr->widget, "black");
    XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, 
                        graph->x - 65,
                        graph->y - 60,
                        graph->x + graph->w + 65,
                        graph->y + graph->h + 40);

    SetColor(PageMgr->gc_line, PageMgr->widget, "white");
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x, (graph->y + graph->h), 
                            (graph->x + graph->w), (graph->y + graph->h) );

    minor_ticks = 1; /* Minor ticks  set 1 hour default */
    major_ticks = 6; /* Major ticks  set 6 hour default */

    ndays = (graph->xmax - graph->xmin)/(24*3600);

    ZHrDisplay = days_cnt  = days_skip  = 1;

    if ( graph->w < 500 ) 
    {
        major_ticks = 12;
        minor_ticks =  2;
    }
    else if (ndays > 10)
    {

        ZHrDisplay  = 0;
        days_skip   = (int)(ndays/10);
        major_ticks = (int)(ndays/10)*24;
        minor_ticks = major_ticks/2;
        days_cnt    = days_skip;

    }

    for (ltime = graph->xmin; ltime <= graph->xmax; ltime += SECONDS_PER_HOUR)
    {
        x = x2pixel( graph, ltime );

        tm_ptr = gmtime(&ltime);

        dy = 5;
        if ( tm_ptr->tm_hour == 0) 
        {

            if ( days_cnt++ % days_skip == 0)
            {

                   dy = 12; 
                if (GroupInfo->grid_lines)
                 XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_grid,  x, (graph->y + graph->h),
                                                             x, graph->y);
                /* *************** */
                /* Hour Annotation */
                /* *************** */

                if ( ZHrDisplay )
                {
                strftime(buf, sizeof(buf), "%H", tm_ptr);
                XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, (x-5), (graph->y + graph->h + 22),
                buf, strlen(buf));
                }

                /* ******************** */
                /* Month/Day Annotation */
                /* ******************** */

                strftime(buf, sizeof(buf), "%m/%d", tm_ptr);
                XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, (x-15), (graph->y + graph->h + 35),
                buf, strlen(buf));
            }


        }
        else 
        {
            if ( tm_ptr->tm_hour % major_ticks  == 0 )
            {
                /* ******************** */
                /*   Hour annotation    */
                /* ******************** */
                dy = 10;
                strftime(buf, sizeof(buf), "%H", tm_ptr);
                   XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, x-5, 
                                    (graph->y + graph->h + 22), buf, strlen(buf));
            }


        }

            /* ******************************** */
            /* major and minor ticks annotation */
            /* ******************************** */
             if ( (tm_ptr->tm_hour % minor_ticks ) == 0)
             XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_line, x, (graph->y + graph->h),
                                                         x, (graph->y + graph->h + dy));
    }

    /* Draw time zone label "Z" for time axis
     * Added by guoxian zhou -- 04-2004
     */
    x = x2pixel( graph, graph->xmax );
    strcpy(buf, "(Z)");
    XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, (x+5), (graph->y + graph->h + 22),
    buf, strlen(buf));

    /* ******************************** */
    /* if grid lines option is on       */
    /* ******************************** */
    if (GroupInfo->grid_lines)
        XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_grid, 
                                (graph->x + graph->w), (graph->y + graph->h),
                                        (graph->x + graph->w), graph->y);

    /* ********************************************* */ 
    /* Draw reference vertical line at present time  */
    /* ********************************************* */ 

    x = x2pixel( graph, curtime);

    if ( x <= (graph->x + graph->w))
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_reference, x,  graph->y,
                                        x, (graph->y + graph->h));
    XFlush(PageMgr->display);
}



/* ******************************************* */
/* Draw x-axis in general - the scaling  based */
/* on min of the mins and max of the maxs      */
/* ******************************************* */
void draw_yaxis(GRAPH_DATA *graph)
{

    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

    float    data_inc,
        data,
        min,
        max,
        ydiff,
        tmp,
        discharge_value,
        stage_value,
        max_discharge;

    int    dx, 
        y, 
        ypixel, 
        xoffset,
        qr_on,
        nticks,
        num_ticks;

    char    buf[BUFSIZ];

    TRACE_DATA    *tptr;

    if ( graph->num_traces < 1 ) return;

    tptr  = (TRACE_DATA *) &graph->traces[0];


    xoffset  = 40;
    max      = graph->ymax;
    min      = graph->ymin;
    data_inc = graph->data_inc;

    if ( min >1000.0) xoffset = 50;

    ydiff = ( max - min );
    graph->display_flow_unit = 0;

    qr_on = 0;
    if(strncmp(tptr->trace_info.pe,"Q",1)==0)
    {
        xoffset  = 45;
        qr_on    = 1;
        max_discharge = max;
    }
    else
        max_discharge = get_DischargeFromStage( graph, max);

    SetColor(PageMgr->gc_line, PageMgr->widget, "white");
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x, graph->y,graph->x,(graph->y + graph->h));

    tmp  = (max-min)/data_inc; nticks = (int)tmp + 1;

    data = min;
    for (num_ticks = 0; num_ticks < nticks; num_ticks++)
    {

        if (ydiff < 1.0)
            sprintf(buf, "%4.2f", data);
        else if (ydiff < 10.0)
            sprintf(buf, "%4.1f", data);
        else if (ydiff >= 10.0 && ydiff <= 100.0)
            sprintf(buf, "%.0f",  data);
        else
            sprintf(buf, "%.0f",  data);

        if ( qr_on ) 
        {
            if ( max_discharge >= 10000.0 )
                sprintf(buf, "%.2f",  (data/1000.0));
            else
                sprintf(buf, "%.0f",  data);
        }

        y = y2pixel( graph,  data );

         dx =  5;
        if( GroupInfo->grid_lines )
        {
            dx = 8;
             XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_grid, 
                                     graph->x,  y,
                                        (graph->x + graph->w),y);
            if ( graph->rdataPtr.valid )
            {
                ypixel = y;

                if ( graph->rdataPtr.StageOrDischarge == STAGE ) 
                {
                     discharge_value = get_DischargeFromStage( graph , data);
                     if ( discharge_value != (RATING_CONVERT_FAILED) )
                         display_DischargeFromStage( graph , ypixel, discharge_value, max_discharge);
                }
                else
                {
                    stage_value = get_StageFromDischarge( graph , data);
                    if ( stage_value != (RATING_CONVERT_FAILED) )
                        display_StageFromDischarge( graph , ypixel, stage_value);

                }
            }
        }

         XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x, y,(graph->x - dx), y);
         XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x - xoffset, (y + 5),buf, strlen(buf));

        data+=data_inc;
    }

    /* ******************************** */
    /* if grid lines option is on       */
    /* ******************************** */
    if( GroupInfo->grid_lines)
         XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_grid, graph->x, graph->y,
                                 (graph->x + graph->w),graph->y);

}

/* *********************************** */
/* Draw the precipitation y-axis for   */
/* derived data from PC                */
/* *********************************** */
void draw_pcAspp_yaxis( GRAPH_DATA *graph , float ymin, float ymax, float  yinc)
{

    extern PAGE_MGR       *PageMgr;

    char    buf[20];

    int    y, xoffset = 6;

    float     ydiff, 
        data,
        dx = 5;

    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x + graph->w + xoffset, graph->y,
                                 graph->x + graph->w + xoffset,(graph->y + graph->h));

    ydiff =  (ymax - ymin);

    for (data = ymin; data <= ymax ; data += yinc)
    {
        if (ydiff < 1.0)
            sprintf(buf, "%4.2f", data);
        else if (ydiff < 10.0)
             sprintf(buf, "%4.1f", data);
        else if (ydiff >= 10.0 && ydiff <= 100.0)
            sprintf(buf, "%4.1f", data);
         else
            sprintf(buf, "%.0f", data);

        y = graph->h - (graph->h * ( data - ymin))/(ymax - ymin);

        XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x + graph->w + xoffset,     graph->y + y,
                                         graph->x + graph->w + xoffset + dx,graph->y + y);

        XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, graph->x + graph->w + 15 , 
                                       (graph->y + y + 5),buf, strlen(buf));

    }

     return;

}

/* **********************************************  */
/* Get unit label for the y-axis in the data base  */
/* **********************************************  */
void getShefPe( char *pe, char *buf)
{

    ShefPe     *sHead,
            *sPtr;

    char  where[BUFSIZ];

     sprintf(where, "where pe = '%s' ",pe);
     if ( ( sHead = GetShefPe(where) ) )
     {

         sPtr = (ShefPe *) ListFirst(&sHead->list);
        if (sPtr)
        {
            strcpy(buf, sPtr->name);
            strcat(buf, " ");
            strcat(buf, sPtr->eng_unit);
        }

        FreeShefPe(sHead);
    }
    else
    {
         strcpy(buf, "Generic Units");
    }

    return;


}

/* ******************************* */
/* Draw unit label for the y-axis  */
/* ******************************* */
void draw_Unitlabel( GRAPH_DATA *graph , char *label )
{
    
    extern PAGE_MGR       *PageMgr;

    int        n, 
            yoffset,
            change_ylabel;

    float        max_discharge;

    char        buf[3];

    TRACE_DATA    *tptr;

    yoffset = (graph->h - 10*strlen (label))/2;

    tptr  = (TRACE_DATA *) &graph->traces[0];

    change_ylabel = 1;

    if(strncmp(tptr->trace_info.pe,"Q",1)==0)
    {

        max_discharge = graph->ymax;

        memset(label,'\0',100);
        if (  max_discharge >= 10000.0)
            sprintf(label, "Total Discharge in KCFS");
        else
            sprintf(label, "Total Discharge in CFS");
    }

    for ( n = 0; n < strlen (label); n++)
    {
        buf[0]=label[n]; buf[1]='\0';
        XDrawString(PageMgr->display,PageMgr->window,PageMgr->gc_grid,
                graph->x - 45,
                graph->y + n*10 + yoffset, 
                buf, strlen(buf));

    }

}


/* ************************************************** */
/* Draw Total discharge or River Stage labels for the */
/* derived data on the right y-axis                   */
/* ************************************************** */
void drawStageFlowLabel( GRAPH_DATA *graph, int stageOrdischarge, float max_discharge)
{
    
    extern PAGE_MGR       *PageMgr;

    int    n, yoffset;

    char    label[80];

    char    buf[3];

    if ( stageOrdischarge == STAGE )
    {
        if (  max_discharge >= 10000.0)
            sprintf(label, "Total Discharge in KCFS");
        else
            sprintf(label, "Total Discharge in CFS");
    }
    else
        sprintf(label, "River Stage in Feet");

    yoffset = (graph->h - 10*strlen (label))/2;

    for ( n = 0; n < strlen (label); n++)
    {
        buf[0]=label[n]; buf[1]='\0';
        XDrawString(PageMgr->display,PageMgr->window,PageMgr->gc_grid,
                graph->x + graph->w + 60,
                graph->y + n*10 + yoffset, 
                buf, strlen(buf));

    }

}

/* ******************************************************* */
/* find and return an active graph number in a page        */
/* ******************************************************* */
int find_active_graph( PAGE_DATA *p, int x, int y)
{
    int n;

    for (n = 0; n< p->num_graphs; n++)
    {
        if ( x >= p->graph[n].x &&  x <= (p->graph[n].x + p->graph[n].w) &&
             y >= p->graph[n].y &&  y <= (p->graph[n].y + p->graph[n].h))

         return(n);

    }
    return (-1);

}

/* ******************************************************* */
/* check if cursor is in current active graph              */
/* ******************************************************* */
int check_graph_bound( GRAPH_DATA *g, int x, int y)
{
    if ( x >= g->x &&  x <= (g->x + g->w) &&
         y >= g->y &&  y <= (g->y + g->h))
                         return(1);

     return ( 0 );

}

/* ************************************************************ */
/* Display  Trace information and min and max of selected trace */
/* ************************************************************ */
void display_trace_minmax( TRACE_DATA *tptr )
{
    extern PAGE_MGR       *PageMgr;


    PAGE_DATA    *pptr = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA    *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

    char         name[20];
    char         buf[200];
    char         tbufmin[100];
    char         tbufmax[100];
    
    char         lid_buf[LOC_ID_LEN + 1];

/* ms*/    char         stn_name[LOC_NAME_LEN + 1];
    char         river_name[STREAM_NAME_LEN + 1];
    
    char        stn_buf[SHOW_LOC_LEN +1];
    char        stream_buf[SHOW_RIVER_LEN + 1];
    
        char            tmpfs[5];    
      
    
    struct tm     *tm_ptr;
    
    sprintf(lid_buf, "%s", tptr->trace_info.lid);
    memset(buf, '\0', sizeof(buf));
        memset(tmpfs, '\0', sizeof(tmpfs));
    
/*ms*/    getStnRiverName(lid_buf, stn_name, river_name);
    
    
    memset(stn_buf,'\0',SHOW_LOC_LEN + 1);
    strncat(stn_buf, stn_name, SHOW_LOC_LEN);  
    strcat(buf, stn_buf);    
        if ( gptr->hdataPtr.flood_stage == -9999 )
                sprintf(tmpfs, "M");
        else
                sprintf(tmpfs, "%.1f", gptr->hdataPtr.flood_stage);
    if(strcmp(river_name,"UNDEFINED") == 0)
    {
        sprintf(buf, "%s (%s)", lid_buf, stn_buf); 
    }
    else
    {
        memset(stream_buf,'\0',SHOW_RIVER_LEN +1); 
        strncat(stream_buf, river_name, SHOW_RIVER_LEN);
        sprintf(buf, "%s (%s - %s) fs=%s", lid_buf, stn_buf, stream_buf,tmpfs);
    }

    SetColor(PageMgr->gc_header, PageMgr->widget, "Cyan");    
    XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_header,
/*ms*/                gptr->x, gptr->y-20, buf, strlen(buf));
    
    tm_ptr = gmtime(&tptr->value_xmin);
    strftime(tbufmin, sizeof(tbufmin), "%m/%d/%y %Hz", tm_ptr);
    tm_ptr = gmtime(&tptr->value_xmax);
    strftime(tbufmax, sizeof(tbufmax), "%m/%d/%y %Hz", tm_ptr);

    sprintf(name,"%s %d %s %s",tptr->trace_info.pe,tptr->trace_info.dur,tptr->trace_info.ts,tptr->trace_info.extremum);
    memset(buf, '\0', sizeof(buf));
    if ( strncmp(tptr->trace_info.pe,"Q",1) == 0)
    {
        if ( tptr->value_ymax >= 10000.0)
            sprintf(buf,"%s min=%.2f %s max=%.2f %s",
             name, (tptr->value_ymin/1000.0), tbufmin, (tptr->value_ymax/1000.0), tbufmax);
        else
            sprintf(buf, "%s min=%.2f %s max=%.2f %s",
             name, tptr->value_ymin, tbufmin, tptr->value_ymax, tbufmax);

    }
    else
    {
        if ( pptr->num_graphs >=5 )
            sprintf(buf,"%s min=%.2f max=%.2f",
             name,tptr->value_ymin, tptr->value_ymax);
        else
            sprintf(buf,"%s min=%.2f %s max=%.2f %s",
             name,tptr->value_ymin, tbufmin, tptr->value_ymax, tbufmax);

    }
    XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, gptr->x,
    gptr->y-5,  buf, strlen(buf)); 

}

/* ************************************************************ */
/* Display Traces' names and information for multiple traces    */
/* ************************************************************ */
void display_trace_name( TRACE_DATA *tptr , int trace_cnt, int ShowpcAspp, int trace_valid)
{
    extern  PAGE_MGR      *PageMgr;

    PAGE_DATA    *Pdata = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA    *gptr = (GRAPH_DATA *) &Pdata->graph[PageMgr->active_graph];

    char     buf[100];
    
    char    lid_buf[LOC_ID_LEN + 1];
    
 /*ms*/ char     stn_name[LOC_NAME_LEN + 1];
    char     river_name[STREAM_NAME_LEN + 1];
    
    char    stn_buf[SHOW_LOC_LEN +1];
    char    stream_buf[SHOW_RIVER_LEN + 1];
        char    tmpfs[5];
    
    int xlimit,    buflen;
    static  int ypos = 5, xpos = 0;
    static int nameLength = 0;
 
    /* Display trace Lid, Location name and River name */
    
    if ( trace_cnt == 0)
    {
        
        sprintf(lid_buf, "%s", tptr->trace_info.lid);
        memset(buf, '\0', sizeof(buf));
                memset(tmpfs, '\0', sizeof(tmpfs));
    
/* ms*/        getStnRiverName(lid_buf, stn_name, river_name);
    
        
        memset(stn_buf,'\0',SHOW_LOC_LEN + 1);
        strncat(stn_buf, stn_name, SHOW_LOC_LEN);  
        strcat(buf, stn_buf);    
                if ( gptr->hdataPtr.flood_stage == -9999 )
                        sprintf(tmpfs, "M");
                else
                        sprintf(tmpfs, "%.1f", gptr->hdataPtr.flood_stage);

        if(strcmp(river_name,"UNDEFINED") == 0)
        {
            sprintf(buf, "%s (%s)", lid_buf, stn_buf); 
        }
        else
        {
            memset(stream_buf,'\0',SHOW_RIVER_LEN + 1); 
            strncat(stream_buf, river_name, SHOW_RIVER_LEN);
            sprintf(buf, "%s (%s - %s) fs=%s", lid_buf, stn_buf, stream_buf, tmpfs);
        }
        
        nameLength = strlen(buf)*8 + 5;

        SetColor(PageMgr->gc_header, PageMgr->widget, "Cyan");
        XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_header,
                gptr->x, gptr->y-20, buf, strlen(buf));            
    }    

    memset(buf,'\0',sizeof(buf));
    if (  ShowpcAspp  == 1 && strncmp("PP",tptr->trace_info.pe,2) == 0)
    {
        sprintf(buf," DERIVED PP");
    }
    else if ( trace_valid )
    {
        sprintf(buf, "%s %d %s %s", tptr->trace_info.pe,tptr->trace_info.dur,
                      tptr->trace_info.ts,tptr->trace_info.extremum);
    }
    else if ( !trace_valid )
    {
        sprintf(buf,"%s %d %s %s(NO DATA) ", tptr->trace_info.pe,tptr->trace_info.dur,
                     tptr->trace_info.ts,tptr->trace_info.extremum);
    }

    
    /* Display trace Name PEDTSEP */
    
/* ms */

    buflen = strlen(buf);

    xlimit =  (gptr->w);  

    xpos+=buflen*8;

    if ( trace_cnt == 0)  
    {
        xpos = 0;
        ypos = 5;
    }

    if ( xpos >= xlimit - 20)  
    {
        xpos = nameLength;
        ypos += 15;
    }

    XSetForeground(PageMgr->display, PageMgr->gc_line, tptr->trace_color);
    XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, gptr->x +
    xpos, gptr->y - ypos, buf, strlen(buf));

    return;


}

/* *********************************************** */
/* Display XY values while crosshairs in active    */
/* *********************************************** */
void search_and_display_trace(int xpix, int ypix, time_t xsecs)
{

    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

    time_t         xvalue;
    float        yvalue, discharge_value, stage_value, max_discharge;

    struct tm     *tm_ptr;
    char        buf[100],
            tbuf[100];

    PAGE_DATA    *Pdata = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA    *gptr = (GRAPH_DATA *) &Pdata->graph[PageMgr->active_graph];

    yvalue = pixel2y ( gptr, ypix );

    if ( xsecs == 0)
    {
        xvalue = pixel2x ( gptr, xpix );
        xvalue+=45;
    }
    else
         xvalue = xsecs;

    tm_ptr = gmtime(&xvalue);
    strftime(tbuf, sizeof(tbuf), "%m/%d/%y %H:%MZ", tm_ptr);
    
    display_width = strlen(tbuf)*8+10; 

    /* Modified by guoxian zhou 06-2004
     * need display both of the discharge value and stage value.
     * sprintf(buf,"%s Value = %.2f",tbuf,yvalue);
     */
     
    TRACE_DATA    *tptr;

    tptr  = (TRACE_DATA *) &gptr->traces[0];

    if(strncmp(tptr->trace_info.pe,"Q",1)==0)
        max_discharge = gptr->ymax;
    else
        max_discharge = get_DischargeFromStage( gptr, gptr->ymax);

    if ( gptr->rdataPtr.valid )
    {
        if ( gptr->rdataPtr.StageOrDischarge == STAGE ) 
        {
            discharge_value = get_DischargeFromStage( gptr, yvalue);
            if ( discharge_value != (RATING_CONVERT_FAILED) )
            {
                if ( max_discharge >= 10000.0 )
                    sprintf(buf,"Value = %.2f ft, %.2f KCFS",yvalue, discharge_value/1000.0);
                else
                    sprintf(buf,"Value = %.2f ft, %.2f CFS",yvalue, discharge_value);
            }
            else
                sprintf(buf,"Value = %.2f",yvalue);
        }
        else
        {
            stage_value = get_StageFromDischarge( gptr , yvalue);
            if ( stage_value != (RATING_CONVERT_FAILED) )
                sprintf(buf,"Value = %.2f CFS, %.2f ft",yvalue, stage_value);
            else
                sprintf(buf,"Value = %.2f",yvalue);

        }
    }
    else
        sprintf(buf,"Value = %.2f",yvalue);
        
    
    int tmp_width = strlen(buf)*8+10;

    SetColor(PageMgr->gc_header, PageMgr->widget, "Cyan");

    if (  GroupInfo->GroupSelected == GROUP )
    {
        char tmp_buf[200];
        sprintf(tmp_buf, "%s %s", tbuf, buf);
        display_width += tmp_width;
        XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background,  gptr->x+5, gptr->y-45,display_width,15);
        XDrawString(PageMgr->display, PageMgr->window, PageMgr->gc_header, gptr->x+10, gptr->y-35, tmp_buf, strlen(tmp_buf));
    }
    else
    {

        if(tmp_width > display_width)
            display_width = tmp_width;

        XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background,  gptr->x+5, gptr->y-65,display_width,15);
        XDrawString(PageMgr->display, PageMgr->window, PageMgr->gc_header, gptr->x+10, gptr->y-50, tbuf, strlen(tbuf));

        XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background,  gptr->x+5, gptr->y-45,strlen(buf)*8+10,15);
        XDrawString(PageMgr->display, PageMgr->window, PageMgr->gc_header, gptr->x+10, gptr->y-35, buf, strlen(buf));
    }

}


/* *********************************** */
/*         Display Page data           */
/* *********************************** */
void display_TSpage_data(int page)
{
    extern PAGE_MGR       *PageMgr;


    int    n;

    PAGE_DATA     *Pdata;
    
    /* ****************************************** */
    /* Get new page Dimension if re-sized by user */
    /* ****************************************** */

    update_pageDimesion( page );

    Pdata = (PAGE_DATA *) &TSpagedata[0];
    for (n = 0; n <Pdata->num_graphs; n++)
    {

        PageMgr->active_graph = n; 

        display_TSgraph_data( PageMgr->active_graph);

    }


    /* ************************************************************ */ 
    /* Display current page number on top right corner of display   */
    /* ************************************************************ */ 

    show_active_page_label ();

    XFlush(PageMgr->display);

    return;
}


/* *********************************** */
/*  Display active graph data          */
/* *********************************** */
void display_TSgraph_data(int active_graph)
{
    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

    PAGE_INFO    *pinfo;
    GRAPH_INFO    *ginfo;
    PAGE_DATA    *pptr;
    GRAPH_DATA    *gptr;


    int i;
    int    n, 
        npts, 
        color_index,
        interp_mode, 
        distrib_mode = 0 ,
        valid_graph,
        trace_cnt,
        trace_valid,
        DerivedPP,
        DisplayHgvars;
 
    static    int first_time = 1;

    time_t    interval_secs;

    char    buf[100];

    TRACE_DATA   *tptr;

    static    TRACE_DATA   *pcAsppPtr;
    Widget widget;

    trace_cnt   = 0;
    DerivedPP   = 0;
    valid_graph = 0;
    interp_mode = 0;

    PageMgr->pcAspp_intervals = 3600;

    interval_secs = PageMgr->pcAspp_intervals;

    pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
    ginfo = (GRAPH_INFO *) &pinfo->graph[active_graph];

    pptr = (PAGE_DATA  *) &TSpagedata[0];
    gptr = (GRAPH_DATA *) &pptr->graph[active_graph];  


    /* ********************************************************* */ 
    /* Make sure that the toggle buttons on the graphs/plot menu */
    /* are set correctly. The options are points, lines or both. */
    /* ********************************************************* */ 

    if ( GroupInfo->grid_lines == 1 )
    {
       XmToggleButtonSetState ( TSGridLinesTB, True, False );
    }
    else
    {
       XmToggleButtonSetState (TSGridLinesTB, False, False );
    }

    for(i = 0; i < 3; i++ )
    {
       if(i == GroupInfo->trace_mode)
       {
          if((widget = XtNameToWidget(TSPlotMO, plotButtonNames[i])) != NULL )
          {
                       XtVaSetValues(widget, XmNset, TRUE, NULL);
          }
       }
       else
       {
          if((widget = XtNameToWidget(TSPlotMO, plotButtonNames[i])) != NULL )
          {
                       XtVaSetValues(widget, XmNset, FALSE, NULL);
          }
       }

    }

    /* ********************************************************* */ 
    /* Allocate memory for pcAsppPtr  and re-use it all time     */
    /* for performance purpose                                   */
    /* ********************************************************* */ 

    if ( pcAsppPtr == NULL || first_time == 1 )
    {
        pcAsppPtr = malloc ( sizeof ( TRACE_DATA ));

        /*Initialize menu so that the corresponding item is selected.
         * Added by guoxian zhou 05-2004
         */
        
        first_time = 0;
     }

    /* ************************************************** */
    /*    Check if selected graph is a valid with data  */
    /* ************************************************** */

    for (n = 0; n < gptr->num_traces; n++)
    {
         if ( gptr->trace_valid[n] ) 
         {
             valid_graph = 1;
             tptr = (TRACE_DATA *) &gptr->traces[n]; 
             if (strncmp("PP",tptr->trace_info.pe,2) == 0)
             {
                /* Displaying PP data already.  No need to do 
                   PC to PP conversion. */
                ginfo->showpp = 0;
                ginfo->derivepp = NO_PC_TO_PP;

             }
         }
    }

    /* Reset  buttons on PC as 1 hour PP menu. */
    for(i = 0; i < 3; i++ )
    {
        if(i == ginfo->derivepp)
        {
            if((widget = XtNameToWidget(TSShowPCAsPPMO, pcPpButtonNames[i])) != NULL )
            {
                XtVaSetValues(widget, XmNset, TRUE, NULL);
            }
        }
        else
        {
            if((widget = XtNameToWidget(TSShowPCAsPPMO, pcPpButtonNames[i])) != NULL )
            {
                XtVaSetValues(widget, XmNset, FALSE, NULL);
            }
        }

    }

    if ( ! valid_graph )
    {
        tptr = (TRACE_DATA *) &gptr->traces[0]; 
        show_nodata_label( gptr , tptr->trace_info.lid, tptr->trace_info.name ); 
        return;

    }

    /* ******************************************************** */
    /* Check and Display scale to categories for current graph  */
    /* ******************************************************** */

    DisplayHgvars = display_floodcat(); 

    /* update all radio buttons' mode for scale and batch scale in the menu */
    for(i = 0; i < 3; i++ )
    {
        if(i == ginfo->showcat)
        {
            if((widget = XtNameToWidget(TSscaleMO, scaleButtonNames[i])) != NULL)
            {
                XtVaSetValues(widget, XmNset, TRUE, NULL);
            }

            if((widget = XtNameToWidget(TSBatchScaleMO, batchButtonNames[i])) != NULL)
            {
                XtVaSetValues(widget, XmNset, TRUE, NULL);
            }
        }
        else
        {
            if((widget = XtNameToWidget(TSscaleMO, scaleButtonNames[i])) != NULL)
            {
                XtVaSetValues(widget, XmNset, FALSE, NULL);
            }

            if((widget = XtNameToWidget(TSBatchScaleMO, batchButtonNames[i])) != NULL)
            {
                XtVaSetValues(widget, XmNset, FALSE, NULL);
            }
        }

    }
    
    /* update toggle button "latest forecast only" in the menu */
    if(ginfo->latestfcstonly == 1)
        XtVaSetValues(TSShowFcstTB, XmNset, TRUE, NULL);
    else
        XtVaSetValues(TSShowFcstTB, XmNset, FALSE, NULL);


    /* ******************************************************** */
    /* If "Show PC as PP" selected then add derived PP trace to */
    /* current graph dynamically.                               */
    /* ******************************************************** */

    if ( ginfo->showpp == 1 )
    {
       for (n = 0; n < gptr->num_traces; n++)
       {
           if ( gptr->trace_on[n] )
           {
                tptr = (TRACE_DATA *) &gptr->traces[n]; 
               if ( strncmp(tptr->trace_info.pe,"PC",2) == 0 && tptr->npts > 1)
               {
                   if ( ginfo->derivepp == INTERPOLATE) { interp_mode  = 1; distrib_mode = 0;}
                   if ( ginfo->derivepp == ASSIGN)      { interp_mode  = 0; distrib_mode = 1;}

                   npts = ts_normalize ( tptr, pcAsppPtr, interval_secs, interp_mode);
                   pcAsppPtr->npts = npts;
                   npts = ts_accum_to_inc3 ( pcAsppPtr , distrib_mode);

                   strcpy (pcAsppPtr->trace_info.name,tptr->trace_info.name);
                   strcpy (pcAsppPtr->trace_info.lid,tptr->trace_info.lid);
                   strcpy (pcAsppPtr->trace_info.pe,"PP");
                   strcpy (pcAsppPtr->trace_info.ts,tptr->trace_info.ts);
                   strcpy (pcAsppPtr->trace_info.extremum,tptr->trace_info.extremum);
                   pcAsppPtr->trace_info.name[0]=pcAsppPtr->trace_info.name[1]='P';

                   pcAsppPtr->npts = npts;

                   add_TStrace_data( gptr, pcAsppPtr );
                   DerivedPP   = 1; 
              }
          }
       }
    }

    
    /* Start display all traces in a selected graph */
    
    tptr = (TRACE_DATA *) &gptr->traces[0]; 

    getShefPe( tptr->trace_info.pe, (char *)&buf);

    draw_xaxis( gptr );
    draw_yaxis( gptr );
    draw_Unitlabel( gptr , buf );

    for (n = 0; n < gptr->num_traces; n++)
    {
    	if ( gptr->trace_on[n] == 1 && gptr->trace_valid[n] == 1)
       	{
            tptr = (TRACE_DATA *) &gptr->traces[n]; 

            trace_valid = 1;
            PageMgr->active_trace = n;

            if ( PageMgr->Edit_active == EDIT_ACTIVE  && n ==  PageMgr->edit_trace &&
                active_graph == PageMgr->edit_graph)
            {
                tptr->trace_color = TScolors[EDIT_COLOR].fg_color;
            }
            else if (GroupInfo->GroupSelected == STATION)
            {
                tptr->trace_color = TScolors[n].fg_color;
            }
            else if (  GroupInfo->GroupSelected == GROUP )
            {
                if ( strlen(tptr->trace_info.color_name) > 0)
				{
                    tptr->trace_color = getMyColor ( PageMgr->display, tptr->trace_info.color_name);
				}
                else
                {
                    color_index = pickMyColor( ginfo, n );
                    tptr->trace_color =  TScolors[color_index].fg_color;
                }
            }

            if ( strncmp(tptr->trace_info.pe,"PP",2) == 0)
            {
                if ( ginfo->showpp == 1 && DerivedPP == 1)
                     display_TStracePcAsPp ( PageMgr->active_trace );
                else
                     display_TStracePp ( PageMgr->active_trace );
            }
            else
                display_TStraceGen ( PageMgr->active_trace );

            /* ******************************************************* */
            /* Only show details of  max/min for single trace in graph */
            /* otherwise show just Lid-PeDTsEP if more than one traces */
            /* in selected graph.                                      */
            /* ******************************************************* */

            if ( gptr->num_traces == 1)
                display_trace_minmax ( tptr );
            else
            {
                 display_trace_name ( tptr , trace_cnt, DerivedPP , trace_valid);
                trace_cnt++;
            }
       	}
   	}

    /* Offset num_traces in current graph by one because derived */
    /* PP trace needs to be deleted from previous  graph -       */
    /* Re-display PC trace one more time to make PC trace on top */

    if ( DerivedPP )
    {
        if ( ginfo->showpp == 1 )
        {
            PageMgr->active_trace = 0;
            display_TStraceGen ( PageMgr->active_trace );
        }
        gptr->num_traces = gptr->num_traces - 1;
    }


    /* Show labels for all invalid traces in current graph */

    for (n = 0; n < gptr->num_traces; n++)
    {
        tptr = (TRACE_DATA *) &gptr->traces[n]; 

        if ( gptr->trace_valid[n] == 0)
        {
            trace_valid = 0; 
            trace_cnt++;
            tptr->trace_color = TScolors[24].fg_color;
            display_trace_name ( tptr , trace_cnt, DerivedPP , trace_valid);
        }
    }


    /* ************************ */
    /* Display Flood Categories */
    /* ************************ */

    if ( DisplayHgvars )
        display_TShgvars ( gptr );

   	/*PageMgr->zoom = 0;*/

  	return;

}

/* ****************************************** */
/*      Display trace data in general         */
/* ****************************************** */
void display_TStraceGen ( int active_trace )
{

    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

    int    n,
        k;

    XPoint     p[MAX_POINTS];



    PAGE_DATA    *pptr = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];
    TRACE_DATA    *tptr = (TRACE_DATA *) &gptr->traces[PageMgr->active_trace];


    XSetForeground(PageMgr->display, PageMgr->gc_line, tptr->trace_color);

    k = 0;


    for ( n = 0; n < tptr->npts; n++)
    {

        if (tptr->TSdata[n].mode == DELETE || tptr->TSdata[n].mode == SETMISSING) continue;

        if (tptr->TSdata[n].x >= gptr->xmin && tptr->TSdata[n].x <= gptr->xmax &&
            tptr->TSdata[n].y >= gptr->ymin && tptr->TSdata[n].y <= gptr->ymax)
        {

            p[k].x = x2pixel(gptr, tptr->TSdata[n].x);
            p[k].y = y2pixel(gptr, tptr->TSdata[n].y);
            k++; 

        }
    }

    if ( GroupInfo->trace_mode == 1 || GroupInfo->trace_mode == 2)
    {

        XDrawLines(PageMgr->display, PageMgr->window, PageMgr->gc_line, p, k, CoordModeOrigin);

    }
    if ( GroupInfo->trace_mode == 0 || GroupInfo->trace_mode == 2 || k == 1)
    {
            for (n = 0; n < k; n++)
            XFillArc(PageMgr->display, PageMgr->window, PageMgr->gc_point, p[n].x-2, p[n].y-2, 4, 4, 0, 360*64);

            for (n = 0; n < k; n++)
            XDrawArc(PageMgr->display, PageMgr->window, PageMgr->gc_line, p[n].x-2, p[n].y-2, 4, 4,  0, 360*64);
    }

    return;

}

/* ****************************************** */
/* Display trace data for precip data only    */
/* ****************************************** */
void  display_TStracePp( int active_trace )
{
    extern PAGE_MGR       *PageMgr;

    int    n, k, x2,x1;

    int     barWidth,
        barHeight;

    XPoint     p[MAX_POINTS];

    PAGE_DATA    *pptr = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];
    TRACE_DATA    *tptr = (TRACE_DATA *) &gptr->traces[PageMgr->active_trace];


    /* ******************************** */
    /* Red when when display regular PP */
    /* else when display Forcast PP     */
    /* ******************************** */

    XSetForeground(PageMgr->display, PageMgr->gc_line, tptr->trace_color);

    PageMgr->pcAspp_intervals = 3600;

    x1 = tptr->TSdata[0].x;
    x2 = tptr->TSdata[0].x + PageMgr->pcAspp_intervals;

    x1 = x2pixel ( gptr, x1);
    x2 = x2pixel ( gptr, x2);

    barWidth  = (x2 - x1);

    k = 0;

    for ( n = 0; n < tptr->npts; n++)
    {

        if (tptr->TSdata[n].mode == DELETE || tptr->TSdata[n].mode == SETMISSING) continue;

        if (tptr->TSdata[n].x >= (gptr->xmin + 3600) && tptr->TSdata[n].x <= gptr->xmax &&
            tptr->TSdata[n].y >= gptr->ymin && tptr->TSdata[n].y <= gptr->ymax)
        {

            p[k].x = x2pixel( gptr, tptr->TSdata[n].x);
            p[k].y = y2pixel( gptr, tptr->TSdata[n].y);
            k++;

        }
    }

    for (n = 0; n < k; n++) 
    {

        barHeight = (gptr->h + gptr->y) - p[n].y;
        if ( barHeight < 1) 
             barHeight = 2;

        if ( barWidth < 1) 
             barWidth  = 1;

        XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_line,  p[n].x - barWidth, p[n].y, 
                                        barWidth,  barHeight);
    }

 XFlush(PageMgr->display);

}



/* *********************************************** */
/* Display precip trace data that derived from PC  */
/* *********************************************** */
void display_TStracePcAsPp( int active_trace )
{
    extern PAGE_MGR       *PageMgr;

    int    n, k, x2,x1;

    int     barWidth,
        barHeight;

    float    ymax, 
        ymin,
        newymax,
        newymin,
        yinc;

    XPoint     p[MAX_POINTS];

    PAGE_DATA    *pptr = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];
    TRACE_DATA    *tptr = (TRACE_DATA *) &gptr->traces[active_trace];


    SetColor(PageMgr->gc_line, TSDrawArea, "Yellow");

    PageMgr->pcAspp_intervals = 3600;

    x1 = tptr->TSdata[0].x;
    x2 = tptr->TSdata[0].x + PageMgr->pcAspp_intervals;

    x1 =  x2pixel ( gptr, x1 );
    x2 =  x2pixel ( gptr, x2 );


    barWidth  = (x2 - x1);

    k = 0;

    ymin = ymax = 0.0;

    for ( n = 0; n < tptr->npts; n++)
        if ( tptr->TSdata[n].y > ymax ) 
            ymax = tptr->TSdata[n].y;

    adjust_pcymax(ymin, ymax, &newymin, &newymax, &yinc);

    for ( n = 0; n < tptr->npts; n++)
    {

        if (tptr->TSdata[n].mode == DELETE || tptr->TSdata[n].mode == SETMISSING) continue;

        if (tptr->TSdata[n].x >= (gptr->xmin + 3600 ) && tptr->TSdata[n].x <= gptr->xmax &&
            tptr->TSdata[n].y >= newymin && tptr->TSdata[n].y <= newymax && tptr->TSdata[n].y != MISSING)
        {

            p[k].x = x2pixel(  gptr, tptr->TSdata[n].x);
             p[k].y =  gptr->h - (gptr->h * (tptr->TSdata[n].y - newymin))/(newymax - newymin) + gptr->y;
            k++;
        }
    }

    for (n = 0; n < k; n++) 
    {

        barHeight = (gptr->h + gptr->y) - p[n].y;
        if ( barHeight < 1) 
             barHeight = 2;

        if ( barWidth < 1) 
             barWidth  = 1;

        XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_line,  p[n].x - barWidth, p[n].y, 
                                        barWidth,  barHeight);
    }

    draw_pcAspp_yaxis( gptr , newymin, newymax, yinc);

     XFlush(PageMgr->display);

}


/* *********************************************** */
/* Update page demension when re-size detected     */
/* *********************************************** */
void update_pageDimesion( int page )
{
    extern PAGE_MGR       *PageMgr;


    PAGE_INFO   *Pinfo;
    PAGE_DATA   *Pdata;
    GRAPH_DATA  *Gdata;


    int     n, ypos;

    Pdata = (PAGE_DATA * )&TSpagedata[0];
    Pinfo = (PAGE_INFO * )&TSpageinfo[page];

    for (n = 0; n < Pinfo->num_graphs; n++)
    {

        Gdata  = (GRAPH_DATA *) &Pdata->graph[n]; 
    
        PageMgr->page_width = getWidgetWidth(TSDrawArea);
        PageMgr->page_height= getWidgetHeight(TSDrawArea);

        XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, 0, 0, 
                         PageMgr->page_width, PageMgr->page_height);


        Gdata->y = 0;
        ypos = Pinfo->graph[n].graph_pos;
        if ( ypos >= 7)
        {
            ypos = ypos - 6;
            Gdata->y = (PageMgr->page_height/2.0);
        }

             Gdata->x = ((ypos-1)*PageMgr->page_width /6.0);

             Gdata->w = ((Pinfo->graph[n].xsize )*PageMgr->page_width /6.0);
             Gdata->h = ((Pinfo->graph[n].ysize )*PageMgr->page_height/2.0);

        Gdata->x = Gdata->x + Gdata->w * PER_CENT_XOFFSET;
        Gdata->y = Gdata->y + Gdata->h * PER_CENT_YOFFSET;
             Gdata->w = Gdata->w * PER_CENT_WIDTH;
             Gdata->h = Gdata->h * PER_CENT_HEIGHT;
    }

    return;

}

/* *********************************************** */
/* Update Graph when zoom is activated             */
/* *********************************************** */
void update_graph(int x1,int y1,int x2,int y2)
{

    extern PAGE_MGR       *PageMgr;

    float    tmp_ymin, tmp_ymax, prev_ymin;
    float    ymin, ymax, ydiff;

    /**
	  * Bug: the data display mess up when the date interval
	  *      is long enough such as more than one month. 
	  *
	  * Reason:
	  * The output of tmpe_xmax is less than the tmp_xmin
	  * when the running date interval is large
	  * because the calculation for tmp_xmax cause data overflow
	  *
	  * Solution:
	  * 1) change the tmp_xmin and tmp_xmax to double type
	  * 2) change the operation order in the formula for
	  *    calculation of tmp_xmin and tmp_xmax to avoid
	  *    overflow
	  * 3) cast back to time_t 
	  *
	  * by guoxian zhou 10-2005
	  **/

/*
    time_t    tmp_xmin, tmp_xmax;
*/
    double    tmp_xmin, tmp_xmax;

    PAGE_DATA  *pptr  = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA *gptr  = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

    /*  *************************************************************** */
    /*  Calculate new mmax min for x_axis and y_axis  when in zoom mode */
    /*  *************************************************************** */

       prev_ymin = gptr->ymin;
/*
       tmp_xmin = gptr->xmin + (x1 - gptr->x)*(gptr->xmax - gptr->xmin)/gptr->w;
*/
       tmp_xmin = (double)gptr->xmin + 
                  (double)(x1 - gptr->x) / gptr->w * (gptr->xmax - gptr->xmin);

       tmp_ymax = gptr->ymin + (gptr->h + gptr->y - y1)*(gptr->ymax - gptr->ymin)/gptr->h; 

/*
       tmp_xmax = gptr->xmin + (x2 - gptr->x)*(gptr->xmax - gptr->xmin)/gptr->w;
*/
       tmp_xmax = (double)gptr->xmin +
                  (double)(x2 - gptr->x) / gptr->w * (gptr->xmax - gptr->xmin);

       tmp_ymin = gptr->ymin + (gptr->h + gptr->y - y2)*(gptr->ymax - gptr->ymin)/gptr->h; 

/*
       gptr->xmin = (tmp_xmin/3600)*3600;
       gptr->xmax = (tmp_xmax/3600)*3600;
*/
       gptr->xmin = ((time_t)tmp_xmin/3600)*3600;
       gptr->xmax = ((time_t)tmp_xmax/3600)*3600;

       gptr->ymin = tmp_ymin;
       gptr->ymax = tmp_ymax;

       if ( gptr->ymin < prev_ymin)
            gptr->ymin = prev_ymin;

       ymin = gptr->ymin; ymax = gptr->ymax;

       ydiff = (ymax - ymin);
       if ( ydiff >= 1.0)
           adjust_ymaxmin(ymin, ymax, &gptr->ymin, &gptr->ymax, &gptr->data_inc);
       else
       {
        gptr->data_inc = ydiff/5.0;
       }

       gptr->old_data_inc = gptr->data_inc;
       gptr->old_xmin = gptr->xmin;
       gptr->old_xmax = gptr->xmax;
       gptr->old_ymin = gptr->ymin;
       gptr->old_ymax = gptr->ymax;

       display_TSgraph_data( PageMgr->active_graph );

}

/* ******************************************************** */
/* Draw crosshairs when crosshairs when button press within */
/* graph                                                    */
/* ******************************************************** */
void draw_crosshairs()
{
    extern PAGE_MGR       *PageMgr;


    PAGE_DATA  *pptr  = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA *gptr  = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

    if ( PageMgr->Editmode ) return;

    XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, PageMgr->last_x, gptr->y,
                                PageMgr->last_x, gptr->y + gptr->h);
    XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, gptr->x, PageMgr->last_y,
                                gptr->x + gptr->w, PageMgr->last_y);

    return;
}

/* ******************************************************** */
/* Draw rubber band when zoom mode is active                */
/* ******************************************************** */
void draw_rubber_band()
{
    extern PAGE_MGR       *PageMgr;

    XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, PageMgr->start_x, PageMgr->start_y,
                                PageMgr->start_x, PageMgr->last_y);
    XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, PageMgr->start_x, PageMgr->last_y,
                                PageMgr->last_x,  PageMgr->last_y);
    XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, PageMgr->last_x,  PageMgr->last_y,
                                PageMgr->last_x,  PageMgr->start_y);
    XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, PageMgr->last_x,  PageMgr->start_y,
                                PageMgr->start_x, PageMgr->start_y);

    return;

}


/* ******************************************************** */
/* get Time Series selected Page Data                       */
/* ******************************************************** */
void get_TSpage_data ( int page )
{
    extern  PAGE_DATA  TSpagedata[MAX_PAGE_DATA];
    extern  PAGE_INFO  TSpageinfo[MAX_PAGE_INFO];

    extern PAGE_MGR       *PageMgr;

    PAGE_INFO  *Pinfo = (PAGE_INFO *) &TSpageinfo[page];
    PAGE_DATA  *Pdata = (PAGE_DATA *) &TSpagedata[0];

    GRAPH_INFO *Ginfo; 
    GRAPH_DATA *Gdata; 

    int    n;

    PageMgr->Editsave    = 0;

    /* *********************************************** */
    /* In edit mode only show traces selected by users */
    /* *********************************************** */

    for ( n = 0; n < Pinfo->num_graphs; n++)
    {

        Ginfo = (GRAPH_INFO *) &Pinfo->graph[n];
        Gdata = (GRAPH_DATA *) &Pdata->graph[n];

        get_TSgraph_data( Ginfo, Gdata );

        find_TSgraph_minmax( Gdata , n, 1);
    }

    Pdata->num_graphs = Pinfo->num_graphs;

}

/* *********************************************** */
/*  find max and mix of the graph based on the     */
/* data in trace/traces of the graph               */
/* *********************************************** */
void find_TSgraph_minmax ( GRAPH_DATA *graph , int ngraph, int chk_fcst)
{

    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

    PAGE_INFO       *pinfo = (PAGE_INFO *)  &TSpageinfo[GroupInfo->current_page];
    GRAPH_INFO      *ginfo = (GRAPH_INFO *) &pinfo->graph[ngraph];

    int         n,
            first_time,
            hasPP,
            hasQR;

    float        ymin=0.0, 
            ymax=0.0;

    TRACE_DATA     *tptr;

    /* ************************************ */
    /* Get min max from traces within graph */
    /* Check if pe = PP then force ymin = 0 */
    /* ************************************ */

    first_time = 1;
    hasPP      = 0;
    hasQR       = 0;

    /* ************************************************ */
    /* Count number of valid traces in current graph   */
    /* ************************************************ */

    /* ******************************** */
    /*       Check for valid traces     */
    /* ******************************** */


    static char    temp_ts[SHEF_TS_LEN + 1] ;
    static char    temp_extremum[SHEF_EX_LEN + 1];
    
    strcpy(temp_ts, "");
    strcpy(temp_extremum, "");
    
    
    for (n = 0; n < graph->num_traces; n++)
    {
        tptr = (TRACE_DATA *) &graph->traces[n];

        if ( ! graph->trace_valid[n] ) 
        {
             graph->trace_on[n] = 0;
        }
         else if( chk_fcst == 1 && tptr->trace_info.isForecast == 1)
         {        

            if ( ginfo->latestfcstonly == 0 ) 
            {
                graph->trace_on [n] = 1;
            }
            else 
            {
                /* need compare the ts and extremum values here
                 * to determin if need display this trace data.
                 * Added by guoxian zhou 05-2004
                 */
                 
                if( strcmp(temp_ts, tptr->trace_info.ts) != 0 ||
                    strcmp(temp_extremum, tptr->trace_info.extremum) != 0  )
                {
                    graph->trace_on [n] = 1;
                    strcpy(temp_ts, tptr->trace_info.ts);
                    strcpy(temp_extremum, tptr->trace_info.extremum);
                }
                else
                    graph->trace_on [n] = 0;
            }

         }
    }


    /* ********************************************************* */
    /* Edit Mode - show only traces were on when done editing    */
    /* ********************************************************* */
    if ( PageMgr->edit_graph == ngraph  && ginfo->latestfcstonly == 2)
    {
        for (n = 0; n < graph->num_traces; n++)
        {
            if ( graph->trace_on[n]  == 1 && PageMgr->traces_on_edit[n] == 0)
                graph->trace_on[n] = 0;

        }
    }

    for(n =0; n < graph->num_traces; n++) 
    {
        if (  graph->trace_on[n] )
        {

            tptr = (TRACE_DATA *) &graph->traces[n];
            if ( strncmp( "PP", tptr->trace_info.pe, 2) == 0) hasPP = 1;
            if ( strncmp( "QR", tptr->trace_info.pe, 2) == 0) hasQR = 1;
            if ( first_time )
            {
                ymin = tptr->ymin;
                ymax = tptr->ymax;
                first_time = 0;
            }

            if ( tptr->ymin < ymin) ymin = tptr->ymin;
            if ( tptr->ymax > ymax) ymax = tptr->ymax;
        }
    }

    if ( hasQR == 1 && fabs(ymax-ymin ) <= 10)
    {
        graph->ymin = ymin - 5.0;
        graph->ymax = ymax + 5.0;
        graph->data_inc = 2.0;
    
    }
    else
        adjust_ymaxmin(ymin, ymax, &graph->ymin, &graph->ymax, &graph->data_inc);

    graph->xmin =  PageMgr->BeginTime;
    graph->xmax =  PageMgr->EndTime;

    /* ********************************************************** */
    /* Store  original graph's max and min for Zoom_Reset purpose */
    /* ********************************************************** */

    graph->org_xmin = graph->xmin;
    graph->org_xmax = graph->xmax;
    graph->org_ymin = graph->ymin;
    graph->org_ymax = graph->ymax;
    graph->org_data_inc = graph->data_inc;

    graph->old_xmin = graph->xmin;
    graph->old_xmax = graph->xmax;
    graph->old_ymin = graph->ymin;
    graph->old_ymax = graph->ymax;
    graph->old_data_inc = graph->data_inc;

    if ( hasPP )  
        graph->ymin = 0;


    /*
    printf("find ... min max %f %f ymin ymax %f %f\n",graph->ymin, graph->ymax, ymin, ymax); 
    */
    return;

}

/* ------------------------------------------------------- */
void get_TSgraph_data ( GRAPH_INFO *Ginfo, GRAPH_DATA *Gdata )
{
    int     n,
        trace_valid,
        trace_on,
        status;

    TRACE_DATA     *Tdata;
    TRACE_INFO     *Tinfo;

    extern  GROUP_INFO    *GroupInfo;
    
    static int first = 0;

    /* ************************************** */
    /* Get all the pre-defined observe traces */
    /* ************************************** */

    Gdata->num_traces      = 0;

    if ( Ginfo->latestfcstonly != 2  && GroupInfo->GroupSelected == STATION) 
    {
        Ginfo->latestfcstonly = 1;

    }
   
    if ((GroupInfo->GroupSelected == STATION))
    {
       if (first == 0)
       {
          first = 1 ;
	  Ginfo->showpp = 1; /* Set PC to PP to ON. */
          Ginfo->derivepp = INTERPOLATE; /* Default as interpolation mode */
       }
       /* DR_18821, to prevent the problem of only showing categories for the
        first graph, comment Ginfo->showcat here and move it outside */

       Ginfo->showcat = get_default_showcat() ; /*get the default showcat value*/
    }

    for(n =0; n < Ginfo->num_traces; n++) 
    {

        Tinfo  = (TRACE_INFO *) &Ginfo->traces[n];
        Tdata  = (TRACE_DATA *) &Gdata->traces[Gdata->num_traces];

        /* ************************************************ */
        /* Copy information from trace info into trace data */
        /* ************************************************ */

        memcpy((char *)&Tdata->trace_info,(char *)Tinfo,sizeof(TRACE_INFO));

        if( ! Tdata->trace_info.isForecast )
        {
            status = get_TSobstrace ( Tdata );

            trace_valid = trace_on = 0;

            if ( status ) 
            {
                trace_valid = trace_on = 1;
            }

            add_TStrace_data(Gdata, Tdata);
            Gdata->trace_on[Gdata->num_traces-1]    = trace_on;
            Gdata->trace_valid[Gdata->num_traces-1] = trace_valid;

        }
        else
        {
            status = get_Allfcst_data ( Gdata, Tdata);
        }

    }

    /* **************************************** */
    /* Loading rating table if pe is HG/HT/QR   */
    /* if pe = QR then stage     = f(discharge) */
    /* if pe = HG then discharge = f(stage)     */
    /* if pe = HT then discharge = f(stage)     */
    /* **************************************** */

    Gdata->hdataPtr.valid = 0;
    Gdata->rdataPtr.valid = 0;
    Gdata->rdataPtr.StageOrDischarge = STAGE;

    for(n =0; n < Ginfo->num_traces; n++) 
    {

        Tdata  = (TRACE_DATA *) &Gdata->traces[n];

        if ( strncmp("Q",Tdata->trace_info.name,1) == 0)
            Gdata->rdataPtr.StageOrDischarge = DISCHARGE;

                init_TSinit_gdata( Gdata );
            if ( strncmp("H",Tdata->trace_info.name,1) == 0 ||
             strncmp("Q",Tdata->trace_info.name,1) == 0)
        {
            Tdata = (TRACE_DATA *) &Gdata->traces[n];
            load_TSrating ( Gdata , Tdata->trace_info.lid);
            load_TShgvars ( Gdata , Tdata->trace_info.lid);
            break;
        }

    }
}

/* ********************************************************* */
/*         Get Forecast data for a selected graph            */
/* ********************************************************* */
int  get_Allfcst_data ( GRAPH_DATA *gptr, TRACE_DATA *tptr)
{

    int     n, 
        nfcst_traces;

    TRACE_DATA *ftrace = NULL;
    TRACE_DATA *tmptrace = NULL;

    printf ( "sizeof TRACE_DATA: %d sizeof GRAPH_DATA: %d\n",
             sizeof ( TRACE_DATA ), sizeof ( GRAPH_DATA ) );

    printf ( "MAX_FCST_TRACES: %d\n", MAX_FCST_TRACES );

    /* Allocate space for the ftrace array. */
    ftrace = ( TRACE_DATA * ) malloc ( sizeof ( TRACE_DATA ) * 
                                       MAX_FCST_TRACES );

    if ( ftrace == NULL )
    {
       fprintf ( stderr, "Could not allocate memory for the ftrace array.\n" );
       return 0;
    }

    nfcst_traces = GetForecast_data( ftrace , tptr);

    if ( nfcst_traces > MAX_FCST_TRACES)
    {
        printf("Number of Forcast traces exceed MAX_FCST_TRACES...\n");
        free ( ftrace );
        ftrace = NULL;
        return ( 0 );
    }


    /* *********************************** */ 
    /* There is no forcast data found then */
    /* Add into current graph anyway -     */
    /* Later to display as invalid trace   */
    /* *********************************** */ 
    
    if ( nfcst_traces == 0 )
    {
        add_TStrace_data(gptr, tptr);
        gptr->trace_on[gptr->num_traces - 1]    = 0;
        gptr->trace_valid[gptr->num_traces - 1] = 0;

    }

     /* **************************************** */
    /* Add all valid Forcast traces to graph    */
     /* **************************************** */
    for ( n = 0; n < nfcst_traces; n ++)
    {
        tmptrace = (TRACE_DATA *)&ftrace[n];
        if ( n > 0 ) strcpy ( tmptrace->trace_info.color_name,"");
        add_TStrace_data(gptr, tmptrace);
    }

    free ( ftrace );
    ftrace = NULL;
            
    return ( nfcst_traces );
}

/* *************************************** */
/* Get Observed trace data                 */
/* *************************************** */
int  get_TSobstrace ( TRACE_DATA *tptr )
{

    int    ok;

    ok = GetObserve_data( tptr );

    if ( ! ok )
        return ( NO_DATA );

    return ( 1 );
}



/* *************************************** */
/* Adjust the min/max for PC data          */
/* *************************************** */
void adjust_pcymax( float minval, float maxval, 
            float *newmin,  float *newmax, float *dinc)
{
    float dminmax;
    dminmax = (maxval - minval);

    if ( dminmax <= 0.5)
    {

        minval = 0.0;
        maxval = (int)(maxval) + 0.5;
        *dinc  = 0.1;
    } 
    else if ( dminmax <= 2.0)
    {

        minval = (int)(minval);
        maxval = (int)(maxval) + 1.0;
        *dinc =  0.5;

    }
    else
    {

        minval = (int)(minval);
        maxval = (int)(maxval) + 1.0;
        *dinc =  1.0;
    }

     *newmin = minval;
     *newmax = maxval;

    return;

}


/* ******************************************* */
/* Adjust the the min/max of y-axis in general */
/* ******************************************* */
void adjust_ymaxmin(    float minval,
            float maxval,
            float *newmin,
            float *newmax, 
            float *dinc)
{
    float     dminmax;
    int     n, j;


    dminmax = (maxval - minval);

    if ( dminmax <= 0.1 ) 
        n = 0;
    else
        n = log10 ( fabs(dminmax) );

    j = pow(10,n); 

    if ( minval < 0 )
        minval = (int)(minval/j)*j - j;
    else
        minval = (int)(minval/j)*j;

    maxval = (int)(maxval/j + 1)*j;
        
    dminmax = (maxval - minval);

    if ( dminmax >= j*1.0) *dinc = 0.2*j;
    if ( dminmax >= j*2.0) *dinc = 0.5*j;
    if ( dminmax >= j*4.0) *dinc = 1.0*j;
    if ( dminmax >= j*8.0) *dinc = 2.0*j;

     *newmin = minval;
     *newmax = maxval;

    return;
}



/* ******************************************* */
/* Load flood categories data from flood_cat   */
/* and riverstat tables in to graph data       */
/* based on selected lid.                      */
/* ******************************************* */
void load_TShgvars ( GRAPH_DATA *graph , char *lid)
{

    Riverstat      *rHead,
               *rPtr;

    Floodcat       *fHead,
             *fPtr;

    TRACE_DATA    *tptr;

    char           where[BUFSIZ],
            buf[10];

    float        tmpmax, tmpmin;

    memset(buf,'\0',10);
    strcat(buf,lid); 
    
    sprintf(where, " WHERE lid = '%s' ", buf);

    /* ********************************** */
    /* Inintialize all to MISSING         */
    /* ********************************** */

    graph->hdataPtr.flood_discharge  = MISSING;
    graph->hdataPtr.action_flow      = MISSING;

    graph->hdataPtr.flood_stage  = MISSING;
    graph->hdataPtr.action_stage = MISSING;
    graph->hdataPtr.major        =  MISSING;
    graph->hdataPtr.moderate     =  MISSING;
    graph->hdataPtr.minor        =  MISSING;

    tptr = (TRACE_DATA *) &graph->traces[0];
    strcpy(graph->hdataPtr.current_pe, tptr->trace_info.name);
    
    rHead = GetRiverstat(where);

    if (rHead)
    {

        rPtr = (Riverstat *) ListFirst(&rHead->list);    

        if (rPtr) 
        {
            if( ! IsNull ( FLOAT, &rPtr->fq) )
                graph->hdataPtr.flood_discharge  = rPtr->fq;

            if( ! IsNull ( FLOAT, &rPtr->action_flow) )
                graph->hdataPtr.action_flow  = rPtr->action_flow;

            if( ! IsNull ( FLOAT, &rPtr->fs) )
                graph->hdataPtr.flood_stage  = rPtr->fs;

            if( ! IsNull ( FLOAT, &rPtr->wstg) )
                graph->hdataPtr.action_stage = rPtr->wstg;
        }

        FreeRiverstat(rHead);
    }


     fHead = GetFloodcat(where);

     if (fHead)
     {
        fPtr = (Floodcat *) ListFirst(&fHead->list);

        if (fPtr)
        {
           if ( strncmp ("Q", tptr->trace_info.name, 1 ) == 0)
           {
            if(  ! IsNull ( FLOAT, &fPtr->major_flow) )
                graph->hdataPtr.major    = fPtr->major_flow;
            if(  ! IsNull ( FLOAT, &fPtr->moderate_flow) )
                graph->hdataPtr.moderate = fPtr->moderate_flow;
            if(  ! IsNull ( FLOAT, &fPtr->minor_flow) )
                graph->hdataPtr.minor    = fPtr->minor_flow;
           }
           else
           {
            if(  ! IsNull ( FLOAT, &fPtr->major_stage) )
                graph->hdataPtr.major    = fPtr->major_stage;
            if(  ! IsNull ( FLOAT, &fPtr->moderate_stage) )
                graph->hdataPtr.moderate = fPtr->moderate_stage;
            if(  ! IsNull ( FLOAT, &fPtr->minor_stage) )
                graph->hdataPtr.minor    = fPtr->minor_stage;
           }
        }

        FreeFloodcat(fHead);

     }
 
    /* *************************** */
    /* Check if data is valid data */
    /* *************************** */

    graph->hdataPtr.valid = 1;

    if ( graph->hdataPtr.major == MISSING && graph->hdataPtr.moderate == MISSING &&  
         graph->hdataPtr.minor == MISSING && graph->hdataPtr.action_stage ==  MISSING &&
         graph->hdataPtr.flood_stage == MISSING) 
    {
        graph->hdataPtr.valid = 0;
        return;
    }

    /* ************************************* */
    /* find Max and Min value of the catgory */
    /* ************************************* */

    tmpmax = 0.0;
    tmpmin = 10000.0;

    if (graph->hdataPtr.major != MISSING)
    {
        if (graph->hdataPtr.major >  tmpmax)    tmpmax = graph->hdataPtr.major;
        if (graph->hdataPtr.major <  tmpmin)    tmpmin = graph->hdataPtr.major;
    }
    if (graph->hdataPtr.moderate != MISSING)
    {
        if (graph->hdataPtr.moderate >  tmpmax)    tmpmax = graph->hdataPtr.moderate;
        if (graph->hdataPtr.moderate <  tmpmin)    tmpmin = graph->hdataPtr.moderate;
    }
    if (graph->hdataPtr.minor != MISSING)
    {
        if (graph->hdataPtr.minor    >  tmpmax)    tmpmax = graph->hdataPtr.minor;
        if (graph->hdataPtr.minor    <  tmpmin)    tmpmin = graph->hdataPtr.minor;
    }

    if (strncmp("Q", tptr->trace_info.name, 1 ) == 0)
    {
        if (graph->hdataPtr.action_flow != MISSING)
        {
            if (graph->hdataPtr.action_flow     > tmpmax) tmpmax = graph->hdataPtr.action_flow;
            if (graph->hdataPtr.action_flow     < tmpmin) tmpmin = graph->hdataPtr.action_flow;
        }
        if (graph->hdataPtr.flood_discharge != MISSING)
        {
            if (graph->hdataPtr.flood_discharge > tmpmax) tmpmax = graph->hdataPtr.flood_discharge;
            if (graph->hdataPtr.flood_discharge < tmpmin) tmpmin = graph->hdataPtr.flood_discharge;
        }
    }
    else
    {
        if (graph->hdataPtr.flood_stage != MISSING)
        {
            if (graph->hdataPtr.flood_stage  > tmpmax) tmpmax = graph->hdataPtr.flood_stage;
            if (graph->hdataPtr.flood_stage  < tmpmin) tmpmin = graph->hdataPtr.flood_stage;
        }
        if (graph->hdataPtr.action_stage != MISSING)
        {
            if (graph->hdataPtr.action_stage > tmpmax) tmpmax = graph->hdataPtr.action_stage;
            if (graph->hdataPtr.action_stage < tmpmin) tmpmin = graph->hdataPtr.action_stage;
        }
    }



    graph->hdataPtr.ymax = tmpmax;
    graph->hdataPtr.ymin = tmpmin;

     return;
}


/* ------------------------------------------------------- */
/* Display labels for selected traces                      */
/* ------------------------------------------------------- */
void setTraceLabel(int selected_graph )
{

    extern     GROUP_INFO  *GroupInfo;
    extern     PAGE_MGR    *PageMgr;

    extern  Widget TSTracesW[40] ;

     XmString     str = NULL ;
    char        name[80];
    char        buf[100];
      Arg        arg[5];
      int         ac, n;

    struct tm       *tm_ptr;

    static  int first = 1;

    PAGE_INFO       *pinfo = (PAGE_INFO *)  &TSpageinfo[GroupInfo->current_page];
    GRAPH_INFO      *ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];

    PAGE_DATA    *pptr  = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA   *gptr  = (GRAPH_DATA *) &pptr->graph[ selected_graph ];
    TRACE_DATA   *tptr;

    if ( gptr->num_traces <= 0) 
            return;

    if (first)
    {
        for (n =0; n < 40; n++)
            XtAddCallback(TSTracesW[n], XmNvalueChangedCallback, TStrace_onoff_CB, (XtPointer) n);
        first = 0;
    }

    static char    previous_ts[SHEF_TS_LEN + 1];
    static char    previous_extremum[SHEF_EX_LEN + 1];
    
    strcpy(previous_ts, " ");
    strcpy(previous_extremum, " ");

    for (n = 0; n < gptr->num_traces; n++)
    {
        tptr = (TRACE_DATA *) &gptr->traces[n];

        if ( tptr->trace_info.dur == 0)
            sprintf(name,"%s %s %s",tptr->trace_info.pe,tptr->trace_info.ts,tptr->trace_info.extremum);
         else
            sprintf(name,"%s %d %s %s",tptr->trace_info.pe,tptr->trace_info.dur,
                      tptr->trace_info.ts,tptr->trace_info.extremum);

        if ( tptr->trace_info.isForecast == 1 && gptr->trace_valid [n] == 1)
        {
            tm_ptr = gmtime(&tptr->basistime);
            strftime(buf, sizeof(buf), "  %m/%d %H:%MZ", tm_ptr);
            strcat (name, buf);

        }

        if ( gptr->trace_valid [n]  == 0) 
            strcat (name, "  No Data");

        ac = 0;
        XtManageChild ( TSTracesW[n] );
        str = XmStringCreateSimple(name);
        XtSetArg(arg[ac], XmNlabelString, str);  ac++;
        XtSetValues(TSTracesW[n], arg, ac);

        if ( gptr->trace_valid [n] == 0) 
        {
            gptr->trace_on [n] = 0;
            XmToggleButtonSetState(TSTracesW[n], False, False);
            continue;
        }

        if ( tptr->trace_info.isForecast)
        {
            if ( ginfo->latestfcstonly == 0 ) /* Show all forecast traces */
            {
                gptr->trace_on [n] = 1; 
                XmToggleButtonSetState(TSTracesW[n], True, False);
            }
            else if ( ginfo->latestfcstonly == 1 ) /* Show only lastest forecast traces */
            {
                /* ************************** */
                /* Show lastest forecast only */
                /* ************************** */

                /* need compare the ts and extremum values here
                 * to determin if need display this trace data.
                 * Added by guoxian zhou 05-2004
                 */
                 
                if( strcmp(previous_ts, tptr->trace_info.ts) != 0 ||
                    strcmp(previous_extremum, tptr->trace_info.extremum) != 0  )
                {
                     
                    gptr->trace_on [n] = 1; 
                    XmToggleButtonSetState(TSTracesW[n], True, False);
                    strcpy(previous_ts, tptr->trace_info.ts);
                    strcpy(previous_extremum, tptr->trace_info.extremum);
                }
                else
                {
                    gptr->trace_on [n] = 0; 
                    XmToggleButtonSetState(TSTracesW[n], False, False);
                }
                
            }
            else if ( PageMgr->edit_graph == selected_graph && ginfo->latestfcstonly == 2)
            {
                /* **************************************************** */
                /* In edit mode - only show traces by user's selection  */
                /* **************************************************** */

                if ( gptr->trace_on [n] == 1)
                    XmToggleButtonSetState(TSTracesW[n], True, False);
                else
                    XmToggleButtonSetState(TSTracesW[n], False, False);

            }
        }
        else
        XmToggleButtonSetState(TSTracesW[n], True, False);

    }

    for ( n = gptr->num_traces; n < 40; n ++) XtUnmanageChild ( TSTracesW[n] );

    if ( str )
        XmStringFree(str);


    return;


}


/* ******************************************************* */
/* Handle for toggling traces on/off by user               */
/* ******************************************************* */
void TStrace_onoff_CB( Widget w, XtPointer ptr, XtPointer cbs )
{
    extern     PAGE_MGR       *PageMgr;

    int     current_trace;


    XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;


    PAGE_DATA    *pptr = (PAGE_DATA  *) &TSpagedata[0];
    GRAPH_DATA   *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

    current_trace = (int) ptr;

    if( gptr->trace_valid [current_trace] )
    {

        gptr->trace_on[current_trace] = tb_state->set;

        find_TSgraph_minmax( gptr , PageMgr->active_graph, 0);
        display_TSgraph_data ( PageMgr->active_graph );
    }


}


/* ************************************************** */
/* Load the rating curve database rating table        */
/* ************************************************** */
void load_TSrating ( GRAPH_DATA *graph , char *rate_lid)
{

    Rating    *rateHead = NULL;
    int    count = 0;
    char    where[BUFSIZ];
    char    buf[20];

    graph->rdataPtr.valid = 0;

    memset(buf,'\0',20);
    strcat(buf,rate_lid);

    sprintf ( where, " WHERE lid = '%s' AND discharge IS NOT NULL ", buf);

    rateHead = GetRating(where);
    if (rateHead != NULL)
    {
        count = ListCount(&rateHead->list);

        if (count < 2)
            graph->rdataPtr.valid = 0;
        else
            graph->rdataPtr.valid = 1;
    }

    if ( rateHead )
    {
        FreeRating ( rateHead );
        rateHead = NULL;
    }

}

/* ************************************************************** */
/* Display all the lookup/derived values of discharge from stage  */
/* ************************************************************** */
void  display_DischargeFromStage( GRAPH_DATA *graph, int ypixel, float value, float max_discharge)
{
    extern     PAGE_MGR       *PageMgr;

    char    buf[40];

    if (  max_discharge >= 10000.0)
        sprintf(buf,"%.2f",value/1000.0);
    else
        sprintf(buf,"%.1f",value);

    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_grid, 
                    (graph->x + graph->w),      ypixel,
                                 (graph->x + graph->w + 5 ), ypixel);

    XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line,
                    (graph->x + graph->w + 7), 
                        (ypixel + 5),
                        buf, strlen(buf));

    if ( graph->display_flow_unit == 0) 
    {

        drawStageFlowLabel( graph , STAGE, max_discharge);
        graph->display_flow_unit = 1;
    }


}

/* *********************************************** */
/* get and return value of discharge from stage    */
/* *********************************************** */
float  get_DischargeFromStage( GRAPH_DATA *graph, float ystage)
{

    float    ydischrg = 0.0;
    
    /*Check to the database only if the PE is "H*".
     * Modified by guoxian zhou 06-2004
     */
    int i;

    for(i=0; i < graph->num_traces; i++)
    {
        if(graph->traces[i].trace_info.pe[0] == 'H')
        {
            ydischrg = stage2discharge(graph->traces[0].trace_info.lid, ystage);
            break;
        }
    }

        /* for some reason the discharge is less than zero then return zero */
    if ( (ydischrg < 0.0) && (ydischrg != RATING_CONVERT_FAILED) )
        return (0.0);
    else
        return ( ydischrg );

}


/* *********************************************** */
/* get and return value of stage from discharge    */
/* *********************************************** */
float get_StageFromDischarge( GRAPH_DATA *graph, float ydischarge)
{

    float    ystage = 0.0;
    /* Check to the database only if the PE of any traces is "Q*". 
     * Modified by guoxian zhou 06-2004
     */
    int i;
    
    for(i=0; i < graph->num_traces; i++)
    {
        if(graph->traces[i].trace_info.pe[0] == 'Q')
        {
            ystage = discharge2stage(graph->traces[0].trace_info.lid, ydischarge);
            break;
        }
    }

    return ( ystage );

}

/* ************************************************************** */
/* Display all the lookup/derived values of stages from discharge */
/* ************************************************************** */
void display_StageFromDischarge( GRAPH_DATA *graph, int ypixel, float ystage)
{

    extern     PAGE_MGR       *PageMgr;

    char    buf[40];

    sprintf(buf,"%.2f",ystage);
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_grid, 
                        (graph->x + graph->w), ypixel,
                                   (graph->x + graph->w + 5 ), ypixel);

     XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line,(graph->x + graph->w + 7), 
                          (ypixel + 5),
                           buf, strlen(buf));
    if ( graph->display_flow_unit == 0) 
    {

            drawStageFlowLabel( graph , DISCHARGE, 0.0);
            graph->display_flow_unit = 1;
    }

    return;
}


/* ************************************************************************* */
/* Display action stage flood_stage,     minor, moderate, major when PE = H* */
/* Display action flow, flood_discharge, minor, moderate, major when PE = Q* */
/* ************************************************************************* */
void display_TShgvars ( GRAPH_DATA *graph )
{

    extern     PAGE_MGR    *PageMgr;
    int    y;

    SetColor(PageMgr->gc_header, PageMgr->widget, "Yellow"); /* action stage   or action flow */

    if ( graph->hdataPtr.current_pe[0] == 'H')
        y = y2pixel ( graph,  graph->hdataPtr.action_stage );
    else
        y = y2pixel ( graph,  graph->hdataPtr.action_flow );

    if ( graph->hdataPtr.action_stage >= 0.0 && y <= (graph->y + graph->h) && y >= graph->y)
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_header, graph->x, y, (graph->x + graph->w), y);

    /* ********************************************* */
    /* set color for flood stage  or discharge stage */
    /* ********************************************* */
    SetColor(PageMgr->gc_header, PageMgr->widget, "Red");    

    if ( graph->hdataPtr.current_pe[0] == 'H')
        y = y2pixel ( graph,  graph->hdataPtr.flood_stage);
    else
        y = y2pixel ( graph,  graph->hdataPtr.flood_discharge);

    if ( graph->hdataPtr.flood_stage >= 0.0  && y <= (graph->y + graph->h) && y >= graph->y)
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_header, graph->x, y, (graph->x + graph->w), y);

    /* ************************* */
    /* set color for minor stage */
    /* ************************* */
    SetColor(PageMgr->gc_header, PageMgr->widget, "Red");   
    y = y2pixel ( graph,  graph->hdataPtr.minor );

    if ( graph->hdataPtr.minor >= 0.0 && y <= (graph->y + graph->h) && y >= graph->y)
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_header, graph->x, y, (graph->x + graph->w), y);

    /* **************************** */
    /* set color for moderate stage */
    /* **************************** */
    SetColor(PageMgr->gc_header, PageMgr->widget, "Blue");  
    y = y2pixel ( graph,  graph->hdataPtr.moderate);

    if ( graph->hdataPtr.moderate >= 0.0 && y <= (graph->y + graph->h) && y >= graph->y)
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_header, graph->x, y, (graph->x + graph->w), y);

    /* **************************** */
    /* set color for major    stage */
    /* **************************** */
    SetColor(PageMgr->gc_header, PageMgr->widget, "Purple");  
    y = y2pixel ( graph,  graph->hdataPtr.major);

    if ( graph->hdataPtr.major >= 0.0 && y <= (graph->y + graph->h) && y >= graph->y)
    XDrawLine(PageMgr->display,PageMgr->window, PageMgr->gc_header, graph->x, y, (graph->x + graph->w),  y);

    XFlush(PageMgr->display);
    return;

}


/* *****************************************  */
/* get and return a oage number by given lid  */
/* *****************************************  */
int find_page_fromlid ( char *lid )
{

    int npage, ngraph, ntrace;

    extern     GROUP_INFO     *GroupInfo;

    PAGE_INFO   *pinfo;
    GRAPH_INFO  *ginfo;
    TRACE_INFO  *tinfo;

    printf("Pages: %d %s\n",GroupInfo->page_count, lid);
    for ( npage = 0; npage < GroupInfo->page_count; npage++)
    {
        pinfo = (PAGE_INFO *) &TSpageinfo[npage];
        for ( ngraph = 0; ngraph < pinfo->num_graphs; ngraph++)
        {
            ginfo = (GRAPH_INFO *) &pinfo->graph[ngraph];
            for ( ntrace = 0; ntrace < ginfo->num_traces; ntrace++)
            {
                tinfo = (TRACE_INFO *) &ginfo->traces[ntrace];
                if ( strcmp ( lid, tinfo->lid ) == 0)
                            return ( npage );
            }
        }


    }

    return (-1);

}


/* *********************************************************** */
/* This function takes the input time series and fills an      */
/* output time series, with regular intervals and interpolated */
/* values when necessary. It does NOT change an accumulated    */
/* series to an incremental one.  It returns the number of     */
/* values in the output normalized time series that were filled*/
/* with non-missing data.                                      */
/* *********************************************************** */
int ts_do_normalize(TRACE_DATA     *TinPtr,
         TRACE_DATA     *ToutPtr,
         time_t     interval_secs,
         int         interp_mode)
{
   
   int      i;
   time_t    begin_time, end_time;

   extern      PAGE_MGR    *PageMgr;

   begin_time = PageMgr->BeginTime;
   end_time   = PageMgr->EndTime;
      
   /* *********************************************************  */
   /* define an accumulated value for each period.               */
   /* loop on the number of desired periods to fill. always      */
   /* fill a value with something, even if the value is missing. */
   /* *********************************************************  */
   
   for (i = 0; ((begin_time + (interval_secs * i) <= end_time)); i++)
   {
      ToutPtr->TSdata[i].x = begin_time + (interval_secs*i);
      
      ToutPtr->TSdata[i].y = ts_do_assign_value(TinPtr, ToutPtr->TSdata[i].x,
                         interval_secs, interp_mode);
      
   }
      
   
   return(i);
}


/* --------------------------------------------------------------- */
double ts_do_assign_value(TRACE_DATA     *TinPtr,
               time_t         norm_time,
               time_t         interval_secs,
               int         interp_mode)
{
   
   int         i;
   int         done;
   
   time_t     start_time, end_time;
   time_t    best_diff, time_diff;
   
   double       assigned_value;
   double    change, fraction;

   
   /* initialize */
   
   assigned_value = MISSING;
   done = 0;
   best_diff = 9999999;
   
   
   /* check for the case where the desired time is before the
       earliest time in the available data */
   
   if (norm_time < TinPtr->TSdata[0].x > norm_time)
   {
      return MISSING ;
   }
   
   
   /* loop on the number of available points */
   
   for (i = 0; i < TinPtr->npts && !done; i++)
   {
      
      /* if there is an exact match, use it */
      
      if ( TinPtr->TSdata[i].mode == DELETE  ||  TinPtr->TSdata[i].mode == SETMISSING) continue;

      if ( TinPtr->TSdata[i].x == norm_time)
      {
     assigned_value = TinPtr->TSdata[i].y;
     done = 1;
      }

      
      /* allow interpolation.  if ever used, this algorithm
     should br reviewed.  */
      
      else if (interp_mode && (i > 0) &&
           (TinPtr->TSdata[i].x     >= norm_time) &&
           (TinPtr->TSdata[i - 1].x <= norm_time)) 
      {
     start_time = TinPtr->TSdata[i - 1].x;
     end_time   = TinPtr->TSdata[i].x;
     time_diff  = end_time - start_time;
     
     if (time_diff == 0)
        fraction = 0.0;
     else
        fraction = (norm_time - start_time) / (double )time_diff;
     
     change = (TinPtr->TSdata[i].y - TinPtr->TSdata[i - 1].y);
          
     assigned_value = (change * fraction) + TinPtr->TSdata[i - 1].y;
     
     done = 1;
      } 

      
      /* if not interpolating, then allow use of values that are
     within a half-interval to be used, and use the one closest to 
     the desired time. */
      
      else if (!interp_mode)
      {
     time_diff = TinPtr->TSdata[i].x - norm_time;
     if (abs(time_diff) <= (interval_secs / 2))
     {             
        if (abs(time_diff) < abs(best_diff))
        {
           assigned_value = TinPtr->TSdata[i].y;
           best_diff = time_diff;
        }
        
        
        /* don't continue looping if the value being checked is
           at least a half-interval past the time being normalized */
        
        if (time_diff >= (interval_secs / 2))
           done = 1;
     }  
      }
   }
      
   
   return(assigned_value);
}



/* **************************************** */
/* convert real X value to pixel value      */
/* **************************************** */
int x2pixel( GRAPH_DATA *graph, time_t x) 
{

    float     xp;
    int    xpixel;

    xp = ((float)graph->w * (float)(x - graph->xmin))/(float)(graph->xmax - graph->xmin) + (float)graph->x;
    xpixel = (int) xp;

    return( xpixel);
}

/* **************************************** */
/* convert real Y value to pixel value      */
/* **************************************** */
int y2pixel( GRAPH_DATA *graph, float y) 
{
    float     yp;
    int     ypixel;

    yp = graph->h - ((float)graph->h * (y - graph->ymin))/(graph->ymax - graph->ymin) + graph->y;
    ypixel = (int) yp;

    return( ypixel);
}

/* **************************************** */
/* convert pixel value  to real Y value     */
/* **************************************** */
float pixel2y ( GRAPH_DATA *graph, int ypix )
{
    float yvalue;

    yvalue = graph->ymin + (float)(graph->h + graph->y - ypix)*(graph->ymax - graph->ymin)/(float)graph->h;

    return ( yvalue );

}

/* **************************************** */
/* convert pixel value  to real time value  */
/* **************************************** */
time_t pixel2x ( GRAPH_DATA *graph, int xpix )
{
    time_t xvalue;
    float  xtmp;

    xtmp = (float)graph->xmin + (float)(xpix - graph->x)*(float)(graph->xmax - graph->xmin)/(float)graph->w;
    xvalue = (time_t) xtmp;

    return( xvalue);

}

/* *************************************************** */
/*      Get color index based on color's name          */
/* *************************************************** */
int getMyColor ( Display *display, char *colorName)
{
    XColor          xcolor,
            unused;
            

    Colormap        cmap;

    cmap    = XDefaultColormap(display,DefaultScreen(display));

    XAllocNamedColor(display, cmap, colorName, &xcolor, &unused);

    return (xcolor.pixel);
}


/* *********************************************************** */
/* Check if the color assigned to current trace is already     */
/* selected color by user's from the timeseries_definition.cfg */
/* file. If it is then assign it to next available color       */
/* *********************************************************** */
int pickMyColor ( GRAPH_INFO *ginfo , int ntrace)
{
    int     n,
        m,
        color;

    int     duplicate;

    TRACE_INFO *tinfo;

       extern PAGE_MGR    *PageMgr;
    extern COLORS  TScolors[MAX_COLORS];

    for ( n = ntrace; n < 40; n++)
    {
        duplicate = 0;
        for ( m = 0; m < ginfo->num_traces; m++)
        {

             tinfo  = (TRACE_INFO *) &ginfo->traces[m];
            color  = getMyColor ( PageMgr->display, tinfo->color_name);
            if ( color == TScolors[n].fg_color) duplicate = 1;

        }
        if ( ! duplicate )break;
    }

    return ( n );
}


/****************************
   Function Name : init_TSinit_gdata
   Purpose       : Initialization function which sets all graph data to MISSING
*****************************/

void init_TSinit_gdata(GRAPH_DATA *graph)
{

  graph->hdataPtr.flood_discharge  = MISSING;
  graph->hdataPtr.action_flow      = MISSING;
  graph->hdataPtr.flood_stage      = MISSING;
  graph->hdataPtr.action_stage     = MISSING;
  graph->hdataPtr.major            = MISSING;
  graph->hdataPtr.moderate         = MISSING;
  graph->hdataPtr.minor            = MISSING;
}

/* ********************************************************* */
/* Get default showcat value from .apps_default file,   */
/* default to 1 if token is not available.                      */
/* Update the radio buttons' mode                            */
/* added by guoxian zhou 06-2004                          */
/* ******************************************************** */
int get_default_showcat ( )
{
    char     gad_value[128], apps_str[] = "timeseries_showcat";

    int        gad_token_len=0, gad_value_len=0, rcfad=0, i;
    
    static int first = 0;
    static int tokenShowcat = 1 ;

    Widget widget;

    if(first == 0)
    {
        first = 1 ;
        gad_token_len = strlen(apps_str);
        rcfad = get_apps_defaults(apps_str, &gad_token_len, gad_value, &gad_value_len);

        if ( rcfad == 0 )
        {
            tokenShowcat = atoi(gad_value);
            tokenShowcat = tokenShowcat - 1;

            /* ************************************************************* */
            /*     ptr=0 scale by data only;                                      */
            /*    ptr=1 scale by data , show categories if within data;         */
            /*    ptr=2 scale by data and  categories;                          */
            /* ************************************************************* */

            if(tokenShowcat < 0 || tokenShowcat > 2)
            {
                fprintf(stderr,"WARNING: The current value \"%s\" set for the token \"timeseries_showcat\" \n", gad_value); 
                fprintf(stderr,"         is not valid. Set to default value:\"scale by data, show categories\".\n");
                tokenShowcat = 1;
            }
        }
        else
        {
            fprintf(stderr, "STATUS: There is no value currently set for the token \"timeseries_showcat\".\n");
            fprintf(stderr, "        Set to default value: \"scale by data, show categories\".\n");
            tokenShowcat = 1;
        }
    }

    /* update all radio buttons' mode */
    for(i = 0; i < 3; i++ )
    {
        if(i == tokenShowcat)
        {
            if((widget = XtNameToWidget(TSscaleMO, scaleButtonNames[i])) != NULL)
                XtVaSetValues(widget, XmNset, TRUE, NULL);

            if((widget = XtNameToWidget(TSBatchScaleMO, batchButtonNames[i])) != NULL)
                XtVaSetValues(widget, XmNset, TRUE, NULL);
        }
        else
        {
            if((widget = XtNameToWidget(TSscaleMO, scaleButtonNames[i])) != NULL)
                XtVaSetValues(widget, XmNset, FALSE, NULL);

            if((widget = XtNameToWidget(TSBatchScaleMO, batchButtonNames[i])) != NULL)
                XtVaSetValues(widget, XmNset, FALSE, NULL);
        }

    }
    
    return tokenShowcat;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/whfs_lib/src/TimeSeriesUtil/RCS/TSutils.c,v $";
 static char rcs_id2[] = "$Id: TSutils.c,v 1.25 2007/12/27 14:23:28 mos Exp $";}
/*  ===================================================  */

}


/* **************************************** */
/* End of  File:           TSutils.c        */
/* **************************************** */

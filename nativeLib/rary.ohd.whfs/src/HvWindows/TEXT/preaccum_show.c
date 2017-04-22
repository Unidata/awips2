/*************************************************************************
   File:           preaccum_show.c
   Purpose:        Provide support for the precip accumulation display.
   
   ************************************************************************/

#include <ctype.h>
#include <errno.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <time.h>
#include <unistd.h>          /* for getpid(), unlink() */

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/FileSB.h>
#include <Xm/Protocols.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>

#include "CurPC.h"
#include "CurPP.h"
#include "DbmsAccess.h" 
#include "DbmsDefs.h"
#include "dbmserrs.h"

#include "GeneralUtil.h"
#include "get_total_precip.h"
#include "List.h"
#include "load_PCPP_data.h"
#include "LoadUnique.h"
#include "Location.h"

#include "preaccum.h"
#include "preaccum_show.h"
#include "get_loc_info.h"  /*get name for particular id*/

/* global variables */
static preaccum_options_struct pa_options;
static preaccum_data_struct    pa_data;

FILE	* fp = NULL ;

Widget preaccumFileSB = (Widget) NULL;   /* file selection box */

/* the number of durations must agree with the number in the .h file. Note
 * that the 0 corresponds to the "Other" duration.  The user specifies
 * this duration in the Other text box. Negative durations are not allowed. */

static const int duration_values [ NUMDURATIONS ] = { 1 , 3 , 6 , 12 , 24 , 
                                                      48 , 72 , 0 } ;
static int durations [ NUMDURATIONS ] ;


/* the memory for these lists is allocated and read once and
   remains for the duration of the program */
static UniqueList * ul_PCHead = NULL ;
static UniqueList * ul_PPHead = NULL ;
static UniqueList * ul_HSAhead = NULL ;
static Location   * lid_HSAhead = NULL ;

/* This percentage of the accumulation interval which must be covered by
   precipitation reports. */
static float  min_percent = 0.0 ;

/************************************************************************
   set_pa_filenames()
   
   **********************************************************************/
static void set_pa_filenames()
{
   long		pidval;
   int          gad_token_len=0, gad_value_len=0;
   char         scratch_dir[128];
   
   
   /* use the process id to uniquely identify session files */
   
   pidval = (long )getpid();
   
   
   /* get the scratch file directory name */ 
   
   gad_token_len = strlen("whfs_product_dir");
   get_apps_defaults("whfs_product_dir", &gad_token_len,
		     scratch_dir, &gad_value_len);
   
   
   /* set the filenames */
   
   sprintf(pa_options.hdrfile,  "%s/%s.%ld", scratch_dir, PA_HDRFILE, pidval);
   sprintf(pa_options.datafile, "%s/%s.%ld", scratch_dir, PA_DATAFILE, pidval);
   sprintf(pa_options.outfile,  "%s/%s.%ld", scratch_dir, PA_OUTFILE, pidval);
   
   
   /* set a variable for use later to prevent sorting and other actions
      on a file that does not yet exist */
   
   pa_options.tempfile_created = 0;
   
   return;
}

/************************************************************************
   preaccum_InitPETSlist()
   
   **********************************************************************/
static void preaccum_InitPETSlist()
{   
   UniqueList		*ulPtr;
   int			cnt, i;   
   char			where[120];
   char			buf[30];   
   XmStringTable        xmStr;
   Arg          	arg[40];   
   int          	ac;
   int			ul_PCcount, ul_PPcount;
   
   
   /* get a list of type sources from the database, for each of
      the two physical elements */

   sprintf(where, "%s",
	   " WHERE pe  ='PC' AND ts LIKE 'R%' ORDER BY 1 ");
   ul_PCHead = LoadUnique("ts", "IngestFilter", where, &ul_PCcount);
   

   sprintf(where, "%s",
	   " WHERE pe = 'PP' AND ts LIKE 'R%' ORDER BY 1 ");
   ul_PPHead = LoadUnique("ts", "IngestFilter", where, &ul_PPcount);

   
   if (ul_PPcount == 0 && ul_PCcount == 0) return;

   
   /* load the PC items into the scrolled lists */
   
   xmStr  = (XmStringTable) XtMalloc(ul_PCcount * sizeof(XmString *));
   
   ulPtr = (UniqueList *) ListFirst(&ul_PCHead->list);
   cnt = 0;
      
   while(ulPtr)
   {
      memset(buf, 0, SHEF_TS_LEN + 1);      
      strncpy(buf, ulPtr->uchar, 2);
            
      xmStr[cnt] = XmStringCreateSimple(buf);
            
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      cnt++;
   }

   
   /* load XmList widget  */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(preaccum_PCtsLI, arg, ac);
        
   
   /* cleanup and return */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
   
   /* repeat for the PP items */
   
   xmStr  = (XmStringTable) XtMalloc(ul_PPcount * sizeof(XmString *));
   
   ulPtr = (UniqueList *) ListFirst(&ul_PPHead->list);
   cnt = 0;
      
   while(ulPtr)
   {
      memset(buf, 0, SHEF_TS_LEN + 1);      
      strncpy(buf, ulPtr->uchar, 2);
            
      xmStr[cnt] = XmStringCreateSimple(buf);
            
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      cnt++;
   }

   
   /* load XmList widget  */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(preaccum_PPtsLI, arg, ac);
     
   
   
   /* cleanup and return */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);

   
   /* select all items in both lists  */
   
   for (i = 1; i <= ul_PCcount; i++)
   {
      XmListSelectPos(preaccum_PCtsLI, i, FALSE);
   }
   for (i = 1; i <= ul_PPcount; i++)
   {
      XmListSelectPos(preaccum_PPtsLI, i, FALSE);
   }
   
   
   /* allocate arrays to help track which
      PE-TS combinations are selected */
   
   pa_options.PCTS_set = (int *)malloc(sizeof(int) * ul_PCcount);
   if (pa_options.PCTS_set == (int *)NULL)
      fprintf(stderr, "Error mallocing PCTS_set.\n");
   
   pa_options.PPTS_set = (int *)malloc(sizeof(int) * ul_PPcount);
   if (pa_options.PPTS_set == (int *)NULL)
      fprintf(stderr, "Error mallocing PPTS_set.\n");

   
   /* save these for future reference */
   
   pa_options.numPC_defined = ul_PCcount;
   pa_options.numPP_defined = ul_PPcount;
   
   return;
}

/************************************************************************
   preaccum_InitHSAlist()
   
   **********************************************************************/
static void preaccum_InitHSAlist ( )
{   
   UniqueList		* ulPtr = NULL;
   int			cnt, i;   
   char			buf [ HYD_SERV_LEN + 1 ] ;   
   XmStringTable        xmStr;
   Arg          	arg[40];   
   int          	ac;
   int			ul_HSAcount ;
   
   /* Retrieve a list of unique HSA offices from the LocClass table in the
      IHFS database. */ 
   ul_HSAhead = LoadUnique ( "hsa" , "LocClass" , "" , & ul_HSAcount ) ;
   
   if ( ul_HSAcount == 0 ) return ;
   
   /* load the HSA offices into the scrolled lists */
   xmStr  = ( XmStringTable ) XtMalloc ( ul_HSAcount * 
		                         sizeof ( XmString * ) ) ;
   
   ulPtr = ( UniqueList * ) ListFirst ( & ul_HSAhead->list ) ;
   cnt = 0 ;
      
   while ( ulPtr )
   {
      memset ( buf , '\0' , HYD_SERV_LEN + 1 ) ;      
      strncpy ( buf , ulPtr->uchar , HYD_SERV_LEN ) ;
            
      xmStr [ cnt ] = XmStringCreateSimple ( buf ) ;
            
      ulPtr = ( UniqueList * ) ListNext ( & ulPtr->node ) ;
      cnt++ ;
   }
   
   /* load XmList widget  */
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(preaccum_hsaLI, arg, ac);
   
   /* cleanup and return */
   for (i = 0; i < cnt; i++)
   {
      XmStringFree(xmStr[i]);
   }

   XtFree ( ( char * ) xmStr ) ;

   pa_options.HSA_set = ( int * ) malloc ( sizeof ( int ) * ul_HSAcount ) ;

   if ( pa_options.HSA_set == ( int * ) NULL )
   {
      fprintf ( stderr, "Error mallocing PCTS_set.\n" ) ;
   }
   
   /* save these for future reference */
   pa_options.numHSA_defined = ul_HSAcount;
   
   return ;
}
/************************************************************************
   preaccum_InitEndtime()
   
   **********************************************************************/
static void preaccum_InitEndtime()
{
   time_t	utimet;
   struct tm	*utm;
   int		synop_hour;
      
   
   /* the endtime is based on the current time  */
   
   time(&utimet);   
   utm = gmtime(&utimet);
   
    
   /* set the endtime string to the previous 
      6 hour value */
   
   if (utm->tm_hour >= 18)
      synop_hour = 18;
   else if (utm->tm_hour >= 12)
      synop_hour = 12;
   else if (utm->tm_hour >= 6)
      synop_hour = 6;
   else
      synop_hour = 0;
      
   memset(pa_options.endtime_str, 0, MAXLEN_ENDTIME);
   sprintf(pa_options.endtime_str, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d", 
	   1900+utm->tm_year, utm->tm_mon + 1, utm->tm_mday,
	   synop_hour, 0, 0);
   
   
   XmTextSetString(preaccum_endTX, pa_options.endtime_str);
   
   return;
}

/************************************************************************
   preaccum_InitDurationList()

   February 4, 2004 - The only portion of this routine being retained is
                      the part which highlights the 5th position of the
		      duration list.
      
   **********************************************************************/
static void preaccum_InitDurationList() 
{  
   /* select only certain items in the list.
      the hours are 1,3,6,12,24,48,72, and Other indexed by 1..8 */
   
   XmListSelectPos ( preaccum_durationLI , 5 , FALSE ) ;

   return;   
}

/************************************************************************
   read_sort_option()
   
   **********************************************************************/
static void read_sort_option()
{  
   int option;
   
   
   option = GetMenuPos(preaccum_sortOM);
   switch (option)
   {
      case 0:
	 pa_options.sort_option = SORT_BY_LID;
	 break;
	 
      case 1:
	 pa_options.sort_option = SORT_BY_VALUE;
	 break;
   }
   
   return;
}


/************************************************************************
   read_duration_set()
   
   **********************************************************************/
static void read_duration_set ( )
{
   int * poslist = NULL;
   int cnt      = 0;
   int i;
   int index = 0 ;
   
   /* Clear all indexes in duration list.  Also, make sure that the
      duration array is in the correct order to start with. */
   for ( i = 0 ; i < NUMDURATIONS ; ++i )
   {
      pa_options.duration_set [ i ] = 0 ;
      durations [ i ] = duration_values [ i ] ;
   }

   XmListGetSelectedPos(preaccum_durationLI, &poslist, &cnt);

   for ( i = 0; i < cnt; i++)
   {
      pa_options.duration_set[ *(poslist + i) - 1] = 1;
   }

   /* Test to determine if the "Other" option is being used.  If it is,
      then, reorder the contents of the duration_set and durations arrays
      so that the "Other" value is shown in the correct order. */ 
   if ( pa_options.duration_set [ NUMDURATIONS - 1 ] == 1 )
   {
      for ( i = 0 ; i < NUMDURATIONS ; ++ i )
      {
         if ( pa_options.other_duration <= durations [ i ] )
         {
            index = i ;
            break ;
         }
      }


      for ( i = NUMDURATIONS - 1 ; i > index ; -- i )
      {
         durations [ i ] = durations [ i - 1 ] ;
         pa_options.duration_set [ i ] = pa_options.duration_set [ i - 1 ] ;   
      } 

      durations [ index ] = pa_options.other_duration ;
      pa_options.duration_set [ index ] = 1 ;
   }
   
   return;   
}


/************************************************************************
   read_PETS_set()
   
   This tracks which of the rows of the list of pe-ts combinations
   are currently selected.
   
   **********************************************************************/
static void read_PETS_set()
{ 
   int 		*poslist = NULL;
   int 		cnt      = 0;
   int 		i;
   
   
   /* clear all indexes in pe-ts list */
   
   for(i = 0; i < pa_options.numPC_defined; i++)
      pa_options.PCTS_set[i] = 0;
   
   for(i = 0; i < pa_options.numPP_defined; i++)
      pa_options.PPTS_set[i] = 0;
   
   
   /* find out which rows are selected */
   
   XmListGetSelectedPos(preaccum_PCtsLI, &poslist, &cnt);

   for (i = 0; i < cnt; i++)
   {
      pa_options.PCTS_set[ *(poslist + i) - 1] = 1;
   }

   pa_options.numPC_selected = cnt ; 
   
   XmListGetSelectedPos(preaccum_PPtsLI, &poslist, &cnt);

   for (i = 0; i < cnt; i++)
   {
      pa_options.PPTS_set[ *(poslist + i) - 1] = 1;
   }

   pa_options.numPP_selected = cnt ;
   
   return;   
}

/************************************************************************
   read_HSA_set()
   
   This tracks which of the rows of the list of pe-ts combinations
   are currently selected.
   
   **********************************************************************/
static void read_HSA_set()
{ 
   int 		*poslist = NULL;
   int 		cnt      = 0;
   int 		i;
   
   /* clear all indexes in HSA list */
   for(i = 0; i < pa_options.numHSA_defined; i++)
   {
      pa_options.HSA_set[i] = 0;
   }
   
   /* find out which rows are selected */
   XmListGetSelectedPos(preaccum_hsaLI, &poslist, &cnt);

   for ( i = 0 ; i < cnt ; ++i )
   {
      pa_options.HSA_set[ *(poslist + i) - 1] = 1;
   }
   
   /* see how many type are selected */
   pa_options.numHSA_selected = cnt ; 
   
   return;   
}

/************************************************************************
   read_end_timestr()
   
   **********************************************************************/
static void read_end_timestr()
{
   char * str = NULL ;
   
   
   str = XmTextGetString(preaccum_endTX);
   strcpy(pa_options.endtime_str, str);
   XtFree(str);
  
   return;
}

/************************************************************************
   read_accum_options()

   Read the various options and load them into the internal
   structure for reference.
   **********************************************************************/
static void read_accum_options()
{
   char	* str = NULL ;
   
   /* get the value of all the settings and switches, 
      starting with the end time */
   read_end_timestr();

   /* get the settings of the switches */
   pa_options.loc_switch     = ( int )
                               XmToggleButtonGetState ( preaccum_locationTB ) ;
   pa_options.details_switch = ( int )
                               XmToggleButtonGetState ( preaccum_detailTB ) ;
   pa_options.PPaccum_switch = ( int )
                               XmToggleButtonGetState ( preaccum_accumTB ) ;
   pa_options.hsa_switch     = ( int )
                               XmToggleButtonGetState ( preaccum_hsaTB ) ;
   pa_options.pets_switch    = ( int )
                               XmToggleButtonGetState ( preaccum_sourceTB ) ;
   
   /* get the location string */
   str = XmTextGetString(preaccum_locationTX);
   strcpy(pa_options.locid, str);
   XtFree(str);

   /* Get the other duration. */
   str = XmTextGetString ( preaccum_otherTX ) ;
   pa_options.other_duration = 0 ;
   pa_options.other_duration = atoi ( str ) ;
   durations [ NUMDURATIONS - 1 ] = pa_options.other_duration ;
   
   XtFree ( str ) ;

   /* get the sort option */
   read_sort_option ( ) ;

   /* Get the list of pe-ts combinations.  Do this only if the
    * filter by data sources toggle button is "on".  */
   read_PETS_set ( ) ;
   
   /* get the selected durations */
   read_duration_set ( ) ;

   /* Get the selected hsa. */
   read_HSA_set ( ) ;
   
   /* now dump for debug */
   /*
   printf("switch for loc,detail, accum=%d %d %d\n",
   pa_options.loc_switch,
   pa_options.details_switch,
   pa_options.PPaccum_switch);
   printf("loc=%s; sort=%d; time= %s\n",
   pa_options.locid, pa_options.sort_option, pa_options.endtime_str);   
   for (i = 0; i < NUMDURATIONS; i++)
      printf("dur %d %d\n", durations[i], pa_options.duration_set[i]);
   
   printf("numPC PP defined/selected = %d/%d %d/%d\n",
   pa_options.numPC_defined, pa_options.numPC_selected,
   pa_options.numPP_defined, pa_options.numPP_selected);
   
   for (i = 0; i < pa_options.numPC_defined; i++)
      printf("PCts %d\n", pa_options.PCTS_set[i]);
   for (i = 0; i < pa_options.numPP_defined; i++)
      printf("PPts %d\n", pa_options.PPTS_set[i]);
   */
   
   return;
}

/************************************************************************
   adjust_timestr()
  
   Reads input string and adjusts time by given amount in given direction,
   then returns modified time string.
   
   **********************************************************************/
static void adjust_timestr(char 	*timestr, 
		           int		dec_inc,
		           long         amount)
{
   int 		y, m, d, h, mi, s;
   struct tm 	utm;
   struct tm	*utmPtr;
   time_t	utimet;
   
   sscanf(timestr,
	  "%4d-%2d-%2d %2d:%2d:%2d", &y, &m, &d, &h, &mi, &s);
   
   utm.tm_sec  = s;
   utm.tm_min  = mi;
   utm.tm_hour = h;
   utm.tm_mday = d;
   utm.tm_mon  = m - 1;
   utm.tm_year = y - 1900;
   
   utimet = gm_mktime(&utm);
   
   if (dec_inc > 0)
      utimet = utimet + amount;
   else
      utimet = utimet - amount;
   
   
   utmPtr = gmtime(&utimet);
   
   s  = utmPtr->tm_sec;
   mi = utmPtr->tm_min;
   h  = utmPtr->tm_hour;
   d  = utmPtr->tm_mday;
   m  = utmPtr->tm_mon + 1;
   y  = utmPtr->tm_year + 1900;
      
   sprintf(timestr,
	   "%.4d-%.2d-%.2d %.2d:%.2d:%.2d", y, m, d, h, mi, s);
   
   return;   
}


/************************************************************************
   increaseEnddayCB()
   
   **********************************************************************/
static void increaseEnddayCB( Widget w, XtPointer ptr, XtPointer cbs )
{  
   
   read_end_timestr();   
   adjust_timestr(pa_options.endtime_str, +1, 24*3600);   
   XmTextSetString(preaccum_endTX, pa_options.endtime_str);
   
   return;   
}


/************************************************************************
   decreaseEnddayCB()
   
   **********************************************************************/
static void decreaseEnddayCB( Widget w, XtPointer ptr, XtPointer cbs )
{
   
   read_end_timestr();   
   adjust_timestr(pa_options.endtime_str, -1, 24*3600);   
   XmTextSetString(preaccum_endTX, pa_options.endtime_str);
   
   return;     
}


/************************************************************************
   increaseendHourCB()
   
   **********************************************************************/
static void increaseendHourCB( Widget w, XtPointer ptr, XtPointer cbs )
{
   
   read_end_timestr();   
   adjust_timestr(pa_options.endtime_str, +1, 3600);   
   XmTextSetString(preaccum_endTX, pa_options.endtime_str);
   
   return;   
}


/************************************************************************
   decreaseendHourCB()
   
   **********************************************************************/
static void decreaseendHourCB( Widget w, XtPointer ptr, XtPointer cbs )
{
   
   read_end_timestr();   
   adjust_timestr(pa_options.endtime_str, -1, 3600);   
   XmTextSetString(preaccum_endTX, pa_options.endtime_str);
   
   return;   
}

/************************************************************************
  filter_station_by_hsa()
   
 ***********************************************************************/
static enum FilterStation filter_station_by_hsa  ( const char * lid )
{
   char * pHsa = NULL ;
   int compare_value ;
   int i ;
   int number_set ;
   static Location * pCurrentLocation = NULL ;
   UniqueList * ulPtr = NULL ;

   /* If all of the HSA's are selected, then use the station. No
      further testing is required. */
   if ( pa_options.numHSA_defined == pa_options.numHSA_selected )
   {
      return UseStation ;
   }

   if ( lid_HSAhead == NULL )
   {
      /* The linked list of Location information has not been loaded yet.
         This is probably because this is the first attempt to filter
         by hsa. */
      lid_HSAhead = GetLocation ( "ORDER BY lid ASC" ) ;

      if ( lid_HSAhead == NULL )
      {
         fprintf ( stderr, "\nIn routine 'filter_station_by_hsa':\n"
                           "Could not retrieve entries from the\n"
                           "Location table.\n" ) ;
         return IgnoreStation ;
      }

      pCurrentLocation = lid_HSAhead ;   
      
   }

   /* Test if the user is resetting this colooping function. */
   if ( lid == NULL )
   {
      pCurrentLocation = lid_HSAhead ;
      return IgnoreStation ;
   } 

   if ( pCurrentLocation == NULL )
   {
      return IgnoreStation ;
   }

   compare_value = strcmp ( pCurrentLocation->lid , lid ) ;

   while ( compare_value < 0 )
   {
      pCurrentLocation = ( Location * ) ListNext ( & pCurrentLocation->node ) ; 
      
      if ( pCurrentLocation == NULL )
      {
         return IgnoreStation ;
      }
     
      compare_value = strcmp ( pCurrentLocation->lid , lid ) ;
   }

   if ( compare_value == 0 )
   {
      pHsa = pCurrentLocation->hsa ;
   }
   else
   {
      return IgnoreStation ;
   }
   
   /* Check to determine if the hsa is in the list selected by the user. */
   ulPtr = ( UniqueList * ) ListFirst ( & ul_HSAhead->list ) ;

   number_set = 0 ;

   for ( i = 0 ;
         i < pa_options.numHSA_defined && 
         number_set < pa_options.numHSA_selected ;
	 ++ i )
   {
      if ( pa_options.HSA_set [ i ] == 1 )
      {
         ++ number_set ;
         compare_value = strncmp ( ulPtr->uchar , pHsa , HYD_SERV_LEN ) ;

	 if ( compare_value == 0 )
         {
            return UseStation ;
         }
      }

      ulPtr = ( UniqueList * ) ListNext ( & ulPtr->node ) ;
   }

   return IgnoreStation ; 
}

/************************************************************************
   set_accum_beginend()
      
   **********************************************************************/
static void set_accum_beginend(time_t	dur_timet,
			time_t	*start_time,
			time_t	*end_time)
{
   int		status ;
   time_t	start_timet, end_timet;
   
   
   /* initialize */
   
   *start_time = *end_time = 0;
      
   
   /* define the time range.  first convert the endtime */
     
   status = yearsec_ansi_to_timet(pa_options.endtime_str, &end_timet);
      
   
   /* the start time is ALWAYS the endtime minus the duration */
   
   start_timet = end_timet - dur_timet; 
   
   
   /* save the args and return */
   
   *start_time = start_timet;
   *end_time   = end_timet;
     
   
   return;
}


/************************************************************************
   load_precip_vals()
   
   Primary driver function for managing the retrieval, derivation, and
   display of the accumulation data for a single lid-pe-ts set of
   data withint the linked list.
   
   **********************************************************************/
static void load_precip_vals(CurPP	** pPtr,
		             int   * p_count)
{
   static char * min_dur_token = "hv_min_dur_filled";
   char          reply [ 20 ];
   int		i;
   time_t	begin_timet, end_timet;   
   int          advance_flag ;
   short int    ending_time_match = EXACT_ENDINGTIME_MATCH;
   static int   first = 1;
   int          num_pp_records ;
   int          num_pc_records ;
   int          last_pos ;
   int          reply_len;
   int          request_len;
   int          status ;
   struct total_precip total_precip ;
   unsigned char settings =  ( unsigned char ) 
	                     REPORT_MISSING_BELOW_MIN_PERCENT ;

   /* Initialize the precipitation processing settings. */

   if (!pa_options.PPaccum_switch)
   {
      settings |= PRECIP_NO_ACCUM ;
   }

   if ( first == 1 )
   {
      first = 0;
      request_len = strlen ( min_dur_token );
      status = get_apps_defaults ( min_dur_token, & request_len,
                                   reply, & reply_len );

      if ( ( status == 0 ) && ( reply_len > 0 ) )
      {
         min_percent = atof ( reply );

         if ( ( min_percent < 0.0 ) || ( min_percent > 1.0 ) )
         {
            min_percent = 0.0;
         }
      }
   }

   /* initialize the temporary structure to hold the data
      for all the durations for this lid-pe-ts */
   
   strcpy(pa_data.lid, ( * pPtr )->lid);
   strcpy(pa_data.pe,  ( * pPtr )->pe);
   strcpy(pa_data.ts,  ( * pPtr )->ts);

   advance_flag = 0 ;
   last_pos = -1 ;

   /* Find the position of the last duration. */
   for ( i = NUMDURATIONS - 1 ; i >= 0 ; -- i ) 
   {
      if ( pa_options.duration_set [ i ] )
      {
         last_pos = i ;
         break ;
      }
   }

   if ( last_pos == -1 ) advance_flag = 1 ;
   
   /* Loop on the number of durations requested.  Advance the 
      precipitation data pointer only on the last record. */
   for (i = 0; i < NUMDURATIONS; i++)
   {      
      if ( i == last_pos ) advance_flag = 1 ;
    
      if (pa_options.duration_set[i])
      {	 
         /* for the current duration, determine the begin and
	    end time */
	 set_accum_beginend((time_t )(durations[i] * 3600),
			    &begin_timet, &end_timet);

	 /* use the PC function if processing PC data.
	    PC data is always derived, so fix the indicator flag to 1 */
	 
	 if (strcmp( (* pPtr)->pe, "PC") == 0) 
	 {
            total_precip = get_total_raw_precip ( ( RawPC ** ) pPtr , 
			                          NULL , 
                                                  begin_timet ,
                                                  end_timet , 
						  ending_time_match, 
                                                  min_percent , 
						  settings ,
                                                  advance_flag , 
                                                  p_count ,
                                                  & num_pp_records ) ;
	    pa_data.hrfill[i] = ( double ) total_precip.hours_covered ;
	    pa_data.amount[i] = total_precip.value;	    
	    pa_data.summed_flag[i] = 1;
	 }
	 
	 /* for PP data, specify whether the user wants the 
	    function to derive the amount as needed.  also,
	    get a flag back indicating whether the data was 
	    derived or not. */
	 
	 else
	 {
            total_precip = get_total_raw_precip ( NULL , 
			                          (RawPP **) pPtr , 
			                          begin_timet ,
                                                  end_timet , 
						  ending_time_match , 
                                                  min_percent , 
						  settings , 
                                                  advance_flag ,
                                                  & num_pc_records , 
						  p_count ) ;

	    pa_data.hrfill[i] = total_precip.hours_covered ;
	    pa_data.amount[i] = total_precip.value ;
	    pa_data.summed_flag[i] = total_precip.summed_flag ;
	 }
      }
      
      
      /* if not considering this duration, initialize the 
	 values anyways */
      
      else
      {
	 pa_data.hrfill[i] = 0;
	 pa_data.amount[i] = MISSING_PRECIP; 
	 pa_data.summed_flag[i] = 0;
      }
   }
   
   
   /* find the maximum value considering all selected durations */
   
   pa_data.max_value = MISSING_PRECIP;   
   for (i = 0; i < NUMDURATIONS; i++)
   {      
      if (pa_options.duration_set[i])
      {	 
	 if (pa_data.amount[i] > pa_data.max_value)
	    pa_data.max_value = pa_data.amount[i];	 
      }
   }
  
   
   return;
}


/************************************************************************
   write_hdr_info()
  
   Write the heading info to the file.
   A dummy first field is needed on every line to
   account for the field which is cut out of the data lines.
      
   **********************************************************************/
static void write_hdr_info()
{
   float        percent; 
   int		i;
   time_t	curtime;
   char		timestr[ANSI_TIME_LEN];
   int		status;   
   
   
   /* open the header file. */
   if ((fp = fopen(pa_options.hdrfile, "w")) == NULL)
   {
      fprintf(stderr, "ERROR: Can't open hdr file %s\n", pa_options.datafile);
      return;     
   }
   
   /* set a flag indicating that the file has been created.
      this is helpful to know when the callback on the sort button
      should attempt to redisplay the sorted data. */
   pa_options.tempfile_created = 1;
   
   /* list the ending time and the accumulations being considered */   
   fprintf(fp, "Precipitation ending: %s for hr duration: ", 
	   pa_options.endtime_str);
   
   for ( i = 0 ; i < NUMDURATIONS ; ++ i )
   {      
      if (pa_options.duration_set[i])
      {
         fprintf(fp, "%d ", durations[i]);
      }
   }

   fprintf(fp, "\n");
   
   /* display the current time */
   
   time(&curtime);
   status = timet_to_yearsec_ansi(curtime, timestr);

   fprintf(fp, "Listing created: %s\n", timestr);
   
   
   /* display some description of the PP data processing methods */
      
   if (pa_options.details_switch)
   {
      if (pa_options.numPP_selected)
      {
	 if (pa_options.PPaccum_switch) 
	    fprintf(fp,
		    "PP values indicated by a 's' are summation of PP reports.\n");
	 else
	    fprintf(fp,
		    "PP values are direct PP reports; no summing of reports.\n");
      }
      
      fprintf(fp,
	      "Number of hours found for each duration are shown in "
	      "parentheses.\n");

      percent = min_percent * 100.0;

      if ( percent > 0.0 )
      {
         fprintf(fp,
              "Total considered missing if hours found < %6.2f%% of hours "
              "requested.\n", percent );
      }
   }
   
   
   /* list some headings for the durations */

   fprintf(fp, "\nLocation Name                 PE TS    ");
   for (i = 0; i < NUMDURATIONS; i++)
   {      
      if (pa_options.duration_set[i])
      {
	    fprintf(fp, "%2d hr  ", durations[i]);
      }
   }
   fprintf(fp, "\n");
   fprintf(fp, "======== ====                 == == \n");
   
   
   /* now close the header file */
   
   fclose(fp);
   
   
   return;
}

   
/************************************************************************
   write_pa_record()
   
   Write the data for a single lid-pe-ts combination to the output file. 
   
   **********************************************************************/
static void write_pa_record ( CurPP *pPtr ,
		              int	 count )
{
   int		i, first;
   char		datastr[12];
   CurPP	*listPtr;
   time_t	timet;
   char 	timestr[30];
   char		derived_str[4];
   struct tm 	*tm_struct;
   int		status;
   char		dur_str[SHEF_DUR_NAME_LEN + 1 + 1];
   ShefDur	*durPtr;
   char		where[50];
   char		qc_str[MAXLEN_QCSYMBOL];
   int          returnvalue;  
   double        dlat, dlon;
   char         name[ALLOWED_NAME_LEN +1];
   
   /* the max_value is placed here for possible sort purposes only.
      in all cases, it it stripped off before presenting the output. */
   
   if (pa_data.max_value != MISSING_PRECIP)
      fprintf(fp, "%.2f ", pa_data.max_value);
   else
      fprintf(fp, "-1. ");
   
   
   /* write the key information */
   returnvalue = get_loc_info(pPtr->lid,&dlat,&dlon,name);  
   fprintf(fp, "%-8s %-20s %-2s %-2s :", pPtr->lid, name, pPtr->pe, pPtr->ts);
   
   /* write out the info for each selected duration */
   for (i = 0; i < NUMDURATIONS; i++)
   {    
      if (pa_options.duration_set[i])
      {
	 if (pa_data.amount[i] == MISSING_PRECIP)
	    sprintf(datastr, " MSG ");
	 else
	    sprintf(datastr, "%5.2f", pa_data.amount[i]);
	 
	 fprintf(fp, "  %s", datastr);
      }
   }
   
   
   /* show the details at the end of the line */
   
   if (pa_options.details_switch)
   {
      first = 1;
      fprintf(fp, " (");
      for (i = 0; i < NUMDURATIONS; i++)
      { 
	 if (pa_options.duration_set[i])
	 {
	    if (first)
	       first = 0;
	    else
	       fprintf(fp, "/");
	    
	    if (pa_data.summed_flag[i] && strcmp(pPtr->pe, "PP") == 0)
	       strcpy ( derived_str , "s" ) ;
	    else
	       strcpy ( derived_str , "" ) ;
	    
	    fprintf(fp, "%.1f%s", pa_data.hrfill[i], derived_str);
	 }
      }
      fprintf(fp, ")");
   }
   
   fprintf(fp, "\n");
   
   
   /* if showing details on only one location, then
      show the full time series.  */
   
   if (pa_options.loc_switch && pa_options.details_switch)
   {
      
      /* when listing out the data, always list in descending
	 cron order.  the order of the linked list varies 
	 depending on whether it is PC or PP, so this needs to
	 be accounted for.  PP is provided in the descending time
	 order, so the first pointer must be set special for PC.
	 also display a column header line */
      
      fprintf(fp, "\n");
      if (strcmp(pPtr->pe, "PC") == 0)
      {
	 /* use this method for assigning the pointer since 
	    ListFirst does not work on any other node in the
	    list besides the true head pointer */
	 
	 listPtr = pPtr;
	 for (i = 1; i < count; i++)
	 {
	    listPtr = (CurPP *) ListNext(&listPtr->node);
	 }
	 fprintf(fp,
		 "     Value     Time      Ext Qualif  QC\n");
      }
      
      else
      {
	 listPtr = pPtr;
	 fprintf(fp,
		 "     Value     Time      Duration         Ext Qualif  QC\n");
      }
      
      
      /* display the actual data now.
	 this function may receive records for more than one
	 TS in the linked list, so we need to loop on the P
	 count instead of looping until the end.
	 loop in the reverse direction for PC data */
      
      for (i = 0; i < count; i++)
      {
	 status = yearsec_dt_to_timet(listPtr->obstime, &timet);	    
	 tm_struct = gmtime(&timet);
	 strftime(timestr, 30, "%m/%d %H:%M", tm_struct);
	 
	 
	 /* build a presentable string for the quality code value */
	 
	 build_qc_symbol(listPtr->quality_code, qc_str);
	 
	 
	 if (strcmp(pPtr->pe, "PC") == 0)
	 {
	    fprintf(fp, "  %8.2f  %s  %s     %s     %s\n",
		    listPtr->value, timestr,
		    listPtr->extremum, listPtr->shef_qual_code, qc_str);
	    
	    listPtr = (CurPP *) ListPrev(&listPtr->node);
	 }
	 
	 else
	 {
	    /* build a presentable string for the duration code value */
	    
	    if (listPtr->dur != 0)
	    {
	       sprintf(where, " WHERE dur=%d ", listPtr->dur);
	       durPtr = GetShefDur(where);
	       if (durPtr == NULL)
	       {
		  sprintf(dur_str, "%d ", listPtr->dur);
	       }
	       else
	       {
		  sprintf(dur_str, "%s ", durPtr->name);
		  FreeShefDur(durPtr);
	       }
	    }
	    else
	       strcpy(dur_str, "");
	    
	    
	    /* now finally print out the real data */
	    
	    fprintf(fp,
		    "  %8.2f  %s  %-16s %s     %s     %s\n",
		    listPtr->value, timestr,
		    dur_str, listPtr->extremum, listPtr->shef_qual_code,
		    qc_str);
	    
	    listPtr = (CurPP *) ListNext(&listPtr->node);
	 }
      }   /* end of loop on time series info */
      fprintf(fp, "\n");
      
   }      /* end of if showing full time series */   
   
  
   return;
}


/************************************************************************
   write_missing_banner()
   
   
   **********************************************************************/
static void write_missing_banner()
{
   
   fprintf(fp, "\n  REQUESTED DATA NOT FOUND.\n");
   
   return;
}


/************************************************************************
   create_display_file()
   
   Create a display ready file from the scratch files.
   This sorts the data file, then appends the 
   header file and the cut and possibly sorted data file
   together to create a final file.
      
   **********************************************************************/
static void create_display_file()
{
   char		command[500];
   char		buf[500];
   
   /* use Unix features to sort the output data file. only sort
      if not processing for a single location. 
      the sort command allows the output to go to the same file! */
   
   if (!pa_options.loc_switch)
   {
      if (pa_options.sort_option == SORT_BY_LID)
	 sprintf(command, "/bin/sort -k 2 %s -o %s; ",
		 pa_options.datafile, pa_options.datafile);
      
      else
	 sprintf(command, "/bin/sort -rn  %s -o %s; ",
		 pa_options.datafile, pa_options.datafile);
   }
   else
      sprintf(command, " ");
   
   
   /* cat the header file to the output file */
   
   sprintf(buf, " /bin/cat %s > %s ;",
	   pa_options.hdrfile, pa_options.outfile);
   strcat(command, buf);
   
   
   
   /* now finally, cut out the first field containing
      the max value, which was put there for sort purposes only,
      and then append the cut info to the output file*/
   
   sprintf(buf, "/bin/cut -d \" \" -f 2- %s >> %s",
	   pa_options.datafile, pa_options.outfile);
   strcat(command, buf);
      
    
   printf("command=%s\n", command);
   system(command);
  
   return;   
}
 

/************************************************************************
   display_textfile()
   Read in the textfile containing all the output
   and display it to the screen.
   
   **********************************************************************/
static int display_textfile()
{   
   FILE 	*fp;
   char  	*str;
   char		errmsg[] = "Error reading file containing output";
   
   
   /* allocate and clear memory for char string str */
   
   str = XtMalloc(MAX_FILE_SIZE);
   memset(str, '\0', MAX_FILE_SIZE);
   
 
   /* open the sort file */
   
   if ((fp = fopen(pa_options.outfile, "r")) == NULL)
   {
      fprintf(stderr, "Error opening %s\n", pa_options.outfile);
      return(-1);
   }
   
   if ((fread(str, sizeof(char), MAX_FILE_SIZE, fp)) <= 0)
   {
      
      fprintf(stderr, "Error reading %s\n", pa_options.outfile);
      str = errmsg;      
   }
   fclose(fp);
   
   XmTextSetString(preaccum_TL, str); 
   XtFree(str);  
   
   
   return(0);   
}


/************************************************************************
   sort_pa_listCB()
   **********************************************************************/
static void sort_pa_listCB()
{
     
   /* read the sort option value, its value is set
      in the global structure */
   
   read_sort_option();
   
   
   /* if data has been generated already, then resort and
      display the data */
   
   if (pa_options.tempfile_created)
   {
      create_display_file();
      
      display_textfile();
   }
   
   
   return;
}


/************************************************************************
   preaccum_closeCB()
   **********************************************************************/
static void preaccum_closeCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   int 		status;

   /* delete the files */
   if (pa_options.tempfile_created)
   {      
      status = unlink(pa_options.hdrfile);

      if (status != 0)
      {
	 fprintf ( stderr, "Error deleting file: %s; errno = %d",
		   pa_options.hdrfile, errno);
      }
      
      status = unlink(pa_options.datafile);
      if (status != 0)
      {
	 fprintf(stderr, "Error deleting file: %s; errno = %d",
		 pa_options.datafile, errno);
      }
      
      status = unlink(pa_options.outfile);
      if (status != 0)
      {
	 fprintf(stderr, "Error deleting file: %s; errno = %d",
		 pa_options.outfile, errno);
      }     
   }   

   /* Free any dynamically allocated memory in pa_options. */
   if ( pa_options.HSA_set != NULL )
   {
      free ( pa_options.HSA_set ) ;
      pa_options.HSA_set = NULL ;
   }

   if ( pa_options.PCTS_set != NULL )
   {
      free ( pa_options.PCTS_set ) ;
      pa_options.PCTS_set = NULL ;
   }

   if ( pa_options.PPTS_set != NULL )
   {
      free ( pa_options.PPTS_set ) ;
      pa_options.PPTS_set = NULL ;
   }

   /* Free the dynamically allocated memory used in the linked lists
      of PC TS, PP TS, HSA, and LID/HSA information. */ 
   if ( ul_PCHead != NULL )
   {
      FreeUnique ( ul_PCHead ) ;
      ul_PCHead = NULL ;
   }

   if ( ul_PPHead != NULL )
   {
      FreeUnique ( ul_PPHead ) ;
      ul_PPHead = NULL ;
   }

   if ( ul_HSAhead != NULL )
   {
      FreeUnique ( ul_HSAhead ) ;
      ul_HSAhead = NULL ;
   }

   if ( lid_HSAhead != NULL )
   {
      FreeLocation ( lid_HSAhead ) ;
      lid_HSAhead = NULL ;
   }

   XtDestroyWidget ( preaccumDS ) ;
   preaccumDS = NULL ;
   
   return ;  
}

/************************************************************************
   preaccum_durationLICB()
   **********************************************************************/
void preaccum_durationLICB(Widget w, XtPointer ptr, XtPointer cbs)
{
   int * poslist = NULL;
   int cnt      = 0;
   int i;

   /* Clear all indexes in duration list.  Also, make sure that the
      duration array is in the correct order to start with. */
   for ( i = 0 ; i < NUMDURATIONS ; ++i )
   {
      pa_options.duration_set [ i ] = 0 ;
      durations [ i ] = duration_values [ i ] ;
   }

   XmListGetSelectedPos(preaccum_durationLI, &poslist, &cnt);

   for ( i = 0; i < cnt; i++)
   {
      pa_options.duration_set[ *(poslist + i) - 1] = 1;
   }

   /* Test to determine if the "Other" option is being used.  If it is,
      then, set the to active, otherwise, set the to inactive */
   if ( pa_options.duration_set [ NUMDURATIONS - 1 ] == 1 )
   {
      XtSetSensitive(preaccum_otherTX, TRUE);	   
   }
   else
   {
      XtSetSensitive(preaccum_otherTX, FALSE);
   }   
   
   return;  
}

/*************************************************************************
   preaccum_save_table()  
   
   Get the name of the file to save and save it by 
   copying the existing sorted file.
   
   **********************************************************************/
static void preaccum_save_table ( Widget w , XtPointer ptr , 
                                  XtPointer call_data )
{
   XmFileSelectionBoxCallbackStruct * cbs =
           ( XmFileSelectionBoxCallbackStruct * ) call_data ;
   XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
   char *name = NULL;
   char cmd_str[300];
   
   
   if(XtIsManaged(preaccumFileSB))
   {
      XtDestroyWidget(preaccumFileSB);
      preaccumFileSB = NULL;
   }
   
   if (! XmStringGetLtoR(cbs->value, charset, &name))
      return;
   
   if (! *name)
   {
      XtFree(name);
      return;
   }
   
   
   /* now finally copy the already created file */
   
   sprintf(cmd_str, "/bin/cp %s %s", pa_options.outfile, name);
   printf("command=%s\n", cmd_str);
   
   system(cmd_str);
   
   XtFree(name);
   
   return;   
}

/*************************************************************************
   preaccum_close_fileSB()
   
   **********************************************************************/
static void preaccum_close_fileSB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   
   /* clean up the display */
   
   if (XtIsManaged(preaccumFileSB))
   {
      XtDestroyWidget(preaccumFileSB);
      preaccumFileSB = NULL;
   }
   
   return;
}

/************************************************************************
   preaccum_saveCB()
   **********************************************************************/
static void preaccum_saveCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	list;
   Arg		args[10];
   int		ac;
   XmString	str;
   char         report_dir[128];
   int          gad_token_len=0, gad_value_len=0;
   
   
   if (!preaccumFileSB)
   {
      preaccumFileSB = XmCreateFileSelectionDialog(GetTopShell(w),
						   "preaccumFileSB", NULL, 0);
      
      
      /* set XmNpattern & XmNdialogStyle */
      
      str = XmStringCreateSimple("*");
      ac = 0;
      XtSetArg(args[ac], XmNpattern, str); ac++;
      XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
      
      
      /* get the directory and set it */ 
      
      gad_token_len = strlen("whfs_report_dir");
      get_apps_defaults("whfs_report_dir", &gad_token_len,
			report_dir, &gad_value_len);
      
      str = XmStringCreateSimple(report_dir);
      XtSetArg(args[ac], XmNdirectory, str); ac++;
      XtSetValues(preaccumFileSB, args, ac);
      XmStringFree(str);
      
      
      XtAddCallback(preaccumFileSB, XmNcancelCallback, preaccum_close_fileSB, NULL);
      XtAddCallback(preaccumFileSB, XmNokCallback, preaccum_save_table, NULL);
      
      list = XmFileSelectionBoxGetChild(preaccumFileSB, XmDIALOG_LIST);
      
      XtAddCallback(list, XmNdefaultActionCallback, preaccum_save_table, NULL);
   }   
   
   SetTitle(preaccumFileSB,"Save File Selection");
   
   if(! XtIsManaged(preaccumFileSB))
      XtManageChild(preaccumFileSB);
   
   
   return;
}

/************************************************************************
   preaccum_printCB()
   **********************************************************************/
static void preaccum_printCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   char command[200];
   char cmd_str[200];
   char lpr_print[128];
   int  gad_token_len=0, gad_value_len=0;
   
   gad_token_len = strlen("whfs_printcommand"); /*token for the print command*/
   get_apps_defaults("whfs_printcommand", &gad_token_len,lpr_print, 
                                          &gad_value_len);
   if(strlen(lpr_print) > 0)
   {
     strcpy(cmd_str,lpr_print);
   }
   else
   {
     strcpy(cmd_str, "lp");
   }   
   
   sprintf(command, "%s %s", cmd_str,pa_options.outfile);
   printf("command=%s\n", command);
   
   system(command);

   return;  
}

/************************************************************************
   get_precip_data()
   
   **********************************************************************/
static CurPP * get_precip_data  ( const char * get_pe , int * record_count )
{
   char *       pLid  = NULL ;
   char **      pTs   = NULL ;
   const char * where = NULL ;
   CurPP *      ppHead = NULL ;
   int		i , cnt ;
   time_t	begin_timet , end_timet ;
   time_t	dur_timet ;
   int		not_getting_all_TS ;
   int          num_ts ;
   int          value ;
   UniqueList	* ulPtr = NULL ;
      
   /* define the data time range based on the longest
      duration requested. this is not necessarily the
      time range of the data query */
   dur_timet = 0 ;

   /* Make sure to check whether or not the "Other" duration was selected.
      If it is set, then there is no guarantee that it is the largest of
      the durations in the list. */
   for (i = NUMDURATIONS - 1; i >= 0; i--)
   {

      if ( pa_options.duration_set[i] )
      {
         value = durations [ i ] * 3600 ;
	 if ( value > dur_timet ) dur_timet = value ;
      }
   }
   
   set_accum_beginend(dur_timet,
		      &begin_timet, &end_timet);
   
   /* adjust the begin time, adding some hours for good measure,
      under certain cirumstances */
   if (strcmp(get_pe, "PC") == 0)
   {      
      begin_timet = begin_timet - (3600 * 1);            
   }
   
   else if (strcmp(get_pe, "PP") == 0)
   {
      if (pa_options.PPaccum_switch)
	 begin_timet = begin_timet - (3600 * 3);
         end_timet = end_timet + ( 3600 * 3);
   }
   
   /* define any location filter */
   if (pa_options.loc_switch)
   {
      pLid = pa_options.locid ;
   }
   
   /* define any TS filter.  only include the TS query filter if not all
      TS are selected,  */ 
   num_ts = 0 ;

   if ( pa_options.pets_switch == 1 )
   { 
      not_getting_all_TS = 0;
   
      if (strcmp(get_pe, "PC") == 0)
      {      
         if (pa_options.numPC_selected < pa_options.numPC_defined)
   	    not_getting_all_TS = 1;           
      }
   
      else if (strcmp(get_pe, "PP") == 0)
      {      
         if (pa_options.numPP_selected < pa_options.numPP_defined)
   	    not_getting_all_TS = 1;           
      }
   
      /* Build the array of TS's to be passed into the load_PP_raw
         and load_PC_raw routines. */
      if ( not_getting_all_TS )
      {
         if (strcmp(get_pe, "PC") == 0)
         {
	    ulPtr = (UniqueList *) ListFirst(&ul_PCHead->list);
	    cnt = pa_options.numPC_defined;
         }
         else
         {
	    ulPtr = (UniqueList *) ListFirst(&ul_PPHead->list);
	    cnt = pa_options.numPP_defined;
         }
      
	 if ( cnt > 0 )
         {
            pTs = ( char ** ) malloc ( cnt * sizeof ( char * ) ) ; 

            if ( pTs == NULL )
            {
               return NULL ;
            }
         }

         for (i = 0; i < cnt; i++)
         {	 	 
	    if (((strcmp(get_pe, "PC") == 0) && pa_options.PCTS_set[i]) ||
	        ((strcmp(get_pe, "PP") == 0) && pa_options.PPTS_set[i]))
	    {		  
               pTs [ num_ts ++ ] = ulPtr->uchar ;
   	    }
	 
	    ulPtr = (UniqueList *) ListNext(&ulPtr->node);
         }

      }
   }

   if (strcmp(get_pe, "PC") == 0)
   {
      ppHead = ( CurPP * ) load_PC_raw ( begin_timet , end_timet , pLid ,
                                         ( const char ** ) pTs , num_ts , 
					 CurRawPrecip , record_count ) ;
      /* Print the query used to retrieve the precipitation data. */ 
      where = get_pcpp_query ( ) ;
      fprintf ( stdout, "\nSELECT * from CurPC %s\n", where ) ;

   }
   else
   {
      ppHead = ( CurPP * ) load_PP_raw ( begin_timet , end_timet , pLid , 
		             ( const char ** ) pTs , num_ts ,
			      CurRawPrecip , record_count ) ;
      /* Print the query used to retrieve the precipitation data. */ 
      where = get_pcpp_query ( ) ;
      fprintf ( stdout, "\nSELECT * from CurPP %s\n", where ) ;
   }

   if ( pTs != NULL )
   {
      free ( pTs ) ;
      pTs = NULL ;
   }

   return ppHead ;
}

/************************************************************************
   preaccum_loaddataCB()
   
   **********************************************************************/
static void preaccum_loaddataCB( Widget w, XtPointer ptr, XtPointer cbs )
{
   char         lid [ LOC_ID_LEN + 1 ];
   CurPP	* ppHead = NULL , * pcHead = NULL ;
   CurPP	* ppPtr = NULL , * pcPtr = NULL ;
   CurPP        * tempPtr = NULL ;
   enum FilterStation ignore_station = UseStation ;
   int		lidts_count ;
   int          pc_count = 0 ;
   int		pp_count = 0 ;
   int          status ;
   
   /* read the options and load them into the internal
      structure for reference */
   read_accum_options();
   
   /* open the temporary data file. */
   if ((fp = fopen(pa_options.datafile, "w")) == NULL)
   {
      fprintf(stderr, "ERROR: Can't open data file %s\n",
	      pa_options.datafile);
      return ;
   }
   
   /* set the cursor to a watch.  do this here, after the possible return
      above due to file open error, so that cursor isn't set wrong */
   SetCursor(preaccumFO, XC_watch);
   
   /* only try and load the data if at least one type source was
      selected for the physical element */
   if (pa_options.numPC_selected > 0)
   {
      /* knowing the user-controlled switch options, define the 
	 where clause for the data and load the data */
      /* build_accum_where("PC", where); */
      pcHead = get_precip_data ( "PC" , & pc_count ) ;
   }
   
   /* ditto for PP data */
   if ( pa_options.numPP_selected > 0 )
   {      
      /* build_accum_where ( "PP" , where ) ; */
      
      ppHead =  get_precip_data ( "PP" , & pp_count ) ;
   }
            
   printf("pc, pp cnt=%d %d\n", pc_count, pp_count);
         
   /* now process the data just retrieved by moving through
      the linked list, one lid-ts combination at a time.
      first get a count of the number of record for the 
      current lid-pe-ts combination */

   /* Initialize the HSA coloop if necessary. */
   if ( pa_options.hsa_switch == 1 )
   {
      ignore_station = filter_station_by_hsa ( NULL ) ;
   }

   pcPtr = pcHead ;

   while ( pcPtr != NULL )
   {	 	 
      /* Perform the filtering by hsa here, if necessary. */
      if ( pa_options.hsa_switch == 1 )
      {
         /* Hsa filtering is active. */
         ignore_station = filter_station_by_hsa ( pcPtr->lid ) ;
      }
   
      if ( ignore_station == UseStation )
      {
         /* Determine the value and load into the global structure,
	    write the data to the output file */
         tempPtr = pcPtr ;
         load_precip_vals(& pcPtr, & lidts_count); 
         write_pa_record(tempPtr, lidts_count);
      }
      else
      {
          /* Advance to the next station skipping all typesources in the
             case where the station is to be ignored due to HSA filtering.
             In the case where the station is to be used, then the
             call to the load_precip_vals routine above automatically advances
             pcPtr to the next unique lid/ts group. */ 
         memset ( lid , '\0', LOC_ID_LEN + 1 ) ; 
         strncpy ( lid, pcPtr->lid, LOC_ID_LEN ) ; 

         pcPtr = ( CurPP * ) ListNext ( & pcPtr->node );

         while ( pcPtr != NULL )
         {
            status = strncmp ( lid , pcPtr->lid, LOC_ID_LEN );

            if ( status != 0 ) break ;

            pcPtr = ( CurPP * ) ListNext ( & pcPtr->node );
         }
       
      }
   }
   
   /* Reset the HSA coloop if necessary. */
   if ( pa_options.hsa_switch == 1 )
   {
      ignore_station = filter_station_by_hsa ( NULL ) ;
   }

   ppPtr = ppHead;
   
   while ( ppPtr != NULL )
   {	 
      /* Perform the filtering by hsa here, if necessary. */
      if ( pa_options.hsa_switch == 1 )
      {
         /* Hsa filtering is active. */
         ignore_station = filter_station_by_hsa ( ppPtr->lid ) ;
      }
   
      if ( ignore_station == UseStation )
      {
         tempPtr = ppPtr ;
         load_precip_vals(& ppPtr, & lidts_count);
         write_pa_record(tempPtr, lidts_count);
      }
      else
      {
          /* Advance to the next station skipping all typesources in the
             case where the station is to be ignored due to HSA filtering.
             In the case where the station is to be used, then the
             call to the load_precip_vals routine above automatically advances
             ppPtr to the next unique lid/ts group. */ 
         memset ( lid , '\0', LOC_ID_LEN + 1 ) ; 
         strncpy ( lid, ppPtr->lid, LOC_ID_LEN ) ; 

         ppPtr = ( CurPP * ) ListNext ( & ppPtr->node );

         while ( ppPtr != NULL )
         {
            status = strncmp ( lid , ppPtr->lid, LOC_ID_LEN );

            if ( status != 0 ) break ;

            ppPtr = ( CurPP * ) ListNext ( & ppPtr->node );
         }
      }
   }
   
   /* show data missing message if no data found */
   
   if (pp_count == 0 && pc_count == 0)
   {
      write_missing_banner();
   }
   
   /* close the temporary data file */
   
   fclose(fp);
   
   
   /* free the curprecip data */
   
   if (pcHead != NULL)
   {
      FreeCurPC( (CurPC *) pcHead);
      pcHead = (CurPP *) NULL ;
   }
   
   if (ppHead != NULL)
   {
      FreeCurPP(ppHead);
      ppHead = NULL;
   }
   
   
   /* open the temporary header file and write the header info to it. */
   
   write_hdr_info();

   
   /* create the output */
   
   create_display_file();
   
   
   /* display the info to the screen */
   
   display_textfile();
   
   
   /* reset the cursor */
   
   UnsetCursor(preaccumFO);
   
   
   return;
}

/************************************************************************
   preaccum_callbacks()
   
   **********************************************************************/
static void preaccum_callbacks()
{  
   
   Atom wmAtom;

   /* callback on the sort option menu */
   
   XtAddCallback(preaccum_lidPB,    XmNactivateCallback, sort_pa_listCB, NULL);
   XtAddCallback(preaccum_maxvalPB, XmNactivateCallback, sort_pa_listCB, NULL);
     
   
   /* callbacks on the time setting operations */
   
   XtAddCallback(preaccum_endDayUpAR,    XmNactivateCallback, increaseEnddayCB,  NULL); 
   XtAddCallback(preaccum_endDayDownAR,  XmNactivateCallback, decreaseEnddayCB,  NULL);    
   XtAddCallback(preaccum_endHourUpAR,   XmNactivateCallback, increaseendHourCB, NULL); 
   XtAddCallback(preaccum_endHourDownAR, XmNactivateCallback, decreaseendHourCB, NULL); 
   

   /* callback on the duration list: if "other" is selected, activate
      preaccum_otherTX, otherwise, deactive preaccum_otherTX */
   XtAddCallback(preaccum_durationLI, XmNmultipleSelectionCallback, preaccum_durationLICB, NULL);

   
   /* callback on the main action button */
   
   XtAddCallback(preaccum_loaddataPB, XmNactivateCallback, preaccum_loaddataCB,NULL);
    
   
   /* callback on the primary pushbuttons */
   
   XtAddCallback(preaccum_closePB, XmNactivateCallback, preaccum_closeCB, NULL);
   XtAddCallback(preaccum_savePB,  XmNactivateCallback, preaccum_saveCB,  NULL);
   XtAddCallback(preaccum_printPB, XmNactivateCallback, preaccum_printCB, NULL);
   
   
   /* Add the callback for the 'X' button on the window frame.  This should
      behave in the same way as the close button does.  The default window
      manager supplied functionality for the 'X' button needs to be
      removed. */
   wmAtom = XmInternAtom ( XtDisplay ( preaccumDS ),
                           "WM_DELETE_WINDOW",
                           FALSE );
   XmAddWMProtocolCallback ( preaccumDS, 
                             wmAtom, 
                             preaccum_closeCB, 
                             preaccumDS );

   return;
}

/************************************************************************
   preaccum_show()
   
   **********************************************************************/
void preaccum_show(Widget w)
{
   if (! preaccumDS )
   {
      create_preaccumDS(GetTopShell(w));

      /* Remove the default functionality of the close window button on the
         Point Precipitation Totals window frame. */
      
      XtVaSetValues ( preaccumDS, XmNdeleteResponse, XmDO_NOTHING, NULL ); 
      /* Initialize the pointers in pa_options to NULL. */
      pa_options.HSA_set = NULL ;
      pa_options.PCTS_set = NULL ;
      pa_options.PPTS_set = NULL ;

      /* set the list of pe/ts combinations. do this only once so that
	 subsequent entries will be quick. howerver, new items in the 
	 list become available, they will not be recognized until the
	 program restarts */
      preaccum_InitPETSlist();
      
      /* initialize info related to the duration list */
      preaccum_InitDurationList();

      /* Initially set the preaccum_otherTX widget to "inactive" */
      XtSetSensitive(preaccum_otherTX, False);

      /* initialize the hsa list */
      preaccum_InitHSAlist() ;
      
      /* define the callbacks for the window */
      preaccum_callbacks(); 

      /* get the product id for this session and set the filenames based
	 in part on the pid */
      set_pa_filenames();
      
   }
   
   if (! XtIsManaged(preaccumDS))
   {
      XtManageChild(preaccumFO);
      XtManageChild(preaccumDS);
      
      preaccum_InitEndtime(); 
   }
   
   return;
   
}

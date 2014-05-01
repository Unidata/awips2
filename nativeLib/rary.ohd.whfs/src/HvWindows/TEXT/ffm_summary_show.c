/***********************************************************************
   ffm_summary_actions.c
         
   PURPOSE   
   Contains the actions associated with the ffm summary dialog.
   
   *********************************************************************/

#include <Xm/Xm.h>
#include <X11/cursorfont.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleBG.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>

#include <string.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#include "ffm_summary.h"
#include "ffm_summary_show.h"

#include "FfmUtils.h"
#include "FfmSummary.h"
#include "GeneralUtil.h"
#include "ArealProductSettings.h"

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "DbmsAccess.h"

#include "LoadUnique.h"

#include "time_defs.h"
#include "ToolDefs.h"
#include "Xtools.h"



/* global information. the number of durations/times must not
   be less than NUM_SUMMARY_DURS */

FfmRadarid	*ffm_radar_ids;

static int *area_listindex;
static int listindex_malloced = 0;

ArealProduct	*products;

FfmSummaryStats	*ffm_stats;
int	num_areas = 0;


/* global data variables */

int 	hr_durs[NUM_SUMMARY_DURS]      = {1, 3, 6, 12, 24};
time_t	precip_timet[NUM_SUMMARY_DURS] = {0, 0, 0,  0,  0};
time_t	ffg_timet[NUM_SUMMARY_DURS]    = {0, 0, 0,  0,  0};
		  


/************************************************************************
   show_ffm_summary()
   
   Main function executed when selecting display of this window.
   
   *********************************************************************/

void show_ffm_summary(Widget 	w)
{    
   static int ffm_summary_created = 0;
   int		cnt;
   
   
   cnt = recordCount("latestaccumgrid", "");;
      
   if (cnt == 0)
   {
      fprintf(stderr,
	      "Summary aborted. No data in LatestAccumGrid; widget=%p.\n", w);
      ErrorDialog(GetTopShell(w),
                 "No summary data available in LatestAccumGrid table.");
      return;
   }
   
   /* create the window */
   
   if (! ffm_summary_created) 
   {
      create_ffm_summaryDS(GetTopShell(w));
      add_ffm_summary_callbacks();
      ffm_summary_created = 1;      
   }
   
   
   if (! XtIsManaged(ffm_summaryDS))
   {      
      /* perform all the operations needed to initially 
	 fill the summary display window */
      
      init_summary_display();
      
      
      /* manage the form and shell */
      
      XtManageChild(ffm_summaryFO);
      XtManageChild(ffm_summaryDS);
   }   
   
   return;
}


/*********************************************************************

   init_summary_display()
   function for initial setup of the summary display.   
   
   *********************************************************************/

void init_summary_display()
{
   summary_area_filter 		area_filter;
   summary_value_filter 	value_filter;
   float			value;  
   summary_sort			sort_flag;
   summary_value_type		value_type;
   
   int				radar_cnt;  
   PrecipType 			gridtype_to_use;
   char				radar_to_use[RADAR_ID_LEN + 1];
   
   ArealProductTypeDescriptor	prod_descr;       
   ArealProductSpecifier	prod_spec;
   
   
   /* define the data set to use in terms of the data
      grid type to use, and the radar data set to use;
      remember option menus count begin at 0, not 1 */
      
   radar_cnt = init_summary_radar(radar_to_use);
   if (radar_cnt == 0)
   {
      fprintf(stderr,
	      "Summary not possible. No data in LatestAccumGrid table.\n");
      return;
   }
      
   
   SetMenuPos(ffm_datatypeOM, 0);      
   gridtype_to_use = STAGE1_PRECIP;
   
   
   /* initialize these fields, even though they are unused */
   
   prod_descr.mode    = COMPARISON_MODE;   
   prod_spec.endTime  = 0;
   prod_spec.duration = 0;
   
   
   /* now initialize the important fields based on the current settings.
      the initial resolution is set here before the data are loaded
      because the data are derived for the given resolution in the
      same process.  the initial resolution should agree with 
      what is set below for the initial area filter */
   
   prod_descr.resolutionLevel = BASIN_RES;
   prod_descr.precipType      = gridtype_to_use;
   strcpy(prod_spec.sourceId, radar_to_use);
   
   
   /* allocate and get the data for the display based on the
      product descriptor and specifier info. the info 
      is passed back via global variables */
   
   load_summary_data(prod_descr, prod_spec);
   
   
   /* set the text box describing the precip and ffg products
      being summarized */
   
   set_summary_prodinfo(prod_descr, prod_spec);
   
   
   /* load the list as per the filter and sort options.
      initialize the related option menus, text field, and
      radio box widgets */
      
   area_filter   = BASIN;
   value_filter  = VALUE_ALL;
   value         = 0.0;  
   set_summary_filter(area_filter, value_filter, value);
      
   sort_flag  = BY_VALUE;   
   set_summary_sort(sort_flag);

   value_type = USE_RATE;
   set_summary_valuetype(value_type);
   
   
   /* knowing all the filter and sort selections, load the list */
      
   load_ffmsummary_list(area_filter, value_filter, value, 
			sort_flag, value_type);
   
   return;
}


/*********************************************************************

   load_summary_data()
   Load the text boxes that describe the product being summarized.
   
   *********************************************************************/

void load_summary_data(ArealProductTypeDescriptor	prod_descr,       
		       ArealProductSpecifier		prod_spec)
   
{
   int	status;
      
   
   /* malloc the products structure */
   
   products = malloc_summaryprods(NUM_SUMMARY_DURS);
   
   
   /* assemble the specified precip and ffg data.  the data
      are retrieved from the special holding place for the 
      latest accumulated grids... 
      note that the prod_descr and prod_spec copies within
      the products structure are NOT filled in... */
     
   status = get_summary_data(prod_descr, prod_spec, NUM_SUMMARY_DURS, hr_durs,
			     products, precip_timet, ffg_timet);
   
   
   /* now compute the derived data from the retrieved data */
   
   ffm_stats = bld_summary_stats(prod_descr, prod_spec, products, &num_areas,
				 NUM_SUMMARY_DURS, hr_durs);
   
   return;
}


/*********************************************************************

   load_summary_prodinfo()
   Build the strings summarizing the times used for each of the
   precip and ffg products.
   
   *********************************************************************/

void set_summary_prodinfo(ArealProductTypeDescriptor	prod_descr,       
			  ArealProductSpecifier		prod_spec)
{
   int 	i;
   char buf[1024];
   char *retstr;
   char	curline[120];
   char hr_str[3];
      

   /* write the descriptions of the precip data */
   
   strcpy(buf, "");
   
   for (i = 0;  i < NUM_SUMMARY_DURS; i++)
   {
      sprintf(hr_str, "%d", hr_durs[i]);
      
      if (products[i].precip_grid != NULL)
      {
	 retstr = format_sumtime(precip_timet[i]);
	 sprintf(curline, "%-2s hour data ending %s\n", hr_str, retstr); 
      }
      else
      {
	 sprintf(curline, "%-2s hour data not available\n", hr_str); 
      }
      strcat(buf, curline);
   }
   XmTextSetString(ffm_precipTX, buf);
   
   
   /* display the descriptions of the ffg data source */
   
   strcpy(buf, "");
   
   for (i = 0;  i < NUM_SUMMARY_DURS; i++)
   {
      sprintf(hr_str, "%d", hr_durs[i]);
      
      if (products[i].ffg_grid != NULL)
      {
	 retstr = format_sumtime(ffg_timet[i]);
	 sprintf(curline, "%-2s hour data valid at %s\n", hr_str, retstr); 
      }
      else
      {
	 sprintf(curline, "%-2s hour data not available\n", hr_str); 
      }
      strcat(buf, curline);
   }
   XmTextSetString(ffm_ffgTX, buf);
   
   return;
}


/*********************************************************************

   load_ffmsummary_list()
   load the list of areas for the summary.
   
   *********************************************************************/

void load_ffmsummary_list(summary_area_filter 	area_filter,
			  summary_value_filter	value_filter,
			  float			value,
			  summary_sort 		sort_flag,
			  summary_value_type	value_type)
{
   XmStringTable        xmStr;
   Arg                  arg[40];
   int                  ac;
   char                 buf[140];
   int                  i, cnt ;
   
    
   /* if there are no areas defined, blank the lists */
   
   if (num_areas == 0)
   {
      blank_summary_list();
      blank_summary_details();
      return;
   }
   
   
   /* malloc an array of indices to the areas in the list for use
      in tracking which areas and their location within the list
      of areas. */
      
   if (listindex_malloced != 0)
   {
      free(area_listindex);
      listindex_malloced = 0;
   }
   
   area_listindex = (int *)malloc(sizeof(int) * num_areas);
   if (area_listindex == NULL)
      fprintf(stderr,
	      "Failed malloc of area_listindex in load_ffmsummary_list()");
   
   listindex_malloced = 1;
   memset(area_listindex, 0, sizeof(int) * num_areas);
   
   
   /* sort the list. the list of areas is passed globally. 
      sort the list before filtering to ensure that the indices
      in the area_list index apply to the correct area order
      in the ffm_stats array */ 
   
   sort_summary_list(value_type);
   
   
   /* get the count of the number of items in the list as per the 
      filter. this function also sets the index to the areas being
      retained in the list of areas; this is passed globally.
      note that one does not need to filter by the area type
      because data are only avilable for the given area type.
      when changing area type the derived data are recomputed. */
      
   cnt = filter_summary_list(value_type);
   cnt = num_areas;
      
   
   /* allocate memory for the motif strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));


   /* load the list of Xm strings */
   
   cnt = 0;
   for (i = 0; i < num_areas; i++)
   {      
      
      /* determine the sort order by which to display the areas */
      
      
      /* if the area passed the filter check, then load it */
      
      if (ffm_stats[i].pass_filter)
      {
	 /* build the display line based on the value type;
	    the ffm_stat values are passed globally */
	 
	 bld_summary_line(i, value_type, buf);
	 
	 xmStr[cnt] = XmStringCreateSimple(buf);
	 cnt++;
      }
      
   }
   
   /* load XmList widget and select first position */

   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(ffm_listLS, arg, ac);


   /* select the first item in the list; the callback then
      loads the other info.  if no items, then blank out the other info */

   if (cnt > 0)
   {
      XmListSetPos(ffm_listLS, 1);
      XmListSelectPos(ffm_listLS, 1, TRUE);
   }
   else
      blank_summary_details();


   /* cleanup and return */

   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
      
   
   return;
}


/************************************************************************
   
   filter_summary_list()
   filters the summary list based on the current settings and returns
   an array indicating which items passed the filter and the count
   of these items.  note the filter by area is not done here as that
   is done by virtue of the data retrieval, which retrieves data only
   for the given area types.
   
   *********************************************************************/

int filter_summary_list(summary_value_type	value_type)
{
   int 				status;
   int				i;
   summary_value_filter		value_filter;
   float			compare_value;
   int				dur_index;
   int				num_areas_remaining;
   int				msg_cnt = 0;
   int                          value = 0 ;
   
   
   /* initialize */
   
   num_areas_remaining = 0;
   
   
   /* get the value of the value filter variables that are not already 
      known. the value type is passed in already. */
      
   status = find_value_filter(&value_filter, &compare_value);
   
   /* loop on the areas and check their data against
      the filter criteria */
   
   for (i = 0; i < num_areas; i++)
   {      
      
      /* get the critical value for use in the comparison */
      
      if (value_type == USE_RATE)
      {
	 dur_index = ffm_stats[i].rate_index;
	 value = ffm_stats[i].dur_stats[dur_index].rate;
      }
      
      else if (value_type == USE_PERCENT)
      {
	 dur_index = ffm_stats[i].percent_index;
	 value = ffm_stats[i].dur_stats[dur_index].percent;
      }
      
      else if (value_type == USE_DIFFERENCE)
      {
	 dur_index = ffm_stats[i].diff_index;
	 value = ffm_stats[i].dur_stats[dur_index].diff;
      }
      
      
      /* perform the comparison. do not filter out missing data 
	 if displaying all items */
      
      if (value == MISSING)
      {
	 msg_cnt++;
	 if (value_filter == VALUE_ALL)
	    ffm_stats[i].pass_filter = 1;
	 else
	    ffm_stats[i].pass_filter = 0;
      }
      
      else
      {
	 if (value_filter == VALUE_GE)
	 {
	    if (value >= compare_value) 
	       ffm_stats[i].pass_filter = 1;
	    else
	       ffm_stats[i].pass_filter = 0;
	 }
	 
	 else if (value_filter == VALUE_LE)
	 {
	    if (value <= compare_value) 
	       ffm_stats[i].pass_filter = 1;
	    else
	       ffm_stats[i].pass_filter = 0;
	 }
	 
	 else
	    ffm_stats[i].pass_filter = 1;
      }
      
      
      if (ffm_stats[i].pass_filter)
      {
	 area_listindex[num_areas_remaining] = i;
	 num_areas_remaining++;
      }
   }
   
   
   /* blank out the remaining areas in the list */
   
   for (i = num_areas_remaining; i < num_areas; i++)
      area_listindex[i] = MISSING;
   
   printf("Summary numareas, before/after filter; msg = %d %d %d\n",
	  num_areas, num_areas_remaining, msg_cnt);
   
   return(num_areas_remaining);
}


/************************************************************************
   
   sort_summary_list()
   
   *********************************************************************/

void sort_summary_list(summary_value_type 	value_type)
{
   int			status;
   summary_sort		sort_flag;
   
   
   /* get the setting of the sort filter */
   
   status = find_summary_sort(&sort_flag);
         
   
   if (sort_flag == BY_ID)
   {
      qsort(ffm_stats, num_areas, sizeof(FfmSummaryStats),
	    compare_ffm_id);
   }
   
   else if (sort_flag == BY_NAME)
   {
      qsort(ffm_stats, num_areas, sizeof(FfmSummaryStats),
	    compare_ffm_name);      
   }
   
   
   /* knowing the value_type to sort by, sort the data */
   
   else if (sort_flag == BY_VALUE)
   {
      if (value_type == USE_RATE)
	 qsort(ffm_stats, num_areas, sizeof(FfmSummaryStats),
	       compare_ffm_rate);      
      else if (value_type == USE_PERCENT)
	 qsort(ffm_stats, num_areas, sizeof(FfmSummaryStats),
	       compare_ffm_percent);      
      else if (value_type == USE_DIFFERENCE)
	 qsort(ffm_stats, num_areas, sizeof(FfmSummaryStats),
	       compare_ffm_difference);      
   }
   
   return;
}


/************************************************************************
   
   compare_ffm_id()
   provides comparison function for qsort by id
   
   *********************************************************************/

int compare_ffm_id(const void *elem1, 
		   const void *elem2)
{
   
   FfmSummaryStats *stat1 = (FfmSummaryStats *) elem1;
   FfmSummaryStats *stat2 = (FfmSummaryStats *) elem2;
   int	rv = -1;
   
   
   rv = strcmp(stat1->area_id, stat2->area_id); 
      
   return rv;
}


/************************************************************************
   
   compare_ffm_name()
   provides comparison function for qsort by name
   
   *********************************************************************/

int compare_ffm_name(const void *elem1, 
		      const void *elem2)
{
   
   FfmSummaryStats *stat1 = (FfmSummaryStats *) elem1;
   FfmSummaryStats *stat2 = (FfmSummaryStats *) elem2;
   int	rv = -1;
   
   
   rv = strcmp(stat1->name, stat2->name);   
   
   return rv;
}


/************************************************************************
   
   compare_ffm_rate()
   provides comparison function for qsort by rate
   
   *********************************************************************/

int compare_ffm_rate(const void *elem1, 
		     const void *elem2)
{
   
   FfmSummaryStats *stat1 = (FfmSummaryStats *) elem1;
   FfmSummaryStats *stat2 = (FfmSummaryStats *) elem2;
   int		rv = -1;
   double	val1, val2;
   
   
   val1 = stat1->dur_stats[stat1->rate_index].rate;
   val2 = stat2->dur_stats[stat2->rate_index].rate;
   
   if (val1 == MISSING && val2 == MISSING)
      rv = strcmp(stat1->area_id, stat2->area_id); 
      
   else if (val1 == MISSING || val2 == MISSING)
   {
      if (val1 == MISSING)
	 rv = 1;
      else
	 rv = -1;
   }
   
   else 
   {
      if (val1 == val2)
	 rv = strcmp(stat1->area_id, stat2->area_id); 
      else if (val1 < val2)
	 rv = 1;
      else if (val1 > val2)
	 rv = -1;
   }
   
   
   return rv;
}


/************************************************************************
   
   compare_ffm_percent()
   provides comparison function for qsort by percent
   
   *********************************************************************/

int compare_ffm_percent(const void *elem1, 
			const void *elem2)
{
   
   FfmSummaryStats *stat1 = (FfmSummaryStats *) elem1;
   FfmSummaryStats *stat2 = (FfmSummaryStats *) elem2;
   int		rv = -1;
   double	val1, val2;
   
   
   val1 = stat1->dur_stats[stat1->percent_index].percent;
   val2 = stat2->dur_stats[stat2->percent_index].percent;
   
   if (val1 == MISSING && val2 == MISSING)
      rv = strcmp(stat1->area_id, stat2->area_id); 
   
   else if (val1 == MISSING || val2 == MISSING)
   {
      if (val1 == MISSING)
	 rv = 1;
      else
	 rv = -1;
   }
   
   else 
   {
      if (val1 == val2)
	 rv = strcmp(stat1->area_id, stat2->area_id); 
      else if (val1 < val2)
	 rv = 1;
      else if (val1 > val2)
	 rv = -1;
   }
   
   return rv;
}


/************************************************************************
   
   compare_ffm_difference()
   provides comparison function for qsort by difference
   
   *********************************************************************/

int compare_ffm_difference(const void *elem1, 
			   const void *elem2)
{
   
   FfmSummaryStats *stat1 = (FfmSummaryStats *) elem1;
   FfmSummaryStats *stat2 = (FfmSummaryStats *) elem2;
   int		rv = -1;
   double	val1, val2;
  
   
   val1 = stat1->dur_stats[stat1->diff_index].diff;
   val2 = stat2->dur_stats[stat2->diff_index].diff;
   
   if (val1 == MISSING && val2 == MISSING)
      rv = strcmp(stat1->area_id, stat2->area_id); 
   
   else if (val1 == MISSING || val2 == MISSING)
   {
      if (val1 == MISSING)
	 rv = 1;
      else
	 rv = -1;
   }
   
   else 
   {
      if (val1 == val2)
	 rv = strcmp(stat1->area_id, stat2->area_id); 
      else if (val1 < val2)
	 rv = 1;
      else if (val1 > val2)
	 rv = -1;
   }
   
   
   return rv;
}


/************************************************************************
   
   bld_summary_line()
   
   *********************************************************************/

void bld_summary_line(int			area_index,
		      summary_value_type	value_type,
		      char			*buf)
{
   char		areaname[LOC_AREANAME_LEN + 1];
   int		dur_index = 0 ;
   char		precip_str[12], ffg_str[12];
   char		rate_str[12], percent_str[12], diff_str[12];
   char		hr_str[3];
   
   
   /* get the index to the duration with the most
      critical value, based on the value type criteria */
   
   if (value_type == USE_RATE)
      dur_index = ffm_stats[area_index].rate_index;
   
   else if (value_type == USE_PERCENT)
      dur_index = ffm_stats[area_index].percent_index;
   
   else if (value_type == USE_DIFFERENCE)
      dur_index = ffm_stats[area_index].diff_index;
   
   else
      fprintf(stderr, "Invalid value_type detected in bld_summary_line()\n");
   
   
   /* do some error checking */
   
   if (dur_index > NUM_SUMMARY_DURS - 1) 
      fprintf(stderr, "Illegal number for dur_index in bld_summary_line.\n");

   if (area_index > num_areas - 1) 
      fprintf(stderr, "Illegal number for area_index in bld_summary_line.\n");
   
   
   /* now format the values accordingly */
   
   if (ffm_stats[area_index].dur_stats[dur_index].precip == MISSING)	 
      strcpy(precip_str,  "    m");
   else
      sprintf(precip_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].precip);
   
   if (ffm_stats[area_index].dur_stats[dur_index].ffg == MISSING)	 
      strcpy(ffg_str,  "    m");
   else
      sprintf(ffg_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].ffg);
   
   
   /* format the derived values */
   
   if (ffm_stats[area_index].dur_stats[dur_index].rate == MISSING)	 
      strcpy(rate_str,  "    m");
   else
      sprintf(rate_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].rate);
   
   if (ffm_stats[area_index].dur_stats[dur_index].percent == MISSING)	 
      strcpy(percent_str,  "    m");
   else
      sprintf(percent_str, "%6d %%",
	      (int )ffm_stats[area_index].dur_stats[dur_index].percent);
   
   if (ffm_stats[area_index].dur_stats[dur_index].diff == MISSING)	 
      strcpy(diff_str,  "    m");
   else
      sprintf(diff_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].diff);
   
   
   /* format the area name, limit the number of characters shown here */
   
   strcpy(areaname, ffm_stats[area_index].name);
   if (strlen(areaname) > 20)
   {
      memset(&areaname[20], 0, 1);
      strcat(areaname, "..");
   }
   
   
   sprintf(hr_str, "%d", hr_durs[dur_index]);
   
   sprintf(buf, "%-8s %-22s %8.1f  %-2s %-8s %-8s %-8s %-8s  %-8s",
	   ffm_stats[area_index].area_id, areaname,
	   ffm_stats[area_index].area, hr_str,
	   precip_str, ffg_str, percent_str, diff_str, rate_str);
   
   
   return;
}


/*********************************************************************

   load_detailed_list()
     
   *********************************************************************/

void load_detailed_list(int	area_index)
{
   XmStringTable        xmStr;
   Arg                  arg[40];
   int                  ac;
   char                 buf[140];
   int                  i, cnt;
   int			dur_index;
   
   cnt = NUM_SUMMARY_DURS;
   
   xmStr  = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));

   
   /* load the list of Xm strings for each duration */
   
   for (dur_index = 0; dur_index < cnt; dur_index++)
   {      
      
      /* build the display line based on the value type,
	 the ffm_stat values are passed globally */
      
      bld_detailed_line(area_index, dur_index, buf);
      
      xmStr[dur_index] = XmStringCreateSimple(buf);
   }
   
   
   /* load XmList widget and select first position */

   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(ffm_detailsLS, arg, ac);


   /* cleanup and return */

   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
   
   return;
}


/************************************************************************
   
   bld_detailed_line()
   
   *********************************************************************/

void bld_detailed_line(int			area_index,
		       int			dur_index,
		       char			*buf)
{
   char 	hr_str[3];
   char		precip_str[12], ffg_str[12];
   char		rate_str[12], percent_str[12], diff_str[12];
   char		flow_str[12], volume_str[12];
   char		max_str[12], min_str[12];
   double	flow_value, volume_value;
   double 	precip, area, conv_factor;
   
   
   /* now format the values accordingly */
   
   if (ffm_stats[area_index].dur_stats[dur_index].precip == MISSING)	 
      strcpy(precip_str,  "    m");
   else
      sprintf(precip_str, "%8.3f",
	      ffm_stats[area_index].dur_stats[dur_index].precip);
   
   if (ffm_stats[area_index].dur_stats[dur_index].ffg == MISSING)	 
      strcpy(ffg_str,  "    m");
   else
      sprintf(ffg_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].ffg);
   
   if (ffm_stats[area_index].dur_stats[dur_index].rate == MISSING)	 
      strcpy(rate_str,  "    m");
   else
      sprintf(rate_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].rate);
   
   if (ffm_stats[area_index].dur_stats[dur_index].percent == MISSING)	 
      strcpy(percent_str,  "    m");
   else
      sprintf(percent_str, "%6d %%",
	      (int )ffm_stats[area_index].dur_stats[dur_index].percent);
   
   if (ffm_stats[area_index].dur_stats[dur_index].diff == MISSING)	 
      strcpy(diff_str,  "    m");
   else
      sprintf(diff_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].diff);
   
   if (ffm_stats[area_index].dur_stats[dur_index].precip_max == MISSING)	 
      strcpy(max_str,  "    m");
   else
      sprintf(max_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].precip_max);
   
   if (ffm_stats[area_index].dur_stats[dur_index].precip_min == MISSING)	 
      strcpy(min_str,  "    m");
   else
      sprintf(min_str, "%8.2f",
	      ffm_stats[area_index].dur_stats[dur_index].precip_min);
   
   
   /* set the hour string */
   
   sprintf(hr_str, "%d", hr_durs[dur_index]);
   
   
   /* set the flow string. the precip units are inches, the area units
      are in sq miles. the flow is in cfs */
   
   if (ffm_stats[area_index].dur_stats[dur_index].precip == MISSING)
   {
      strcpy(flow_str,   "    m");
      strcpy(volume_str, "    m");
   }
   else
   {
      precip = ffm_stats[area_index].dur_stats[dur_index].precip;
      area   = ffm_stats[area_index].area;
      
      conv_factor = 645.3333;  /* = (5280*5280) / (12 * 3600) for cfs units */
      
      flow_value = (precip * area / (double )hr_durs[dur_index]) * conv_factor;  
      sprintf(flow_str, "%8.1f", flow_value);
      
      
      /* determine the volume also */
      
      conv_factor = 2323.2;    /* = (5280*5280) / (12 * 1000.) for kcf units */
      
      volume_value = (precip * area * conv_factor);  
      sprintf(volume_str, "%8.1f", volume_value);
   }
   
   
   /* only consider those groups that match the selected criteria */
   
   sprintf(buf, "%-2s    %-8s   %-8s   %-8s   %-8s   %-8s   %-8s   %-8s",
	   hr_str, precip_str, ffg_str, percent_str, diff_str,
	   rate_str,  max_str, min_str);
   
   return;
}


/************************************************************************

   close_ffm_summary()
   Closes the ffm_summary window.
   
   *********************************************************************/

void close_ffm_summary()
{
   /* free the allocated memory */
   
   free_summaryprods();
   
   free(ffm_stats);
      
   free(area_listindex);
   listindex_malloced = 0;
   
   free(ffm_radar_ids);
   
   /* close the window */
   
   XtUnmanageChild(ffm_summaryDS);
   
   return;
}


/************************************************************************
   
   free_summaryprods()
   
   *********************************************************************/

void free_summaryprods()
{
   int 	i;
   
   
   for (i = 0; i < NUM_SUMMARY_DURS; i++)
   {      
      if (products[i].precip_grid != NULL) free(products[i].precip_grid);
      if (products[i].ffg_grid != NULL)    free(products[i].ffg_grid);
   }
   free(products);
   
   
   return;
}

/************************************************************************

   init_summary_radar()   
   Load the option menu with the list of radars with 
   available data and load 
   
   *********************************************************************/

int init_summary_radar(char	*radid)
{
   UniqueList   *ulHead, *ulPtr;
   int 		num_radars;
   int		cnt, radar_index;
   Widget	radaridPB;
   char		primary_radar[128];
   int		gad_token_len=0, gad_value_len=0;   
   
   
   /* initialize */
   
   num_radars = 0;
   memset(radid, 0, RADAR_ID_LEN + 1);   
   
   
   /* get the list of unique radars from the table containing 
      the input data grids */
      
   ulHead = LoadUnique("radid", "LatestAccumGrid", " ", &cnt);
   if (cnt == 0)
   {
      fprintf(stderr, "No areas defined in LatestAccumGrid.\n");
      return(num_radars);
   }
   
   else
      num_radars = cnt;
   
   
   /* allocate memory for the list of radars */
   
   ffm_radar_ids = (FfmRadarid *)malloc(sizeof(FfmRadarid) * num_radars);
   if (ffm_radar_ids == NULL)
      fprintf(stderr, "Failed malloc of ffm_radar_ids");
   
   
   /* get the name of the primary radar */
   
   gad_token_len = strlen("whfs_primary_radar");
   get_apps_defaults("whfs_primary_radar", &gad_token_len, primary_radar, &gad_value_len);
   if (strlen(primary_radar) <= 0)
      fprintf(stderr,
	      "whfs_primary_radar not defined for summary radar list.\n");
   
   
   /* clean out any existing entries from the option menu */
   
   DestroyChildren(ffm_radarPDM);
   
   
   /* load up the option menu with the list of radars.
      initialize the radar in the event the primary radar 
      not found, or is not in the list */

   cnt = radar_index = 0;
   strncpy(radid, ulHead->uchar, RADAR_ID_LEN);
   
   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   while (ulPtr != NULL)
   {
      memset(&ulPtr->uchar[RADAR_ID_LEN], 0, 1);
      
      radaridPB =
	 XtVaCreateManagedWidget(ulPtr->uchar, xmPushButtonWidgetClass,
				 ffm_radarPDM, NULL);
      
      
      /* add callbacks also while the pushbuttons are being created */
      
      XtAddCallback(radaridPB, XmNactivateCallback, new_datasetsCB, NULL);
      
      
      /* check if this is the primary radar */
      
      if (primary_radar != NULL &&
	  strcmp(ulPtr->uchar, primary_radar) == 0)
      {
	 strcpy(radid, ulPtr->uchar);
	 radar_index = cnt;
      }
      
      
      /* add callbacks also while the pushbuttons are being created */
      
      XtAddCallback(ffm_gageradarPB, XmNactivateCallback, new_datasetsCB, NULL);
      
      
      /* load the id into memory;  fix 6/23/98*/
      
      strcpy(ffm_radar_ids[cnt], ulPtr->uchar);
      
      
      /* increment the count, and get the next item */
      
      cnt++;
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
   }
   
   FreeUnique(ulHead);  
   
   
   /* select the radar, whether it is the primary radar as found
      above, or is the first one in the list */
   
   SetMenuPos(ffm_radarOM, radar_index);
   
   
   return(num_radars);
}


/*********************************************************************

   set_summary_filter()
   Sets the option menus for the list filter options.
   
   *********************************************************************/

void set_summary_filter(summary_area_filter	area_filter,
			summary_value_filter	value_filter,
			float			value)
{
   char	valuestr[10];
   
   /* set the area filter option menu */
   
   if (area_filter == BASIN)
      SetMenuPos(ffm_areasOM, 0);
   
   else if (area_filter == ZONE)
      SetMenuPos(ffm_areasOM, 1);
   
   else if (area_filter == COUNTY)
      SetMenuPos(ffm_areasOM, 2);
   
   else
      fprintf(stderr, "Invalid area_filter option specified.\n");
      
   
   /* set the value compare option menu */
   
   if (value_filter == VALUE_GE)
   {
      SetMenuPos(ffm_valcompOM, 0);
      Sensitize(ffm_valcompareTX);
   }
   
   else if (value_filter == VALUE_LE)
   {
      SetMenuPos(ffm_valcompOM, 1);
      Sensitize(ffm_valcompareTX);
   }
   
   else if (value_filter == VALUE_ALL)
   {
      SetMenuPos(ffm_valcompOM, 3);
      DeSensitize(ffm_valcompareTX);
   }
   
   else
      fprintf(stderr, "Invalid value_filter option specified.\n");
   
   
   sprintf(valuestr, "%7.2f", value);
   XmTextSetString(ffm_valcompareTX, valuestr);
   
   
   return;
}


/*********************************************************************

   set_summary_sort()
   Sets the option menu for the list sort option. 
   
   *********************************************************************/

void set_summary_sort(summary_sort 	flag)
{

   if (flag == BY_VALUE)
      SetMenuPos(ffm_sortOM, 0);
   
   else if (flag == BY_ID)
      SetMenuPos(ffm_sortOM, 1);
   
   else if (flag == BY_NAME)
      SetMenuPos(ffm_sortOM, 2);
   
   else
      fprintf(stderr, "Invalid summary sort option %d\n", flag); 
   
   return;
}


/*********************************************************************

   set_summary_valuetype()
   Sets the proper button in the value type radio box.
   
   *********************************************************************/

void set_summary_valuetype(summary_value_type 	value_type)
{

   if (value_type == USE_RATE)
   {
      XmToggleButtonGadgetSetState(ffm_rate_valueTB,    1, 0);   
      XmToggleButtonGadgetSetState(ffm_percent_valueTB, 0, 0);   
      XmToggleButtonGadgetSetState(ffm_diff_valueTB,    0, 0);
   }
   else if (value_type == USE_PERCENT)
   {
      XmToggleButtonGadgetSetState(ffm_rate_valueTB,    0, 0);   
      XmToggleButtonGadgetSetState(ffm_percent_valueTB, 1, 0);   
      XmToggleButtonGadgetSetState(ffm_diff_valueTB,    0, 0);
   }
   
   else if (value_type == USE_DIFFERENCE)
   {
      XmToggleButtonGadgetSetState(ffm_rate_valueTB,    0, 0);   
      XmToggleButtonGadgetSetState(ffm_percent_valueTB, 0, 0);   
      XmToggleButtonGadgetSetState(ffm_diff_valueTB,    1, 0);
   }
   
   else
      fprintf(stderr, "Invalid summary value type specified %d.\n",
	      value_type);
   
   
   return;
}


/************************************************************************

   find_gridtype()
      
   *********************************************************************/

int find_gridtype(PrecipType	*gridtype)
{
   int	menuitem;
   int	status = 0;
   
   
   /* get the current value of the option menu indicating which 
      precip type to use.  note that the numbers begin at count 0 */
   
   menuitem = GetMenuPos(ffm_datatypeOM);
   
   if (menuitem == 0)
      *gridtype = STAGE1_PRECIP;
   
   else if (menuitem == 1)
      *gridtype = STAGE2_GAGE_ONLY_PRECIP;
   
   else if (menuitem == 2)
      *gridtype = STAGE2_GAGE_RADAR_PRECIP;
   
   else
   {
      status = -1;
      fprintf(stderr, "Invalid gridtype item selected %d\n.", menuitem);
      *gridtype = STAGE2_GAGE_ONLY_PRECIP;
   }
   
   return(status);
}


/************************************************************************

   find_summary_radar()
      
   *********************************************************************/

int find_summary_radar(char	*radarid)
{
   int	menuitem;
   int	status = 0;
   
   
   /* get the current value of the option menu indicating which 
      precip type to use.  note that the numbers begin at count 0 */
   
   menuitem = GetMenuPos(ffm_radarOM);

   strcpy(radarid, ffm_radar_ids[menuitem]);
      
   return(status);
}


/************************************************************************

   find_summary_valuetype()   
   
   *********************************************************************/

int find_summary_valuetype(summary_value_type *value_type)
{
   int		status;
   
   /* initialize */
   
   *value_type = USE_RATE;
   status = -1;
   
   
   /* find the current value */
   
   if (XmToggleButtonGetState(ffm_rate_valueTB))
   {
      *value_type = USE_RATE;
      status = 0;
   }
   
   else if (XmToggleButtonGetState(ffm_percent_valueTB))
   {
      *value_type = USE_PERCENT;
      status = 0;
   }
   
   else if (XmToggleButtonGetState(ffm_diff_valueTB))
   {
      *value_type = USE_DIFFERENCE;
      status = 0;
   }
   
   
   return(status);
}


/************************************************************************

   find_area_filter()
      
   *********************************************************************/

int find_area_filter(summary_area_filter *area_filter)
{
   int	menuitem;
   int	status = 0;
   
   
   /* get the current value of the option menu indicating which 
      area filter to use.  note that the numbers begin at count 0 */
   
   menuitem = GetMenuPos(ffm_areasOM);
   
   if (menuitem == 0)
      *area_filter = BASIN;
   
   else if (menuitem == 1)
      *area_filter = ZONE;
   
   else if (menuitem == 2)
      *area_filter = COUNTY;
   
   else
   {
      status = -1;
      fprintf(stderr, "Invalid area_filter item selected %d\n.", menuitem);
      *area_filter = BASIN;
   }
   
   return(status);
}


/************************************************************************

   find_value_filter()
      
   *********************************************************************/

int find_value_filter(summary_value_filter 	*value_filter,
		      float			*compare_value)
{
   int		menuitem;
   int		status = 0;
   char		*valuestr;
   int		numitems;
   float 	value;
   
   
   /* first get the option menu setting */
   
   menuitem = GetMenuPos(ffm_valcompOM);
   
   if (menuitem == 0)
      *value_filter = VALUE_GE;
   
   else if (menuitem == 1)
      *value_filter = VALUE_LE;
   
   else if (menuitem == 3)
      *value_filter = VALUE_ALL;
   
   else
   {
      status = -1;
      fprintf(stderr, "Invalid value_filter item selected %d\n.", menuitem);
      *value_filter = VALUE_GE;
   }
   
   
   /* now get the actual numeric value. if there is an error reading the
      string then reset it */
   
   valuestr = XmTextGetString(ffm_valcompareTX);
   numitems = sscanf(valuestr, "%f", &value);
   
   if (numitems != 1 || value > 1000. || value < -1000.)
   {
      fprintf(stderr, "Error reading summary compare value %s. Resetting\n",
	      valuestr);
      sprintf(valuestr, "%7.2f", 0.00);
      XmTextSetString(ffm_valcompareTX, valuestr);
   }
   
   else
      *compare_value = value;
   
   if (valuestr != NULL) XtFree((char *)valuestr);
   
   return(status);
}


/************************************************************************

   find_summary_sort()
      
   *********************************************************************/

int find_summary_sort(summary_sort *sort_flag)
{
   int	menuitem;
   int	status = 0;
   
   
   /* get the current value of the option menu indicating which 
      area filter to use.  note that the numbers begin at count 0 */
   
   menuitem = GetMenuPos(ffm_sortOM);
   
   if (menuitem == 0)
      *sort_flag = BY_VALUE;
   
   else if (menuitem == 1)
      *sort_flag = BY_ID;
      
   else if (menuitem == 2)
      *sort_flag = BY_NAME;
   
   else
   {
      status = -1;
      fprintf(stderr, "Invalid sort_flag item selected %d\n.", menuitem);
      *sort_flag = BY_ID;
   }
   
   
   return(status);
}


/************************************************************************

   blank_summary_list()
     
   *********************************************************************/

void blank_summary_list()
{
   Arg                  arg[40];
   int                  ac;
   int                  cnt;
   
   cnt = 0;
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetValues(ffm_listLS, arg, ac);
   
   return;
}


/************************************************************************
   
   blank_summary_details()
   
   *********************************************************************/

void blank_summary_details()
{
   Arg                  arg[40];
   int                  ac;
   int                  cnt;
   
   cnt = 0;
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetValues(ffm_detailsLS, arg, ac);

   
   SetLabel(ffm_detailsframeLB, "");
   
   return;
}


/************************************************************************

   format_sumtime()
   Formats a time string for output.
   
   *********************************************************************/

char *format_sumtime(time_t timeval)
{
   static char outstr[80];
   struct tm *tm_struct;
   
   tm_struct = gmtime(&timeval);
   strftime(outstr, 80, "%H:%M %a %m/%d", tm_struct); 
   
   return(outstr);
}



/*********************************************************************

                =========================================
   ========================= FFM SUMMARY CALLBACKS ================
                =========================================     
   
   *********************************************************************/

void add_ffm_summary_callbacks(void)
{
   Atom wmAtom;
      
   /* window frame callback */
   
   wmAtom = XmInternAtom(XtDisplay(ffm_summaryDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(ffm_summaryDS, wmAtom, ok_ffm_summaryCB, NULL);
      
   
   /* button callbacks for data grid option menu. the same callback
      is used when reselecting a radar.  however, since the radar 
      selections are set dynamically, the callbacks are defined
      as part of that operation of adding the push button widgets
      for the radar options */

   XtAddCallback(ffm_radarPB,     XmNactivateCallback, new_datasetsCB, NULL);
   XtAddCallback(ffm_gagePB,      XmNactivateCallback, new_datasetsCB, NULL);
   XtAddCallback(ffm_gageradarPB, XmNactivateCallback, new_datasetsCB, NULL);
   
   
   /* button callbacks for area filter option menu this one is unique
      in that it forces a recompute of the stats */

   XtAddCallback(ffm_basinPB,  XmNactivateCallback, area_filterCB, NULL);
   XtAddCallback(ffm_zonePB,   XmNactivateCallback, area_filterCB, NULL);
   XtAddCallback(ffm_countyPB, XmNactivateCallback, area_filterCB, NULL);

   
   /* button callbacks for all other filter options and settings that would
      force an unpdate of the displayed list */
         
   XtAddCallback(ffm_valcompgePB,  XmNactivateCallback, summary_list_updateCB,
		 NULL);
   XtAddCallback(ffm_valcomplePB,  XmNactivateCallback, summary_list_updateCB,
		 NULL);
   XtAddCallback(ffm_valcompallPB,  XmNactivateCallback, summary_list_updateCB,
		 NULL);
   
   
   XtAddCallback(ffm_valcompareTX, XmNlosingFocusCallback, summary_list_updateCB,
		 NULL);
   XtAddCallback(ffm_valcompareTX, XmNactivateCallback, summary_list_updateCB,
		 NULL);
   
   
   XtAddCallback(ffm_sort_valuePB, XmNactivateCallback, summary_list_updateCB,
		 NULL);
   XtAddCallback(ffm_sort_idsPB,   XmNactivateCallback, summary_list_updateCB,
		 NULL);
   XtAddCallback(ffm_sort_namePB,  XmNactivateCallback, summary_list_updateCB,
		 NULL);
      
   
   XtAddCallback(ffm_rate_valueTB,    XmNvalueChangedCallback, 
		 value_type_radioCB, NULL);   
   XtAddCallback(ffm_percent_valueTB, XmNvalueChangedCallback, 
		 value_type_radioCB, NULL);   
   XtAddCallback(ffm_diff_valueTB,    XmNvalueChangedCallback, 
		 value_type_radioCB, NULL);   

   
   /* List callbacks */
   
   XtAddCallback(ffm_listLS, XmNdefaultActionCallback,   import_ffm_summary,
		 NULL);
   XtAddCallback(ffm_listLS, XmNbrowseSelectionCallback, import_ffm_summary,
		 NULL);

   
   /* callbacks for window buttons */
   
   XtAddCallback(ffm_exitPB,   XmNactivateCallback, ok_ffm_summaryCB, NULL);
   
   return;
}


/***************************************************************

   new_datasetsCB()
   if the data source is reset in terms of either a different grid
   source or a different radar, then the data for the areas must
   obtained and the derived data must be recomputed.
   
   *************************************************************/

void new_datasetsCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   ArealProductTypeDescriptor	prod_descr;       
   ArealProductSpecifier	prod_spec;
   
   
   /* load up the product descriptor and specifier based on the
      current settings */
   
   find_prod_info(&prod_descr, &prod_spec);
   
   
   /* now get the summary data for this data set after freeing
      up the existing data */
   
   free_summaryprods();
   load_summary_data(prod_descr, prod_spec);
   
   
   /* load the text boxes describing the data just retrieved */
   
   set_summary_prodinfo(prod_descr, prod_spec);
   
   
   /* free the memory allocated for the areas
      and compute the derived data from the retrieved data.
      this function allocates the memory it needs.
      the ffm_stats, products, num_areas, hr_durs are retined
      globally in this file. */   
   
   free(ffm_stats);
   num_areas = 0;
   
   ffm_stats = bld_summary_stats(prod_descr, prod_spec, products, &num_areas,
				 NUM_SUMMARY_DURS, hr_durs);
   
   
   /* now update the list of areas after getting the current value of
      the settings indicating how to load the list */
   
   summary_list_updateCB(w, NULL, NULL);
   
   
   return;
}


/***************************************************************
   
   area_filterCB()
   if the area filter is reset, then the data for the areas must
   be re-derived. find the prod_descr and prod_spec value.
   
   *************************************************************/

void area_filterCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   ArealProductTypeDescriptor	prod_descr;       
   ArealProductSpecifier	prod_spec;
   
   /* load up the product descriptor and specifier based on the
      current settings */
   
   find_prod_info(&prod_descr, &prod_spec);
   
   
   /* free the memory allocated for the areas
      and compute the derived data from the retrieved data.
      this function allocates the memory it needs.
      the ffm_stats, products, num_areas, hr_durs are retined
      globally in this file. */   

   free(ffm_stats);
   num_areas = 0;
   
   ffm_stats = bld_summary_stats(prod_descr, prod_spec, products, &num_areas,
				 NUM_SUMMARY_DURS, hr_durs);
   
   
   /* now update the list of areas after getting the current value of
      the settings indicating how to load the list */
      
   summary_list_updateCB(w, NULL, NULL);
   
   
   return;
}


/***************************************************************
   
   find_prod_info()
   find the info from the gui settings to load into the
   product descriptor and specifier
   
   *************************************************************/

void find_prod_info(ArealProductTypeDescriptor	*prod_descr,       
		    ArealProductSpecifier	*prod_spec)   
{
   summary_area_filter		area_filter;
   int				status;
   PrecipType			gridtype;
   char				radar_to_use[RADAR_ID_LEN + 1];
   
   
   /* initialize these fields, even though they are unused */
   
   prod_descr->mode = COMPARISON_MODE;   
   prod_spec->endTime  = 0;
   prod_spec->duration = 0;
   
   
   /* now initialize the important fields based on the current settings.
      the radar_to_use is a global variables */
   
   status = find_area_filter(&area_filter);
   if (area_filter == BASIN)
      prod_descr->resolutionLevel = BASIN_RES;
   
   else if (area_filter == ZONE)
      prod_descr->resolutionLevel = ZONE_RES;
   
   else if (area_filter == COUNTY)
      prod_descr->resolutionLevel = COUNTY_RES;
   
   
   status = find_gridtype(&gridtype);
   prod_descr->precipType = gridtype;
   
   
   status = find_summary_radar(radar_to_use);
   strcpy(prod_spec->sourceId, radar_to_use);
   
   return;
}


/***************************************************************
   
   *************************************************************/

void value_type_radioCB(Widget w,
			XtPointer ptr, 
			XmToggleButtonCallbackStruct *cbs)
{
   int			toggle_set;
   
   toggle_set = cbs->set;
      
   /* for radio boxes, this callback is called twice, once when
      turning off the previously turned on choice, and then again
      when turning on the new choice.  to avoid going thru the
      callback processing twice, check to see they button sete
      to allow skipping the first of the two times */
   
   if (toggle_set == 1)
      summary_list_updateCB(w, NULL, NULL);
   
   return;
}


/***************************************************************

   general function for controlling the update of the summary
   list based on the current settings
   
   *************************************************************/

void summary_list_updateCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   summary_area_filter	area_filter;
   summary_value_filter	value_filter;
   float		compare_value;
   summary_value_type 	value_type;
   summary_sort		sort_flag;
   int			status;
   
   
   /* get the current value of the setting which indicate how
      to load the list.  also set the sensitivity of the 
      value compare text field in the event this callback
      is invoked because of a value filter was selected */
   
   status = find_area_filter(&area_filter);
   
   status = find_value_filter(&value_filter, &compare_value);
   if (value_filter != VALUE_ALL)
      Sensitize(ffm_valcompareTX);
   else
      DeSensitize(ffm_valcompareTX);
      
   status = find_summary_valuetype(&value_type);
   status = find_summary_sort(&sort_flag);
   
   
   /*  load the list with the new settings */
   
   load_ffmsummary_list(area_filter, value_filter, compare_value,
			sort_flag, value_type);
   
   return;
}
   

/***************************************************************

   callback on the summary list selection, which results in the
   detailed list being loaded 
   
   *************************************************************/
   
void import_ffm_summary(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
   int	area_index, itemnum;
   char	frame_str[40];

   
   /* get currently selected list position and determine the
      forecast point it is for */

   itemnum = cbs->item_position;
   area_index = area_listindex[itemnum - 1];
  
   
   /* set the label of the frame */
   
   sprintf(frame_str, "Details for %s - %s",
	   ffm_stats[area_index].area_id, ffm_stats[area_index].name);
   SetLabel(ffm_detailsframeLB, frame_str);
   
      
   /* load the details for the selected area into the other list */
      
   load_detailed_list(area_index);

   
   return;
}


/***************************************************************
   
   callback on the close button
   
   *************************************************************/

void ok_ffm_summaryCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   close_ffm_summary();
   
   return;
}



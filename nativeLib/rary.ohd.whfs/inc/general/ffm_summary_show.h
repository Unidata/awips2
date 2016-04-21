/***************************************************************
   ffm_summary_ca.h

   ***************************************************************/

#ifndef FFM_SUMMARY_CA_H
#define FFM_SUMMARY_CA_H

#include "ArealProductSettings.h"
#include "FfmSummary.h"
#include "DbmsDefs.h"

typedef enum _summary_area_filter
   {
   BASIN,
   COUNTY,
   ZONE
   } summary_area_filter;

typedef enum _summary_value_filter
   {
   VALUE_GE,
   VALUE_LE,
   VALUE_SEPARATOR,
   VALUE_ALL
   } summary_value_filter;

typedef enum _summary_sort
   {
   BY_VALUE,
   BY_ID,
   BY_NAME
   } summary_sort;

typedef enum _summary_value_type
   {
   USE_RATE,
   USE_PERCENT,
   USE_DIFFERENCE
   } summary_value_type;

typedef char	FfmRadarid[RADAR_ID_LEN + 1];


/*  action prototypes */

//void show_ffm_summary(Widget 	w);

void init_summary_display();

void load_summary_data(ArealProductTypeDescriptor	prod_descr,
		       ArealProductSpecifier		prod_spec);

void set_summary_prodinfo(ArealProductTypeDescriptor	prod_descr,
			  ArealProductSpecifier		prod_spec);


void load_ffmsummary_list(summary_area_filter 	area_filter,
			  summary_value_filter	value_filter,
			  float			value,
			  summary_sort 		sort_flag,
			  summary_value_type	value_type);

int filter_summary_list(summary_value_type	value_type);

void sort_summary_list(summary_value_type	value_type);

void bld_summary_line(int			area_index,
		      summary_value_type	value_type,
		      char			*buf);

void load_detailed_list(int	area_index);
void bld_detailed_line(int			area_index,
		       int			dur_index,
		       char			*buf);

void close_ffm_summary(void);

void free_summaryprods();

int init_summary_radar(char *radid);

void set_summary_filter(summary_area_filter	area_filter,
			summary_value_filter	value_filter,
			float			value);


void set_summary_sort(summary_sort 	flag);
int compare_ffm_id(const void *elem1,
		   const void *elem2);
int compare_ffm_name(const void *elem1,
		     const void *elem2);
int compare_ffm_rate(const void *elem1,
		     const void *elem2);
int compare_ffm_percent(const void *elem1,
			const void *elem2);
int compare_ffm_difference(const void *elem1,
			   const void *elem2);


void set_summary_valuetype(summary_value_type 	value_type);

int find_gridtype(PrecipType	*gridtype);
int find_summary_radar(char	*radarid);

int find_summary_valuetype(summary_value_type *value_type);
int find_area_filter(summary_area_filter *area_filter);
int find_value_filter(summary_value_filter 	*value_filter,
		      float			*compare_value);
int find_summary_sort(summary_sort *sort_flag);

void blank_summary_list();
void blank_summary_details();

char *format_sumtime(time_t timeval);



/* callback prototypes  ----------------------------------------- */

void add_ffm_summary_callbacks(void);

void new_datasetsCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);

void area_filterCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);

void find_prod_info(ArealProductTypeDescriptor	*prod_descr,
		    ArealProductSpecifier	*prod_spec);

/* void value_type_radioCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);
void value_type_radioCB(Widget w, XtPointer ptr, XmToggleButtonCallbackStruct *cbs); */
void value_type_radioCB();

void summary_list_updateCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);

void ok_ffm_summaryCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);

/* void import_ffm_summary(Widget w, XtPointer ptr, XmListCallbackStruct *cbs); !!! */
void	import_ffm_summary();


#endif

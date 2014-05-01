/* struct_defs.h */


/* AV 2/6/01 added sac_snow in non_univ_techniques_struct 
*/

#ifndef struct_defs_h
#define struct_defs_h

#include <X11/Intrinsic.h>

#define MAXPOLY 10
#define MAX_POINTS_IN_POLYGON  20



typedef struct HRAP {
	float   x;
	float   y;
	}       HRAP;

typedef struct {
	int     x;
	int     y;
	}       point;

typedef struct {
	char    id[20];
	char    SegmentID[9];   /* For MAP Basins only...       */
	char    name[21];       /* added for county names - dp 2/26/94 */
	int     npts;
	int     order;
	HRAP    *hrap;
	float   map[30];
	int     map_date;
	}       overlay_struct;


typedef struct {
	char    id[20];
	int     npts;
	HRAP    *hrap;
	}       county_struct;


typedef struct
	{
	char    name[30];
	char    river[30];
	char    id[9];
	float   Lat;
	float   Long;
	}       fcstpoint_struct;



typedef struct {
	short int      **data_array;
	int            maximum_columns, maximum_rows;
	int            num_levels;
	int            *levels;
	int            states_on;
	int            county_on;
	int            rivers_on;
	int            basins_on;
	int            rings_on;
	int            cities_on;
	int            forecastPoints_on;
	int            currentForecastGroup_on;
	int            fcstGroup_bounds_on;
	int            ssnum;
	point          origin;
	point          center;
	GC             *gc;
	GC             *Basin_gc;
	Pixmap         pix;
	Pixmap         pixbase;
	Pixmap         highlightedArea_pixmap;
	Region         selectedRegion;
	Region         HighlightedRegion;
	Region         ForecastGroupRegion;
	float          zoom_factor;
	Widget         w;
	Dimension      width;
	Dimension      height;
	}      draw_struct;



typedef struct
	{
	int     npoly;
	int     npoints[MAXPOLY];
	XPoint  points[MAXPOLY][MAX_POINTS_IN_POLYGON];
	GC      gc;
	Widget  shell;
	int     isite;
	int     xpoly,xpt;
	} rubber_poly_data;



typedef struct
	{
	int     start_x;
	int     start_y;
	int     last_x;
	int     last_y;
	GC      gc;
	Widget  w;
	} rubber_band_data;


typedef struct
	{
	XPoint  start;
	XPoint  last;
	GC      gc;
	Widget  w;
	} rubber_line_data;


typedef struct
	{
	short   center_x;
	short   center_y;
	XArc    circle[2];
	GC      gc;
	} circle_rubber_band_data;



typedef struct
	{
	Widget          query_shell;
	Widget          MAP_Basin_label;
	Widget          County_label;
	Widget          tools_shell;
	Widget          arrow_tool;
	Widget          marquee_tool;
	Widget          locator_tool;
	Widget          query_tool;
	Widget          circle_tool;
	Widget          scale_tool;
	Widget          zoomIn_tool;
	Widget          zoomOut_tool;
	} MappingWidgetStruct;


typedef struct _ZoomStruct
	{
	point                   origin;
	point                   center;
	point                   upperLeft;
	point                   lowerRight;
	int                     rows;
	int                     columns;
	struct  _ZoomStruct     *prevLevel;
	struct  _ZoomStruct     *nextLevel;
	} ZoomStruct;




#include "extra_e19.h"

typedef struct _non_univ_tech
	{
	int             snow;           
	int             frost;          
	int             upsc;           
	int             upwe;           
	int             sac_snow;           
	int             printsma;
	int             printsnw;
	int             prtro;
	int             tables;
	}       non_univ_techniques_struct;


typedef struct _e19
	{
	char            id[50];
	int             in_fgroup;
	char            name[9];
	char            description[21];
	int             num_MAP_basins; /* Number of MAP basins         */
	char            **MAP_names;    /* Array of MAP basin names     */
/*	extra_e19       extra;*/
	extra_e19       *extra;
	int NumRC; /*number rating curve or forecast points,or length of extra*/
	char   f_group[9]; /*this 4 variables moved from extra_e19.h*/
	char   c_group[9];
	char   upstream[5][9];
	char   downstream[2][9];
	}       e19_data;

typedef struct _node
	{
	int                     status;
	int                     computed_status;
	struct  _e19            e19;
	overlay_struct          **MAP_data;     /* An array of pointers to structs for each MAP basin   */
	Region                  segment_region;
	struct  _non_univ_tech  *techniques;
	Widget                  parent_widget;
	Widget                  segment_widget;
	Widget                  popup_shell;
	int                     which_tree;
	struct  _node           *parent;
	struct  _node           *left;
	struct  _node           *mid_left;
	struct  _node           *center;
	struct  _node           *mid_right;
	struct  _node           *right;
	}       node;



typedef struct
	{
	char            segment_name[9];
	int             status_id;
	}       seg_status;


typedef struct
	{
	Widget          on;
	Widget          off;
	Widget          no_change;
	}       buttonStruct;


typedef struct
	{
	Widget          forecastGroup_list;
	Widget          carryoverGroup_list;
	Widget          carryoverDate_list;
	XmString        *carryover_dates;
	}       FcstGroupSelectionDialogStruct;


typedef struct
	{
	Widget          shell;
	Widget          list;
	Widget          show;
	char            *filePath[2];   /* Array of two pointers to char,       */
					/* 1st gives the path to OFS Mods,      */
					/* 2nd gives the path to new Mods       */
	int             *arrayPosition; /* Array of ints;                       */
					/* for a given list item, identifies    */
					/* which path to follow to get the      */
					/* newest Mod of the selected type...   */
	}       CurrentModStruct;



typedef struct
	{
	Widget          toplevel;
	Widget          bb;
	Widget          run_info_bb;
	Widget          ForecastGroup_label;
	Widget          Scale_TextField;
	Widget          form_widget;
	Widget          menuBar;
	Widget          sw_for_delete_list;
	Widget          delete_list;
	Widget          delete_shell;
	Widget          tree_shell;
	Widget          version_shell;
	Widget          version_ToggleButton;
	Widget          sw_for_tree_widgets;
	Widget          rc_for_tree_widgets;
	Widget          run_info_shell;
	Widget          listWidget_for_forecastSegments;
	Widget          control_mainMenuItem;
	Widget          options_mainMenuItem;
	Widget          display_mainMenuItem;
	Widget          mods_mainMenuItem;
	Widget          begin_widget;
	Widget          new_ForecastGroup_widget;
	Widget          rerun_widget;
	Widget          next_widget;
	Widget          revert_widget;
	Widget          keepSubset_widget;
	Widget          deleteSegments_widget;
	Widget          reset_widget;
	Widget          setDates_widget;
	Widget          universal_widget;
	Widget          techniques_widget;
	Widget          non_universal_widget;
	Widget          showDeletedSegments_widget;
	Widget          showRunSegments_widget;
	Widget          showTimeSeriesTable_widget;
	Widget          showTulsaPlot_widget;
	Widget          showPlotTS_widget;
	Widget          showOtherMods_widget;
	Widget          showModsViewer_widget;
	Widget          showRatingCurve_widget;
	Widget          showOperationsTable_widget;
	Widget          showFGroup_Topology_widget;
	Widget          showCurrentMods_widget;
	Widget          setDates_popup_shell;
	the_widgets     *controls;
	the_widgets     *viewDates;
	Widget          universalTech_popup_shell;
	Widget          non_universalTech_popup_shell;
	Widget          universalTech_ok_widget;
	Widget          inputFrame_label;
	Widget          outputFrame_label;
	Widget          modsFrame_label;
	Widget          non_univ_swindow;
	Widget          non_univ_list;
	Widget          continue_widget;
	Widget          select_next_shellWidget;
	Widget          selectNext_listWidget;
	Widget          select_widget;
	node            *head[MAX_NUM_SUBGROUPS];
	Widget          all_toggleButton;
	Widget          selected_toggleButton;
	buttonStruct    snowButtons;
	buttonStruct    frostButtons;
	buttonStruct    upscButtons;
	buttonStruct    upweButtons;
	buttonStruct    sac_snowButtons;
	Widget          NWSRFS_working_shell;
	Widget          NWSRFS_stopped_shell;
	XtIntervalId    stopped_timeout_id;
	char            segment_selected[9];
	char            current_segment[9];
	char            *ForecastGroupNames;            /* 8 characters each            */
	int             NumForecastGroups;              /* Number of Forecast Groups... */
	char            selected_ForecastGroupName[9];  /* 9 characters, incld. '\0'... */
	Widget          previous_segment;
	Widget          go_to_widget;
	Widget          run_multiple_widget;
	Widget          currentSegment_widget;
	Widget          send_mods_shell;
	int             fg_label_height;
	char            *current_command;
	Widget          non_universalTech_ok_widget;
        
	Widget          popupHelp_shell;
	Cursor          cursor;
	Widget          FcstGroup_selectionBoxShell;
	Widget          FcstGroup_selectionBoxCancel;
	Widget          files_date_label;
	Widget          mods_date_label;
	int             GoToNextForecastGroup;
	FcstGroupSelectionDialogStruct  *dataStruct;
	MappingWidgetStruct             *mappingStruct;
	circle_rubber_band_data         *CircleToolData;
	rubber_line_data                *ScaleToolData;
	GC                              ScaleToolData_gc;
	ZoomStruct      *zoom;
	draw_struct     *overlays;                  /* formerly global 'rad_data'   */
	Widget          main_canvas;                /* Start mapping widgets        */
	Widget          drawArea_SWindow;           /*         .                    */
	Widget          Latitude_widget;            /*         .                    */
	Widget          Longitude_widget;           /*         .                    */
	Widget          Distance_widget;            /*         .                    */
	Widget          states_widget;              /*         .                    */
	Widget          county_widget;              /*         .                    */
	Widget          rivers_widget;              /*         .                    */
	Widget          cities_widget;              /*         .                    */
	Widget          basin_boundaries_widget;    /*         .                    */
	Widget          forecastPoints_widget;      /*         .                    */
	Widget          FcstGroup_widget;           /*         .                    */
	Widget          FcstGroupBoundaries_widget; /* End mapping widgets          */
	CurrentModStruct        *currentModData;
        Widget          sac_widget;
        Widget          snow_widget;
	}       the_widget_struct;




 typedef struct
	{
	the_widget_struct       *dataStruct;
	node                    *branch;
	}       tree_data_struct;



node    *null_it();
node    *find_it();

non_univ_techniques_struct      *non_univ_Techniques;

rubber_band_data                rbdata;

draw_struct                     *rad_data;
draw_struct                     zoom_data;

overlay_struct                  **bound;
overlay_struct                  **fgbasin;
overlay_struct                  **mapbasin;
overlay_struct                  **river;
overlay_struct                  **state;
overlay_struct                  **county;

fcstpoint_struct                *forecastpoints;

HRAP                            LatLongToHrap();
HRAP                            HrapToLatLong();


#ifndef READ_DATA_DEF
#define READ_DATA_DEF

overlay_struct                  **read_overlay_data();

#endif /* READ_DATA_DEF         */

#endif /* struct_defs_h */

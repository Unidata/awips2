/*  mod_struct.h   - last changed 11/06/90 */

#ifndef mod_struct_h
#define mod_struct_h

#define MAX_RUN_PERIOD  744



typedef struct
	{
	Widget          newMods;
	Widget          modsFromFile;
	Widget          modsFromOFS;
	}       mod_viewer_Widgets;


typedef struct
	{
	Widget          menu_widget;
	Widget          shell;
	Widget          delete;
	Widget          un_delete;
	Widget          text_widget[100];
	}       viewer_struct;



typedef struct
	{
	XmString        operationType[20];
	XmString        operationName[20];
	}       mod_list_Widgets;

typedef struct
	{
	float           lower_warning_limit;
	float           upper_warning_limit;
	float           lower_error_limit;
	float           upper_error_limit;
	int             lower_warning_inclusive;
	int             upper_warning_inclusive;
	int             lower_error_inclusive;
	int             upper_error_inclusive;
	}       mod_limits_data;

#include "mod_data.h"
	
#include "ifp_struct.h"

typedef struct
	{
	Widget                  toplevel;
	Widget                  do_mods;
	Widget                  mods_popup_shell;
	Widget                  text_widget;
	Widget                  warning_shell;
	Widget                  errorMessageWidget;
	Widget                  modListWidget;                          /*     N E W      */
	Widget                  nextTSDate;                             /* added 05/29/90 */
	Widget                  tsPopupShell;                           /* added 06/04/90 */
	Widget                  tooManyQMeans_shell;                    /* added 10/25/90 */
	the_widgets             *tsDateWidgets[MAX_RUN_PERIOD];         /* added 06/07/90 */
	int                     currentTSWidget;                        /* added 06/08/90 */
	int                     prevTSWidget;                           /* added 06/08/90 */
	date                    *tsStartDate;                           /* added 06/12/90 */
	date                    *tsEndObsDate;                          /* added 09/11/90 */
	date                    *tsEndDate;                             /* added 08/08/90 */
	date                    *nextTSdate;                            /* added 12/14/90 */
	Widget                  arrowDateUp;                            /* added 06/13/90 */
	Widget                  arrowDateDown;                          /* added 06/13/90 */
	the_widgets             *runDisplay;                            /* added 06/13/90 */
	Widget                  tsControl;                              /* added 06/13/90 */
	Widget                  nextTS;                                 /* added 06/13/90 */
	mod_data                *modsDataStruct;                        /* added 06/13/90 */
	Widget                  tablePlotControl;                       /* added 06/15/90 */
	Widget                  modStart_bb;                            /* added 07/05/90 */
	Widget                  modEnd_bb;                              /* added 07/05/90 */
	char                    *currentModName;                        /* added 07/06/90 */
	mod_list_Widgets        *modOperations;
	float                   *d_float_array;                         /* added 08/08/90 */
	float                   *p_float_array;
	char                    **p_char_array;
	float                   *ts_float_array;                        /* added 05/15/90 */
	char                    **ts_char_array;                        /* added 05/15/90 */
	Widget                  cancelLastMod;                          /* added 11/06/90 */
	Widget                  noTSDatesShell;                         /* added 11/06/90 */
	Widget                  ts_popup_bb;                            /* added 11/08/90 */
	XmString                xmDateString[MAX_RUN_PERIOD];           /* added 12/04/90 */
	Widget                  dateListWidget;                         /* added 12/04/90 */
	Widget                  ts_DateList;                            /* added 12/14/90 */
	Widget                  ts_FlowsList;                           /* added 12/14/90 */
	Widget                  mods_menuBar;                           /* added 01/28/91 */
	Widget                  save_mods;                              /* added 01/30/91 */
	char                    *nextModName;                           /* added 01/31/91 */
	Widget                  ok_to_save_button;                      /* added 02/01/91 */
	Widget                  mod_not_saved_warning;                  /* added 02/03/91 */
	Widget                  new_mods_listWidget;                    /* added 02/21/91 */
	Widget                  mods_viewer_shell;                      /* added 02/21/91 */
	Widget                  show_all_of_type_widget;                /* added 03/26/91 */
	Widget                  show_all_widget;                        /* added 03/26/91 */
	Widget                  show_widget;                            /* added 03/26/91 */
	Widget                  remove_widget;                          /* added 03/26/91 */
	Widget                  mods_scale_widget;                      /* added 04/30/91 */
	Widget                  help_shell;                             /* added 06/06/91 */
	mods_plot_struct        *mods_plot_struct_pointer;              /* added 09/03/91 */
	Widget                  oldMods_button;                         /* added 09/09/91 */
	mod_viewer_Widgets      *viewer_widgets;                        /* added 09/19/91 */
	viewer_struct           *modsFromFile_data;                     /* added 09/20/91 */
	}       some_display_widgets;

#endif


/*****************************************************************************
* FILENAME:            choose_rfcwide_date.c   
* NUMBER OF MODULES:   4
* GENERAL INFORMATION:
*   MODULE 1:          display_rfcwide_date_window   
* DESCRIPTION:         This routine displays window providing user selection  
*                      of which hour to run.    
*                   
*   MODULE 3:          add_date_window_callbacks  
* DESCRIPTION:         This routine redisplays choose date window with dates and    
*                      times of Last Saved and Last Input columns  
*                      updated.                                        
*
*   MODULE 4:          create_working_dialog_RFCW
* DESCRIPTION:         This routine  creates message box informing user which 
*		       process is running when the main application is launched.
*
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell-Linux
* MODIFICATION HISTORY:
*   MODULE #    DATE             PROGRAMMER        DESCRIPTION/REASON
*          
*      4       March 1, 2002    Bryon Lawrence  Revision, sequence of appearing 
*      			      Moria Shebsovich  popup windows when the 
*                                               application is launched first 
*					        was changed: first appears the 
*					        popup message box, then main 
*					        application window, then 
*						choosing date and time via menu 
*					        item MpeControl-> Choose Hour.
*******************************************************************************
*/

#include <time.h>
#include <Xm/ComboBox.h>
#include <Xm/List.h>
#include <Xm/Protocols.h>
#include <stdlib.h>

#include "newhour_RFCW.h"
#include "quit_rfcwide.h"
#include "read_precip_data.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"

#include "choose_date.h"
#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"
#include "time_defs.h"
#include "arrow_button_callbacks.h"
#include "gageqc_gui.h"
#include "Xtools.h"
#include "choose_rfcwide_date.h"
#include "rfcwide_callbacks.h"
#include "map.h"
#include "mpe_log_utils.h"
#include "post_functions.h"

#define GUI_WIDTH 340 
#define GUI_HEIGHT 620
#define BUTTON_WIDTH 80


extern char temp_date[5];
extern char temp_month[5];
extern char temp_day[5];
extern char temp_hour[5];
extern char temp_num_of_days[5];
extern char restore_on_cancel_num_of_days[5];
extern int dates_struct_count;
extern int restore_on_cancel_dates_struct_count;
static int check = 0;
static int unregister_check = 0;
extern char save_last[MAX_GAGEQC_DAYS * HOURS_PER_DAY][25];
extern char exec_last[MAX_GAGEQC_DAYS * HOURS_PER_DAY][25];
extern char save_manual[MAX_GAGEQC_DAYS * HOURS_PER_DAY][25];

extern struct _dqc_run_date dqc_run_date_new;

static int date_up_arrow_BT_sensitivity_check = 0;

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void sensitize_gageqc_menu_options ( Widget w, 
                                            XtPointer client_data,
                                            XtPointer call_data )
{
    /* Make sure that the precipitation, temperature, and freezing level
       items are selectable. */
    Sensitize ( widget_struct->qc_precipitation );
    Sensitize ( widget_struct->qc_temperature );
    Sensitize ( widget_struct->qc_freezing );

    /* Make sure that the PRISM data options are selectable. */
    Sensitize ( prism_widget );
    Sensitize ( widget_struct->monthly_max_temp );
    Sensitize ( widget_struct->monthly_min_temp );

    /* Make sure that the clear data option is available. */
    Sensitize ( widget_struct->clear_widget );

    /* Make sure that the save level 2 data option is sensitized. */
    Sensitize ( widget_struct->save_level2_data );
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void close_date_window ( Widget w, XtPointer clientdata,XtPointer calldata )
{
   XtUnmanageChild ( choose_date_SH );
}

void send_date_to_gageqc_engine_struct (Widget w, XtPointer clientdata,XtPointer calldata)
{
        /* Copy the user selected date and duration information from the
           choose data period window to the dqc_run_date_new structure. */
	dqc_run_date_new.dqc_data_year = atoi(temp_date);	
	dqc_run_date_new.dqc_data_month = atoi(temp_month);	
	dqc_run_date_new.dqc_data_day = atoi(temp_day);	
        dqc_run_date_new.dqc_num_days = atoi(temp_num_of_days);

        date_st3 = dates[dates_struct_count];
}

/*****************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   add_date_window_callbacks  
* PURPOSE:       redisplays "Choose date" window with dates and  
*                times of Last Saved and Last Input columns updated.  
*                                                                                
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS   
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.  
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
******************************************************************************
*
* Function type:
*   void
*
* Called by function:
*   (callback) Choose Hour option (main window)
*
* Functions called:
*   get_dates_RFCW
*
****************************************************************************/
static void add_date_window_callbacks () 
{
	XtAddCallback(display_mpe_data_PB,XmNactivateCallback, newhour_RFCW, (XtPointer)OkHourPopup);
	XtAddCallback(display_mpe_data_PB, XmNactivateCallback, hour_sensitive_RFCW, NULL);
	XtAddCallback(display_mpe_data_PB, XmNactivateCallback, close_date_window, choose_date_SH);
	if(unregister_check == 0)
	{
		XtAddCallback(display_mpe_data_PB, XmNactivateCallback, set_values_for_change_and_cancel, NULL);
		unregister_check = 1;
	}
	XtAddCallback(close_PB, XmNactivateCallback, restore_working_date, choose_date_SH);
	XtAddCallback(close_PB, XmNactivateCallback, close_date_window, choose_date_SH);
	//XtAddCallback(help_PB, XmNactivateCallback, popup_help_window, "MAIN");
	XtAddCallback(days_up_arrow_BT, XmNactivateCallback, days_increment_callback, NULL);    
	XtAddCallback(days_down_arrow_BT, XmNactivateCallback, days_decrement_callback, NULL);
	XtAddCallback(date_up_arrow_BT, XmNactivateCallback, date_increment_callback, NULL);
	XtAddCallback(date_down_arrow_BT, XmNactivateCallback, date_decrement_callback, NULL);
	XtAddCallback(hour_up_arrow_BT, XmNactivateCallback, hour_increment_callback, NULL);
	XtAddCallback(hour_down_arrow_BT, XmNactivateCallback, hour_decrement_callback, NULL);
	XtAddCallback(precipitation_PB, XmNactivateCallback, send_date_to_gageqc_engine_struct, NULL);
	XtAddCallback(precipitation_PB,XmNactivateCallback,set_values_for_change_and_cancel,NULL);
	XtAddCallback(precipitation_PB,XmNactivateCallback,close_date_window, choose_date_SH);
	XtAddCallback(precipitation_PB, XmNactivateCallback, edit_precip_gages_callback, (XtPointer)& area_values_TB);
	XtAddCallback(precipitation_PB, XmNactivateCallback, sensitize_gageqc_menu_options, NULL);
	XtAddCallback(temperature_PB, XmNactivateCallback, send_date_to_gageqc_engine_struct, NULL);
	XtAddCallback(temperature_PB, XmNactivateCallback,set_values_for_change_and_cancel, NULL);
	XtAddCallback(temperature_PB, XmNactivateCallback, close_date_window, choose_date_SH);
	XtAddCallback(temperature_PB, XmNactivateCallback, edit_temp_gages_callback, (XtPointer) & area_values_TB);
	XtAddCallback(temperature_PB, XmNactivateCallback, sensitize_gageqc_menu_options, NULL);
	XtAddCallback(freezing_PB, XmNactivateCallback, send_date_to_gageqc_engine_struct, NULL);
	XtAddCallback(freezing_PB, XmNactivateCallback, set_values_for_change_and_cancel, NULL);
        XtAddCallback(freezing_PB, XmNactivateCallback, close_date_window, choose_date_SH);
	XtAddCallback(freezing_PB, XmNactivateCallback, edit_freezing_gages_callback, (XtPointer) & area_values_TB);
	XtAddCallback(freezing_PB, XmNactivateCallback, sensitize_gageqc_menu_options, NULL);
}
/******************************************** END add_date_window_callbacks ***/

/*****************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   display_rfcwide_date_window 
* PURPOSE:       This routine displays window providing user selection of which  
*                hour to run.                                     
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
*   None
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
******************************************************************************
*
* Function type:
*   void
*
* Called by function:
*   main
*
* Functions called:
*   (callback) select_callback
*   (callback) ok_callback
*   (callback) popdown_shell
*   (callback) pop_down
*   (callback) create_help_Dialog
*   get_dates_RFCW

************************************* BEGIN display_rfcwide_date_window *****/

void choose_hour_manage(Widget ww)
{
	Atom wmAtom;
	Widget www;
	int i = 0;
        int return_value;
	XmString xmstring = NULL;
	char** names_of_rfc_areas = NULL;
	int num_tokens = 0;
	char mpe_dqc_options_value[20];
	int len = 0;

	if ( choose_date_SH == NULL )
	{
                /* For the first time the choose hour window is created,
                   initialize variables needed for the MPE Editor Gage QC
                   tools. */
                initialize_gageqc ( );
                initialize_gageqc_gui ( );

	        www = GetTopShell ( ww );
		create_choose_date_SH (www);
		add_date_window_callbacks ();
	        wmAtom = XmInternAtom(XtDisplay(choose_date_SH), 
                         "WM_DELETE_WINDOW",FALSE);
	        XmAddWMProtocolCallback(choose_date_SH, wmAtom, 
                          restore_working_date, choose_date_SH);
	        XmAddWMProtocolCallback(choose_date_SH, wmAtom, 
                          close_date_window, choose_date_SH);
		names_of_rfc_areas = (char**) get_rfc_areas(&num_tokens);

		for(i=0;i<num_tokens;i++)
		{
			xmstring = XmStringGenerate(
                                   (XtPointer)names_of_rfc_areas[i],
                                   XmFONTLIST_DEFAULT_TAG,
                                   XmCHARSET_TEXT,NULL);
			XmComboBoxAddItem(area_values_CMB, xmstring, i, False);
			XmStringFree(xmstring);
		}

		if(num_tokens > 0)
		{
			XtVaSetValues(area_values_TB, XmNvalue, 
                        names_of_rfc_areas[0], NULL);
		}

                len = 0;
	        memset(mpe_dqc_options_value, '\0', 20);
	        len = strlen("mpe_dqc_options");
	        get_apps_defaults("mpe_dqc_options", &len, mpe_dqc_options_value, &len);

	        if(len == 0)
	        {
	           logMessage("mpe_dqc_options token not defined...turning off daily qc options...\n");
	        }

                return_value = strcasecmp(mpe_dqc_options_value, "on"); 

	        if( return_value != 0 )
	        {
	           XtSetSensitive(precipitation_PB, FALSE); 
	           XtSetSensitive(temperature_PB, FALSE); 
	           XtSetSensitive(freezing_PB, FALSE); 
	           XtSetSensitive(days_up_arrow_BT, FALSE); 
	           XtSetSensitive(days_down_arrow_BT, FALSE); 
	           XtSetSensitive(num_of_days_TB, FALSE); 
	           XtSetSensitive(area_values_CMB, FALSE); 
	           XtSetSensitive(area_values_TB, FALSE); 
	        }
	}
	else
	{
		XtRemoveCallback(display_mpe_data_PB, 
                                 XmNactivateCallback, 
                                 set_values_for_change_and_cancel, NULL);
	}

	if(date_up_arrow_BT_sensitivity_check == 0)
	{
	   XtSetSensitive(date_up_arrow_BT, FALSE);
	   date_up_arrow_BT_sensitivity_check = 1;
	}
	
	XtManageChild(choose_date_SH);
	XtManageChild(choose_date_FO);
	get_dates_RFCW();
	set_choose_hour_window_values(0);
        set_button_handles ( precipitation_PB, temperature_PB, freezing_PB );
}

void display_rfcwide_date_window(Widget ww, XtPointer clientdata, 
                                 XtPointer calldata)
{
	choose_hour_manage(ww);
}

/*************************************** END display_rfcwide_date_window *****/

void set_choose_hour_window_values(int call)
{
  if((dates_struct_count >= 0) && (dates_struct_count < (MAX_GAGEQC_DAYS * HOURS_PER_DAY)))
  {
  	memset(temp_date,'\0',5);
  	sprintf(temp_date,"%d",dates[dates_struct_count].year);
  	
  	memset(temp_month,'\0',5);
  	sprintf(temp_month,"%02d",dates[dates_struct_count].month);

  	memset(temp_day,'\0',5);
  	sprintf(temp_day,"%02d",dates[dates_struct_count].day);

  	memset(temp_hour,'\0',5);
  	sprintf(temp_hour,"%02d",dates[dates_struct_count].hour);
  
	if(call == 0)
	{
		XtVaSetValues(ending_year_TB, XmNvalue, temp_date, NULL);
  		XtVaSetValues(month_TB, XmNvalue, temp_month, NULL);
  		XtVaSetValues(day_TB, XmNvalue, temp_day, NULL);
  		XtVaSetValues(num_of_days_TB, XmNvalue, temp_num_of_days, NULL);
  		XtVaSetValues(hour_TB, XmNvalue, temp_hour, NULL);
		SetLabel(last_saved_value_LB, save_last[dates_struct_count]);
		SetLabel(last_exec_value_LB, exec_last[dates_struct_count]);
		SetLabel(manually_saved_value_LB, save_manual[dates_struct_count]);
	}
  }
  if(check == 0)
  {
  	set_values_for_change_and_cancel();
	check = 1;
  }
}

/******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   create_working_dialog_RFCW 
* PURPOSE:       This routine creates message box informing user that process is
*                initializing data. 
*                                                                                
* ARGUMENTS:
*   None
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
******************************************************************************
*
* Function type:
*    Widget structure
* 
* Called by function:
*   popup_rfcwide_workingDialog
*
*Functions called:
*   none
**************************************** BEGIN create_working_dialog_RFCW ***/
Widget create_working_dialog_RFCW()
{
   Widget       working, button;
   Arg          wargs[3];
   int          n;
   XmString xstr ;

 n=0;
 xstr = XmStringCreateLtoR ( "Preparing MPE display" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n] , XmNmessageString , xstr ) ; n++ ;
 XtSetArg(wargs[n], XmNdefaultPosition, FALSE); n++;
 working = XmCreateWorkingDialog(toplevel, "working", wargs, n);
 XmStringFree ( xstr ) ;
 
 button = XmMessageBoxGetChild(working, XmDIALOG_OK_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(working, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(working, XmDIALOG_CANCEL_BUTTON);
 XtUnmanageChild(button);

 return(working);
}

/****************************************** END create_working_dialog_RFCW ***/

void set_choose_hour_window_to_previous ( const struct _dqc_run_date * pDate )
{
   int i;

   /* Reset the date struct count. Find the date struct entry which
      mathes the supplied year, month, and day. */
   for ( i = 0 ; i < ( MAX_GAGEQC_DAYS * HOURS_PER_DAY ); ++i )
   {
      if ( ( dates [ i ].year == pDate->dqc_data_year ) &&
           ( dates [ i ].month == pDate->dqc_data_month ) &&
           ( dates [ i ].day == pDate->dqc_data_day ) )
      {
         dates_struct_count = i;
         break;
      }
   }

   memset(temp_date,'\0',5);
   sprintf(temp_date,"%d",dates[dates_struct_count].year);
  	
   memset(temp_month,'\0',5);
   sprintf(temp_month,"%02d",dates[dates_struct_count].month);

   memset(temp_day,'\0',5);
   sprintf(temp_day,"%02d",dates[dates_struct_count].day);

   memset(temp_hour,'\0',5);
   sprintf(temp_hour,"%02d",dates[dates_struct_count].hour);

   memset(temp_num_of_days,'\0',5);
   sprintf(temp_num_of_days, "%02d",pDate->dqc_num_days);
  
   /* Probably do not want to do this here.  There is no guarantee
      that the choose data period window is visible. */
   XtVaSetValues(ending_year_TB, XmNvalue, temp_date, NULL);
   XtVaSetValues(month_TB, XmNvalue, temp_month, NULL);
   XtVaSetValues(day_TB, XmNvalue, temp_day, NULL);
   XtVaSetValues(num_of_days_TB, XmNvalue, temp_num_of_days, NULL);
   XtVaSetValues(hour_TB, XmNvalue, temp_hour, NULL);
   SetLabel(last_saved_value_LB, save_last[dates_struct_count]);
   SetLabel(last_exec_value_LB, exec_last[dates_struct_count]);
   SetLabel(manually_saved_value_LB, save_manual[dates_struct_count]);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/choose_rfcwide_date.c,v $";
 static char rcs_id2[] = "$Id: choose_rfcwide_date.c,v 1.33 2007/07/12 19:10:41 lawrence Exp lawrence $";}
/*  ===================================================  */

}

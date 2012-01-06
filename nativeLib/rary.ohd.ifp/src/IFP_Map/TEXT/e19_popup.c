/* ***************************************************************************

	e19_popup.c

		build_popup()           : Creates popup window for displaying E19 data
		popup_segment_info()    : pops-up E19 window for the selected segment

		Coded by                : Tom Adams, NWS/Office of Hydrology/HRL



   *************************************************************************** */

#include <X11/X.h>
#include "libXifp.h"
#include "ifp_atoms.h"
#include "libXs.h"
#include "techniques.h"
#include "globals.h"

#include "struct_defs.h"
#include "run_partial.h"
#include "c_call_f/fconvt.h"/*-- added by AV -- */


static char * labels[] =
		{
		"Forecast Point:",
		"Description:",
		"River name:",
		"Station name:",
		"Forecast group:",
		"Carryover group:",
		"Upstream segments:",
		"Downstream segments:",
		"Latitude (degrees):",
		"Longitude (degrees):",
		"Type of forecast point:",
		"Forecast point area (sq mi):",
		"Total area above forecast point (sq mi):",
		"Flood stage (ft):",
		"Flood flow (cfs):",
		"Secondary stage (ft):",
		"Alert stage (ft):",
		"Alert flow (cfs):",
		"Gage zero (ft):",
		"Record flood stage (ft):",
		"Record flood flow (cfs):",
		"Date of record flood:",
		"Comment about record flood:",
		"Upper limit of rating curve (ft):"
		};




/* ********************************************************

	build_popup
		creates a popup widget

   ******************************************************** */

void build_popup(parent, enable, segment)
	Widget          parent;
	Widget          enable;         /* The segment-ID widget that is clicked-on to pop-up the popup */
	node            *segment;
{
	Widget          bb;
	Widget          popup;
	Widget          row_column_widget;
	Widget          button;
	Widget          popup_swindow;
	Widget          list_rc_widget;
	Widget          labels_list;
	Widget          data_list;

	Arg             wargs[8];
	int             i, n,RCIndex;
	int             pixel, dateInt;

	float           temp_float;
	float           multFactor;
	float           addConstant;
	int             errorFlag;

	char            string[51], dateString[15];
	char            english_units[4];

	XmString        xmstring_segmentLabel, xmstring_segmentName;
	XmString        *xmstring_forecast_pt_labels, *xmstring_forecast_pt_data;
	XmString        xmstring_result, xmstring_temp;

	static char     *month_char[13] = { "January", "February", "March",
					    "April",   "May",      "June",
					    "July",    "August",   "September",
					    "October", "November", "December",
					    "Invalid"
					  };
        


 memset(dateString, '\0', 15);


/*      Create a popup shell to hold the dialog box                             */
 popup = XtCreatePopupShell("e19_popup_shell", transientShellWidgetClass, parent, NULL, 0);
 segment->popup_shell = popup;

/*      Register the callback to popup the E19 data window...                   */
 XtAddCallback(enable, XmNactivateCallback, popup_segment_info, popup);

/*      Create a BulletinBoard widget to hold the fields                        */
 bb = XtVaCreateManagedWidget("e19_bboard", xmFormWidgetClass, popup,
			     XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
			     XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
			     NULL);

 row_column_widget = XtVaCreateManagedWidget("e19_rc_widget", xmRowColumnWidgetClass, bb,
					    XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
					    XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
					    XmNnumColumns,       2,
					    XmNorientation,      XmHORIZONTAL,
					    XmNtopAttachment,    XmATTACH_FORM,
					    XmNleftAttachment,   XmATTACH_FORM,
					    XmNrightAttachment,  XmATTACH_FORM,
					    NULL);


 strcpy(string, "Segment: ");
 xmstring_segmentLabel = XmStringCreate(string , XmSTRING_DEFAULT_CHARSET);
 XtVaCreateManagedWidget("forecast_pt_label", xmLabelWidgetClass, row_column_widget,
			XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
			XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
			XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
			NULL);


 strcpy(string, segment->e19.name);
 XtVaCreateManagedWidget("forecast_pt_name", xmLabelWidgetClass, row_column_widget,
			XtVaTypedArg, XmNlabelString, XmRString, string, strlen(string)+1,
			XtVaTypedArg, XmNforeground, XmRString, "yellow", strlen("yellow")+1,
			XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
			NULL);



 popup_swindow = XtVaCreateManagedWidget("popup_swindow", xmScrolledWindowWidgetClass, bb,
					XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
					XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
					XmNscrollingPolicy,  XmAUTOMATIC,
					XmNtopAttachment,    XmATTACH_WIDGET,
					XmNtopWidget,        row_column_widget,
					XmNleftAttachment,   XmATTACH_FORM,
					XmNrightAttachment,  XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_POSITION,
					XmNbottomPosition,   92,
					NULL);


/*      Create a "done" button and register a popdown callback...               */
 button = XtVaCreateManagedWidget("done", xmPushButtonWidgetClass, bb,
				 XtVaTypedArg, XmNlabelString, XmRString, "Close", strlen("Close")+1,
				 XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
				 XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
				 XmNtopAttachment,    XmATTACH_POSITION,
				 XmNtopPosition,      93,
				 XmNleftAttachment,   XmATTACH_FORM,
				 XmNrightAttachment,  XmATTACH_FORM,
				 XmNbottomAttachment, XmATTACH_FORM,
				 NULL);

/*      Register the Callback function to pop-down the e19 data...              */
 XtAddCallback(button, XmNactivateCallback, pop_down_shell, popup);


 list_rc_widget = XtVaCreateManagedWidget("list_rc_widget", xmRowColumnWidgetClass, popup_swindow,
					 XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
					 XtVaTypedArg, XmNbackground, XmRString, "gray", strlen("gray")+1,
					 XmNnumColumns, segment->e19.NumRC+1,
					 XmNorientation, XmHORIZONTAL,
					 NULL);

/*      Following items appear in a (segment->e19.NumRC+1)-column list widget...           */

/*      Create label widgets for the dialog box...                      */
 xmstring_forecast_pt_labels = (XmString *) XtMalloc(sizeof(XmString) * XtNumber(labels));
 for (i = 0; i < XtNumber(labels); i++)
	{
	xmstring_forecast_pt_labels[i] = XmStringCreate(labels[i], XmSTRING_DEFAULT_CHARSET);
	}

 labels_list = XtVaCreateManagedWidget("labels_list", xmListWidgetClass, list_rc_widget,
				      XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
				      XtVaTypedArg, XmNbackground, XmRString, "ivory", strlen("gray")+1,
				      XmNitems,            xmstring_forecast_pt_labels,
				      XmNitemCount,        XtNumber(labels),
				      XmNvisibleItemCount, XtNumber(labels),
				      XtNsensitive,        FALSE,
				      NULL);

/* --------------- Create the labels & fill the E19 data fields --------------- */
for (RCIndex=0;RCIndex<segment->e19.NumRC;RCIndex++)
{

 xmstring_forecast_pt_data = (XmString *) XtMalloc(sizeof(XmString) * XtNumber(labels));

 n = 0;  /*      It's mandatory that: XtNumber(labels) = n - 1 ...               */
 xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.extra[RCIndex].RCName, XmSTRING_DEFAULT_CHARSET);
 n++;
 
 xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.description, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- River name -------------------------- */
 if(strcmp(segment->e19.extra[RCIndex].river_name, "None                ") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("Missing", XmSTRING_DEFAULT_CHARSET);
 else
   xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.extra[RCIndex].river_name, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Station name -------------------------- */
 if(strcmp(segment->e19.extra[RCIndex].station_name, "None                ") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("Missing", XmSTRING_DEFAULT_CHARSET);
 else
   xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.extra[RCIndex].station_name, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Forecast group -------------------------- */
 if(strcmp(segment->e19.f_group, "EMPTY") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("Missing", XmSTRING_DEFAULT_CHARSET);
 else
   xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.f_group, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Carryover group -------------------------- */
 if(strcmp(segment->e19.c_group, "EMPTY") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("Missing", XmSTRING_DEFAULT_CHARSET);
 else
   xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.c_group, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Upstream segments -------------------------- */
 if(strcmp(segment->e19.upstream[0], "EMPTY") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("None", XmSTRING_DEFAULT_CHARSET);
 else
  {
   xmstring_temp = XmStringCreate(segment->e19.upstream[0], XmSTRING_DEFAULT_CHARSET);
   xmstring_result = XmStringCopy(xmstring_temp);

   i = 1;
   while(strcmp(segment->e19.upstream[i], "EMPTY") != 0)
	{
         if(xmstring_result != NULL)/*--AV add check here for linux */
	     XmStringFree(xmstring_result);
	 xmstring_result =
	   XmStringConcat(xmstring_temp,
			  XmStringCreate(",  ", XmSTRING_DEFAULT_CHARSET));
         if(xmstring_temp != NULL)/*--AV add check here for linux */                 
	     XmStringFree(xmstring_temp);
	 xmstring_temp = XmStringCopy(xmstring_result);

	 XmStringFree(xmstring_result);
	 xmstring_result =
	   XmStringConcat(xmstring_temp,
			  XmStringCreate(segment->e19.upstream[i], XmSTRING_DEFAULT_CHARSET));
	 XmStringFree(xmstring_temp);
	 xmstring_temp = XmStringCopy(xmstring_result);

	 i++;
	 if(i > 4) break;
	}
   xmstring_forecast_pt_data[n] = XmStringCopy(xmstring_result);
   XmStringFree(xmstring_result);
   
  }
n++;

/* -------------------------- Downstream segments -------------------------- */
 if(strcmp(segment->e19.downstream[0], "EMPTY") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("None", XmSTRING_DEFAULT_CHARSET);
 else
  {
   xmstring_temp = XmStringCreate(segment->e19.downstream[0], XmSTRING_DEFAULT_CHARSET);
   xmstring_result = XmStringCopy(xmstring_temp);

   i = 1;
   while(strcmp(segment->e19.downstream[i], "EMPTY") != 0)
	{
	 XmStringFree(xmstring_result);
	 xmstring_result =
	   XmStringConcat(xmstring_temp,
			  XmStringCreate(",  ", XmSTRING_DEFAULT_CHARSET));
	 XmStringFree(xmstring_temp);
	 xmstring_temp = XmStringCopy(xmstring_result);

	 XmStringFree(xmstring_result);
	 xmstring_result =
	   XmStringConcat(xmstring_temp,
			  XmStringCreate(segment->e19.downstream[i], XmSTRING_DEFAULT_CHARSET));
	 XmStringFree(xmstring_temp);
	 xmstring_temp = XmStringCopy(xmstring_result);

	 i++;
	 if(i > 2) break;
	}
   xmstring_forecast_pt_data[n] = XmStringCopy(xmstring_result);
   XmStringFree(xmstring_result);
   
   
  }
 n++;

/* -------------------------- Latitude -------------------------- */
 if(segment->e19.extra[RCIndex].latitude < -998.) sprintf(string, "%s", "Missing");
 else sprintf(string, "%.2f", segment->e19.extra[RCIndex].latitude);
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Longitude -------------------------- */
 if(segment->e19.extra[RCIndex].longitude < -998.) sprintf(string, "%s", "Missing");
 else sprintf(string, "%.2f", segment->e19.extra[RCIndex].longitude);
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Forecast Point Type -------------------------- */
 if(strcmp(segment->e19.extra[RCIndex].forecast_point_type, "None                ") == 0)
   xmstring_forecast_pt_data[n] = XmStringCreate("Missing", XmSTRING_DEFAULT_CHARSET);
 else
   xmstring_forecast_pt_data[n] = XmStringCreate(segment->e19.extra[RCIndex].forecast_point_type, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Local Area -------------------------- */
 if(segment->e19.extra[RCIndex].local_area < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].local_area;
/*      fconvt("KM2 ", "L2  ", english_units, &multFactor, &addConstant, &errorFlag);   */
	multFactor = 0.38610216;
	addConstant = 0.0;
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Total Area -------------------------- */
 if(segment->e19.extra[RCIndex].total_area < -998.)
   sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].total_area;
/*      fconvt("KM2 ", "L2  ", english_units, &multFactor, &addConstant, &errorFlag);   */
	multFactor = 0.38610216;
	addConstant = 0.0;
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Flood Stage -------------------------- */
 if(segment->e19.extra[RCIndex].flood_stage < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].flood_stage;
	FCONVT("M   ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.2f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Flood Flow -------------------------- */
 if(segment->e19.extra[RCIndex].flood_flow < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].flood_flow;
	FCONVT("CMS ", "L3/T", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Secondary Stage -------------------------- */
 if(segment->e19.extra[RCIndex].secondary_stage < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].secondary_stage;
	FCONVT("M   ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.2f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Warning Stage -------------------------- */
 if(segment->e19.extra[RCIndex].warning_stage < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].warning_stage;
	FCONVT("M   ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.2f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Warning Flow -------------------------- */
 if(segment->e19.extra[RCIndex].warning_flow < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].warning_flow;
	FCONVT("CMS ", "L3/T", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Gage Zero -------------------------- */
 if(segment->e19.extra[RCIndex].gage_zero < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].gage_zero;
	FCONVT("M   ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Record Stage -------------------------- */
 if(segment->e19.extra[RCIndex].record_stage < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].record_stage;
	FCONVT("M   ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.2f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Record Flow -------------------------- */
 if(segment->e19.extra[RCIndex].record_flow < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].record_flow;
	FCONVT("CMS ", "L3/T", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Date of Record -------------------------- */
 if(segment->e19.extra[RCIndex].date_of_record == -999) sprintf(dateString, "%s", "Missing");
 else   {
	dateInt = segment->e19.extra[RCIndex].date_of_record/1000000;              /* month        */
	if(dateInt < 1 || dateInt > 12)  dateInt = 13;
	sprintf(string, "%s ", month_char[dateInt - 1]);
	strcpy(dateString, string);
	dateInt = (segment->e19.extra[RCIndex].date_of_record%1000000) /10000;     /* day          */
	sprintf(string, "%d, ", dateInt);
	strcat(dateString, string);
	dateInt = segment->e19.extra[RCIndex].date_of_record%10000;                /* year         */
	sprintf(string, "%d", dateInt);
	strcat(dateString, string);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(dateString, XmSTRING_DEFAULT_CHARSET);
 n++;

/* -------------------------- Record Flood Comment -------------------------- */
 if(strcmp(segment->e19.extra[RCIndex].record_flood_comment, "None                ") == 0)
	xmstring_forecast_pt_data[n] = XmStringCreate("Missing", XmSTRING_DEFAULT_CHARSET);
 else
	xmstring_forecast_pt_data[n] =
			XmStringCreate(segment->e19.extra[RCIndex].record_flood_comment, XmSTRING_DEFAULT_CHARSET); n++;

/* -------------------------- Rating Curve Limit -------------------------- */
 if(segment->e19.extra[RCIndex].rating_curve_limit < -998.) sprintf(string, "%s", "Missing");
 else   {
	temp_float = segment->e19.extra[RCIndex].rating_curve_limit;
	FCONVT("M   ", "L   ", english_units, &multFactor, &addConstant, &errorFlag);
	temp_float = temp_float * multFactor + addConstant;

	sprintf(string, "%.0f", temp_float);
	}
 xmstring_forecast_pt_data[n] = XmStringCreate(string, XmSTRING_DEFAULT_CHARSET);

 data_list = XtVaCreateManagedWidget("data_list", xmListWidgetClass, list_rc_widget,
				    XtVaTypedArg, XmNforeground, XmRString, "navy", strlen("navy")+1,
				    XtVaTypedArg, XmNbackground, XmRString, "ivory", strlen("gray")+1,
				    XmNitems,            xmstring_forecast_pt_data,
				    XmNitemCount,        XtNumber(labels),
				    XmNvisibleItemCount, XtNumber(labels),
				    NULL);
  }/*end of for RCIndex loop*/
}




/* **************************************************************

	popup_segment_info()

   ************************************************************** */

void popup_segment_info(w, popup, call_data)
	Widget                  w;
	Widget                  popup;
	XmAnyCallbackStruct     *call_data;
{

	Display                 *display;
	Window                  theWindow;
	XWindowAttributes       attributes;


 if((call_data->event->xbutton.state & ShiftMask) != ShiftMask)
	{
	 /*printf("Shift key is not pressed...\n");  */                    
	return;
	}

 display = XtDisplay(w);

 if(XtIsRealized(popup))
	{
	theWindow = XtWindow(popup);

	XGetWindowAttributes(display, theWindow, &attributes);

	if(attributes.map_state == IsUnmapped) XtPopup(popup, XtGrabNone);
	else XtPopdown(popup);
	}
 else   {
	XtPopup(popup, XtGrabNone);
	}


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/e19_popup.c,v $";
 static char rcs_id2[] = "$Id: e19_popup.c,v 1.4 2002/10/10 16:55:18 dws Exp $";}
/*  ===================================================  */

}


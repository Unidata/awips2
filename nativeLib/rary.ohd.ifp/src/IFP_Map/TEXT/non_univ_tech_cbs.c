
/* ************************************************************************************************************

	non_univ_tech_cbs.c :
			source file containing the code to create the callback functions for the
			'Non-universal techniques' to set various units and time-zone preferences
			for input, computation, and output (display) in NWSRFS and the Interactive
			Forecast Program (ie, the graphical user interface to NWSRFS).

	Coded by     :  Tom Adams (NWS/Office of Hydrology/Hydrologic Research Laboratory)
	Date         :  3/21/91
	Revised      :  AV added SACSNOW technique 


   ************************************************************************************************************ */



#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"






/* ****************************************************************************************************

	Radio button and Check box callbacks...

   **************************************************************************************************** */




/* ********************************************************************

	show_nonUniversal_segmentList()
		the user has elected to select from the list of available
		segments to change, so map the scrolled window containing
		the list & set the "...All_segs_selected" flag to FALSE

   ******************************************************************** */
/* global variables use for turn on/off non-universal techniques 
                                    for sacsnow states  
   sacsnowCurrentVal                                           */

extern int sacsnowCurrentVal;

void show_nonUniversal_segmentList(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

	Arg             wargs[3];
	Display         *display;



display = XtDisplay(global_toplevel);

XMapWindow(display, XtWindow(someWidgets->non_univ_swindow));
XMapSubwindows(display, XtWindow(someWidgets->non_univ_swindow));

non_univ_techs_All_segs_selected = FALSE;

}


/* ********************************************************************

	hide_nonUniversal_segmentList()
		the user has elected to select all the available
		segments to change, so un-map the scrolled window
		containing the list & set the "...All_segs_selected"
		flag to TRUE

   ******************************************************************** */

void hide_nonUniversal_segmentList(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

	Arg             wargs[3];
	Display         *display;



display = XtDisplay(global_toplevel);

XUnmapWindow(display, XtWindow(someWidgets->non_univ_swindow));
XUnmapSubwindows(display, XtWindow(someWidgets->non_univ_swindow));

XmListDeselectAllItems(someWidgets->non_univ_list);
non_univ_techs_All_segs_selected = TRUE;

}



/* ********************************************************************

	snow_on_cb()
		callback to set the 'snow' flag to ON

   ******************************************************************** */

void snow_on_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->snow = ON;


}


/* ********************************************************************

	snow_off_cb()
		callback to set the 'snow' flag to OFF

   ******************************************************************** */

void snow_off_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

 non_univ_Techniques->snow = OFF;
 
}


/* ********************************************************************

	snow_no_change_cb()
		callback to set the 'snow' flag to OFF

   ******************************************************************** */

void snow_no_change_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->snow = NO_CHANGE;

}


/* ********************************************************************

	frost_on_cb()
		callback to set the 'frost' flag to ON

   ******************************************************************** */

void frost_on_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->frost = ON;

}


/* ********************************************************************

	frost_off_cb()
		callback to set the 'frost' flag to OFF

   ******************************************************************** */

void frost_off_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->frost = OFF;

}


/* ********************************************************************

	frost_no_change_cb()
		callback to set the 'frost' flag to OFF

   ******************************************************************** */

void frost_no_change_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->frost = NO_CHANGE;

}

/* ********************************************************************

	sac_snow_on_cb()
		callback to set the 'sac_snow' flag to ON

   ******************************************************************** */

void sac_snow_on_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

    non_univ_Techniques->sac_snow = ON;
    sacsnowCurrentVal = 1;
    XtSetSensitive(someWidgets->sac_widget, TRUE);
    XtSetSensitive(someWidgets->snow_widget, TRUE);
    
}


/* ********************************************************************

	sac_snow_off_cb()
		callback to set the 'sac_snow' flag to OFF

   ******************************************************************** */

void sac_snow_off_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{
    
    non_univ_Techniques->sac_snow = OFF;
    sacsnowCurrentVal = 0;
    XtSetSensitive(someWidgets->sac_widget, FALSE);
    XtSetSensitive(someWidgets->snow_widget, FALSE);
}


/* ********************************************************************

	sac_snow_no_change_cb()
		callback to set the 'sac_snow' flag to OFF

   ******************************************************************** */

void sac_snow_no_change_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->sac_snow = NO_CHANGE;


}

/* ********************************************************************

	upsc_on_cb()
		callback to set the 'upsc' flag to ON

   ******************************************************************** */

void upsc_on_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->upsc = ON;

}


/* ********************************************************************

	upsc_off_cb()
		callback to set the 'upsc' flag to OFF

   ******************************************************************** */

void upsc_off_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->upsc = OFF;

}


/* ********************************************************************

	upsc_no_change_cb()
		callback to set the 'upsc' flag to OFF

   ******************************************************************** */

void upsc_no_change_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->upsc = NO_CHANGE;

}


/* ********************************************************************

	upwe_on_cb()
		callback to set the 'upwe' flag to ON

   ******************************************************************** */

void upwe_on_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->upwe = ON;

}


/* ********************************************************************

	upwe_off_cb()
		callback to set the 'upwe' flag to OFF

   ******************************************************************** */

void upwe_off_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->upwe = OFF;

}


/* ********************************************************************

	upwe_no_change_cb()
		callback to set the 'upwe' flag to OFF

   ******************************************************************** */

void upwe_no_change_cb(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

non_univ_Techniques->upwe = NO_CHANGE;

}


/* ****************************************************************************

	set_non_univ_techniques()
		copy the values in the global structure 'non_univ_Techniques'
		to the non_univ_techniques_struct held in the 'node' structure
		for each of the selected segments...

   **************************************************************************** */

void set_non_univ_techniques(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	XmAnyCallbackStruct     *call_data;
{

	Arg             wargs[2];

	char            *selected_item_string, string[9];
	/* AV changed unsigned char to char due to linux port error 7/16/01 */
	char            *first_blank;

	int             i;
	int             j;
	int             num_selectedListItems;
	XmString        *xmSelectedListItems;
	node            *segment_node;



memset(string, '\0', 9);


if(non_univ_techs_All_segs_selected == TRUE)
	{
	XtSetArg(wargs[0], XmNitems, &xmSelectedListItems);
	XtSetArg(wargs[1], XmNitemCount, &num_selectedListItems);
	XtGetValues(someWidgets->non_univ_list, wargs, 2);
	}
else    {
	XtSetArg(wargs[0], XmNselectedItems, &xmSelectedListItems);
	XtSetArg(wargs[1], XmNselectedItemCount, &num_selectedListItems);
	XtGetValues(someWidgets->non_univ_list, wargs, 2);
	}


for(j = 0; j < num_selectedListItems; j++)
	{
	memset(string, '\0', 9);
	XmStringGetLtoR(xmSelectedListItems[j], XmSTRING_DEFAULT_CHARSET, &selected_item_string);

	first_blank = (char *)strchr(selected_item_string, ' ');
	if(first_blank != NULL)
		{
		strncpy(string, selected_item_string, first_blank - selected_item_string);
		}
	else strcpy(string, selected_item_string);

	for(i = 0; i <= sub_group_num; i++)
		{
		if((segment_node = find_it(someWidgets->head[i], string)) != NULL)
			{
			if(non_univ_Techniques->snow != NO_CHANGE)
				segment_node->techniques->snow = non_univ_Techniques->snow;
			if(non_univ_Techniques->frost != NO_CHANGE)
				segment_node->techniques->frost = non_univ_Techniques->frost;
			if(non_univ_Techniques->upsc != NO_CHANGE)
				segment_node->techniques->upsc = non_univ_Techniques->upsc;
			if(non_univ_Techniques->upwe != NO_CHANGE)
				segment_node->techniques->upwe = non_univ_Techniques->upwe;
			if(non_univ_Techniques->sac_snow != NO_CHANGE)
				segment_node->techniques->sac_snow = non_univ_Techniques->sac_snow;
			}
        /*
        printf("in non_univ_tech_cbs: %s %d %d %d %d %d\n", segment_node->e19.name,
                                   segment_node->techniques->snow,
                                   segment_node->techniques->frost,
                                   segment_node->techniques->upsc,
                                   segment_node->techniques->upwe,
				   segment_node->techniques->sac_snow);
         */
 
		}
	}

}



/* ****************************************************************************

	set_snowModel_buttons_to_default()


   **************************************************************************** */

void set_snowModel_buttons_to_default(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

	Arg             wargs[2];




 fill_non_univ_techniques_struct();


 XtSetArg(wargs[0], XmNset, TRUE);
 if(non_univ_Techniques->snow) XtSetValues(someWidgets->snowButtons.on, wargs, 1);
 else XtSetValues(someWidgets->snowButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->snowButtons.no_change, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 if(non_univ_Techniques->frost) XtSetValues(someWidgets->frostButtons.on, wargs, 1);
 else XtSetValues(someWidgets->frostButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->frostButtons.no_change, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 if(non_univ_Techniques->upsc) XtSetValues(someWidgets->upscButtons.on, wargs, 1);
 else XtSetValues(someWidgets->upscButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upscButtons.no_change, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 if(non_univ_Techniques->upwe) XtSetValues(someWidgets->upweButtons.on, wargs, 1);
 else XtSetValues(someWidgets->upweButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upweButtons.no_change, wargs, 1);

 /* added for SAC/SNOW interface 2/2/01 */
 XtSetArg(wargs[0], XmNset, TRUE);
 if(non_univ_Techniques->sac_snow) XtSetValues(someWidgets->sac_snowButtons.on, wargs, 1);
 else XtSetValues(someWidgets->sac_snowButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->sac_snowButtons.no_change, wargs, 1);


}



/* ****************************************************************************

	set_snowModel_buttons_for_selectedSegment()


   **************************************************************************** */

void set_snowModel_buttons_for_selectedSegment(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	XmListCallbackStruct    *call_data;
{

	Arg             wargs[2];
	node            *this_node;
	char            *segmentName;
	char            *string;
	char            *first_blank;
	int             i;




 segmentName = (char *) malloc(9);
 memset(segmentName, '\0', 9);

 /*     Get the name of the selected segment and be sure to remove any trailing         */
 /*     spaces from its name... since 'find_it' is looking for an exact match.          */

 XmStringGetLtoR(call_data->item, XmSTRING_DEFAULT_CHARSET, &string);
 if((first_blank = strstr(string, " ")) != NULL)
	     strncpy(segmentName, string, first_blank - string);
 else strcpy(segmentName, string);


 /* Find the node for 'segmentName'...                                                  */
 for(i = 0; i <= sub_group_num; i++)
	{
	if((this_node = find_it(someWidgets->head[i], segmentName)) != NULL) break;
	}

 XtSetArg(wargs[0], XmNset, TRUE);
 if(this_node->techniques->snow)
	{
	XtSetValues(someWidgets->snowButtons.on, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->snowButtons.off, wargs, 1);
	}
 else   {
	XtSetValues(someWidgets->snowButtons.off, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->snowButtons.on, wargs, 1);
	}
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->snowButtons.no_change, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 if(this_node->techniques->frost)
	{
	XtSetValues(someWidgets->frostButtons.on, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->frostButtons.off, wargs, 1);
	}
 else   {
	XtSetValues(someWidgets->frostButtons.off, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->frostButtons.on, wargs, 1);
	}
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->frostButtons.no_change, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 if(this_node->techniques->upsc)
	{
	XtSetValues(someWidgets->upscButtons.on, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->upscButtons.off, wargs, 1);
	}
 else   {
	XtSetValues(someWidgets->upscButtons.off, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->upscButtons.on, wargs, 1);
	}
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upscButtons.no_change, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 if(this_node->techniques->upwe)
	{
	XtSetValues(someWidgets->upweButtons.on, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->upweButtons.off, wargs, 1);
	}
 else   {
	XtSetValues(someWidgets->upweButtons.off, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->upweButtons.on, wargs, 1);
	}
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upweButtons.no_change, wargs, 1);

 /* added for SAC/SNOW graphic technique 2/2/01 */
 XtSetArg(wargs[0], XmNset, TRUE);
 if(this_node->techniques->sac_snow)
	{
	XtSetValues(someWidgets->sac_snowButtons.on, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->sac_snowButtons.off, wargs, 1);
	}
 else   {
	XtSetValues(someWidgets->sac_snowButtons.off, wargs, 1);
	XtSetArg(wargs[0], XmNset, FALSE);
	XtSetValues(someWidgets->sac_snowButtons.on, wargs, 1);
	}
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->sac_snowButtons.no_change, wargs, 1);

}




/* ****************************************************************************

	set_snowModel_buttons_to_noChange()


   **************************************************************************** */

void set_snowModel_buttons_to_noChange(w, someWidgets, call_data)
	Widget                          w;
	the_widget_struct               *someWidgets;
	XmToggleButtonCallbackStruct    *call_data;
{

	Arg             wargs[2];


 XtSetArg(wargs[0], XmNset, TRUE);
 XtSetValues(someWidgets->snowButtons.no_change, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->snowButtons.on, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->snowButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 XtSetValues(someWidgets->frostButtons.no_change, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->frostButtons.on, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->frostButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 XtSetValues(someWidgets->upscButtons.no_change, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upscButtons.on, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upscButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, TRUE);
 XtSetValues(someWidgets->upweButtons.no_change, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upweButtons.on, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->upweButtons.off, wargs, 1);

 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->sac_snowButtons.no_change, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->sac_snowButtons.on, wargs, 1);
 XtSetArg(wargs[0], XmNset, FALSE);
 XtSetValues(someWidgets->sac_snowButtons.off, wargs, 1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/non_univ_tech_cbs.c,v $";
 static char rcs_id2[] = "$Id: non_univ_tech_cbs.c,v 1.3 2002/02/14 15:47:38 dws Exp $";}
/*  ===================================================  */

}





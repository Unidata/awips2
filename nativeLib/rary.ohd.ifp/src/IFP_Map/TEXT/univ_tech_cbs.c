
/* ************************************************************************************************************

	univer_tech_cbs.c :
			source file containing the code to create the callback functions for the
			'Universal techniques' to set various units and time-zone preferences for
			input, computation, and output (display) in NWSRFS and the Interactive
			Forecast Program (ie, the graphical user interface to NWSRFS).

	Coded by     :  Tom Adams (NWS/Office of Hydrology/Hydrologic Research Laboratory)
	Date         :  3/15/91
	Revised      :  3/18/91 3/19/91 3/20/91


   ************************************************************************************************************ */



#include "libXifp.h"
#include "run_dates.h"
#include "libXs.h"
#include "ifp_atoms.h"
#include "techniques.h"
#include "globals.h"
#include "struct_defs.h"
#include "run_partial.h"
#include "c_call_f/fcitzc.h"
#include "c_call_f/set_fctime_cb.h"
#include "c_call_f/julda.h"

void set_Input_Z(w, someWidgets, call_data)
	Widget                  w;
	the_widget_struct       *someWidgets;
	caddr_t                 *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Z", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "Z   ");

}

void set_Input_EST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("EST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "EST ");

}

void set_Input_CST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("CST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "CST ");

}

void set_Input_MST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("MST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "MST ");

}

void set_Input_PST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("PST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "PST ");

}

void set_Input_AST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("AST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "AST ");

}

void set_Input_HST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("HST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "HST ");

}

void set_Input_NST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("NST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "NST ");

}

void set_Input_EDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("EDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "EDT ");

}

void set_Input_CDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("CDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "CDT ");

}

void set_Input_MDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("MDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "MDT ");

}

void set_Input_PDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("PDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "PDT ");

}

void set_Input_ADT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("ADT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "ADT ");

}

void set_Input_HDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("HDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "HDT ");

}

void set_Input_NDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("NDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->inputFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->input_time_zone_code, "NDT ");

}





void set_Output_Z(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Z", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "Z   ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_EST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("EST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "EST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_CST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("CST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "CST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_MST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("MST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "MST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_PST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("PST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "PST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_AST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("AST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "AST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_HST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("HST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "HST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_NST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("NST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "NST ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_EDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("EDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "EDT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_CDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("CDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "CDT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_MDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("MDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "MDT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_PDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("PDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "PDT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_ADT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("ADT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "ADT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_HDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("HDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "HDT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}

void set_Output_NDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];
	int             itz, idsav;


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("NDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->outputFrame_label, wargs, 1);

FCITZC(&itz, &idsav, "NDT ");
temp_univ_Techniques->output_time_zone = itz;
temp_univ_Techniques->output_daylight_savings = idsav;

}





void set_Mods_Z(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("Z", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "Z   ");

}

void set_Mods_EST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("EST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "EST ");

}

void set_Mods_CST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("CST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "CST ");

}

void set_Mods_MST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("MST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "MST ");

}

void set_Mods_PST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("PST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "PST ");

}

void set_Mods_AST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("AST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "AST ");

}

void set_Mods_HST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("HST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "HST ");

}

void set_Mods_NST(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("NST", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "NST ");

}

void set_Mods_EDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("EDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "EDT ");

}

void set_Mods_CDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("CDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "CDT ");

}

void set_Mods_MDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("MDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "MDT ");

}

void set_Mods_PDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("PDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "PDT ");

}

void set_Mods_ADT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("ADT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "ADT ");

}

void set_Mods_HDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("HDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "HDT ");

}

void set_Mods_NDT(w, someWidgets, call_data)
	Widget                w;
	the_widget_struct     *someWidgets;
	caddr_t               *call_data;
{

	Arg             wargs[3];


XtSetArg(wargs[0], XmNlabelString, XmStringCreate("NDT", XmSTRING_DEFAULT_CHARSET));
XtSetValues(someWidgets->modsFrame_label, wargs, 1);

strcpy(temp_univ_Techniques->mod_time_zone_code, "NDT ");

}




/* ****************************************************************************************************

	Radio button and Check box callbacks...

   **************************************************************************************************** */



/* ************************************************************************

	change_univ_Techniques_struct()

   ************************************************************************ */

void change_univ_Techniques_struct(w, client_data, call_data)
	Widget                  w;
	caddr_t                 client_data;
	XmAnyCallbackStruct     *call_data;
{

	Display         *display;
	Window          root;

 display = XtDisplay(w);
 root = DefaultRootWindow(display);

*universalTechniques = *temp_univ_Techniques;

XChangeProperty
	(
	display,
	root,
	IFPA_time_zone_code,
	IFPA_time_zone_code_type,
	8,
	PropModeReplace,
	universalTechniques->input_time_zone_code,
	sizeof(char) * strlen(universalTechniques->input_time_zone_code)
	);

XChangeProperty(
	display,
	root,
	IFPA_general_units,
	IFPA_general_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(temp_univ_Techniques->metric_units),
	sizeof(int)
	);

XChangeProperty(
	display,
	root,
	IFPA_mods_general_units,
	IFPA_mods_general_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(temp_univ_Techniques->mod_units),
	sizeof(int)
	);

XChangeProperty(
	display,
	root,
	IFPA_mods_API_units,
	IFPA_mods_API_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(temp_univ_Techniques->mod_api_units),
	sizeof(int)
	);

XChangeProperty(
	display,
	root,
	IFPA_mods_SAC_units,
	IFPA_mods_SAC_units_type,
	8,
	PropModeReplace,
	(unsigned char *)&(temp_univ_Techniques->mod_sac_units),
	sizeof(int)
	);
}



/* ************************************************************************

	update_Dates_timeZone_code()

   ************************************************************************ */

void update_Dates_timeZone_code(w, client_data, call_data)
	Widget                  w;
	caddr_t                 client_data;
	XmAnyCallbackStruct     *call_data;
{
	Display         *display;
	Window          root;

	date            *the_date;

	int             type, format, nitems, left;
	int             the_month, the_day, the_year, the_hour;
	int             julian_day, julian_hour;
	int             julday, julhr;
	int             month, day, year, hour;
	int             zondum, dlsdum;
	int             julianHours;

	char            time_zone_code[5];

	long            offset = 0;





 display = XtDisplay(w);
 root = DefaultRootWindow(display);

 /*     Get the Run start date from the root window window property...          */

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_start_date,
	offset,
	(long) sizeof(date),
	FALSE,
	IFPA_run_start_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&the_date
	) == Success && type == IFPA_run_start_date_type){

	the_month = the_date->month;
	the_day = the_date->day;
	the_year = the_date->year;
	the_hour = the_date->hour;
	strcpy(time_zone_code, the_date->time_zone);


	/*      Convert the month, day, year, hour to julian days & hours for the       */
	/*      time zone code of the date obtained from the window property...         */

	SET_FCTIME_CB();
	
	/* replace call to julda2 with FCITZC and JULDA for y2k */
        FCITZC(&zondum, &dlsdum, time_zone_code);
	JULDA(&julian_day, &julian_hour,
		&the_month,
		&the_day,
		&the_year,
		&the_hour,
		&zondum, &dlsdum, time_zone_code);

	/*      Convert julian_day & julian_hour to julianHours...                      */
	julianHours = 24*(julian_day - 1) + julian_hour;

	/*      Get the updated month, day, year, hour for the new time zone code...    */
	get_month_day_year_hour_tzc(julianHours, &julday, &julhr,
				    &month, &day, &year, &hour, &zondum, &dlsdum,
				    temp_univ_Techniques->input_time_zone_code);

	the_date->month = month;
	the_date->day = day;
	the_date->year = year;
	the_date->hour = hour;
	strcpy(the_date->time_zone, temp_univ_Techniques->input_time_zone_code);

	/*      Update the date held in the root window window property...              */
	XChangeProperty(
		display,
		root,
		IFPA_run_start_date,
		IFPA_run_start_date_type,
		8,
		PropModeReplace,
		(unsigned char *)the_date,
		sizeof(date)
		);

	}

 /*     Get the Run start date from the root window window property...          */

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_end_obs_date,
	offset,
	(long) sizeof(date),
	FALSE,
	IFPA_run_end_obs_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&the_date
	) == Success && type == IFPA_run_end_obs_date_type){

	the_month = the_date->month;
	the_day = the_date->day;
	the_year = the_date->year;
	the_hour = the_date->hour;
	strcpy(time_zone_code, the_date->time_zone);


	/*      Convert the month, day, year, hour to julian days & hours for the       */
	/*      time zone code of the date obtained from the window property...         */
	/* replace call to julda2 with FCITZC and JULDA for y2k */
        FCITZC(&zondum, &dlsdum, time_zone_code);	
	JULDA(&julian_day, &julian_hour,
		&the_month,
		&the_day,
		&the_year,
		&the_hour,
		&zondum, &dlsdum, time_zone_code);

	/*      Convert julian_day & julian_hour to julianHours...                      */
	julianHours = 24*(julian_day - 1) + julian_hour;

	/*      Get the updated month, day, year, hour for the new time zone code...    */
	get_month_day_year_hour_tzc(julianHours, &julday, &julhr,
				    &month, &day, &year, &hour, &zondum, &dlsdum,
				    temp_univ_Techniques->input_time_zone_code);

	the_date->month = month;
	the_date->day = day;
	the_date->year = year;
	the_date->hour = hour;
	strcpy(the_date->time_zone, temp_univ_Techniques->input_time_zone_code);

	/*      Update the date held in the root window window property...              */
	XChangeProperty(
		display,
		root,
		IFPA_run_end_obs_date,
		IFPA_run_end_obs_date_type,
		8,
		PropModeReplace,
		(unsigned char *)the_date,
		sizeof(date)
		);

	}

 /*     Get the Run end date from the root window window property...                    */

 if(XGetWindowProperty
	(
	display,
	root,
	IFPA_run_end_date,
	offset,
	(long) sizeof(date),
	FALSE,
	IFPA_run_end_date_type,
	(Atom *)&type,
	(int *)&format,
	(unsigned long *)&nitems,
	(unsigned long *)&left,
	(unsigned char **)&the_date
	) == Success && type == IFPA_run_end_date_type){

	the_month = the_date->month;
	the_day = the_date->day;
	the_year = the_date->year;
	the_hour = the_date->hour;
	strcpy(time_zone_code, the_date->time_zone);


	/*      Convert the month, day, year, hour to julian days & hours for the       */
	/*      time zone code of the date obtained from the window property...         */
	        /* replace call to julda2 with FCITZC and JULDA for y2k */
                FCITZC(&zondum, &dlsdum, time_zone_code);	
		JULDA(&julian_day, &julian_hour,
		&the_month,
		&the_day,
		&the_year,
		&the_hour,
		&zondum, &dlsdum, time_zone_code);

	/*      Convert julian_day & julian_hour to julianHours...                      */
	julianHours = 24*(julian_day - 1) + julian_hour;

	/*      Get the updated month, day, year, hour for the new time zone code...    */
	get_month_day_year_hour_tzc(julianHours, &julday, &julhr,
				    &month, &day, &year, &hour, &zondum, &dlsdum,
				    temp_univ_Techniques->input_time_zone_code);

	the_date->month = month;
	the_date->day = day;
	the_date->year = year;
	the_date->hour = hour;
	strcpy(the_date->time_zone, temp_univ_Techniques->input_time_zone_code);

	/*      Update the date held in the root window window property...              */
	XChangeProperty(
		display,
		root,
		IFPA_run_end_date,
		IFPA_run_end_date_type,
		8,
		PropModeReplace,
		(unsigned char *)the_date,
		sizeof(date)
		);

	}

}



/* ************************************************************************

	english_toggleButton_callback()

   ************************************************************************ */

void english_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->metric_units = ENGLISH_UNITS;

}


/* ************************************************************************

	metric_toggleButton_callback()

   ************************************************************************ */

void metric_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->metric_units = METRIC_UNITS;

}


/* ************************************************************************

	general_english_toggleButton_callback()

   ************************************************************************ */

void general_english_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_units = ENGLISH_UNITS;

}



/* ************************************************************************

	general_metric_toggleButton_callback()

   ************************************************************************ */

void general_metric_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_units = METRIC_UNITS;

}


/* ************************************************************************

	api_english_toggleButton_callback()

   ************************************************************************ */

void api_english_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_api_units = ENGLISH_UNITS;

}



/* ************************************************************************

	api_metric_toggleButton_callback()

   ************************************************************************ */

void api_metric_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_api_units = METRIC_UNITS;

}


/* ************************************************************************

	sac_english_toggleButton_callback()

   ************************************************************************ */

void sac_english_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_sac_units = ENGLISH_UNITS;

}


/* ************************************************************************

	sac_metric_toggleButton_callback()

   ************************************************************************ */

void sac_metric_toggleButton_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_sac_units = METRIC_UNITS;

}


/* ************************************************************************

	warning_callback()

   ************************************************************************ */

void warning_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmToggleButtonCallbackStruct     *call_data;
{

temp_univ_Techniques->mod_warning = call_data->set;

}


/* ************************************************************************

	futurePrecip_callback()

   ************************************************************************ */

void futurePrecip_callback(w, client_data, call_data)
	Widget                           w;
	caddr_t                          client_data;
	XmScaleCallbackStruct            *call_data;
{
/* Changed to allow future precip work with new
   future precip specs.  Now use a scale widget
   to get user input for number of hours of qpf to use.
   dp - 14 June 95
*/
temp_univ_Techniques->future_precip = call_data->value;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/univ_tech_cbs.c,v $";
 static char rcs_id2[] = "$Id: univ_tech_cbs.c,v 1.4 2006/04/07 13:30:48 aivo Exp $";}
/*  ===================================================  */

}





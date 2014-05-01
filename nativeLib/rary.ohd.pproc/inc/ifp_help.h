
/* ********************************************************************************************

	ifp_help.h
				Data structures, etc., needed for online help Dialogs


	Coded by:               Tom Adams
	Affiliation:            NOAA/NWS/Office of Hydrology/Hydrologic Research Laboratory

	Initially coded:        05/14/91
	Last modified:          06/15/92


   ******************************************************************************************** */

#ifndef IFP_HELP_H
#define IFP_HELP_H

int     IsContextHelpWindowDown;

/*      Data structure passed by Callback functions for 'Help' buttons...       */
typedef struct _help_struct
	{
	char            *message_widget_name;
	Widget          parent;
	}       help_struct;



/*      Data structure passed by Event handlers of widgets...                   */
typedef struct
	{
	Widget          top_help_shell;
	char            *widget_name;
	}       help_cb_struct;

#endif /* #ifndef IFP_HELP_H */

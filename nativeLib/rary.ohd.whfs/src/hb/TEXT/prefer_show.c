/*
	File:		prefer_show.c
	Date:		March 1996
	Author:		Dale Shelton / Chip Gobs
	
	Purpose:	Provides support for the Preferences DS.
*/


#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include "Xtools.h"
#include "user_prefs.h"
#include "prefer_cbs.h"
#include "prefer.h"
#include "hybase.h"
#include "hbAS.h"
#include "callbacks.h"

static int prefer_active ;

void	ShowPrefDs(Widget w)
{
	if (! prefDS)
	{
		create_prefDS(GetTopShell(w));
		prefer_callbacks();
	}
	
	
	if (! XtIsManaged(prefDS))
	{
		prefer_load();
		XtManageChild(prefFM);
		XtManageChild(prefDS);
	}
	
	return;
}


void	prefer_callbacks(void)
{
	Atom	wmAtom;

	
	/*
		Window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(prefDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(prefDS, wmAtom, prefer_close, NULL);

	
	/*
		Widget callbacks.
	*/
	XtAddCallback(pfokPB,    XmNactivateCallback,     prefer_save,   NULL);
	XtAddCallback(pfclosePB, XmNactivateCallback,     prefer_close,  NULL);
	XtAddCallback(pfcoTB,    XmNvalueChangedCallback, prefer_fields, NULL);
	XtAddCallback(pfbasinTB, XmNvalueChangedCallback, prefer_fields, NULL);
	XtAddCallback(pfriverTB, XmNvalueChangedCallback, prefer_fields, NULL);
	XtAddCallback(pflatTB,   XmNvalueChangedCallback, prefer_fields, NULL);
	return;
}


void	prefer_fields(Widget w, XtPointer ptr, XmToggleButtonCallbackStruct *cbs)
{
/*
fprintf(stderr, "PREVIOUS: %d\n", prefer_active);
*/

	/*
		Adjust the state of active counter.
	*/
	if (cbs->set)
	   	prefer_active++;
	else
	   	prefer_active--;
	
/*
fprintf(stderr, "CURRENT: %d\n", prefer_active);
*/
	
	/*
		Sensitize or desensitize as appropriate.
	*/
	if (prefer_active == 3)
	   	prefer_sensitize_flds(FLDS_OFF);
	else
	   	prefer_sensitize_flds(FLDS_ON);
	return;
}


void	prefer_load(void)
{
   	WidgetList	widgetList;
	Arg		arg[2];
	long		code;
	int		cnt;
	
	
	/*
		Clear the default values of the form.
	*/
	prefer_clear();
	prefer_active = 0;
	
	
	/*
		Set title preference information.
	*/
	code = get_title_preference();
	if (code & TITLE_HB5)
		XmToggleButtonSetState(pflidTB, True, False);
	
	if (code & TITLE_NAME)
		XmToggleButtonSetState(pfnameTB, True, False);

	
	/*
		Set field preference information.
	*/
	code = get_field_preference();
	if ((code & FLD_COUNTY) && (prefer_active < 4))
	{
	   	XmToggleButtonSetState(pfcoTB, True, False);
	   	prefer_active++;
	}
	
	
	if ((code & FLD_BASIN) && (prefer_active < 4))
	{
	   	XmToggleButtonSetState(pfbasinTB, True, False);
		prefer_active++;
	}
	
	
	if ((code & FLD_STREAM) && (prefer_active < 4))
	{
	   	XmToggleButtonSetState(pfriverTB, True, False);
		prefer_active++;
	}
	
	
	if ((code & FLD_LATLON) && (prefer_active < 4))
	{
	   	XmToggleButtonSetState(pflatTB, True, False);
		prefer_active++;
	}
	
	
	/*
		If three (3) toggles have been activated, 
		desensitize the other options.
	*/
	if (prefer_active == 3)
	   	prefer_sensitize_flds(FLDS_OFF);	

	
	/*
		Set sort preference information.
	*/
	code = get_sort_preference();
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pfsortRC, arg, 2);
	if (code < cnt)
		XmToggleButtonSetState(widgetList[code], True, False);
	return;
}


void	prefer_clear(void)
{
   	WidgetList	widgetList;
	Arg		arg[2];
	int		cnt,
	   		i;
	
	
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(headerRC, arg, 2);
	for (i = 0; i < cnt; i++)
		XmToggleButtonSetState(widgetList[i], False, False);

	
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pffldsRC, arg, 2);
	for (i = 0; i < cnt; i++)
	   	XmToggleButtonSetState(widgetList[i], False, False);
	
	
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pfsortRC, arg, 2);
	for (i = 0; i < cnt; i++)
		XmToggleButtonSetState(widgetList[i], False, False);

	return;
}


void	prefer_save(Widget w, XtPointer ptr, XtPointer cbs)
{
   	WidgetList	widgetList;
	Arg		arg[2];
	long		code;
	int		cnt,
	   		pos,
	   		i;
	
	
	code = 0;
	if (XmToggleButtonGetState(pflidTB))
		code |= TITLE_HB5;
		
	if (XmToggleButtonGetState(pfnameTB))
		code |= TITLE_NAME;
				
	set_title_preference(code);
	
	
	/*
		Set the value of the fieldlist variable.
	*/
	code = 0;
	if (XmToggleButtonGetState(pfcoTB))
	    	code |= FLD_COUNTY;
	 
	if (XmToggleButtonGetState(pfbasinTB))
	    	code |= FLD_BASIN;
	 
	if (XmToggleButtonGetState(pfriverTB))
	   	code |= FLD_STREAM;
	
	if (XmToggleButtonGetState(pflatTB))
	   	code |= FLD_LATLON;
	set_field_preference(code);
	
	
	/*
		Get state of station toggles.
	*/
	code = 0;
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pfsortRC, arg, 2);
	for (i = 0; i < cnt; i++)
	   	if (XmToggleButtonGetState(widgetList[i]))
		   	code = (long) i;
	set_sort_preference(code);
  
	
	/*
		Set the main window menu options to 
		the appropriate settings.
	*/
        pos = get_sort_preference();
        SetMenuPos(hbsortOM, pos);
        set_stat_list(NULL, NULL, NULL);

	
	/*
		Close dialog.
	*/
	prefer_close(w, NULL, NULL);	
	return;
}


void	prefer_sensitize_flds(const long state)
{
   	WidgetList	widgetList;
	Arg		arg[2];
	int		cnt,
	   		i;
	
	
	/*
		Get a list of the widget children belonging
		to the row column.
	*/
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(pffldsRC, arg, 2);
	
	
	/*
		Iterate through the list, and set 
		the state of the widgets.
	*/
	if (state)
	{
	 	for (i = 0; i < cnt; i++)
		   	Sensitize(widgetList[i]);
	}
	else
	{
	   	for (i = 0; i < cnt; i++)
		   	if (! XmToggleButtonGetState(widgetList[i]))
			   	DeSensitize(widgetList[i]);
	}
	
	
	return;
}


void	prefer_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(prefDS))
   {
      XtDestroyWidget(prefDS);
      prefDS = NULL;
   }
   
   return;
}



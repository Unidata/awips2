/*
	File:
	Date:
	Author:
	
	Purpose:
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include "Xtools.h"



void	clearForm(Widget parent)
{
   	WidgetList	widgetList;
	Arg		arg[2];
	unsigned char	rcType;
	int		maxValue,
			minValue,
			newValue,
			cnt,
	   		i;
	
	
	
	/*
		Ensure the parent widget is derived from
		the XmManager widget class.
	*/
	if (XmIsManager(parent))
	{
	   	/*
	   		Get all widget children from the
			parent, and check the widget class
			performing the appropriate clear 
			action.
		*/
        	XtSetArg(arg[0], XmNchildren, &widgetList);
        	XtSetArg(arg[1], XmNnumChildren, &cnt);
        	XtGetValues(parent, arg, 2);
        	for (i = 0; i < cnt; i++)
		{
		   	/*
		   		Clear XmManager children.
			*/
		   	if (XmIsManager(widgetList[i]))
			   	clearForm(widgetList[i]);
			
			
		   	/*
		   		Clear XmText children.
			*/
		   	if (XmIsText(widgetList[i]))
			   	XmTextSetString(widgetList[i], "");
			
			
			/*
				Clear XmTextField children.
			*/
			if (XmIsTextField(widgetList[i]))
			   	XmTextFieldSetString(widgetList[i], "");
			
			
			/*
				Clear XmToggleButton children.
			*/
			if (XmIsToggleButton(widgetList[i]))
			   	XmToggleButtonSetState(widgetList[i], False, False);
			
			
			/*
				Clear XmToggleButtonGadget children.
			*/
			if (XmIsToggleButtonGadget(widgetList[i]))
			   	XmToggleButtonGadgetSetState(widgetList[i], False, False);
			
			
			/*
				Clear XmOptionMenu children.
			*/
			if (XmIsRowColumn(widgetList[i]))
			{
			   	XtSetArg(arg[0], XmNrowColumnType, &rcType);
				XtGetValues(widgetList[i], arg, 1);
				if (rcType == XmMENU_OPTION)
			   		SetMenuPos(widgetList[i], 0);
			}


			/*
				Clear XmScale children.
			*/
			if (XmIsScale(widgetList[i]))
			{
				XtSetArg(arg[0], XmNmaximum, &maxValue);
				XtSetArg(arg[1], XmNminimum, &minValue);
				XtGetValues(widgetList[i], arg, 2);


				newValue = (maxValue + minValue) / 2;
				XmScaleSetValue(widgetList[i], newValue);
			}
		}
	}
	
 	return;  
}

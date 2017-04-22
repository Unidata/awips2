
#include "hv_util.h"
#include "HvTime.h"

/***************************************************************************/

void setEditable(Widget w, Boolean isEditable)
{
   	Arg args[10];
	int ac = 0;
   
	
        XtSetArg(args[ac], XmNeditable, isEditable); ac++;
	XtSetValues(w, args, ac);
   
       return;  
}


/***************************************************************************/

void	setFormEditable(Widget parent, Boolean isEditable)
{
   	WidgetList	widgetList;
	Arg		arg[2];
        int		cnt,
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
			   	setFormEditable(widgetList[i], isEditable);
			
			
		   	/*
		   		Clear XmText children.
			*/
		   	if (XmIsText(widgetList[i]))
			        setEditable(widgetList[i], isEditable); 
			
			
			/*
				Clear XmTextField children.
			*/
			if (XmIsTextField(widgetList[i]))
			        setEditable(widgetList[i], isEditable); 
			   
		
		}
	}
	
 	return;  
}

/***********************************************************************/

Boolean isNullDt(dtime_t *dt)
{
     if  (IsNull( DATETIME, (void *) dt) == ISNULL)
     {
	 return 1;	  
	  
     }
     
     else
     {  
     	 return 0;
     }
}

/**********************************************************************/

time_t  get_yearsec_time_t(dtime_t *dt)
{
 
     time_t timet;
     int rv;
     
     if ( ! isNullDt(dt) )
     {
	  rv = yearsec_dt_to_timet(*dt, &timet);
     }
     else
     {
	  timet = 0;
     }
     return timet;
   
}

/*********************************************************************/

unsigned long getValueColor(double value,
			    unsigned long *colors,
			    long numColors,
			    double *thresholds,
			    long numThresholds,
			    unsigned long defaultColor
			   )
{   

	int  chosenThresholdIndex = -1;
	int  chosenColorIndex = -1;
	int i;
	unsigned long color = -1;

	
	/*
		Find the color that matches the amount
	*/
	for ( i = numThresholds; i >= 0; i--)
	{
		if (value >= thresholds[i])
		{   
		      chosenThresholdIndex = i;
		      break;
	   	}
	}
	
	
	if (chosenThresholdIndex < numColors)
	{
		chosenColorIndex = chosenThresholdIndex;   
	}
	else
	{
	 	chosenColorIndex = numColors - 1;   
	}
	
	
	/*
		set the color
	*/
	     
	if (chosenColorIndex != -1)
		color = colors[chosenColorIndex];
        else
	      color = defaultColor;
	     
	return color;
}

/**************************************************************************/

void	printCurrentTime(void)
{
   	char	ansiTime[ANSI_TIME_LEN];
	time_t currentTime;
	
	
	hfs_time(&currentTime);
    	timet_to_yearsec_ansi(currentTime, ansiTime);
	
	printf("%s\n",ansiTime);
	
 	return;  
}

/**************************************************************************/
void    profileTime(char *string)
{
       	char	ansiTime[ANSI_TIME_LEN];
	time_t currentTime;
   
	hfs_time(&currentTime);
    	timet_to_yearsec_ansi(currentTime, ansiTime);
	
	printf("%s  %s\n",string, ansiTime);   
   
}
/**************************************************************************/

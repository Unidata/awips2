#include <X11/cursorfont.h>
#include <Xm/Protocols.h>
#include <time.h>
#include <stdlib.h>

#include "time_defs.h"
#include "gageqc_defs.h"
#include "stage3.h"

extern char temp_date[5];
extern char temp_month[5];
extern char temp_day[5];
extern char temp_hour[5];
extern char temp_num_of_days[5];

static char local_date[5];
static char local_month[5];
static char local_day[5];
static char local_hour[5];

extern int dates_struct_count;
extern void set_choose_hour_window_values(int call);
extern Widget date_up_arrow_BT;
extern Widget date_down_arrow_BT;


void set_call(int call_val)
{
	set_choose_hour_window_values(call_val);
}

void hour_increment(int inc_call_val)
{
	int i;
	for(i=0;i<(MAX_GAGEQC_DAYS * HOURS_PER_DAY);i++)
	{
		memset(local_date, '\0', 5);
		sprintf(local_date, "%d", dates[i].year);
		memset(local_month, '\0', 5);
		sprintf(local_month, "%02d", dates[i].month);
		memset(local_day, '\0', 5);
		sprintf(local_day, "%02d", dates[i].day);
		memset(local_hour, '\0', 5); 
		sprintf(local_hour, "%02d", dates[i].hour);
		if((!strcmp(local_date, temp_date)) && (!strcmp(temp_month,local_month)) && (!strcmp(local_day,temp_day)) && (!strcmp(local_hour,temp_hour)))
		{
			if(i != 0)
			{
				dates_struct_count = i-1;
				set_call(inc_call_val);
				if(i == 1)
				{
					XtSetSensitive(date_up_arrow_BT, FALSE);
					XtSetSensitive(date_down_arrow_BT, TRUE);
				}
				else if(i != 1)
				{
					XtSetSensitive(date_down_arrow_BT, TRUE);
				}
				break;
			}
		}
	}
}

void hour_decrement(int dec_call_val)
{
	int i;
	for(i=0;i<(MAX_GAGEQC_DAYS * HOURS_PER_DAY);i++)
	{
		memset(local_date, '\0', 5);
		sprintf(local_date, "%d", dates[i].year);
		memset(local_month, '\0', 5);
		sprintf(local_month, "%02d", dates[i].month);
		memset(local_day, '\0', 5);
		sprintf(local_day, "%02d", dates[i].day);
		memset(local_hour, '\0', 5);
		sprintf(local_hour, "%02d", dates[i].hour);
		if((!strcmp(local_date, temp_date)) && (!strcmp(temp_month,local_month)) && (!strcmp(local_day,temp_day)) && (!strcmp(local_hour,temp_hour)))
		{
			if(i != (MAX_GAGEQC_DAYS * HOURS_PER_DAY -1))
			{
				dates_struct_count = i+1;
				set_call(dec_call_val);
				if(i == (MAX_GAGEQC_DAYS * HOURS_PER_DAY -2))
				{
					XtSetSensitive(date_down_arrow_BT, FALSE);
					XtSetSensitive(date_up_arrow_BT, TRUE);
				}
				else if(i != (MAX_GAGEQC_DAYS * HOURS_PER_DAY -2))
				{
					XtSetSensitive(date_up_arrow_BT, TRUE);
				}
				break;
			}
		}
	}
}

void hour_increment_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
	hour_increment(0);
}
void hour_decrement_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
	hour_decrement(0);
}
void date_increment_callback(Widget w, int *client_data, XmAnyCallbackStruct *call_data)
{
	int i;
	for(i=0;i<24;i++)
	{
		hour_increment(1);
	}
	set_call(0);
}
void date_decrement_callback(Widget w, int *client_data, XmAnyCallbackStruct *call_data)
{
	int i;
	for(i=0;i<24;i++)
	{
		hour_decrement(1);
	}
	set_call(0);
}
void days_increment_callback(Widget w, int *client_data, XmAnyCallbackStruct *call_data)
{
	int count = 0;
	count = atoi(temp_num_of_days);
	if(count == 10)
	{
		count = 1;
	}
	else
	{
		count++;
	}
	sprintf(temp_num_of_days, "%d", count);
	set_call(0);
}
void days_decrement_callback(Widget w, int *client_data, XmAnyCallbackStruct *call_data)
{
	int count = 0;
	count = atoi(temp_num_of_days);
	if(count == 1)
	{
		count = 10;
	}
	else
	{
		count--;
	}
	sprintf(temp_num_of_days, "%d", count);
	set_call(0);
}

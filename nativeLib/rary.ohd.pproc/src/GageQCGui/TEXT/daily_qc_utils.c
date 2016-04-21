#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <string.h>
#include <ctype.h>

#include "GeneralUtil.h"
#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"

char *area_val_local = NULL;
static char* area_temp_local = NULL;
static int call_init_once_check = 0;
static int call_apps_once_check = 0;
static char* area_is_master = NULL;
static Widget text_box;
static reset_function reset_data_period_window = NULL ;

void set_reset_function ( reset_function function )
{ 
   reset_data_period_window = function;
}

reset_function get_reset_function ( )
{
   return reset_data_period_window ;
}


void set_text_box_widget ( Widget * box )
{
   text_box = * box;
}

Widget get_text_box_widget ( )
{
   return text_box;
}

void init_dqc_local_date_and_area_values()
{
	if(call_init_once_check == 0)
	{
		area_val_local = (char*) malloc(sizeof(char)*250);
		area_temp_local = (char*) malloc(sizeof(char)*250);
		area_is_master = (char*) malloc(sizeof(char)*250);

		memset(area_val_local,'\0',250);
	 	memset(area_temp_local,'\0',250);
		memset(area_is_master,'\0',250);

		XtVaGetValues(text_box, XmNvalue, &area_temp_local, NULL);

                /* Initialize the dqc run date structure members. */
		dqc_run_date.dqc_data_year = 0;
		dqc_run_date.dqc_data_month = 0;
		dqc_run_date.dqc_data_day = 0;
                dqc_run_date.dqc_num_days = 0;
		
		call_init_once_check = 1;	
	}
}

void free_area_val_mem()
{
	if(area_val_local != NULL)
	{
		free(area_val_local);
		area_val_local = NULL;
	}
	if(area_temp_local != NULL)
	{
		free(area_temp_local);
		area_temp_local = NULL;
	}
	if(area_is_master != NULL)
	{
		free(area_is_master);
		area_is_master = NULL;
	}
}

int is_area_master()
{
	int rfc_areas_token_length = 0;
	int i;

	if(call_apps_once_check == 0)
	{
		rfc_areas_token_length = strlen("mpe_site_id");
		get_apps_defaults("mpe_site_id",&rfc_areas_token_length,
                                  area_is_master,&rfc_areas_token_length);
		strip_lblanks(area_is_master);
		strip_tblanks(area_is_master);
		for(i=0;i<strlen(area_is_master);i++)
		{
			if(isupper(area_is_master[i]))
			{
				area_is_master[i] = tolower(area_is_master[i]);
			}
		}
		call_apps_once_check = 1;
	}

	XtVaGetValues(text_box, XmNvalue, &area_temp_local, NULL);
	if(!strcmp(area_is_master,area_temp_local))
	{
		return 1;//true
	}
	return 0;//false
}

int dqc_run_date_changed()
{
	if ( (dqc_run_date.dqc_data_year !=  dqc_run_date_new.dqc_data_year) ||
	   (dqc_run_date.dqc_data_month !=  dqc_run_date_new.dqc_data_month) ||
	   (dqc_run_date.dqc_data_day !=  dqc_run_date_new.dqc_data_day)  ||
           (dqc_run_date.dqc_num_days != dqc_run_date_new.dqc_num_days) )
	{
		return 1;//true
	}
	return 0;//false
}

int dqc_run_area_changed()
{
	XtVaGetValues(text_box, XmNvalue, &area_temp_local, NULL);

	if(strcmp(area_val_local,area_temp_local) != 0)
	{
		return 1;//true	
	}
        else
        {
	       return 0;//false
        }
}

void set_dqc_area_and_date ( )
{
   strcpy ( area_val_local, area_temp_local );
   dqc_run_date.dqc_data_year =  dqc_run_date_new.dqc_data_year;
   dqc_run_date.dqc_data_month =  dqc_run_date_new.dqc_data_month;
   dqc_run_date.dqc_data_day =  dqc_run_date_new.dqc_data_day;
   dqc_run_date.dqc_num_days = dqc_run_date_new.dqc_num_days;
}

void reset_dqc_area_and_date ( )
{
   /* This routine is needed for the cases where a new DailyQC period
      had been chosen, but there are still some days in the current daily_qc
      dataset that need to be QC'd.  This will reset the chosen year,month,
      day, number of days, and area to the previously selected ones. */ 
   strcpy ( area_temp_local, area_val_local );
   dqc_run_date_new.dqc_data_year = dqc_run_date.dqc_data_year;
   dqc_run_date_new.dqc_data_month = dqc_run_date.dqc_data_month;
   dqc_run_date_new.dqc_data_day = dqc_run_date.dqc_data_day;
   dqc_run_date_new.dqc_num_days = dqc_run_date.dqc_num_days;

   /* Set the widget text fields. */
   reset_data_period_window ( & dqc_run_date_new );  
}

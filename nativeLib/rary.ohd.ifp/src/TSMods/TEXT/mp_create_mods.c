/* File: mp_create_mods.c
 */

#include "mods_plot.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "Mods_everythingStruct.h"
/* Add new code to handle UHGCDATE - AV 4/14/04 */
/*  mp_create_mods function creates mods for the UHGCHNG, UHGCDATE, ROCHNG, and
    RRICHNG mods.  The UHGCHNG mod expects to receive the entire set
    of ordinates so it is filled differently than the ROCHNG and RRICHNG.
    For those, the function will compare the original time series array
    (orig_ts_array) with the ts_array in existance when the done button
    is clicked on for the time series passed to it.  It then fills a
    mod structure for each separate group of changed data.
*/

void  mp_create_mods(Mods_everythingStruct *data, int modsgroup, char *currentSegment, 
                               int *UHstartdate, int *UHenddate)
   /*  data->modsPlotData      mods plot data structure pointer     */
{
   int      i, j;               /* Counters             */
   int      start_change;       /* Beginning of time series changes     */
   int      end_change;         /* End of time series changes           */
   int      start_display;      /* Starting time period for the graphical display       */
   int      start_switch=0;     /* flag to determine if at start of changes (0=yes)     */
   char     *command_name;      /* Command name pointer                 */

/*AV -- if(data->modsPlotData->mod_type_sw == UH)
   command_name = "UHGCHNG";

else if(data->modsPlotData->mod_type_sw == UHD)
   command_name = "UHGCDATE";
else if(data->modsPlotData->mod_type_sw == ROCHNG)
   command_name = "ROCHNG";
else if(data->modsPlotData->mod_type_sw == RRICHNG)
   command_name = "RRICHNG"; end*/
/*AV 4/15/05 added for UHG change mod with start and end dates */
switch (data->modsPlotData->mod_type_sw){
   case UH:
      command_name = "UHGCHNG";
      break;
   case UHD:
      command_name = "UHGCDATE";
      break;
   case ROCHNG:
      command_name = "ROCHNG";
      break;
   case RRICHNG:
      command_name = "RRICHNG";
      break;
   default:
      break;
}

/* check that there is still room to make another mod */
if(data->ModIndex > MAX_MODS)
{
   printf("ERROR: data->ModIndex > MAX_MODS\n");
   return;
}

if(data->modsPlotData->mod_type_sw == UH)
{
   data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
   data->ModArray[data->ModIndex]->type          = Mod_format_B3;
   data->ModArray[data->ModIndex]->b3.type       = Mod_format_B3;
   strcpy (data->ModArray[data->ModIndex]->b3.command, command_name);
   start_display = data->modsPlotData->start_run + data->modsPlotData->delta_t;
   
   /* For new mods - UH only has valid date - set others to 0 - dp - 3 Oct. 1995 */
   /*
   data->ModArray[data->ModIndex]->b3.start_date = data->modsPlotData->start_run;
   data->ModArray[data->ModIndex]->b3.end_date = data->modsPlotData->end_run;
   */
   data->ModArray[data->ModIndex]->b3.start_date = 0;
   data->ModArray[data->ModIndex]->b3.end_date = 0;
   data->ModArray[data->ModIndex]->b3.valid_date = data->modsPlotData->valid_run;
   data->ModArray[data->ModIndex]->b3.num_values = data->modsPlotData->num_pts-1;
   data->ModArray[data->ModIndex]->b3.type_of_id = (int)NULL;
   strcpy (data->ModArray[data->ModIndex]->b3.id, data->modsPlotData->seg_name);
   data->ModArray[data->ModIndex]->b3.values = &data->modsPlotData->ts_array[0][0];
   strcpy(data->ModArray[data->ModIndex]->b3.keyword, "");
   data->ModArray[data->ModIndex]->b3.number_of_opers = 1;
   strcpy ((char *)data->ModArray[data->ModIndex]->b3.opname, data->modsPlotData->op_name[0]);
   /*
   printf("type = %d mod.typ = %d num_val = %d start = %d command = %s seg = %s\n",
	  data->ModArray[data->ModIndex]->type,
	  data->ModArray[data->ModIndex]->b3.type,
	  data->ModArray[data->ModIndex]->b3.num_values,
	  data->ModArray[data->ModIndex]->b3.start_date,
	  data->ModArray[data->ModIndex]->b3.command,
	  data->ModArray[data->ModIndex]->b3.id);
   printf("op_name = %s num_oper = %d keyword = %s type_of_id = %d end_date = %d\n",
	  data->ModArray[data->ModIndex]->b3.opname,
	  data->ModArray[data->ModIndex]->b3.number_of_opers,
	  data->ModArray[data->ModIndex]->b3.keyword,
	  data->ModArray[data->ModIndex]->b3.type_of_id,
	  data->ModArray[data->ModIndex]->b3.end_date);
   */
   /* check that there is still room to make another mod */
   if(data->ModIndex > MAX_MODS)
   {
      printf("creating new mod will make data->ModIndex > MAX_MODS\n");
      return;
   }
  
   else
      data->ModIndex++;
 
} /* end of if */
else if(data->modsPlotData->mod_type_sw == UHD){
   data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));
   data->ModArray[data->ModIndex]->type          = Mod_format_B3;
   data->ModArray[data->ModIndex]->b3.type       = Mod_format_B3;
   strcpy (data->ModArray[data->ModIndex]->b3.command, command_name);
   start_display = data->modsPlotData->start_run + data->modsPlotData->delta_t;
   data->ModArray[data->ModIndex]->b3.start_date = (int)UHstartdate;
   data->ModArray[data->ModIndex]->b3.end_date = (int)UHenddate;
    data->ModArray[data->ModIndex]->b3.valid_date = data->modsPlotData->valid_run;
   data->ModArray[data->ModIndex]->b3.num_values = data->modsPlotData->num_pts-1;
   data->ModArray[data->ModIndex]->b3.type_of_id = 0;
   strcpy (data->ModArray[data->ModIndex]->b3.id, data->modsPlotData->seg_name);
   data->ModArray[data->ModIndex]->b3.values = &data->modsPlotData->ts_array[0][0];
   strcpy(data->ModArray[data->ModIndex]->b3.keyword, "");
   data->ModArray[data->ModIndex]->b3.number_of_opers = 1;
   strcpy ((char *)data->ModArray[data->ModIndex]->b3.opname, data->modsPlotData->op_name[0]);
   if(data->ModIndex > MAX_MODS)
   {
      printf("creating new mod will make data->ModIndex > MAX_MODS\n");
      return;
   }
   else
      data->ModIndex++;
}
else  /* ROCHNG or RRICHNG */
   for(i=0; i<data->modsPlotData->num_ts_sel; i++)
      for (j=0; j<data->modsPlotData->num_pts-1; j++)
      {
	 if (data->modsPlotData->ts_array[i][j] != data->modsPlotData->orig_ts_array[i][j])
	 {
	    if (start_switch == 0)
	    {
	       start_change = j;           /* beginning of ts changes */
	       start_switch = 1;           /* flag to determine if at start of changes (0=yes) */
	    }

	    if (data->modsPlotData->ts_array[i][j+1] == data->modsPlotData->orig_ts_array[i][j+1])
	    {
	       /*printf("\nts[%d][%d] = %f  orig_ts[%d][%d] = %f\n", i, j,
			 data->modsPlotData->ts_array[i][j], i, j, data->modsPlotData->orig_ts_array[i][j]);
	       printf("start_mod = %d\n", start_display+j*6);*/
	       data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));

	       end_change = j;
	       start_switch = 0;

	       data->ModArray[data->ModIndex]->type          = Mod_format_B3;
	       data->ModArray[data->ModIndex]->b3.type       = Mod_format_B3;
	       strcpy (data->ModArray[data->ModIndex]->b3.command, command_name);
	       start_display = data->modsPlotData->start_run + data->modsPlotData->delta_t;
	       data->ModArray[data->ModIndex]->b3.start_date =  start_display +
							    start_change*data->modsPlotData->delta_t;
               data->ModArray[data->ModIndex]->b3.valid_date = data->modsPlotData->valid_run;
	       data->ModArray[data->ModIndex]->b3.num_values = (end_change - start_change) + 1;
	       if (data->modsPlotData->mod_type_sw == UH)
		  data->ModArray[data->ModIndex]->b3.end_date =
					      data->ModArray[data->ModIndex]->b3.start_date +
					      data->ModArray[data->ModIndex]->b3.num_values *
									     data->modsPlotData->delta_t;
	       else
		  data->ModArray[data->ModIndex]->b3.end_date = 0;

	       data->ModArray[data->ModIndex]->b3.type_of_id = modsgroup;

	       strcpy (data->ModArray[data->ModIndex]->b3.id, data->modsPlotData->seg_name);
	       data->ModArray[data->ModIndex]->b3.values = &data->modsPlotData->ts_array[i][start_change];
	       strcpy(data->ModArray[data->ModIndex]->b3.keyword, "");
	       /* Fgroup/Range mods */
	       if (data->ModArray[data->ModIndex]->b3.type_of_id ==1 || 
				 data->ModArray[data->ModIndex]->b3.type_of_id ==2) 
		   {
			strcpy(data->ModArray[data->ModIndex]->b3.id, currentSegment);
	                data->ModArray[data->ModIndex]->b3.number_of_opers = 0;
		   }
	       else /* segment mods */
		   {
		   strcpy ((char *)data->ModArray[data->ModIndex]->b3.opname, data->modsPlotData->op_name[i]);
	           data->ModArray[data->ModIndex]->b3.number_of_opers = 1;
		   }
	       /* 
	       printf("type = %d mod.typ = %d num_val = %d start = %d command = %s seg = %s\n",
		      data->ModArray[data->ModIndex]->type,
		      data->ModArray[data->ModIndex]->b3.type,
		      data->ModArray[data->ModIndex]->b3.num_values,
		      data->ModArray[data->ModIndex]->b3.start_date,
		      data->ModArray[data->ModIndex]->b3.command,
		      data->ModArray[data->ModIndex]->b3.id);
	       printf("op_name = %s num_oper = %d keyword = %s type_of_id = %d end_date = %d\n",
		      data->ModArray[data->ModIndex]->b3.opname,
		      data->ModArray[data->ModIndex]->b3.number_of_opers,
		      data->ModArray[data->ModIndex]->b3.keyword,
		      data->ModArray[data->ModIndex]->b3.type_of_id,
		      data->ModArray[data->ModIndex]->b3.end_date);
	       */
	       /* check that there is still room to make another mod */
	       if(data->ModIndex > MAX_MODS)
	       {
		  printf("creating new mod will make data->ModIndex > MAX_MODS\n");
		  return;
	       }
	       else
		  data->ModIndex++;
	    }
	  /*  else
	    {
	       printf("ts = %f orig_ts = %f ", data->modsPlotData->ts_array[i][j],
			data->modsPlotData->orig_ts_array[i][j]);
	    } */
	 }  /* end of for num_pts */
      }  /* end of for */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_create_mods.c,v $";
 static char rcs_id2[] = "$Id: mp_create_mods.c,v 1.7 2006/04/07 14:34:45 aivo Exp $";}
/*  ===================================================  */

}  /* end of Create_mods */

/*******************************************************************************
* FILENAME:            newhour_RFCW.c
* NUMBER OF MODULES:   2
* GENERAL INFORMATION:
*   MODULE 1:          newhour_RFCW
* DESCRIPTION:         The callback for the OK button on the choose hour
*                      window. It loads the data for the new hour.
*
*   MODULE 2:          hour_sensitive_RFCW 
* DESCRIPTION:         Another callback for the OK button on the choose hour
*                      window.  It sensitizes and desensitizes the menu
*                      items on the MPEcontrol and the MPEfields menus.
*
* ORIGINAL AUTHOR:     Unknown 
* CREATION DATE:       Unknown
* ORGANIZATION:        HSEB/OHD-11
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER     DESCRIPTION/REASON
*          2        10/14/2004   Bryon Lawrence Added check of the
*                                               mpe_generate_list and
*                                               mpe_qpe_fieldtype tokens
*                                               to determine which MPEfields
*                                               items to sensitize.
********************************************************************************
*/

#include <string.h>
#include <X11/cursorfont.h>
#include <Xm/ToggleB.h>

#include "display7x7_show.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "GeneralUtil.h"
#include "get_mpe_product_state.h"
#include "map_defines.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "map_resource.h"
#include "newhour_RFCW.h"
#include "read_precip_data.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Xtools.h"
#include "gageqc_defs.h"

extern int dates_struct_count;
extern int restore_on_cancel_dates_struct_coun;
char temp_date[5];
char temp_month[5];
char temp_day[5];
char temp_hour[5];
extern char temp_num_of_days[5];

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   newhour_RFCW
* PURPOSE:       This is one of the callbacks registered on the OK button
*                on the choose date window.  It reads the new data to be
*                displayed for that hour.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME          DESCRIPTION/UNITS
*   Input  Widget      w             The parent widget the callback originated
*                                    in.
*   Input  XtPointer   clientdata    Any data passed by the programmer.
*   Input  XtPointer   calldata      Callback data passed by the system.
*   
* RETURNS:
*   None
* 
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE           NAME               DESCRIPTION
* ControlMenuItemInfo   delta              Indicates who called this routine. 
*                                          Quit -- when called by Quit
*                                                  option
*                                          NextHour -- when called by next
*                                                      hour option
*                                          PrevHour -- when called by 
*                                                      previous hour option
*                                          OkHourPopup -- when called by OK
*                                                         option of
*                                                         choose_dates popup
*
* The enumerated values for Delta are defined in the ControlMenuItemInfo
* enumeration which is defined in the newhour_RFCW.h file.
*
* Display *             display            The X-Motif display
* enum DisplayFieldData display_field_type The type of field being displayed.
* int                   date_array_index   Points to the date selected by
*                                          the user in the date array.
* extern int            display_7x7_flag   Indicates if the display 7 x 7
*                                          window is open.  If it is 
*                                          then close it when swtiching hours.
* extern int            display_gage_table Indicates if the gage table is 
*                                          currently being displayed.  If it
*                                          is then close it.
* static int            first_retrieval    Keeps track of whether or not 
*                                          this is the first retrieval of
*                                          MPE data for purposes of 
*                                          displaying the Save Data dialog.  
* int                   gage_ids_flag      Indicates if there are gage ids
*                                          displayed.
* int                   gage_values_flag   Indicates if there are gage values
*                                          displayed.
* int                   i                  A loop index variable.
* Widget                message            The save message dialog box widget. 
*
*
* DATA FILES AND/OR DATABASE:
*
* Requires MPE data files, radar data files, and gage data from the IHFS
* database.
*
* ERROR HANDLING:
* None.
********************************************************************************
*/

void sensitize_save_buttons ( )
{
   extern char exec_last[][25];
   int split_screen;
   int status;

   /* Make sure the index to the array of date structures is
      within bounds.  If not, return. */
   if ( ( dates_struct_count >= 0 ) &&  
        ( dates_struct_count < MAX_GAGEQC_DAYS * HOURS_PER_DAY ) ) 
   {
      split_screen = is_screen_split ( );

      /* Check to determine whether or not to sensitize the the 
         options to save the top (and bottom if in split screen mode)
         fields as the Best Estimate QPE. */
      status = strcmp ( exec_last[dates_struct_count], "NA" );

      if ( status == 0 )
      {
         /* Desensitize the the save top window option. */
         DeSensitize ( savemaintop_widget );
        
         /* If in split screen mode, desensitize, the 
         save bottom window option as well. */

         if ( split_screen == 1 )
         {
            DeSensitize ( savemainbottom_widget );
         }
      
      }
      else
      {
         Sensitize ( savemaintop_widget );

         if ( split_screen == 1 )
         {
            Sensitize ( savemainbottom_widget );
         }
      }
   }
}

void newhour_RFCW (Widget w, XtPointer clientdata, XtPointer calldata)
{
   static char *mpe_split_screen_token = "mpe_split_screen";
   char reply[10] = { '\0' };
   ControlMenuItemInfo delta;
   Display *display = NULL;
   enum DisplayFieldData display_field_type;
   int date_array_index;
   extern int display_7x7_flag;
   extern int display_gage_table;
   static int first_retrieval = 1;
   int gage_ids_flag;
   int gage_values_flag;
   int reply_len;
   int request_len;
   int split;
   int status;
   extern rubber_band_data rbdata;
   Widget message;

   display_field_type = rad_data[0].field_type;

   delta = (ControlMenuItemInfo) clientdata;

   if ((DataSaved == FALSE) && (first_retrieval == 0))
   {
      date_prev = date_st3;
      switch (display_field_type)
      {
         case display_rMosaic:
         case display_bMosaic:
         case display_lMosaic:
         case display_mMosaic:
         case display_rfcMosaic:
         case display_mlMosaic:
         case display_gageOnly:
         case display_satPrecip:
         case display_lsatPrecip:
         case display_srMosaic :
         case display_sgMosaic :
         case display_srgMosaic :
         case display_avgrMosaic:
         case display_maxrMosaic:
         case display_p3Mosaic :
         case display_rfcbMosaic :
         case display_rfcmMosaic :
         case display_Xmrg :

	    message = create_save_data_dialog_RFCW (delta);
	    XtManageChild (message);
	    XtVaSetValues (message, XmNx, 40, XmNy, 40, NULL);
	    return;

      default:

	 /* Do Nothing. */
	 break;
      }

   }

   /* Retrieve the latest date and RWResult information.
      The call to this routine will make sure that the dates_struct_count
      variable references the correct date. */
   get_dates_RFCW ( );

   /* Check to make sure that the date array bounds are not execeeded. 
      If the requested datetime is out of bounds, then
      remain with the currently displayed date. */

   if ((ControlMenuItemInfo) clientdata != OkHourPopup)
   {
      if (((int) delta < 0) && (dates_struct_count != 0))
      {
	 dates_struct_count--;
      }
      if (((int) delta > 0)
	  && (dates_struct_count != (MAX_GAGEQC_DAYS * HOURS_PER_DAY - 1)))
      {
	 dates_struct_count++;
      }
   }
   date_array_index = dates_struct_count;


   if ((date_array_index >= 0) &&
       (date_array_index < (MAX_GAGEQC_DAYS * HOURS_PER_DAY)))
   {
      date_st3 = dates[date_array_index];
      sprintf (datetime, "%04d-%02d-%02d %02d:00:00", date_st3.year,
	       date_st3.month, date_st3.day, date_st3.hour);
   }

   /* Call the function which determines whether or not to 
      sensitize the save buttons.  Only do this if the save buttons
      exist.  For example, the save buttons will not exist if 
      this function is being called from 
      */
   if ( ( savemaintop_widget != NULL ) && ( savemainbottom_widget != NULL ) )
   {
      sensitize_save_buttons ( );
   }

   /* Sensitize / Desensitize the next and previous hour menu items
      according to the value of date_array_index. */
   if (date_array_index == 1 && widget_struct->next_widget != NULL)
   {
      Sensitize (widget_struct->next_widget);
   }
   else if (date_array_index == 0 && widget_struct->next_widget != NULL)
   {
      DeSensitize (widget_struct->next_widget);
   }
   else if ((date_array_index == (MAX_GAGEQC_DAYS * HOURS_PER_DAY) - 2)
	    && (widget_struct->prev_widget != NULL))
   {
      Sensitize (widget_struct->prev_widget);
   }
   else if ((date_array_index == (MAX_GAGEQC_DAYS * HOURS_PER_DAY) - 1)
	    && (widget_struct->prev_widget != NULL))
   {
      DeSensitize (widget_struct->prev_widget);
   }

  logMessage ("\n");
  logMessage ("******************* %s *********************", date_st3.ldate);
  logMessage ("\n");

/*--------------------------------------------------------------*/
/*   display watch cursor                                       */
/*--------------------------------------------------------------*/
   mSetCursor (M_WATCH);
   display = _get_map_display ();

   /* Force the cursor to display right away. */
   XFlush (display);

 /*--------------------------------------------------------------------*/
   /*     Read radar Data                                                */
 /*--------------------------------------------------------------------*/

   ReadRadarData ();

 /*--------------------------------------------------------------------*/
   /*     Read Gage Data and store in structure                          */
 /*--------------------------------------------------------------------*/

   /* Check if there are mpe gage identifiers and or values being 
      displayed. */
   gage_values_flag = isThereMpeGageValues ();
   gage_ids_flag = isThereMpeGageIds ();

   /* Free the gage data. */
   if (gage != NULL)
   {
      free (gage);
      gage = NULL;
      ngages = 0;
   }


   if (gage_values_flag == 1 || gage_ids_flag == 1)
   {
      ReadGageData_RFCW ();
   }

   if (display_7x7_flag == 1)
   {
      /* End the display 7x7 feature when switching hours. */
      close7x7Callback (NULL, NULL, NULL);
   }

   if (display_gage_table == 1)
   {
      /* Popdown the Gage Table when switching hours. */
      popdown_gagetable (NULL, NULL, NULL);
   }

 /*--------------------------------------------------------------------*/
   /*     initialize variables                                           */
 /*--------------------------------------------------------------------*/

   num_gage_edit = 0;
   num_rad_edit = 0;
   nmosaichr = 0;
   num_prev_poly = 0;
   num_draw_precip_poly = 0;
   deletepoly_flag = 0;
   DataSaved = FALSE;

 /*-----------------------------------------------------------------*/
   /*     Display previous field type                                 */
 /*-----------------------------------------------------------------*/

   display_mpe_data (0);

   /* On the first call to this routine, check to see if the user wants
      a split mpe screen. */
   if (first_retrieval == 1)
   {
      request_len = strlen (mpe_split_screen_token);

      /* Get the value of the mpe_split_screen token. */
      status = get_apps_defaults (mpe_split_screen_token,
				  &request_len, reply, &reply_len);

      if (reply_len > 0)
      {
	 if (reply[1] == 'N' || reply[1] == 'n')
	 {
	    split_screen_callback (NULL, NULL, NULL);
	 }
      }

      /* If this is the HRAP projection, force a good HRAP fit. */
      status = mGetMapProjection ();

      if (status == M_HRAP)
      {
	 rbdata.zoom_state = True;
	 mArealZoom (NULL, NULL, NULL);
      }

      first_retrieval = 0;
   }

   split = is_screen_split ();

   if (split == 1)
   {
      display_mpe_data (1);
   }

}

/******************************** END newhour_RFCW*************/


/********************************************************************/
/*  FUNCTION NAME:   hour_sensitive_RFCW                            */
/*       FUNCTION:   turns off sensitivity of Next Hour and Previous*/
/*                     Hour if first or last hour is chosen from    */
/*                     choose dates list                            */
/*********************************************************************

Function type:
   void

Called by function:
   (expose callback) create_rfcwide_interface

******************************** BEGIN hour_sensitive_RFCW *****/

void hour_sensitive_RFCW (Widget w, XtPointer clientdata, XtPointer calldata)
{
   static Boolean sensitize_transmit_button = False;
   static Boolean sensitize_transmit_bias_button = False;
   const char *pMpeProduct = NULL;
   const char **pQpeFields = NULL;
   static char *mpe_qpe_to_sbn_token = "mpe_send_qpe_to_sbn";
   static char *mpe_transmit_bias_token="mpe_transmit_bias";
   char token_value[120];
   static int first = 1;
   int i;
   int j;
   int length;
   static int mpe_field_states[NUM_BEST_PRODUCTS] = { 0 };
   int product_state;
   int reply_length;
   int status;
   const int verbose = 1;

   /* Check if this is the first time this routine is called. If it is,
      then check the tokens which indicate whether or not the 
      local bias multisensor mosaic and the local biased satellite mosaic
      should be displayed. */
   /* New logic October 14, 2004.  Added call to get_mpe_product_state
      to determine which products MPE FieldGen is going to create.   This
      allows the MPEfield options to be selectively sensitized and
      desensitized. */
   if (first == 1)
   {
      /* Get the value of the mpe_send_qpe_to_d2d token. */
      length = strlen (mpe_qpe_to_sbn_token);

      get_apps_defaults (mpe_qpe_to_sbn_token, &length, token_value,
			 &reply_length);

      if (reply_length > 0)
      {
	 status = strcasecmp (token_value, "ON");

	 if (status == 0)
	 {
	    sensitize_transmit_button = True;
	 }
      }

      /* Get the value of the mpe_transmit_bias token. */
      length = strlen (mpe_transmit_bias_token);

      get_apps_defaults (mpe_transmit_bias_token, &length,
			 token_value, &reply_length);

      if (reply_length > 0)
      {
	 status = strcasecmp (token_value, "ON");

	 if (status == 0)
	 {
	   sensitize_transmit_bias_button = True;
	 }
      }


      /* Get the array of MPE qpe fields. */
      pQpeFields = get_qpe_fields_array ();

      for (i = 0; i < NUM_BEST_PRODUCTS; ++i)
      {
	 pMpeProduct = pQpeFields[i];
	 length = strlen (pMpeProduct);

	 get_mpe_product_state (pMpeProduct, &length, &verbose,
				&product_state, &status);

	 if (status != 0)
	 {
	    flogMessage (stderr, "\nIn routine 'hour_sensitive_RFCW':\n"
		     "An error was encountered in "
		     "get_mpe_product_state.  Setting all\n"
		     "MPE products to ON by default.\n");

	    for (j = 0; j < NUM_BEST_PRODUCTS; ++j)
	    {
	       mpe_field_states[j] = 1;
	    }

	    break;
	 }

	 if (product_state == 1)
	 {
	    mpe_field_states[i] = 1;
	 }
      }

      first = 0;
   }

   for (i = 0; i < (MAX_GAGEQC_DAYS * HOURS_PER_DAY); i++)
      if (strcmp (date_st3.cdate, dates[i].cdate) == 0)
	 break;

   Sensitize (widget_struct->clear_widget);
   Sensitize (widget_struct->next_widget);
   Sensitize (widget_struct->prev_widget);
   Sensitize (widget_struct->radar_site_widget);
   Sensitize (widget_struct->rerun_widget);
   Sensitize (widget_struct->display_bias_widget);
   Sensitize (widget_struct->single_gage_widget);
   Sensitize (widget_struct->timelapse6_widget);
   Sensitize (widget_struct->timelapse12_widget);
   Sensitize (widget_struct->timelapse24_widget);
   Sensitize (widget_struct->timelapseother_widget);
   Sensitize (fullscreen_widget);
   Sensitize (splitscreen_widget);

   /* Only ungrey the Transmit RFC QPE button if
      the mpe_send_qpe_to_d2d token is ON. */
   if ((sensitize_transmit_button == True) &&
       (widget_struct->transmit_rfc_qpe != NULL))
   {
      Sensitize (widget_struct->transmit_rfc_qpe);
   }

   if (widget_struct->stoptime_widget != NULL)
   {
      DeSensitize (widget_struct->stoptime_widget);
   }

   /* Sensitize the Gages menu items. */
   /*Sensitize ( widget_struct->qc_precipitation );
      Sensitize ( widget_struct->qc_temperature );
      Sensitize ( widget_struct->qc_freezing ); */
   Sensitize (widget_struct->pseudo_widget);
   Sensitize (widget_struct->gage_table_widget);
   Sensitize (showids_widget);
   Sensitize (showval_widget);
   Sensitize (widget_struct->gage_color_menu);
   Sensitize (widget_struct->gage_missing_menu);

   if ( ( widget_struct->rfc_qpe_mosaic != NULL ) &&
        ( mpe_field_states [ RFCMOSAIC ] == 1 ) )
   {
      Sensitize ( widget_struct->rfc_qpe_mosaic );
   }

   if ( sensitize_transmit_bias_button == True &&
        widget_struct->transmit_rfc_bias != NULL )
   {
      Sensitize ( widget_struct->transmit_rfc_bias );
   }

   if (mpe_field_states[RMOSAIC] == 1)
   {
      Sensitize (rmosaic_widget);
   }

   if (mpe_field_states[AVGRMOSAIC] == 1)
   {
      Sensitize (avgrmosaic_widget);
   }

   if (mpe_field_states[MAXRMOSAIC] == 1)
   {
      Sensitize (maxrmosaic_widget);
   }

   if (mpe_field_states[BMOSAIC] == 1)
   {
      Sensitize (bmosaic_widget);
   }

   if (mpe_field_states[MMOSAIC] == 1)
   {
      Sensitize (mmosaic_widget);
   }

   if (mpe_field_states[MLMOSAIC] == 1)
   {
      Sensitize (mlmosaic_widget);
   }

   if (mpe_field_states[P3LMOSAIC] == 1)
   {
      Sensitize (p3lmosaic_widget);

      /* Only if the P3LMOSAIC is being used should the
       * option to display the gage triangles be made available. */
      Sensitize (widget_struct->gage_triangles);
   }

   if (mpe_field_states[GAGEONLY] == 1)
   {
      Sensitize (gageonly_widget);
   }

   /* The best estimate QPE is always created. */
   Sensitize (xmrg_widget);

   /* The mulihour product is always available because it is 
      based on the best estimate qpe. */
   Sensitize (multihour_widget);

   /* The height field is always created. It is a reference field. */
   Sensitize (height_widget);

   /* The index field is always created. It is a reference field. */
   Sensitize (index_widget);

   /* The local span field is always created. It is a reference field. */
   Sensitize (locspan_widget);

   Sensitize (locbias_widget);

   /* The local bias mosaic. */
   if (mpe_field_states[LMOSAIC] == 1)
   {
      Sensitize (bias_widget);
   }

   /* The PRISM field is always available.  It is a reference field. */
   Sensitize (prism_widget);

   /* Make the Monthly Max and Min Temperature PRISM fields available. */
   Sensitize (widget_struct->monthly_max_temp);
   Sensitize (widget_struct->monthly_min_temp);

   if (mpe_field_states[SATPRE] == 1)
   {
      Sensitize (satprecip_widget);
   }

   if (mpe_field_states[LSATPRE] == 1)
   {
      Sensitize (lsatprecip_widget);
   }

   if ( mpe_field_states[SRMOSAIC] == 1 )
   {
      Sensitize ( srmosaic_widget );
   }

   if ( mpe_field_states[SGMOSAIC] == 1 )
   {
      Sensitize ( sgmosaic_widget );
   }

   if ( mpe_field_states[SRGMOSAIC] == 1 )
   {
      Sensitize ( srgmosaic_widget );
   }

   if ( mpe_field_states[RFCBMOSAIC] == 1 )
   {
      Sensitize ( rfcbmosaic_widget );
   }

   if ( mpe_field_states[RFCMMOSAIC] == 1 )
   {
      Sensitize ( rfcmmosaic_widget );
   }

   Sensitize (timelapse_widget);
   Sensitize (drawpoly_widget);
   Sensitize (deletepoly_widget);
   sensitize_save_buttons ( );

   if (i == 0)
   {
      XtSetSensitive (widget_struct->next_widget, FALSE);
   }

   if (i == (MAX_GAGEQC_DAYS * HOURS_PER_DAY) - 1)
   {
      XtSetSensitive (widget_struct->prev_widget, FALSE);
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/newhour_RFCW.c,v $";
 static char rcs_id2[] = "$Id: newhour_RFCW.c,v 1.36 2007/10/18 18:07:23 lawrence Exp $";}
/*  ===================================================  */

}

/******************************** END hour_sensitive_RFCW **********/

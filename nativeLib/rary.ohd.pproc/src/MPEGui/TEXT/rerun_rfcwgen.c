/*=========================================================================*/
/*                         FILE NAME:   rerun_rfcwgen.c                    */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   rerun_rfcwgen                      */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <X11/cursorfont.h>

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "gage_pp_init.h"
#include "gage_pp_write_rec.h"
#include "HourlyPP.h"
#include "precip_total.h"
#include "read_precip_data.h"
#include "read_radar_grids.h"
#include "rerun_rfcwgen.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "time_convert.h"
#include "ToolDefs.h"
#include "update_rawPP.h"
#include "Xtools.h"
#include "CodeTimer.h"
#include "main_mpe_fieldgen.h"

/***************************************************************************/
/*  FUNCTION NAME:   rerun_rfcwgen()                                       */
/*       FUNCTION:   rerun mpe_fieldgen calculations                       */
/***************************************************************************/
/*
Function type:
   void

Called by function:
   callback from Rerun RFCWGen button */

/************************************** BEGIN rerun_rfcwgen **************/

void rerun_rfcwgen( Widget w , XtPointer clientdata , XtPointer calldata )
{
   char         command [ 250 ] ;
   char         dirname [ 128 ] ; 
   const char manual_qc_code = 'M';
   char         msgstr [ 512 ] ;
   char         obsdate [ OBSYTD + 1 ] ;
   const char zero_offset_code = '0';
   static char * psstr = "PSEUDO" ;
   double hourly_value;
   static Cursor watch_cursor = (Cursor) NULL;
   double       new_pseudo_value ;
   double        pp_value;
   GagePPoptions options ;
   HourlyPP     hourlyPP ;
   int          hour_slot ;
   int          i, len ;
   int          j;
   int          pp_1hr_dur = 1001;
   int          split;
   int          status ;
   long int     irc;
   long int     num_update ;
   short int    new_hourly_value;
   short int    revision [ NUM_HOURLY_SLOTS ] ;
   short int    revision_6hour [ NUM_6HOURLY_SLOTS ] ;
   WriteInfo write_info ;

   char ** argv_in_mpe_editor = NULL;

 codetimer timer;
 init_timer ( & timer );
 start_timer ( & timer );
   
/*--------------------------------------------------------------*/
/*   display watch cursor                                       */
/*--------------------------------------------------------------*/

   if (watch_cursor == (Cursor) NULL)
   {
      watch_cursor = XCreateFontCursor(XtDisplay(rad_data[0].w), XC_watch);
   }

   XDefineCursor(XtDisplay(rad_data[0].w), XtWindow(rad_data[0].w), 
                 watch_cursor);
   XFlush(XtDisplay(rad_data[0].w));

   /* Test if any gages have been edited */
   num_gage_edit = 0 ;

   if ( gage != NULL )
   {
      for ( i = 0 ; i < ngages ; i++ )
      {
         if ( strcmp ( gage [ i ].edit , "" ) != 0 ) num_gage_edit ++ ;
      }
   }

   /* Store any gage edits into the HourlyPP or PseudoGageVal table. */
   if ( num_gage_edit > 0 )
   {
      options.shef_duplicate = USE_REVCODE ;

      for ( i = 0 ; i < ngages ; ++ i )
      {

         if (strcmp(gage[i].edit,"") != 0)
         {

            /* Determine if this is a pseudo gage or a real gage. */ 
            if ( ( strncmp ( psstr , gage [ i ].id , 6  ) == 0 ) )
            { 
             /* The values are stored differently for 
                pseudo gages. */
               if ( ( gage[i].edit[0] == 'M' ) || 
                    ( gage[i].edit[0] == 'm' ) )
               {
                  new_pseudo_value = -999. ;
               }
               else
               {
                  new_pseudo_value = atof ( gage [ i ].edit ) * 25.4 ;
               }

               update_pseudo_RFCW ( gage [ i ].id , datetime ,
                                    new_pseudo_value , & num_update ) ;

               if ( num_update != 1 ) 
               {
                  fprintf ( stderr , "\nIn routine 'rerun_rfcwide':\n"
                                     "Could not update record in "
                                     "PseudoGageVal table.\n"
                                     "Gage id: %s obstime %s value %7.2f\n" ,
                                      gage [ i ].id , datetime , 
                                      new_pseudo_value ) ;  
               }
            }
            else
            {
               if ( ( gage[i].edit[0] == 'M' ) || 
                    ( gage[i].edit[0] == 'm' ) )
               {
                  new_hourly_value = MISSING_PRECIP;
		  pp_value = MISSING_PRECIP;
               }
               else
               {
		  pp_value = atof ( gage [ i ].edit );
                  hourly_value =  pp_value * (double) 100.0000;
		  hourly_value = round ( hourly_value );
		  pp_value = round ( pp_value );
                  new_hourly_value = ( short int ) hourly_value;
               }

               /* Update records in HourlyPP and/or PseudoGageRadarVal 
                  tables. */
               hour_slot = gage_pp_init ( & hourlyPP,
                                          datetime,
                                          gage [ i ].id, 
                                          gage [ i ].ts,
                                          new_hourly_value,
                                          obsdate,
                                          zero_offset_code,
                                          manual_qc_code );

               for ( j = 0; j < NUM_HOURLY_SLOTS; ++j )
               {
                  revision [ j ] = 0;
               }

               for ( j = 0; j < NUM_6HOURLY_SLOTS; ++j )
               {
                  revision_6hour [ j ] = 0;
               }
               
               revision [ hour_slot - 1 ] = 1 ;

               status = gage_pp_write_rec ( & hourlyPP , & write_info , msgstr ,
                                            "PP" , obsdate , & options , 
                                            revision , revision_6hour, 1 ) ; 

               if ( status != GPP_OK )
               {
                  fprintf ( stderr , "\nIn routine 'rerun_rfcwide':\n"
                                     "The call to 'gage_pp_write_rec' "
                                     "failed for the following reason:\n"
                                     "%s\n" , msgstr ) ;
               }

	       /* Create/update a PP record in the RawPP table for this
		  report. */
	       update_rawPP ( datetime, gage [ i ].id, gage [ i ].ts, 
	                      manual_qc_code, pp_1hr_dur, pp_value );
             }
         }
      }
   }



   /*-------------------------------------------------------------------------*/
   /*     Read Gage Data and store in structure                               */
   /*-------------------------------------------------------------------------*/
//   During re-runs of mpe_fieldgen, auto QC'ing of gages is not done
//   hence there is no need for mpe_editor to look for changes made
//   in the database by mpe_fieldgen. Hence the call below is no longer
//   needed.
   ReadGageData_RFCW();

   /*  Rerun mpe_fieldgen
       mpe_fieldgen opens the database therefore must close and reopen
       database */

/*   closedb(&irc);

   if(irc !=0)
   {
      printf("informix error# %ld ",irc);
      printf(" occurred attempting to close database before\n"
             "rerunning mpe_fieldgen\n");
   }
*/
   len = strlen("pproc_bin");
   get_apps_defaults("pproc_bin",&len,dirname,&len);

   argv_in_mpe_editor = (char**) malloc(4*sizeof(char*));
   if(argv_in_mpe_editor != NULL)
   {
      for(i=0;i<4;i++)
      {
         argv_in_mpe_editor[i] = (char*) malloc(100*sizeof(char));
	 if(argv_in_mpe_editor[i] == NULL)
	 {
	    printf("Fatal memory allocation error\n");
	    exit(0);
	 }
      }
   }
   else
   {
      printf("Fatal memory allocation error\n");
      exit(0);
   }
 
   sprintf(argv_in_mpe_editor[0], "%s ", "mpe_fieldgen.LX");
   sprintf(argv_in_mpe_editor[1], "%d", 1);
   sprintf(argv_in_mpe_editor[2], "%02d", date_st3.hour);
   sprintf(argv_in_mpe_editor[3], "%02d%02d%04d", date_st3.month,date_st3.day,date_st3.year);

   set_mpe_editor_call();
   main_mpe_fieldgen_for_calls_from_editor(4, (char**) argv_in_mpe_editor);
   unset_mpe_editor_call();
  
   sprintf(command,"%s/rerun_mpe_fieldgen  %02d  %02d%02d%04d",dirname,
 		   date_st3.hour,date_st3.month,date_st3.day,date_st3.year);
   printf("rerunning mpe_fieldgen using command: %s\n", command);
   system(command);

   /* Open the database  */


/*   startdb(&irc);

   if(irc !=0)
   {
     printf("informix error# %ld ",irc);
     printf(" occurred attempting to open database after rerunning\n"
            "mpe_fieldgen\n");
     exit(1);
   }
*/
   /* Read radar data */

   ReadRadarData();



   /*-----------------------------------------------------------------*/
   /*     Display previous field type                                 */
   /*-----------------------------------------------------------------*/

   display_mpe_data ( 0 );

   split = is_screen_split ( );

   if ( split == 1 )
   {
      display_mpe_data ( 1 );
   }

   /* Make sure the save top window widget is desensitized. */
   Sensitize ( savemaintop_widget );

   if ( split == 1 )
   {
      Sensitize ( savemainbottom_widget );
   }

   /*-------------------------------------------------------------------------*/
   /*    Remove Watch Cursor                                                  */
   /*-------------------------------------------------------------------------*/
   XUndefineCursor(XtDisplay(rad_data[0].w), XtWindow(rad_data[0].w));

stop_timer_and_print_elapsed_time ( & timer, "Elapsed Time for rerun_rfcwgen", stdout );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/MPEGui/RCS/rerun_rfcwgen.c,v $";
 static char rcs_id2[] = "$Id: rerun_rfcwgen.c,v 1.18 2007/07/11 16:54:41 lawrence Exp $";}
/*  ===================================================  */

}
/********************************************* END rerun_rfcwgen ************/

/*=========================================================================*/
/*                         FILE NAME:   display_field.c                    */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_field                      */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>

#include "delete_polygons_show.h"
#include "display_field.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "draw_precip_poly_RFCW.h"
#include "get_mpe_colors.h"
#include "map_library.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "newhour_RFCW.h"
#include "post_functions.h"
#include "ReadSPE.h"
#include "read_xmrg.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"
#include "Xtools.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_field()                                       */
/*       FUNCTION:   display gridded field                                 */
/***************************************************************************

Function type:
   void

Called by function:
   display_bmosaic
   display_rmosaic
   display_mmosaic
   display_lmosaic
       .
       .
       .

Functions called
   set_colorvalues

************************************** BEGIN display_field **************/

int display_mpe_data ( int map_index )
{
   int status = 0 ;

   switch ( rad_data [ map_index ].field_type )
   {
    case display_rMosaic :

       display_rmosaic ( map_index ) ;
       break ;

    case display_avgrMosaic :

       display_avgrmosaic ( map_index );
       break ;

    case display_maxrMosaic :

       display_maxrmosaic ( map_index );
       break;

    case display_mMosaic :

       display_mmosaic ( map_index ) ;
       break ;

    case display_mlMosaic :

       display_mlmosaic ( map_index ) ;
       break ;

    case display_bMosaic :

       display_bmosaic ( map_index ) ;
       break ;

    case display_Height :

       display_height ( map_index ) ;
       break ;

    case display_Index :

       display_index ( map_index ) ;
       break ;

    case display_gageOnly :

       display_gageonly ( map_index ) ;
       break ;

    case display_lMosaic :

       display_lmosaic ( map_index ) ;
       break ;

    case display_p3Mosaic :

       display_p3lmosaic ( map_index );
       break ;

    case display_Locspan :

       display_locspan ( map_index ) ;
       break ;

    case display_Locbias :

       display_locbias ( map_index ) ;
       break ;

    case display_satPrecip :

       display_satpre ( map_index ) ;
       break ;

    case display_lsatPrecip :

       display_lsatpre ( map_index ) ;
       break ;

    case display_Xmrg :

       display_xmrg ( map_index ) ;
       break ;

    case display_Prism :

       display_prism ( map_index ) ;
       break ;

    case display_maxtempPrism :

       display_max_temp_prism ( map_index );
       break;

    case display_mintempPrism :

       display_min_temp_prism ( map_index );
       break;

    case display_rfcMosaic :

       display_rfcmosaic ( map_index );
       break;

    case display_srMosaic :

       display_srmosaic ( map_index );
       break;

    case display_sgMosaic :

       display_sgmosaic ( map_index );
       break;

    case display_srgMosaic :

       display_srgmosaic ( map_index );
       break;

    case display_rfcbMosaic :

       display_rfcbmosaic ( map_index );
       break;

    case display_rfcmMosaic :

       display_rfcmmosaic ( map_index );
       break;

    default :

       flogMessage ( stderr , "\nIn routine \"display_mpe_data\":\n"
                          "Unrecognized MPE display type.\n" ) ;
       status = 1 ;
   }

   return status ;
}

void initialize_display_memory ( int map_index )
{
  int i;

  /*--------------------------------------------------------------------------*/
  /*  if first display, then set overlay options and malloc space for arrays  */
  /*--------------------------------------------------------------------------*/
  if ( rad_data [ map_index ].first_display == 1)
  {
     rad_data[ map_index ].rfc_on=overlay_def.irfc;
     rad_data[ map_index ].states_on=overlay_def.istate;
     rad_data[ map_index ].rings_on=overlay_def.iradring;
     rad_data[ map_index ].rivers_on=overlay_def.iriver;
     rad_data[ map_index ].basins_on=overlay_def.ibasbound;
     rad_data[ map_index ].cities_on=overlay_def.icity;
     rad_data[ map_index ].county_on=overlay_def.icounty;
     rad_data[ map_index ].gages_on=overlay_def.igage;
     rad_data[ map_index ].maximum_columns = MAXX;
     rad_data[ map_index ].maximum_rows = MAXY;
     rad_data[ map_index ].origin.x = XOR;
     rad_data[ map_index ].origin.y = YOR;

     rad_data[ map_index ].data_array = ( int ** )
                             malloc ( ( rad_data[ map_index ].maximum_columns )
                             * sizeof ( int * ) ) ;

     if ( rad_data[ map_index ].data_array == NULL )
     {
        flogMessage ( stderr , "In routine \"display_field\":\n"
                           "An error was encountered while attempting to\n"
                           "dynamically allocate memory for data\n"
                           "array \"rad_data.data_array\".\n"
                           "Aborting the display of the multisensor\n"
                           "precipitation data.\n" ) ;
        return ;
     }

     for ( i = 0 ; i < rad_data[ map_index ].maximum_columns ; ++ i )
     {
        rad_data[ map_index ].data_array [ i ] = NULL ;
        rad_data[ map_index ].data_array [ i ] = ( int * ) malloc (
                                    ( rad_data[ map_index ].maximum_rows ) *
                                    sizeof ( int ) ) ;

        if ( rad_data[ map_index ].data_array [ i ] == NULL )
        {
           flogMessage ( stderr , "In routine \"display_field\":\n"
                              "An error was encountered while attempting to\n"
                             "dynamically allocate memory for data\n"
                             "array \"rad_data.data_array\".\n"
                             "Aborting the display of the multisensor\n"
                             "precipitation data.\n" ) ;
          display_field_free_memory ( ) ;
          return ;
       }
    }

    rad_data [ map_index ].first_display = 0;
  }

  if ( data_array_tmp == NULL )
  {
     data_array_tmp = ( int ** )
                        malloc ( ( rad_data[ map_index ].maximum_columns )
                        * sizeof ( int * ) ) ;

     if ( data_array_tmp == NULL )
     {
        flogMessage ( stderr , "In routine \"display_field\":\n"
                           "An error was encountered while attempting to\n"
                           "dynamically allocate memory for temporary\n"
                           "data array \"data_array_tmp\".\n"
                           "Aborting the display of the multisensor\n"
                           "precipitation data.\n" ) ;

        display_field_free_memory ( ) ;
        return ;
     }

     for ( i = 0 ; i < rad_data[ map_index ].maximum_columns ; ++ i )
     {
        data_array_tmp [ i ] = NULL ;
        data_array_tmp [ i ] = ( int * ) malloc (
                               ( rad_data[ map_index ].maximum_rows ) *
                            sizeof ( int ) ) ;

        if ( data_array_tmp [ i ] == NULL )
        {
           flogMessage ( stderr , "In routine \"display_field\":\n"
                              "An error was encountered while attempting to\n"
                              "dynamically allocate memory for temporary\n"
                              "data array \"data_array_tmp\".\n"
                              "Aborting the display of the multisensor\n"
                              "precipitation data.\n" ) ;
           display_field_free_memory ( ) ;
           return ;
        }
     }
  }
}

void display_field ( char fname [ ] , int len_fname, int map_index )
{

 static enum DisplayFieldData previous_display_field_type = display_missing ;
 float                mm_factor ;
 int                  i, j, ifile = 0 ;
 int                  num_bytes = 16;
 int                  spe_index ;
 short int *          site_spe = NULL ;
 short int *          temp = NULL ;
 short int            u_thres ;
 int                  word_position = 1;
 short int            x_factor ;
 enum TestByteResult  result = DontFlipBytes ;
 NamedColorSetGroup * pColors = NULL;

 /*----------------------------------------------------------------------*/
 /*  Retrieve the default colors to use for each of the fields.          */
 /*----------------------------------------------------------------------*/

 /*----------------------------------------------------------------------*/
 /*  get colors/levels for appropriate field                             */
 /*----------------------------------------------------------------------*/
 pColors = get_mpe_default_colors ( );

 MPEGui_set_colorvalues ( & rad_data [ map_index ], pColors ) ;

 /*--------------------------------------------------------------------------*/
 /*  if first display, then set overlay options and malloc space for arrays  */
 /*--------------------------------------------------------------------------*/
 initialize_display_memory ( map_index );

 first_display = FALSE ;

 if ( ( previous_display_field_type == display_multiHour ) &&
      ( rad_data [ map_index ].field_type != display_multiHour ) )
 {
    /* Sensitize the widgets deactivated by the former call to display the
       multi-hour precipitation amounts. */
   if ( widget_struct->next_widget != NULL )
   {
      /* Desensitize items on the MPEcontrol menu. */
      Sensitize ( widget_struct->next_widget ) ;
      Sensitize ( widget_struct->prev_widget ) ;
      sensitize_save_buttons ( );
      Sensitize ( widget_struct->rerun_widget ) ;

            /* DeSensitize items on the Tools menu. */
      Sensitize ( widget_struct->clear_widget ) ;
      Sensitize ( fullscreen_widget );
      Sensitize ( splitscreen_widget );

      /* Sensitize items on the BaseFields Menu. */
      Sensitize ( widget_struct->gage_triangles );

      /* DeSensitize items on the Polygons menu. */
      Sensitize ( drawpoly_widget ) ;
      Sensitize ( deletepoly_widget ) ;

      /* Desensitize the items on the Gage menu. */
      Sensitize ( widget_struct->qc_precipitation );
      Sensitize ( widget_struct->qc_temperature );
      Sensitize ( widget_struct->qc_freezing );
      Sensitize ( widget_struct->pseudo_widget );
      Sensitize ( widget_struct->gage_table_widget );
      Sensitize ( widget_struct->single_gage_widget ) ;
      Sensitize ( showids_widget );
      Sensitize ( showval_widget );
      Sensitize ( widget_struct->gage_missing_menu );
      Sensitize ( widget_struct->gage_color_menu );

      /* Desensitize the items on the Climo menu. */
      Sensitize ( widget_struct->display_bias_widget ) ;
      Sensitize ( widget_struct->monthly_max_temp );
      Sensitize ( widget_struct->monthly_min_temp );

      /* Desensitize the items on the Misc menu. */
      Sensitize ( bias_widget ) ;
      Sensitize ( widget_struct->radar_site_widget ) ;
      Sensitize ( timelapse_widget ) ;
      Sensitize ( widget_struct->timelapse6_widget ) ;
      Sensitize ( widget_struct->timelapse12_widget ) ;
      Sensitize ( widget_struct->timelapse24_widget ) ;
      Sensitize ( widget_struct->timelapseother_widget ) ;
      Sensitize ( widget_struct->stoptime_widget ) ;
      Sensitize ( multihour_widget );

   }
 }

 previous_display_field_type = rad_data [ 0 ].field_type ;

 switch ( rad_data [ map_index ].field_type )
 {

    case display_satPrecip :

       /* Allocate space for the array that will contain the SPE data. */
      site_spe = ( short int * ) malloc ( sizeof ( short int ) *
                                          MAXX * MAXY ) ;

      if ( site_spe == NULL )
      {
         flogMessage ( stderr , "\nIn routine 'display_field':\n"
                            "Could not allocate memory for the site_spe\n"
                            "array.\n" ) ;
         display_field_free_memory ( ) ;
         return ;
      }


      ReadSPE ( fname , & XOR , & YOR , & MAXX , & MAXY , site_spe ,
                & x_factor , & u_thres , & mm_factor , & ifile ) ;

      if ( ifile == 0 )
      {
         for ( i = 0 ; i < MAXY ; i ++ )
         {
            spe_index = MAXX * i ;

            for ( j = 0 ; j < MAXX ; j ++ )
            {
               rad_data[ map_index ].data_array[j][i] = site_spe [ j + spe_index ] ;
            }
         }
      }

      if ( site_spe != NULL )
      {
         free ( site_spe ) ;
         site_spe = NULL ;
      }

      break ;

    case display_Prism :
    case display_maxtempPrism :
    case display_mintempPrism :

       /* Test the PRISM header to determine if the bytes in the file need
          to be flipped. */
       TestByteOrder_ ( fname, & num_bytes, & word_position, & result );

    case display_rMosaic :
    case display_avgrMosaic :
    case display_maxrMosaic :
    case display_bMosaic :
    case display_lMosaic :
    case display_mMosaic :
    case display_mlMosaic :
    case display_p3Mosaic :
    case display_rfcMosaic :
    case display_Xmrg :
    case display_gageOnly :
    case display_subValue :
    case display_Height :
    case display_Index :
    case display_Locspan :
    case display_Locbias :
    case display_lsatPrecip :
    case display_srMosaic :
    case display_sgMosaic :
    case display_srgMosaic :
    case display_rfcbMosaic :
    case display_rfcmMosaic :

       temp = ( short * ) malloc ( MAXX * sizeof ( short ) ) ;

       if ( temp == NULL )
       {
          flogMessage ( stderr , "In routine \"display_field\":\n"
                             "An error was encountered while attempting to\n"
                             "allocate memory for the temporary data array\n"
                             "\"temp\".  Aborting the display of the\n"
                             "multisensor precipitation data.\n" ) ;
          display_field_free_memory ( ) ;
          return ;
       }

       /* Test to determine the system that this file was created on. */
       if ( ( rad_data [ map_index ].field_type != display_Prism )  &&
            ( rad_data [ map_index ].field_type != display_maxtempPrism ) &&
            ( rad_data [ map_index ].field_type != display_mintempPrism ) )
       {
          TestXmrgByteOrder_ ( fname , & XOR , & result ) ;

          if ( result == FlipTestFailed )
          {
             flogMessage ( stderr , "\nIn routine \"display_field\":\n"
                                "The call to \"TestXmrgByteOrder_\" failed.\n"
                                "Check to see if %s exists.\n" , fname ) ;
          }
       }

       if ( result != FlipTestFailed )
       {
          for ( i = 0 ; i < MAXY ; ++i )
          {
            read_xmrg ( & MAXX , & MAXY , & i , fname , & len_fname , & ifile ,
                        temp ) ;

            if ( ifile != 0 )
            {
              logMessage ( "Error reading %s -- missing data substituted\n",
                        fname ) ;
               break;
            }

            if ( result == FlipBytes )
            {
               Swap2Bytes_ ( temp , ( size_t * ) & MAXX ) ;
            }

            for ( j = 0 ; j < MAXX ; ++ j )
            {
               * ( * ( rad_data[ map_index ].data_array + j ) + i ) =
                   * ( temp + j ) ;
               * ( * ( data_array_tmp + j ) + i ) = * ( temp + j ) ;
            }

          }
       }

       break ;

    case display_multiHour :

       break ;

    case display_missing :

       flogMessage ( stderr , "In routine \"display_field\":\n"
                          "Reached the \"display_missing\" case in the\n"
                          "switch/case statement.  Program control should\n"
                          "never reach this case.  Check the program logic\n"
                          "for an error. The value of \"display_field_type\"\n"
                          "is %d.\n" , rad_data [ map_index ].field_type ) ;
       ifile = 1 ;

       break ;

    default :

       flogMessage ( stderr , "In routine \"display_field\":\n"
                          "Reached the \"default\" case in the\n"
                          "switch/case statement.  Program control should\n"
                          "never reach this case.  Check the program logic\n"
                          "for an error. The value of \"display_field_type\"\n"
                          "is %d.\n" , rad_data [ map_index ].field_type ) ;
       ifile = 1 ;

       break ;
 }

 /* If any of PRISM toggle buttons is on, then any MPE display is displayed too, make
 the toggle button deslected so that user could not deselect the PRISM toggle button
 to clear all MPE display */

 if ( widget_struct->next_widget != NULL )
 {
    if ((rad_data [ map_index ].field_type != display_Prism ) &&
        (rad_data [ map_index ].field_type != display_maxtempPrism ) &&
        ( rad_data[ map_index ].field_type != display_mintempPrism ))
    {
        if (XmToggleButtonGetState (mpeClimo[ MonthlyPrecipItem] ) )
           XmToggleButtonSetState(mpeClimo[ MonthlyPrecipItem ],False,False);

        if (XmToggleButtonGetState (mpeClimo[ MonthlyMaxTempItem] ) )
           XmToggleButtonSetState(mpeClimo[ MonthlyMaxTempItem ],False,False);

        if (XmToggleButtonGetState (mpeClimo[ MonthlyMinTempItem] ) )
           XmToggleButtonSetState(mpeClimo[ MonthlyMinTempItem ],False,False);
    }
 }

 if ( ifile != 0 || result == FlipTestFailed )
 {
   for ( i = 0 ; i < MAXX ; ++ i )
   {
      for ( j = 0 ; j < MAXY ; ++ j )
      {
         * ( * ( rad_data[ map_index ].data_array + i ) + j ) = -999 ;
         * ( * ( data_array_tmp + i ) + j ) = -999 ;
      }
   }

 }

 /*--------------------------------------------------*/
 /*  display field                                   */
 /*--------------------------------------------------*/

 /* Add polygons to this file. */
 apply_edit_polygons ( rad_data[ map_index ].data_array,
                       date_st3.cdate,
                       date_st3.year,
                       date_st3.month,
                       date_st3.day,
                       date_st3.hour,
                       scale_factor,
                       units_factor,
                       rad_data[ map_index ].field_type,
                       rad_data[ map_index ].maximum_rows,
                       rad_data[ map_index ].maximum_columns,
                       0,
                       0,
                       0,
                       0 );

 /* Check if the delete polygon list is displayed.  If it is,
    then update it. */
 update_polygon_list ( );

 /* Indicate to the exposure routine that there is now
    Mpe data to plot. */
 turnOnMpeData ( ) ;

 /* Free the memory used by the "temp" array. */
 if ( temp != NULL )
 {
    free ( temp ) ;
    temp = NULL ;
 }

 mUpdateMap ( map_index );
 mUpdateLegend ( map_index );

}

/********************************************* END display_field ************/

/***************************************************************************/
/*  FUNCTION NAME:   display_field_free_memory()                           */
/*       FUNCTION:   This routine frees dynamically allocated memory used  */
/*                   by the display_field routine.                         */
/***************************************************************************

Function type:
   void

Called by function:
   display_field

Functions called
  Only standard library routines are called here.

************************************** BEGIN display_field_free_memory *******/

void display_field_free_memory ( )
{
   int i ;
   int j ;

   for ( j = 0; j < NUM_MAP_SCREENS; ++ j )
   {
      /* Free the radar data array. */
      rad_data[j].first_display = 1;

      if ( rad_data[ j ].data_array != NULL )
      {
         for ( i = 0 ; ( rad_data[ j ].data_array [ i ] != NULL )
                       && ( i < rad_data[ j ].maximum_columns ) ; ++ i )
         {
            free ( rad_data[ j ].data_array [ i ] ) ;
            rad_data[ j ].data_array [ i ] = NULL ;
         }

         free ( rad_data[ j ].data_array ) ;
         rad_data[ j ].data_array = NULL ;
      }

      /* Free the temporary data array. */
      if ( data_array_tmp != NULL )
      {
         for ( i = 0 ; ( data_array_tmp [ i ] != NULL )
                         && ( i < rad_data[ j ].maximum_columns ) ; ++ i )
         {
            free ( data_array_tmp [ i ] ) ;
            data_array_tmp [ i ] = NULL ;
         }

        free ( data_array_tmp ) ;
        data_array_tmp = NULL ;

      }

      /* Free the radar data graphics context. */
      if ( rad_data[ j ].gc != NULL )
      {
         for ( i = 0 ; i < rad_data [j].previous_num_levels ; ++ i )
         {
            if ( rad_data[ j ].gc [ i ] != NULL ) XFreeGC ( display ,
                                                    rad_data[ j ].gc [ i ] ) ;
         }

         free ( rad_data[ j ].gc ) ;
         rad_data[ j ].gc = NULL ;
      }
   }

   first_display = TRUE ;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/display_field.c,v $";
 static char rcs_id2[] = "$Id: display_field.c,v 1.35 2007/10/18 18:07:16 lawrence Exp $";}
/*  ===================================================  */

}

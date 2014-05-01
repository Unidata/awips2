
                                       #include <errno.h>
#include <unistd.h> /* This is for the getlogin() routine. */
#include "color_threshold_show.h"
#include "get_colorvalues.h"
#include "map_library.h"
#include "Xtools.h"

/*
	File:		color_threshold_show.c
	Date:		1/05/1998
	Author:		Chip Gobs

	Purpose:	Do the work of the Color Threshold Dialog

*/

/*
    Globals
*/
#define  MAX_COLOURS 600
static char application_name [ APP_NAME_LEN + 1 ];
int	 ct_numColorNames;
RussText ct_colorNames[MAX_COLOURS];

ColorValue * ct_ColorValueHead = NULL;

/* Contains the information which helps determine which records
   from the ColorValue table to display in the Color Thresholds window. */
static ColorWindowSetup color_window_setup = { NULL, NULL, 0, NULL, 'E',
                                               NULL };
static int color_window_initialized = 0;

static NamedColorSetGroup * pColorGroup = NULL;

/***********************************************************************/

ColorValue * get_hardcoded_colorvalues ( const char * application_name ,
                                         const char * color_use_name ,
                                         int duration ,
                                         char threshold_unit ,
                                         int  * numcol ,
                                         int  * numlev,
                                         const NamedColorSetGroup *
                                               pColorSetGroup )
{
   static const char * hardcoded_user_id = "hardcoded" ;
   const char * user_id = NULL;

   ColorValue * cvHead = NULL ;
   ColorValue * cvNode = NULL ;

   int i ;
   int j ;
   int status ;

   user_id = hardcoded_user_id ;

   /*-----------------------------------------------*/
   /*  levels/colors not found in colorvalue table  */
   /*  define hardcoded levels and colors.          */
   /*-----------------------------------------------*/

   /* Loop over the default colors provided by the user.
      Look for the specified color use name. */
   for ( i = 0; i < pColorSetGroup->set_count; ++i )
   {
      status = strcmp ( color_use_name,
               pColorSetGroup->color_group_array[i].color_use_db_name );

      if ( status == 0 )
      {
          printf ( "default levels, colors for %s uses.\n",
                   pColorSetGroup->color_group_array[i].color_use_db_name );
          * numcol =
                pColorSetGroup->color_group_array[i].threshold_array.length;
          * numlev = * numcol - 2;

          /* Set the duration.  There is one case when this is not
             zero. */
          duration = pColorSetGroup->color_group_array[i].default_duration;
          break;
      }

   }

   if ( i == pColorSetGroup->set_count )
   {

      /*--------------------------------------------------------------*/
      /*  if use_name not defined for application then print message  */
      /*   and exit                                                   */
      /*--------------------------------------------------------------*/

      fprintf ( stderr , "\nIn routine 'get_hardcoded_colorvalues':\n"
                         "Colors/levels not defined for application = %s"
                         "use_name = %s  logname = %s\n" ,
                          application_name , color_use_name , user_id ) ;
      exit(1);

    }

    /* Build a linked list containing the default color value
       information. */
    for ( j = 0 ; j < ( * numcol ) ; ++ j )
    {
       cvNode = ( ColorValue * ) malloc ( sizeof ( ColorValue ) ) ;

       if ( cvNode == NULL )
       {
          fprintf ( stderr , "\nIn routine 'get_hardcoded_colorvalues':\n"
                             "Could not allocate %d bytes of memory\n"
                             "for a new node in the linked list of\n"
                             "ColorValue structures.\n" ,
                             sizeof ( ColorValue ) ) ;
          return NULL ;
       }

       memset ( cvNode->userid, '\0', USERID_LEN + 1 );
       strncpy ( cvNode->userid , user_id, USERID_LEN );
       memset ( cvNode->application_name, '\0', APP_NAME_LEN + 1 );
       strncpy ( cvNode->application_name , application_name, APP_NAME_LEN);

       memset (cvNode->color_use_name, '\0', COLOR_USE_NAME_LEN + 1 );
       strncpy ( cvNode->color_use_name ,
                pColorSetGroup->color_group_array[i].color_use_db_name,
                COLOR_USE_NAME_LEN );

       cvNode->duration = duration ;
       cvNode->threshold_value =
       pColorSetGroup->color_group_array[i].threshold_array.thresholds[j].value;
       cvNode->threshold_unit [ 0 ] = threshold_unit ;
       cvNode->threshold_unit [ 1 ] = '\0' ;
       memset ( cvNode->color_name, '\0', COLOR_NAME_LEN + 1 );
       strncpy ( cvNode->color_name ,
                 pColorSetGroup->color_group_array[i].threshold_array.thresholds[j].colorName,
                 COLOR_NAME_LEN );

       if ( cvHead == NULL )
       {
          cvHead = cvNode ;
          ListInit ( & cvNode->list ) ;
       }

       ListAdd ( & cvHead->list , & cvNode->node ) ;
    }

    return cvHead ;
}

static void color_threshold_load_main_list_callback ( Widget w ,
                                                      XtPointer clientdata ,
                                                      XtPointer calldata )
{
   ColorValue colorValue;

   color_threshold_unload_widgets ( & colorValue ) ;
   color_threshold_load_main_list ( & colorValue ) ;
}

/**********************************************************************/

static void create_option_menu_buttons ( )
{
   int i;
   XmString push_button_label;
   Widget push_button;

   for ( i = 0; i < color_window_setup.num_color_use_names; ++i )
   {
      push_button_label = XmStringCreateLocalized (
                          color_window_setup.color_use_strings [ i ] );
      //push_button = XtVaCreateManagedWidget (
       //                           color_window_setup.color_use_strings [ i ],
        //                          xmPushButtonWidgetClass,
        //                          ct_colorUsePDM,
         //                         XmNlabelString,
          //                        push_button_label,
           //                       NULL );
      XmStringFree ( push_button_label );
      XtAddCallback ( push_button , XmNactivateCallback ,
                      color_threshold_load_main_list_callback , NULL ) ;
   }

   /* Set the Option Menu History. */
   //SetMenuPos ( ct_colorUseOM, 0 );

}

/**********************************************************************/

static void color_threshold_update_mpe_display ( )
{
   mUpdateMap ( 0 );
   mUpdateLegend ( 0 );
   mUpdateMap ( 1 );
   mUpdateLegend ( 1 );

   return ;
}

void color_threshold_show(Widget w)
{
   ColorValue colorValue;
   /*colorThresholdDS = NULL;*/
   if (1)
   {

        if ( color_window_initialized == 0 )
        {
           printf ( "The color window must be first initialized by calling "
                    "routine initialize_color_threshold_window.\n" );
           return;
        }

	//create_colorThresholdDS(GetTopShell(w));
        create_option_menu_buttons ( );
	add_color_threshold_callbacks();
   }

   int i = 1;
   if (i == 1)
   {
	/*
	     Manage widget
	*/
	//XtManageChild(colorThresholdFO);
	/*XtManageChild(colorThresholdDS);*/

	/*
	     load main List
	*/
	loadColorNameList();

        /* Retrieve the current option menu selections from the
           Color Thresholds GUI. */
        color_threshold_unload_widgets ( & colorValue ) ;
	color_threshold_load_main_list ( & colorValue ) ;

        //XmListSelectPos(ct_colorValueLI, 1, True);
   }

	return;
}

/**********************************************************************/
int initialize_color_threshold_window( NamedColorSetGroup * pColorSetGroup,
                                       const char * application_name,
                                       const char * user_id,
                                       char threshold_unit )
{
   int i;

   pColorGroup = pColorSetGroup;

   color_window_setup.num_color_use_names = pColorSetGroup->set_count;

   /* Allocate memory for the color use names.  These are the color
      use names as weill be found in the ColorValue table in the IHFS
      database. */
   color_window_setup.color_use_names = ( char ** )
                             malloc ( color_window_setup.num_color_use_names *
                             sizeof ( char * ) );

   /* If memory allocation fails return a 0 to indicate failure. */
   if ( color_window_setup.color_use_names == NULL )
   {
      return 0;
   }

   /* Allocate memory for the color use strings.  These are the strings which
      will appear on the Color Use options menu on the Color Thresholds
      window. */
   color_window_setup.color_use_strings = ( char ** )
                            malloc ( color_window_setup.num_color_use_names *
                            sizeof ( char * ) );

   /* If memory allocation fails return a 0 to indicate failure. */
   if ( color_window_setup.color_use_strings == NULL )
   {
      return 0;
   }

   for ( i = 0; i < color_window_setup.num_color_use_names; ++i )
   {
      color_window_setup.color_use_names[i] =
            strdup (pColorSetGroup->color_group_array[i].color_use_db_name);

      if ( color_window_setup.color_use_names [ i ] == NULL )
      {
        return 0;
      }

      color_window_setup.color_use_strings[i] =
      strdup ( pColorSetGroup->color_group_array[i].color_use_display_string);

      if ( color_window_setup.color_use_strings [ i ] == NULL )
      {
         return 0;
      }
   }

   color_window_setup.application_name = strdup ( application_name );

   if ( color_window_setup.application_name == NULL )
   {
      return 0;
   }

   color_window_setup.threshold_unit = threshold_unit;

   color_window_setup.user_id = strdup ( user_id );

   if ( color_window_setup.user_id == NULL )
   {
      return 0;
   }

   color_window_initialized = 1;

   return 1;
}

/****************************************************************************
*  Saves the currently displayed color set as the office color set.
****************************************************************************/
static void save_office_color_set ( Widget w,
                                    XtPointer client_data,
                                    XtPointer call_data )
{
   char userid [ USERID_LEN + 1 ];
   ColorValue * cvNode = NULL;
   ColorValue color_threshold_settings;
   ColorValue color_threshold_settings_save;

   /* Not sure if this is needed. */
   color_threshold_unload_widgets ( & color_threshold_settings ) ;

   /* Store the userid for future reference. */
   memset ( userid, '\0', USERID_LEN + 1 );
   strncpy ( userid, color_threshold_settings.userid, USERID_LEN );

   /* Set the userid in the list of ColorValue structures to be
      'default'. */
   memset ( color_threshold_settings.userid, '\0', USERID_LEN + 1 );
   strncpy ( color_threshold_settings.userid, "default", USERID_LEN );

   color_threshold_settings_save = color_threshold_settings;

   /* Walk through each node of the linked list of ColorValue structures.
      Copy each threshold value and color name into the office color
      structure.  Save each entry to the ColorValue table.  Do this only
      for the duration given in the Hours text box on the Color Thresholds
      window. */

   if ( ct_ColorValueHead != NULL )
   {
      cvNode = ( ColorValue * ) ListFirst ( & ct_ColorValueHead->list ) ;

      while ( cvNode != NULL )
      {
         if ( cvNode->duration == color_threshold_settings.duration )
         {
            /* We do not want to modify the linked list of ColorValue
               structures.  Must be a copy of each node as it is processed. */
            memset ( color_threshold_settings.color_name, '\0',
                     COLOR_NAME_LEN + 1 );
            strncpy ( color_threshold_settings.color_name, cvNode->color_name,
                      COLOR_NAME_LEN );

            color_threshold_settings.threshold_value = cvNode->threshold_value;

            color_threshold_save_hvColorValue ( & color_threshold_settings ) ;
         }

         /* Retrieve the next color name / value pair from the Color
            Value linked list. */
         cvNode = ( ColorValue * ) ListNext ( & cvNode->node ) ;
      }
   }

   color_threshold_save_hvColorValue ( & color_threshold_settings_save ) ;

   /* Reset the userid to what it was as unloaded from the Color
      Thresholds Window. */
   memset ( color_threshold_settings.userid, '\0', USERID_LEN + 1 );
   strncpy ( color_threshold_settings.userid, userid, USERID_LEN );

   /* Update the current ColorValue linked list. */
   color_threshold_load_main_list ( & color_threshold_settings ) ;

   //XmListSelectPos(ct_colorValueLI, 1, True);
   color_threshold_update_mpe_display ( ) ;

   return ;
}

/****************************************************************************
*  Asks the user if he REALLY wants to save the currently displayed color set
*  as the office color set.
****************************************************************************/

static void save_office_set_confirm ( Widget w,
                                      XtPointer client_data,
                                      XtPointer call_data )
{
   ColorValue * pSelectedValue = NULL ;
   int	pos = 1;//ListRsrcGetFirstSelectedPos ( ct_colorValueLI ) ;
   Widget	qstDS,
		okPB;

   if ( ct_ColorValueHead != NULL )
   {
      pSelectedValue = ( ColorValue * )
                       ListNth ( & ct_ColorValueHead->list , pos ) ;
      /*qstDS = QuestionDialog(colorThresholdDS,
                    "This may overwrite the current office color set. "
                    "Continue?");*/
      //SetTitle(qstDS, "Office Color Set Save Confirmation");

      /*
        Get the XmMessageBox ok button,
        and associate a callback to it.
      */
      okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
      XtAddCallback(okPB, XmNactivateCallback, save_office_color_set, NULL );

      if(! XtIsManaged(qstDS))
      {
         XtManageChild(qstDS);
      }
   }

   return;
}

static void copy_default_color_set_to_user_color_set ( Widget w,
                                                       XtPointer client_data,
                                                       XtPointer call_data )
{
   char where_clause [ 512 ];
   ColorValue color_threshold_settings;
   ColorValue * cvNode = NULL;
   ColorValue * nextNode = NULL;
   ColorValue * pHardCodedColorSet = NULL;
   int numcol;
   int numlev;
   Widget infoDS;

   /* Retrieve the settings from the Color Thresholds window. */
   color_threshold_unload_widgets ( & color_threshold_settings ) ;

   /* Retrieve the hardcoded color list. There should always be
      a hardcoded color list. */
   pHardCodedColorSet = get_hardcoded_colorvalues (
                                   color_threshold_settings.application_name,
                                   color_threshold_settings.color_use_name,
                                   color_threshold_settings.duration,
                                   color_threshold_settings.threshold_unit[0],
                                   & numcol,
                                   & numlev,
                                   pColorGroup );

   if ( pHardCodedColorSet == NULL )
   {
      /* Launch a dialog box indicating that there is no
         hardcoded color set. */
      /*infoDS = InfoDialog ( colorThresholdDS,
                            "A Default Color Set Does Not Exist." );
      SetTitle ( infoDS , "No Default Color Set" ) ;*/
      return;

   }

   /* Delete the user color list, if it exists */
   /* Create the where clause to delete the user color list. */
   sprintf ( where_clause, "WHERE userid='%s' "
                           "AND application_name='%s' "
                           "AND color_use_name='%s' "
                           "AND duration=%ld",
                           color_threshold_settings.userid,
                           color_threshold_settings.application_name,
                           color_threshold_settings.color_use_name,
                           color_threshold_settings.duration );

   DeleteColorValue ( where_clause );

   /* Store the hardcoded color list as the user color list. */
   cvNode = ( ColorValue * ) ListFirst ( & pHardCodedColorSet->list ) ;

   while ( cvNode != NULL )
   {
      /* Copy the userid into the node. */
      memset ( cvNode->userid, '\0', USERID_LEN + 1 );
      strncpy ( cvNode->userid, color_threshold_settings.userid, USERID_LEN );

      color_threshold_save_hvColorValue ( cvNode ) ;

      /* Retrieve the next color name / value pair from the Color
         Value linked list. */
      cvNode = ( ColorValue * ) ListNext ( & cvNode->node ) ;
   }

   /* Free the linked list of hardcoded color value structures. */
   cvNode = ( ColorValue * ) ListFirst ( & pHardCodedColorSet->list );

   while ( cvNode != NULL )
   {
     nextNode = (ColorValue * ) ListNext ( & cvNode->node );
     free ( cvNode );
     cvNode = nextNode;
   }

   color_threshold_load_main_list ( & color_threshold_settings ) ;
   //XmListSelectPos(ct_colorValueLI, 1, True);
   color_threshold_update_mpe_display ( ) ;

}

/************************************************************************/

static void copy_default_to_user_set_confirm ( Widget w,
                                               XtPointer client_data,
                                               XtPointer call_data )
{
   ColorValue * pSelectedValue = NULL ;
   int	pos = 1;//ListRsrcGetFirstSelectedPos ( ct_colorValueLI ) ;
   Widget	qstDS,
		okPB;

   if ( ct_ColorValueHead != NULL )
   {
      pSelectedValue = ( ColorValue * )
                       ListNth ( & ct_ColorValueHead->list , pos ) ;
      /*qstDS = QuestionDialog(colorThresholdDS,
                    "This may overwrite the current user color set. "
                    "Continue?");
      SetTitle(qstDS, "Color Set Copy Confirmation");*/

      /*
        Get the XmMessageBox ok button,
        and associate a callback to it.
       */
       okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
       XtAddCallback(okPB, XmNactivateCallback,
                     copy_default_color_set_to_user_color_set,
                     ( XtPointer ) pSelectedValue );

       if(! XtIsManaged(qstDS))
       {
          XtManageChild(qstDS);
       }
   }

   return;
}

static void copy_office_color_set_to_user_color_set ( Widget w,
                                                      XtPointer client_data,
                                                      XtPointer call_data )
{
   char where_clause [512];
   ColorValue   color_threshold_settings;
   ColorValue * pOfficeColorSet = NULL;
   ColorValue * pOfficeColorSetNode = NULL;
   Widget infoDS;

   /* Retrieve the Color Threshold Window settings. */
   color_threshold_unload_widgets ( & color_threshold_settings ) ;

   /* Based on these settings, build the where clause. */
   sprintf ( where_clause, "WHERE userid='default' "
                           "AND application_name='%s' "
                           "AND color_use_name='%s' "
                           "AND duration=%ld "
                           "AND threshold_unit='%s' ",
                           color_threshold_settings.application_name,
                           color_threshold_settings.color_use_name,
                           color_threshold_settings.duration,
                           color_threshold_settings.threshold_unit );

   /* Check if there is a currently defined office set. */
   pOfficeColorSet = GetColorValue ( where_clause );

   /* If not, show a dialog box indicating that there is no office
      color set to copy for this color_use and duration. */
   if ( pOfficeColorSet == NULL )
   {
      /*infoDS = InfoDialog ( colorThresholdDS,
                            "An Office Color Set does not Exist." );
      SetTitle ( infoDS , "No Office Color Set" ) ;*/
      return;
   }

   /* Delete the user color list, if it exists */
   /* Create the where clause to delete the user color list. */
   sprintf ( where_clause, "WHERE userid='%s' "
                           "AND application_name='%s' "
                           "AND color_use_name='%s' "
                           "AND duration=%ld",
                           color_threshold_settings.userid,
                           color_threshold_settings.application_name,
                           color_threshold_settings.color_use_name,
                           color_threshold_settings.duration );

   DeleteColorValue ( where_clause );

   /* Loop over the the Office Color set.  For each ColorValue node,
      rename the 'default' userid to the current userid. */
   pOfficeColorSetNode = pOfficeColorSet;

   while ( pOfficeColorSetNode != NULL )
   {
      memset ( pOfficeColorSetNode->userid, '\0', USERID_LEN + 1 );
      strncpy ( pOfficeColorSetNode->userid,
                color_threshold_settings.userid, USERID_LEN );
      color_threshold_save_hvColorValue ( pOfficeColorSetNode ) ;
      pOfficeColorSetNode = ( ColorValue * )
                            ListNext ( & pOfficeColorSetNode->node );
   }

   /* Free the memory associated with the office color value linked
      list. */
   FreeColorValue ( pOfficeColorSet );
   pOfficeColorSetNode = NULL;

   /* Update the current ColorValue linked list. */
   color_threshold_load_main_list ( & color_threshold_settings ) ;

   //XmListSelectPos(ct_colorValueLI, 1, True);
   color_threshold_update_mpe_display ( ) ;
}

static void copy_office_to_user_set_confirm ( Widget w,
                                              XtPointer client_data,
                                              XtPointer call_data )
{
   Widget	qstDS,
		okPB;


   /*qstDS = QuestionDialog(colorThresholdDS,
                         "This may overwrite the current user color set. "
                         "Continue?");*/
   /* Save the office color set with the current user id.  This may
      overwrite color records beloging to the user. */
   //SetTitle(qstDS, "Color Set Copy Confirmation");

   /*
     Get the XmMessageBox ok button,
     and associate a callback to it.
    */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback,
                 copy_office_color_set_to_user_color_set,
                 NULL );

   if( ! XtIsManaged ( qstDS ) )
   {
      XtManageChild ( qstDS );
   }

   return;
}

static void delete_user_color_set ( Widget w,
                                    XtPointer client_data,
                                    XtPointer call_data )
{
   char where_clause[512];
   ColorValue color_threshold_settings;

   /* Not sure if this is needed. */
   color_threshold_unload_widgets ( & color_threshold_settings ) ;

   /* Build a where clause. */
   sprintf ( where_clause, "WHERE userid='%s' "
                           "AND application_name='%s' "
                           "AND color_use_name='%s' "
                           "AND duration=%ld",
                           color_threshold_settings.userid,
                           color_threshold_settings.application_name,
                           color_threshold_settings.color_use_name,
                           color_threshold_settings.duration );
   DeleteColorValue ( where_clause );

   /* Update the current ColorValue linked list. */
   color_threshold_load_main_list ( & color_threshold_settings ) ;

   //XmListSelectPos(ct_colorValueLI, 1, True);
   color_threshold_update_mpe_display ( ) ;

   return ;

}

static void delete_user_set_confirm ( Widget w,
                                      XtPointer client_data,
                                      XtPointer call_data )
{
   ColorValue * pSelectedValue = NULL ;
   int	pos = 1;//ListRsrcGetFirstSelectedPos ( ct_colorValueLI ) ;
   Widget	qstDS,
		okPB;

   if ( ct_ColorValueHead != NULL )
   {
      pSelectedValue = ( ColorValue * )
                       ListNth ( & ct_ColorValueHead->list , pos ) ;
      /*qstDS = QuestionDialog(colorThresholdDS,
                    "This will delete the current user color set. "
                    "Continue?");
      SetTitle(qstDS, "Delete User Color Set Confirmation");*/

      /*
        Get the XmMessageBox ok button,
        and associate a callback to it.
       */
       okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
       XtAddCallback(okPB, XmNactivateCallback, delete_user_color_set,
                     ( XtPointer ) pSelectedValue );

       if(! XtIsManaged(qstDS))
       {
          XtManageChild(qstDS);
       }
   }

   return;
}

static void delete_office_color_set ( Widget w,
                                      XtPointer client_data,
                                      XtPointer call_data )
{
   char where_clause [ 512 ];
   ColorValue color_threshold_settings;

   /* Unload the settings from the Color Thresholds Window. */
   color_threshold_unload_widgets ( & color_threshold_settings ) ;

   /* Set the userid to 'default'.  This is the id of the office
      color set. */
   memset ( color_threshold_settings.userid, '\0', USERID_LEN + 1);
   strncpy ( color_threshold_settings.userid, "default", USERID_LEN );

   /* Build a where clause. */
   sprintf ( where_clause, "WHERE userid='default' "
                           "AND application_name='%s' "
                           "AND color_use_name='%s' "
                           "AND duration=%ld",
                           color_threshold_settings.application_name,
                           color_threshold_settings.color_use_name,
                           color_threshold_settings.duration );
   DeleteColorValue ( where_clause );

   /* Update the current ColorValue linked list. */
   color_threshold_load_main_list ( & color_threshold_settings ) ;

   //XmListSelectPos(ct_colorValueLI, 1, True);
   color_threshold_update_mpe_display ( ) ;

   return ;

}

static void delete_office_set_confirm ( Widget w,
                                        XtPointer client_data,
                                        XtPointer call_data )
{
   ColorValue * pSelectedValue = NULL ;
   int	pos = 1;//ListRsrcGetFirstSelectedPos ( ct_colorValueLI ) ;
   Widget	qstDS,
		okPB;

   if ( ct_ColorValueHead != NULL )
   {
      pSelectedValue = ( ColorValue * )
                       ListNth ( & ct_ColorValueHead->list , pos ) ;
      /*qstDS = QuestionDialog(colorThresholdDS,
                    "This will delete the office color set. "
                    "Continue?");
      SetTitle(qstDS, "Delete Office Color Set Confirmation");*/

      /*
        Get the XmMessageBox ok button,
        and associate a callback to it.
       */
      okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
      XtAddCallback(okPB, XmNactivateCallback, delete_office_color_set, NULL);

      if(! XtIsManaged(qstDS))
      {
         XtManageChild(qstDS);
      }
   }

   return;
}

/**********************************************************************/

void	add_color_threshold_callbacks(void)
{
	Atom		atom;

	/*
		Window manager callbacks.
	*/
	/*atom = XmInternAtom(XtDisplay(colorThresholdDS), "WM_DELETE_WINDOW",
                            False);
	XmAddWMProtocolCallback(colorThresholdDS, atom, color_threshold_close,
                                NULL);*/

	/*
		Widget callbacks.
	*/
        /*XtAddCallback(ct_colorValueLI, XmNdefaultActionCallback,
                      color_threshold_load_widgets_callback, NULL);
        XtAddCallback(ct_colorValueLI, XmNbrowseSelectionCallback,
                      color_threshold_load_widgets_callback, NULL);

        XtAddCallback(ct_colorNameLI, XmNdefaultActionCallback,
                      pickColorCallback, NULL);
        XtAddCallback(ct_colorNameLI, XmNbrowseSelectionCallback,
                      pickColorCallback, NULL);*/

	/*XtAddCallback(ct_saveuserPB, XmNactivateCallback,
                      color_threshold_apply, NULL);
	XtAddCallback(ct_closePB, XmNactivateCallback, color_threshold_close,
                      NULL);

	XtAddCallback(ct_deleteentryPB, XmNactivateCallback,
                      color_threshold_confirm_delete, NULL);
	XtAddCallback(ct_copydefaultPB, XmNactivateCallback,
                      copy_default_to_user_set_confirm, NULL);*/

        /* New callbacks ... added Sept 12, 2006. */
      /*  XtAddCallback(ct_saveofficesetPB, XmNactivateCallback,
                      save_office_set_confirm, NULL );
        XtAddCallback(ct_copyofficesetPB, XmNactivateCallback,
                      copy_office_to_user_set_confirm, NULL );
        XtAddCallback(ct_deleteusersetPB, XmNactivateCallback,
                      delete_user_set_confirm, NULL );
        XtAddCallback(ct_deleteofficesetPB, XmNactivateCallback,
                      delete_office_set_confirm, NULL );*/

	/*
		Add TextFilter callbacks.
	*/

	return;
}

/****************************************************************************/

/*
void	color_threshold_add_callbacks(void)
{

   XtAddCallback(color_thresholdTE, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE_AND_VALIDSYMBOLS+SPACES));
   XtAddCallback(cphoneTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);


   return;
}


void	color_threshold_remove_callbacks(void)
{

   XtRemoveCallback(color_thresholdTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE_AND_VALIDSYMBOLS+SPACES));
   XtRemoveCallback(cphoneTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);

   return;
}
*/

/************************************************************************/

void	color_threshold_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   ColorValue colorValue;

   color_threshold_save ( & colorValue ) ;

   /* Update the MPE data display if necessary. */
   color_threshold_update_mpe_display ( ) ;

   return ;
}

/************************************************************************/

int color_threshold_save ( ColorValue * pColorValue )
{
     ColorValue * cvNode = NULL ;
     int compare_result ;
     int pos = -1;

     color_threshold_unload_widgets ( pColorValue ) ;

     /* Check to deterine if the list of colors does not currently
        belong to the user.  If it doesn't then make it the user's new
        color list. */
     if ( ct_ColorValueHead != NULL )
     {
         cvNode = ( ColorValue * ) ListFirst ( & ct_ColorValueHead->list ) ;

         if ( cvNode != NULL )
         {
            compare_result = strcmp ( pColorValue->userid , cvNode->userid ) ;

            if ( compare_result != 0 )
            {
                 while ( cvNode != NULL )
                 {
                    strcpy ( cvNode->userid , pColorValue->userid ) ;
                    color_threshold_save_hvColorValue ( cvNode ) ;
                    cvNode = ( ColorValue * ) ListNext ( & cvNode->node ) ;
                 }
            }
         }
     }

     /* Implement the user's changes. */
     color_threshold_save_hvColorValue ( pColorValue ) ;

     color_threshold_load_main_list ( pColorValue ) ;

     pos = getColorValueListPos(ct_ColorValueHead, pColorValue);

     //XmListSelectPos(ct_colorValueLI, pos, True);
     //XmListSetPos(ct_colorValueLI, pos);

     return 0 ;
}

/************************************************************************/

void    color_threshold_unload_widgets(ColorValue *colorValue)
{
   	int colorUse;
	ThresholdType thresholdType;

	char		* buf = NULL ;
        char            * userid = NULL ;

	int  		pos = 0;
	long		hours;
	long		seconds;

	memset(colorValue, '\0', sizeof(ColorValue));

	/*
	     Retrieve the userid.
	*/
        /* Get the user's id. */
        userid = color_window_setup.user_id;
	strcpy(colorValue->userid , userid ) ;

	/*
	     For NOW set application_name to "hmapmpe"
	     and threshold_unit to 'E'
	*/
	strcpy(colorValue->application_name,
               color_window_setup.application_name );
	colorValue->threshold_unit[0] = color_window_setup.threshold_unit;

	/*
	     unload Color Use OM
	*/
	/*colorUse = GetMenuPos(ct_colorUseOM);*/

        strcpy ( colorValue->color_use_name ,
                 color_window_setup.color_use_names [ colorUse ] ) ;

	/*
	    unload ct_thresholdTypeOM widget
	*/
//	thresholdType = GetMenuPos(ct_thresholdTypeOM);

	switch (thresholdType)
	{
	   case MISSING_TYPE:
		colorValue->threshold_value = MISSING_VALUE;
	      break;

	   case LESS_MIN_TYPE:
		colorValue->threshold_value = LESS_THAN_MIN_VALUE;
	      break;


	   case GREATER_EQUAL_TYPE:

		colorValue->threshold_value = 1.0;

/*		if ( ( buf = XmTextGetString(ct_thresholdValueTE) ) != NULL )
		{
		     if (IsNull(CHAR, buf) == NOTNULL)
		     {
			  colorValue->threshold_value = atof(buf);
		     }
		     XtFree(buf);

	        }
*/
	        break;

	   default:
	      break;
	}


	/*
		Get duration value
	*/
/*	if ( ( buf = XmTextGetString(ct_durationTE) ) != NULL )
	{
		if ( IsNull(CHAR, buf) == NOTNULL)
		{
		     	hours = atof(buf);
		        seconds = hours * SECONDS_PER_HOUR;
			colorValue->duration = seconds;
		}
		XtFree(buf);
	}
*/

	/*
	     get selected color
	*/
//	pos = ListRsrcGetFirstSelectedPos(ct_colorNameLI);
	if (pos > 0)
	{
             strcpy(colorValue->color_name, ct_colorNames[pos - 1]);
	}

	return;
}
/************************************************************************/

void color_threshold_save_hvColorValue(ColorValue *colorValue)
{

	char		where[BUFSIZ];
	char		msg[BUFSIZ];
	long		count;
	long 		error;


        hvColorValueCreateKeyWhere ( colorValue , where , COLOR_UPDATE );

	count =	recordCount("ColorValue", where);


	if (count > 0)
	{
	     if ((error = UpdateColorValue(colorValue, where)) != 0)
	     {
		  sprintf(msg, "Unable to update record: %ld\n", error);
		  //ErrorDialog(colorThresholdDS, msg);
	     }
	}

	else
	{
	     if ((error = PutColorValue(colorValue)) != 0)
	     {
		  sprintf(msg, "Unable to insert record: %ld\n", error);
		  //ErrorDialog(colorThresholdDS, msg);

	     }
	}
}

/************************************************************************/

void	color_threshold_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(1)
   {
      //XtDestroyWidget(colorThresholdDS);
      //colorThresholdDS = NULL;

      if (ct_ColorValueHead)
      {
           FreeColorValue(ct_ColorValueHead);
           ct_ColorValueHead = NULL;
      }
   }

   return;
}

/************************************************************************/

void color_threshold_confirm_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   ColorValue * pSelectedValue = NULL ;
   int	pos = 1;//ListRsrcGetFirstSelectedPos ( ct_colorValueLI ) ;
   Widget       infoDS ;
   Widget	qstDS,
		okPB;


   if ( pos == -1 )
   {
      return ;
   }

   if ( ct_ColorValueHead != NULL )
   {
      pSelectedValue = ( ColorValue * )
                       ListNth ( & ct_ColorValueHead->list , pos ) ;

      /* Check to make sure that the user is not trying to delete a missing
         or minimum threshold value.  These values are required by
         Hydroview/MPE. */
      if ( ( pSelectedValue->threshold_value == MISSING_VALUE ) ||
           ( pSelectedValue->threshold_value == LESS_THAN_MIN_VALUE ) )
      {
         /* Can't delete this value. */
        /* infoDS = InfoDialog ( colorThresholdDS ,
                      "Can't delete missing or minimum threshold entries." ) ;
         SetTitle ( infoDS , "Delete Warning" ) ;*/
      }
      else
      {

        /* qstDS = QuestionDialog(colorThresholdDS,
                                "Do you wish to delete this entry?");
         SetTitle(qstDS, "Delete Confirmation");*/

         /*
           Get the XmMessageBox ok button,
   	   and associate a callback to it.
         */
         okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
         XtAddCallback(okPB, XmNactivateCallback, color_threshold_delete,
                       ( XtPointer ) pSelectedValue );

         if(! XtIsManaged(qstDS))
            XtManageChild(qstDS);
      }
   }

   return;
}

/************************************************************************/

void	color_threshold_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   char	where[BUFSIZ] ;
   ColorValue colorValue ;
   int  compare_result ;
   int	pos = 1;//ListRsrcGetFirstSelectedPos(ct_colorValueLI) ;
   ColorValue selected_color_value ;
   ColorValue * cvNode = NULL ;
   ColorValue * pSelectedValue = ( ColorValue * ) ptr ;

   /* Check to make sure that there is a selected position in the list. */
   if ( pos == -1 )
   {
      return ;
   }

   /* Unload the values of the widgets. */
   color_threshold_unload_widgets ( & colorValue ) ;

   if ( ct_ColorValueHead != NULL )
   {
      cvNode = ( ColorValue * ) ListFirst ( & ct_ColorValueHead->list ) ;

      if ( cvNode != NULL )
      {
         compare_result = strcmp ( colorValue.userid , cvNode->userid ) ;

         if ( compare_result != 0 )
         {

            while ( cvNode != NULL )
            {
               strcpy ( cvNode->userid , colorValue.userid ) ;
               color_threshold_save_hvColorValue ( cvNode ) ;
               cvNode = ( ColorValue * ) ListNext ( & cvNode->node ) ;
            }

            if ( pSelectedValue != NULL )
            {
               strcpy ( pSelectedValue->userid , colorValue.userid ) ;
            }

         }

         if ( pSelectedValue != NULL )
         {
            /* Make a copy of the selected Color Value. This is
               because the color_threshold_load_main_list routine
               frees the linked list. */
           strcpy ( selected_color_value.userid , pSelectedValue->userid ) ;
           strcpy ( selected_color_value.application_name ,
                    pSelectedValue->application_name ) ;
           strcpy ( selected_color_value.color_use_name ,
                    pSelectedValue->color_use_name ) ;
           selected_color_value.duration = pSelectedValue->duration ;
           selected_color_value.threshold_value =
                                           pSelectedValue->threshold_value ;
           selected_color_value.threshold_unit [ 0 ] =
                                       pSelectedValue->threshold_unit [ 0 ] ;
           strcpy ( selected_color_value.color_name ,
                    pSelectedValue->color_name ) ;

            /*
      	      Delete record from dbms,
            	   and reload list and select pos.
            */
            //SetCursor(colorThresholdFO, XC_watch);

            hvColorValueCreateKeyWhere ( pSelectedValue , where ,
                                         COLOR_DELETE ) ;

            if ( strlen ( where ) )
            {
               DeleteColorValue ( where ) ;
               color_threshold_load_main_list ( & selected_color_value ) ;

               //XmListSelectPos(ct_colorValueLI, pos, True);
               //XmListSetPos(ct_colorValueLI, pos);

               /* Update the MPE data display if necessary. */
               color_threshold_update_mpe_display ( ) ;
            }

            //UnsetCursor(colorThresholdFO);
         }
      }
   }

   return ;
}

/************************************************************************/

void	color_threshold_confirm_default ( Widget w , XtPointer ptr ,
                                          XtPointer cbs )
{
   ColorValue * pColorValue = NULL ;
   Widget	qstDS,
		okPB;

   if ( ct_ColorValueHead != NULL )
   {

      pColorValue = ( ColorValue * ) ListFirst ( & ct_ColorValueHead->list ) ;

      if ( pColorValue != NULL )
      {
         /*qstDS = QuestionDialog ( colorThresholdDS ,
                                  "This will set your colors\n"
                                  "to the default set. Continue?");
         SetTitle ( qstDS , "Set Default Confirmation" ) ;*/

         /* Get the XmMessageBox ok button, and associate a callback to it.  */
         okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
         XtAddCallback ( okPB , XmNactivateCallback , color_threshold_default ,
                         ( XtPointer ) pColorValue ) ;
      }
   }

   return;
}

/************************************************************************/

void color_threshold_default ( Widget w , XtPointer ptr , XtPointer cbs )
{
   char	where [ BUFSIZ ] ;
   ColorValue colorValue ;
   ColorValue * pColorValue = ( ColorValue * ) ptr ;
   ColorValue selected_color_value ;
   int compare_result ;
   int pos = 1 ;

   if ( pColorValue != NULL )
   {
      /* Make a copy of the selected Color Value. This is
         because the color_threshold_load_main_list routine
         frees the linked list. */
      strcpy ( selected_color_value.userid , pColorValue->userid ) ;
      strcpy ( selected_color_value.application_name ,
               pColorValue->application_name ) ;
      strcpy ( selected_color_value.color_use_name ,
               pColorValue->color_use_name ) ;
      selected_color_value.duration = pColorValue->duration ;
      selected_color_value.threshold_value =
                                     pColorValue->threshold_value ;
      selected_color_value.threshold_unit [ 0 ] =
                                  pColorValue->threshold_unit [ 0 ] ;
      strcpy ( selected_color_value.color_name ,
               pColorValue->color_name ) ;

      /* Unload the values of the widgets. */
      color_threshold_unload_widgets ( & colorValue ) ;

      compare_result = strcmp ( colorValue.userid ,
                                selected_color_value.userid ) ;

      if ( compare_result == 0 )
      {
         /* BAL 12/9/2004. Delete all durations. */
         sprintf ( where , " WHERE userid = '%s'"
                           " AND application_name = '%s'"
                           " AND color_use_name = '%s'"
                           " AND threshold_unit = '%c'" ,
                           selected_color_value.userid ,
                           selected_color_value.application_name ,
                           selected_color_value.color_use_name ,
                           selected_color_value.threshold_unit [ 0 ] ) ;

         fprintf ( stdout , "%s\n" , where ) ;

         DeleteColorValue ( where ) ;
         color_threshold_load_main_list ( & selected_color_value ) ;

         //XmListSelectPos(ct_colorValueLI, pos , True);
         //XmListSetPos(ct_colorValueLI, pos ) ;

         /* Update the MPE data display if necessary. */
         color_threshold_update_mpe_display ( ) ;

         //UnsetCursor(colorThresholdFO);
      }
   }

   return ;
}


void  color_threshold_load_widgets(ColorValue *colorValue)
{
        char buffer[BUFSIZ];
        /* int use = (int) RADAR_MOSAIC_USE ; */
	int i;
	int pos;
        int use = 0;

	/*
	     load ColorUse option menu
	*/
        for ( i = 0 ; i < color_window_setup.num_color_use_names ; ++ i )
        {
           if ( ( strcmp ( colorValue->color_use_name ,
                           color_window_setup.color_use_names [ i ] ) ) == 0 )
           {
              use = i ;
              break ;
           }
        }

	//SetMenuPos ( ct_colorUseOM , use ) ;

	/*
	     load ct_durationTE
	*/
	sprintf(buffer, "%ld", colorValue->duration/SECONDS_PER_HOUR);
//	XmTextSetString(ct_durationTE, buffer);


	/*
	     load ct_thresholdTypeOM and ct_thresholdValueTE
	*/
	if (colorValue->threshold_value == MISSING_VALUE)
	{
//	     SetMenuPos(ct_thresholdTypeOM, MISSING_TYPE);
	     sprintf(buffer, "N/A");
//	     XmTextSetString(ct_thresholdValueTE, buffer);
	}
	else if (colorValue->threshold_value == LESS_THAN_MIN_VALUE)
	{
//	     SetMenuPos(ct_thresholdTypeOM, LESS_MIN_TYPE);
	     sprintf(buffer, "N/A");
//	     XmTextSetString(ct_thresholdValueTE, buffer);
	}
	else
	{
//	     SetMenuPos(ct_thresholdTypeOM, GREATER_EQUAL_TYPE);
	     sprintf(buffer, "%7.2f", colorValue->threshold_value);
//	     XmTextSetString(ct_thresholdValueTE, buffer);
	}



	/*
	    select the correct list item for the list of colors.
	*/
	pos = -1;
	for (i = 0; i < ct_numColorNames; i++)
	{
	     if (strcmp(colorValue->color_name, ct_colorNames[i]) == 0)
	     {
                  pos = i;
		  break;
	     }
	}

	if (pos != -1)
	{
	     pos += 1;
//	     XmListSelectPos(ct_colorNameLI, pos, True);
//	     XmListSetPos(ct_colorNameLI, pos);
	}


        return;
}

/************************************************************************/

void    color_threshold_load_widgets_callback(Widget w, XtPointer ptr,
				     XtPointer cbs)
{

     ColorValue *cPtr = NULL;
     int pos = ListRsrcGetFirstSelectedPos(w);

     cPtr = (ColorValue *) ListNth(&ct_ColorValueHead->list, pos);

     if ( cPtr != NULL )
     {
          color_threshold_load_widgets ( cPtr );
     }

     return;
}
/************************************************************************/

void loadColorNameList(void)
{
     ColorName *cHead = NULL;
     ColorName *cPtr = NULL;
     int count = 0;

     cHead = GetColorName(" ORDER by color_name ");
     if (cHead)
     {
	  cPtr = (ColorName *) ListFirst(&cHead->list);

          while (cPtr)
	  {
	       memset(&ct_colorNames[count], '\0', sizeof(RussText) );
	       strcpy(ct_colorNames[count], cPtr->color_name);
	       count++;
	       if (count >= MAX_COLOURS)
	          break;
	       cPtr = (ColorName *) ListNext(&cPtr->node);
	  }

	  ct_numColorNames = count;

	  FreeColorName(cHead);
     }


//     loadXmList100(ct_colorNameLI, ct_colorNames, ct_numColorNames);


     return;
}

/***********************************************************************/

void    pickColorCallback(Widget w, XtPointer ptr,  XtPointer cbs)
{
     char colorName[COLOR_NAME_LEN+1];
     int pos = 1;//ListRsrcGetFirstSelectedPos(ct_colorNameLI);

     if (pos != -1)
     {
          strcpy(colorName, ct_colorNames[pos - 1]);
     }


     //SetForeground(ct_selectedColorTE,
	//	   GetNamedColor(ct_selectedColorTE, colorName));

    //XmTextSetString(ct_selectedColorTE, colorName);

     return;
}

/***********************************************************************/
void color_threshold_load_main_list( ColorValue * pColorValue )
{
     ColorValue * cPtr = NULL;

     int count = 0;
     RussText text[1000];
     int hours ;
     int duration = -9999;
     int ncolors = 0 ;
     int nlevels = 0 ;
     int status;
     char thresholdText [ BUFSIZ ] ;
     char hoursText [ BUFSIZ ] ;
     XmString label_string;

     if ( pColorValue != NULL )
     {

        if ( ct_ColorValueHead != NULL )
        {
             FreeColorValue(ct_ColorValueHead);
             ct_ColorValueHead = NULL;
        }

        ct_ColorValueHead = get_colorvalues ( pColorValue->userid ,
                                              application_name ,
                                              pColorValue->color_use_name ,
                                              duration , 'E' , & ncolors ,
                                              & nlevels,
                                              pColorGroup ) ;

        if ( ct_ColorValueHead != NULL )
        {
   	     count = 0;
	     cPtr = (ColorValue *) ListFirst(&ct_ColorValueHead->list);

             /* Set the label indicating where this color list has been taken
                from:  User, Office, or Default.  User means that the color list
                was taken from the user.  Office means that the color list was
                taken from the office level color list.  Default means that the
                color list was taken from the hardcoded set.  One of these
                three should always be available. */
             status = strcmp ( cPtr->userid, "hardcoded" );

             if ( status == 0 )
             {
                /* This is the default color set. */
                label_string = XmStringCreateLocalized ( "Default" );
//                XtVaSetValues ( ct_sourceLB, XmNlabelString, label_string,
//                                NULL );
                XmStringFree ( label_string );
                label_string = NULL;
             }
             else
             {
                status = strcmp ( cPtr->userid, "default" );

                if ( status == 0 )
                {
                   /* This is the office color set. */
                   label_string = XmStringCreateLocalized ( "Office" );
//                   XtVaSetValues ( ct_sourceLB, XmNlabelString, label_string,
//                                   NULL );
                   XmStringFree ( label_string );
                   label_string = NULL;
                }
                else
                {
                   /* Assume the color list came from the user's set. */
                   label_string = XmStringCreateLocalized ( "User" );
//                   XtVaSetValues ( ct_sourceLB, XmNlabelString, label_string,
//                                   NULL );
                   XmStringFree ( label_string );
                   label_string = NULL;
                }
             }

             while (cPtr)
	     {
	          hours = cPtr->duration/SECONDS_PER_HOUR;

	          if (hours == 1)
	          {
	               sprintf(hoursText,"%d hour ",hours);
	          }
	          else
	          {
	               sprintf(hoursText,"%d hours",hours);
	          }



	          if (cPtr->threshold_value == MISSING_VALUE)
	          {
	               strcpy(thresholdText, "MISSING");
	          }
	          else if (cPtr->threshold_value == LESS_THAN_MIN_VALUE)
	          {
		       strcpy(thresholdText, "< MINIMUM");
	          }
	          else
	          {
	               sprintf(thresholdText, ">= %7.2f",
		   	    cPtr->threshold_value);
	          }

	          sprintf(text[count], "%-15s %-12s %-10s    %-25s",
		          cPtr->color_use_name,
		          hoursText,
		          thresholdText,
		          cPtr->color_name);

	          count++;

	       cPtr = (ColorValue *) ListNext(&cPtr->node);
	     }
        }
        else
        {
           /* Set the flag indicating that user has no defined colors
              for this product. */
           /* Load the default color set.  If it is not available, then
              obtain the hardcoded color set. */
        }

        //loadXmList100(ct_colorValueLI, text, count);
     }

     return;
}

/***********************************************************************/

int hvColorValueIsEqual(ColorValue *value1, ColorValue *value2)
{
     int isEqual = False;


     if (strcmp(value1->color_use_name,value2->color_use_name) == 0)
     {
         if (value1->duration == value2->duration)
	 {
	      if (value1->threshold_value == value2->threshold_value)
	      {
	           if(strcmp(value1->color_name, value2->color_name) == 0)
		   {
		        isEqual = True;
		   }
	      }
	 }

     }


     return isEqual;
}

/***********************************************************************/

int     getColorValueListPos(ColorValue *head, ColorValue *value)
{
     int pos = -1;
     ColorValue *ptr;
     int count = 1;

     ptr = (ColorValue*) ListFirst(&head->list);

     while(ptr)
     {
          if (hvColorValueIsEqual(ptr, value))
	  {
	       pos = count;
	       break;
	  }

	  count++;
	  ptr = (ColorValue *) ListNext(&ptr->node);
     }

     return pos;
}
/***********************************************************************/

void hvColorValueCreateKeyWhere ( ColorValue * colorValue , char * where ,
                                  QueryType query_type )
{
     switch ( query_type )
     {
        case COLOR_SELECT :

           sprintf ( where, " WHERE userid = '%s' AND application_name = '%s'"
                            " AND threshold_unit = 'E'"
		            " AND color_use_name = '%s'"
                            " AND duration = %ld AND threshold_value = %f ",
                            colorValue->userid , application_name ,
		            colorValue->color_use_name,
		            colorValue->duration,
		            colorValue->threshold_value );
           break ;

        case COLOR_UPDATE :

           sprintf ( where, " WHERE userid = '%s' AND application_name = '%s'"
                            " AND threshold_unit = 'E'"
		            " AND color_use_name = '%s'"
                            " AND duration = %ld AND "
                            " threshold_value BETWEEN %f AND %f ",
                            colorValue->userid , application_name ,
		            colorValue->color_use_name,
		            colorValue->duration,
		            colorValue->threshold_value - 0.0001 ,
                            colorValue->threshold_value + 0.0001 ) ;
           break ;

        case COLOR_SELECT_ALL_DUR :

           sprintf ( where, " WHERE userid = '%s' AND application_name = '%s'"
                            " AND threshold_unit = 'E'"
		            " AND color_use_name = '%s'"
                            " AND threshold_value = %f ",
                            colorValue->userid , application_name ,
		            colorValue->color_use_name,
		            colorValue->threshold_value );
           break ;

        case COLOR_DELETE :

           sprintf ( where, " WHERE userid = '%s' AND application_name = '%s'"
                            " AND threshold_unit = 'E'"
		            " AND color_use_name = '%s'"
                            " AND duration = %ld"
                            " AND threshold_value BETWEEN %f"
                            " AND %f" ,
                            colorValue->userid , application_name ,
		            colorValue->color_use_name ,
		            colorValue->duration ,
		            colorValue->threshold_value - 0.0001 ,
                            colorValue->threshold_value + 0.0001 ) ;
           break ;

     }

     return;
}

/***********************************************************************/

void hvColorValuePrint(ColorValue *colorValue)
{
     printf("use: %s  duration: %ld   value: %f  colorname: %s\n",
	    colorValue->color_use_name,
	    colorValue->duration,
	    colorValue->threshold_value,
	    colorValue->color_name);

     return;
}
/***********************************************************************/

void setApplicationName ( const char * app_name )
{
   memset ( application_name, '\0', APP_NAME_LEN + 1 );
   strncpy ( application_name, app_name, APP_NAME_LEN );
}

const char * getApplicationName ( )
{
   return application_name;
}

void FreeColorWindowSetupMemory ( )
{
   int i;

   if ( color_window_setup.color_use_names != NULL )
   {
      for ( i = 0; i < color_window_setup.num_color_use_names; ++i )
      {
         free ( color_window_setup.color_use_names [ i ] );
      }

      free ( color_window_setup.color_use_names );
      color_window_setup.color_use_names = NULL;
   }

   if ( color_window_setup.color_use_strings != NULL )
   {
      for ( i = 0; i < color_window_setup.num_color_use_names; ++i )
      {
         free ( color_window_setup.color_use_strings [ i ] );
      }

      free ( color_window_setup.color_use_strings );
      color_window_setup.color_use_strings = NULL;
   }

   if ( color_window_setup.application_name != NULL )
   {
      free ( color_window_setup.application_name );
      color_window_setup.application_name = NULL;
   }

   if ( color_window_setup.user_id != NULL )
   {
      free ( color_window_setup.user_id );
      color_window_setup.user_id = NULL;
   }
}

/*****************************************************************************
 *
 * Module: map_menubar.c
 *
 * Description: contains routines that deal with the menubar
 * 
 * Modification History:  March 2006, modified _create_menu_tools to
 *                        add submenu for "Set Station Icon Size".
 *
 *****************************************************************************/

/* include files */

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <ctype.h>
#include "map_defines.h"
#include "map_resource.h"
#include "map_menubar.h"
#include "map_menubar_cb.h"
#include "map_convert.h"
#include "map.h"
#include "map_library.h"
#include "Xtools.h"
#include "GeneralUtil.h"


#define MAX_USER_OVERLAYS 21
#define NUM_OVERLAYS 50
#define MAX_RECORD_LENGTH 1024


/* global variables */

Widget map_menubar;

Widget map_menu_items [ num_of_menu_items ] ;

/*****************************************************************************
 * Routine: _get_menu_item_toggle_widget
 *
 * Description: Returns the widget of the toggle button of the active item
 *              on the overlays menu. 
 *             
 *****************************************************************************/

Widget _get_menu_item_toggle_widget ( enum menu_items item)
{
   return map_menu_items [ item ] ;    
}


/*****************************************************************************
 *
 * Routine: create_menu_separator
 *
 * Description: routine creates a separator widget on the menubar
 *
 *****************************************************************************/

void _create_menu_separator(Widget pane)
{
  XtVaCreateManagedWidget("seperator",xmSeparatorWidgetClass,pane,
			  XmNforeground,_get_color("Black"),
			  XmNbackground,_get_color("CadetBlue"),
			  NULL);
}

/*****************************************************************************
 *
 * Routine: create_cascade()
 *
 * Description: routine creates a cascade widget on the menubar
 *
 *****************************************************************************/

void _create_cascade( Widget menu , Widget * pane , Widget * cascade ,
                      char * text , char mnemonic )
{
  Arg al [ 64 ] ;
  char temp[50];
  int ac = 0  ;
  XmString str;

  sprintf ( temp , "%s_pane" , text ) ;
  XtSetArg ( al [ ac ] , XmNtearOffModel , XmTEAR_OFF_ENABLED ) ;
  ac ++ ;

 * pane = ( Widget ) XmCreatePulldownMenu ( menu , temp , al , ac ) ;
 
  str = XmStringCreateLocalized(text);
  sprintf(temp,"%s_cascade",text);

  
  *cascade = (Widget) XtVaCreateManagedWidget(temp,
					     xmCascadeButtonWidgetClass,menu,
					     XmNlabelString,str,
					     XmNsubMenuId,*pane,
					     XmNbackground,_get_color("CadetBlue"),
					     XmNforeground,_get_color("Black"),
                                             XmNmnemonic , mnemonic ,			    
					     NULL ) ;

  XmStringFree(str);
}

/*****************************************************************************
 *
 * Routine: create_menu_item()
 *
 * Description: routine creates a map item for the given cascade
 *
 *****************************************************************************/

void _create_menu_item(Widget cascade, Widget *item, char *name, char mnemonic,
			char *accel, char *text_ac)
{
  XmString str, str_ac;
 
  str = XmStringCreateLocalized(name);
  str_ac = XmStringCreateLocalized(text_ac);
  
  *item = (Widget) XtVaCreateManagedWidget(name,
					   xmPushButtonWidgetClass,cascade,
					   XmNlabelString,str,
					   XmNforeground,_get_color("Black"),
					   XmNbackground,
                                           _get_color("CadetBlue"),
                                           XmNmnemonic , mnemonic , 
					   XmNaccelerator, accel,
					   XmNacceleratorText, str_ac,
					   NULL ) ;

  XmStringFree(str);
  XmStringFree(str_ac);
}
/*****************************************************************************
 *
 * Routine: create_menu_item_toggle()
 *
 * Description: routine creates a map item for the given cascade
 *
 *****************************************************************************/

void _create_menu_item_toggle ( Widget cascade,Widget *item, char *name ,
                                char mnemonic, char * accel, char * text_ac )
{
  XmString str, str_ac;

  str = XmStringCreateLocalized(name);
  str_ac = XmStringCreateLocalized(text_ac);
  
  *item = (Widget) XtVaCreateManagedWidget(name,
			   xmToggleButtonWidgetClass,cascade,
 		           XmNlabelString,str,
			   XmNindicatorType,XmN_OF_MANY,
			   XmNforeground,_get_color("Black"),
			   XmNbackground,_get_color("CadetBlue"),
                           XmNmnemonic , mnemonic , 
			   XmNaccelerator, accel,
			   XmNacceleratorText, str_ac,
			   NULL ) ;

  XmStringFree(str);
  XmStringFree(str_ac);
}

/*****************************************************************************
 *
 * Routine: _create_menu_file()
 *
 * Description: routine creates the file menu options
 *
 *****************************************************************************/

void _create_menu_file( )
{
  extern int stand_alone ;

  _create_cascade(map_menubar,&map_menu_items[map_file_pane],
		  &map_menu_items[map_file_cascade],"File" , 'F');

  _create_menu_item(map_menu_items[map_file_pane]
		    ,&map_menu_items[map_file_save],
		    "Save as gif ..." , 'S', "Alt<Key>S", "Alt+S");
  _create_menu_separator(map_menu_items[map_file_pane]);

  _create_menu_item(map_menu_items[map_file_pane],
		    &map_menu_items[map_file_print]
		    
		    ,"Print Image" , 'P', NULL, NULL );
  _create_menu_item(map_menu_items[map_file_pane],
		    &map_menu_items[map_file_reverse_print],
		    "Print Reverse Image" , 'r', NULL, NULL );
  _create_menu_separator(map_menu_items[map_file_pane]);
  _create_menu_item(map_menu_items[map_file_pane],
		    &map_menu_items[map_file_close],"Close" , 'C', "Alt<Key>C","Alt+C");

  XtAddCallback(map_menu_items[map_file_save],XmNactivateCallback,_save_image,
		NULL);
  XtAddCallback(map_menu_items[map_file_print],XmNactivateCallback,
		_print_image,NULL);
  XtAddCallback(map_menu_items[map_file_reverse_print],XmNactivateCallback,
		_print_image_reverse,NULL);
  XtAddCallback(map_menu_items[map_file_close],XmNactivateCallback,
		_close_map_screen, ( XtPointer ) & stand_alone ) ;

}

/*****************************************************************************
 *
 * Routine: _create_menu_tools()
 *
 * Description: routine creates the tools menu options
 *
 *****************************************************************************/

void _create_menu_tools( )
{
  static int first = TRUE;
  char   token_string[60];
  int    i, token_len, string_len;
  
  _create_cascade(map_menubar,&map_menu_items[map_tool_pane],
		  &map_menu_items[map_tool_cascade],
		  "Tools" , 'T');
  _create_menu_item(map_menu_items[map_tool_pane],
		    &map_menu_items[map_areal_zoom],"Areal Zoom" , 'Z', 
                    "Ctrl<Key>Z","Ctrl+Z");
  _create_cascade(map_menu_items[map_tool_pane],
		  &map_menu_items[map_tool_zoom],
		  &map_menu_items[map_tool_zoom_cascade],"Point Zoom" , 'o');
  _create_menu_item(map_menu_items[map_tool_zoom],
		    &map_menu_items[map_tool_zoom_in],"In" , 'I', "Ctrl<Key>I","Ctrl+I");
  _create_menu_item(map_menu_items[map_tool_zoom],
		    &map_menu_items[map_tool_zoom_out],"Out" , 'O', "Ctrl<Key>O","Ctrl+O");
  XtAddCallback(map_menu_items[map_areal_zoom],XmNactivateCallback,
		mArealZoom, NULL ) ;
  XtAddCallback(map_menu_items[map_tool_zoom_in],XmNactivateCallback,
		_zoom_map_in,NULL);
  XtAddCallback(map_menu_items[map_tool_zoom_in],XmNactivateCallback,
		_zoom_map_in,NULL);
  XtAddCallback(map_menu_items[map_tool_zoom_out],XmNactivateCallback,
		_zoom_map_out,NULL);

  _create_cascade(map_menu_items[map_tool_pane],
		  &map_menu_items[map_tool_pan],
		  &map_menu_items[map_tool_pan_cascade],"Pan" , 'P');
  _create_menu_item(map_menu_items[map_tool_pan],
		    &map_menu_items[map_tool_pan_north],"Up" , 'U', "Ctrl<Key>U","Ctrl+U");
  _create_menu_item(map_menu_items[map_tool_pan],
		    &map_menu_items[map_tool_pan_south],"Down" , 'D',"Ctrl<Key>D","Ctrl+D");
  _create_menu_item(map_menu_items[map_tool_pan],
		    &map_menu_items[map_tool_pan_east],"Right" , 'R',"Ctrl<Key>R","Ctrl+R");
  _create_menu_item(map_menu_items[map_tool_pan],
		    &map_menu_items[map_tool_pan_west],"Left" , 'L',"Ctrl<Key>L","Ctrl+L");

  XtAddCallback(map_menu_items[map_tool_pan_north],XmNactivateCallback,
		_pan_map_up,NULL);
  XtAddCallback(map_menu_items[map_tool_pan_south],XmNactivateCallback,
		_pan_map_down,NULL);
  XtAddCallback(map_menu_items[map_tool_pan_east],XmNactivateCallback,
		_pan_map_right,NULL);
  XtAddCallback(map_menu_items[map_tool_pan_west],XmNactivateCallback,
		_pan_map_left,NULL);

  _create_menu_item(map_menu_items[map_tool_pane],
		    &map_menu_items[map_tool_recenter],"Recenter" , 'R', NULL, NULL );
  XtAddCallback(map_menu_items[map_tool_recenter],XmNactivateCallback,
		_recenter_cb,NULL);

  _create_menu_separator(map_menu_items[map_tool_pane]);
  _create_menu_item_toggle(map_menu_items[map_tool_pane],
			   &map_menu_items[map_toolbar_option],"Tool Bar" ,
                           'T', "Ctrl<Key>T","Ctrl+T" ) ;
  XtAddCallback(map_menu_items[map_toolbar_option],XmNvalueChangedCallback,
		_show_toolbar,NULL);
  if (_get_toolbar() == M_ON)
    XmToggleButtonSetState(map_menu_items[map_toolbar_option],True,False);
    
  
  mCreateMenuSeparator ( map_menu_items [ map_tool_pane ] );
  
  /* Add the Font menu.  This used to be on the Overlay Menu. */
    _create_cascade ( map_menu_items [ map_tool_pane ] ,
                    & map_menu_items [ map_tool_font ] ,
                    & map_menu_items [ map_tool_font_cascade ] ,
                    "Set Font" , 'F' ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_font ] ,
                      & map_menu_items [ map_tool_font_miniscule ] ,
                      "Miniscule" , 'M' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_font ] ,
                      & map_menu_items [ map_tool_font_very_small ] ,
                      "Very Small" , 'V' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_font ] ,
                      & map_menu_items [ map_tool_font_small ] ,
                      "Small" , 'S' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_font ] ,
                      & map_menu_items [ map_tool_font_normal ] ,
                      "Normal" , 'N' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_font ] ,
                      & map_menu_items [ map_tool_font_large ] ,
                      "Large" , 'L' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_font ] ,
                      & map_menu_items [ map_tool_font_very_large ] ,
                      "Very Large" , 'e' , NULL , NULL ) ;
                      
  XtAddCallback(map_menu_items [ map_tool_font_miniscule ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_font, ( XtPointer ) M_MINISCULE ) ;
  XtAddCallback(map_menu_items [ map_tool_font_very_small ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_font, ( XtPointer ) M_TINY ) ;
  XtAddCallback(map_menu_items [ map_tool_font_small ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_font, ( XtPointer ) M_SMALL ) ;
  XtAddCallback(map_menu_items [ map_tool_font_normal ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_font, ( XtPointer ) M_MEDIUM ) ;
  XtAddCallback(map_menu_items [ map_tool_font_large ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_font, ( XtPointer ) M_LARGE ) ;
  XtAddCallback(map_menu_items [ map_tool_font_very_large ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_font, ( XtPointer ) M_HUGE ) ;
     
  XmToggleButtonSetState ( map_menu_items [ map_tool_font_small ],True,
                           False ) ;
			   
  /*create the cascade for set station icon size*/
  			   
  /*get parameters to determine the icon sizes, first from token station_icon_size */
   
  if (first) 
  {
    token_len = strlen("station_icon_size");
    get_apps_defaults("station_icon_size", &token_len, token_string, 
                      &string_len);

    /*convert the token value to upper case*/

    for (i = 0; i < string_len; i++)
    {
       if ( islower(token_string[i]) )
	       token_string[i] = toupper(token_string[i]);
    }

    if (string_len > 0)
    {
       if (strcmp(token_string, "VSMALL" ) == 0 )	
	  set_pdsize_flag = M_S_VSMALL;	  	               
       else if (strcmp(token_string, "SMALL" ) == 0 )	
	  set_pdsize_flag = M_S_SMALL;	             	    
       else if (strcmp(token_string, "MEDIUM" ) == 0 )	
	  set_pdsize_flag = M_S_MEDIUM;	             	
       else if (strcmp(token_string, "LARGE" ) == 0 )	
	  set_pdsize_flag = M_S_LARGE;	             	
    }
    else     
       set_pdsize_flag = M_S_SMALL;

    /*define_pdsize(set_pdsize_flag);*/
    first = FALSE;
  }    			   

  mCreateMenuSeparator ( map_menu_items [ map_tool_pane ] );			   
  
  _create_cascade ( map_menu_items [ map_tool_pane ] ,
                    & map_menu_items [ map_tool_pdsize ] ,
                    & map_menu_items [ map_tool_pdsize_cascade ] ,
                    "Set Station Icon Size" , 'S' ) ; 
		    
  _create_menu_item_toggle ( map_menu_items [ map_tool_pdsize ] ,
                      & map_menu_items [ map_tool_pdsize_vsmall ] ,
                      "Very Small" , 'V' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_pdsize ] ,
                      & map_menu_items [ map_tool_pdsize_small ] ,
                      "Small" , 'S' , NULL , NULL ) ;
  _create_menu_item_toggle ( map_menu_items [ map_tool_pdsize ] ,
                      & map_menu_items [ map_tool_pdsize_medium ] ,
                      "Medium" , 'M' , NULL , NULL ) ;	
  _create_menu_item_toggle ( map_menu_items [ map_tool_pdsize ] ,
                      & map_menu_items [ map_tool_pdsize_large ] ,
                      "Large" , 'L' , NULL , NULL ) ;	
  
  XtAddCallback(map_menu_items [ map_tool_pdsize_vsmall ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_pdsize, ( XtPointer ) M_S_VSMALL ) ;
  XtAddCallback(map_menu_items [ map_tool_pdsize_small ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_pdsize, ( XtPointer ) M_S_SMALL ) ;
  XtAddCallback(map_menu_items [ map_tool_pdsize_medium ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_pdsize, ( XtPointer ) M_S_MEDIUM ) ;
  XtAddCallback(map_menu_items [ map_tool_pdsize_large ] ,
		XmNvalueChangedCallback ,
		(XtCallbackProc) _set_map_pdsize, ( XtPointer ) M_S_LARGE ) ;
  
  if (set_pdsize_flag == M_S_VSMALL)		
     XmToggleButtonSetState ( map_menu_items [ map_tool_pdsize_vsmall ],True,
                              False ) ;				
  else if (set_pdsize_flag == M_S_SMALL)
     XmToggleButtonSetState ( map_menu_items [ map_tool_pdsize_small ],True,
                              False ) ;				
  else if (set_pdsize_flag == M_S_MEDIUM)
     XmToggleButtonSetState ( map_menu_items [ map_tool_pdsize_medium ],True,
                              False ) ;				     
  else if (set_pdsize_flag == M_S_LARGE)
     XmToggleButtonSetState ( map_menu_items [ map_tool_pdsize_large ],True,
                              False ) ;				    
  else
     XmToggleButtonSetState ( map_menu_items [ map_tool_pdsize_small ],True,
                              False ) ;				               	   
                      
}

/*****************************************************************************
 *
 * Routine: _create_menu_projections()
 *
 * Description: routine creates the projections menu options
 *
 *****************************************************************************/

void _create_menu_projections( )
{
  _create_cascade(map_menubar,&map_menu_items[map_projection_pane],
		  &map_menu_items[map_projection_cascade],
		  "Projections" , 'P');
  _create_menu_item(map_menu_items[map_projection_pane],
		    &map_menu_items[map_projection_flat],"Flat Lat/Lon" , 
                    'F',NULL, NULL );
  _create_menu_item(map_menu_items[map_projection_pane],
		    &map_menu_items[map_projection_polar],
		    "Polar Stereographic" , 'P', NULL, NULL );
  _create_menu_item(map_menu_items[map_projection_pane],
		    &map_menu_items[map_projection_hrap],
		    "HRAP" , 'H', NULL, NULL );

  XtAddCallback(map_menu_items[map_projection_flat],XmNactivateCallback,
		_set_projection,
		(XtPointer)M_FLAT);
  XtAddCallback(map_menu_items[map_projection_polar],XmNactivateCallback,
		_set_projection,
		(XtPointer)M_POLAR);
  XtAddCallback(map_menu_items[map_projection_hrap],XmNactivateCallback,
		_set_projection,
                (XtPointer)M_HRAP);

}

/*****************************************************************************
 *
 * Routine: _create_menu_overlay()
 *
 * Description: routine creates the overlay menu options
 *
 *****************************************************************************/


void _create_menu_overlay( )
{
   _init_overlays ( ) ;
  
  _create_cascade(map_menubar,&map_menu_items[map_overlay_pane],
		  &map_menu_items[map_overlay_cascade],
		  "Overlays" , 'O');
  
  // Create the All Streams / Lakes menu overlay item. 
  _create_cascade ( map_menu_items [ map_overlay_pane ] ,
                    & map_menu_items [ map_overlay_streams_lakes ] ,
                    & map_menu_items [ map_overlay_streams_lakes_cascade ] ,
                    "Streams/Lakes" , 'S' ) ;

//DeSensitize ( map_menu_items[map_overlay_pane]); 

_create_menu_item_toggle ( map_menu_items [ map_overlay_streams_lakes ] ,
			     & map_menu_items[map_overlay_streams] ,
			     "All Streams/Lakes" , 'A', NULL, NULL ) ;
 
 
  XtAddCallback ( map_menu_items [ map_overlay_streams ] , 
		  XmNvalueChangedCallback,
		  ( XtCallbackProc ) _set_overlay,(XtPointer ) M_STREAMS ) ;


  // Create the Streams/Rivers/Lakes Box.
  //   Create the Major Streams / Lakes menu overlay item. 
  _create_menu_item_toggle ( map_menu_items [ map_overlay_streams_lakes ],
			     &map_menu_items[map_overlay_rivers],
			     "Major Streams/Lakes" , 'M', NULL, NULL );
  
  XtAddCallback ( map_menu_items [ map_overlay_rivers ] , 
		  XmNvalueChangedCallback,
		  ( XtCallbackProc ) _set_overlay, ( XtPointer ) M_RIVERS ) ;


  // Create the No Streams/Rivers/Lakes overlay menu item. 
  _create_menu_item_toggle( map_menu_items [ map_overlay_streams_lakes ] ,
			    & map_menu_items [ map_overlay_no_streams ] ,
			    "No Streams/Lakes" , 'N' , NULL , NULL ) ;
  

XtAddCallback ( map_menu_items [ map_overlay_no_streams ] , XmNvalueChangedCallback ,
		(XtCallbackProc) _set_overlay , ( XtPointer ) M_NOSTREAMS ) ;


  _create_menu_separator(map_menu_items[map_overlay_pane]);

  // Create the Basins overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_basins],
			   "Basin Boundaries" , 'B', "Ctrl<Key>B","Ctrl+B" );
  XtAddCallback(map_menu_items[map_overlay_basins],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_BASINS);
    
  // Create the Basin Names overlay menu item. 
  _create_menu_item_toggle ( map_menu_items[map_overlay_pane],
                             &map_menu_items[map_overlay_basin_names],
                             "Basin Names", 'a', NULL, NULL );
                     
  // Add the callback for the basin names overlay. 
  // Check the state of the toggle button.
  // This overlay is not implemented yet.  Desensitize it until it is implemented. 
  DeSensitize ( map_menu_items[map_overlay_basin_names]);
  
  // Create the County overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_county],
			   "Counties" , 'C', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_county],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_COUNTY);

  // Create the County Warning Areas overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_cwa],
			   "County Warning Areas" , 'o', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_cwa],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_CWA);

  // Create the Hydrologic Service Area overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_hsa],
			   "Hydro Service Areas" , 'y', NULL, NULL );
  //  XtAddCallback(map_menu_items[map_overlay_hsa],XmNvalueChangedCallback,
//		(XtCallbackProc) _set_overlay,(XtPointer)M_CWA);
 // if ( _get_overlay_status ( M_CWA ) == M_ON )
  //  XmToggleButtonSetState(map_menu_items[map_overlay_cwa],True,False); 
  // This option has not yet been implemented.  Grey it out until it is. 
  DeSensitize ( map_menu_items[map_overlay_hsa]);  

  // Create the RFC overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_rfc],
			   "RFC Boundaries" , 'R', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_rfc],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_RFC_BOUNDARY);

  // Create the States overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_states],
			   "States" , 'S', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_states],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_STATE);

  // Create the Zones overlay menu item.  
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_zones],
			   "Zones" , 'Z', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_zones],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_ZONES);
    
  // Create the Topography submenu. 
  _create_cascade ( map_menu_items [ map_overlay_pane ] ,
                    & map_menu_items [ map_overlay_topography ] ,
                    & map_menu_items [ map_overlay_topography_cascade ] ,
                    "Topography" , 'g' ) ;
  _create_menu_item_toggle(map_menu_items[map_overlay_topography],
			   &map_menu_items[map_overlay_topo],
			   "Image" , 'I', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_topo],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_TOPOGRAPHY);
  _create_menu_item_toggle(map_menu_items[map_overlay_topography],
			   &map_menu_items[map_overlay_topo_contour],
			   "Contour" , 'C', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_topo_contour],
                XmNvalueChangedCallback,
	        (XtCallbackProc) _set_overlay,
                (XtPointer)M_TOPOGRAPHY_CONTOUR);
  _create_menu_item_toggle(map_menu_items[map_overlay_topography],
                           &map_menu_items[map_overlay_no_topo],
                           "None", 'N', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_no_topo],
                XmNvalueChangedCallback,
	        (XtCallbackProc) _set_overlay,
                (XtPointer)M_TOPOGRAPHY_NONE);
                            
  _create_menu_separator(map_menu_items[map_overlay_pane]);

  // Create the City overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_city],
			   "Cities/Towns" ,'i', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_city],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_CITY_TOWN);

  // Create the Highways and Roads overlay menu items.  
  
  _create_cascade ( map_menu_items [ map_overlay_pane ] ,
                    & map_menu_items [ map_overlay_highways ] ,
                    & map_menu_items [ map_overlay_highways_cascade ] ,
                    "Highways/Roads" , 'H' ) ;

  _create_menu_item_toggle ( map_menu_items [ map_overlay_highways ] ,
                             & map_menu_items[map_overlay_highways_and_roads] ,
                             "Highways/Roads" , 'h', NULL, NULL ) ;

  XtAddCallback(map_menu_items [ map_overlay_highways_and_roads ] ,
                XmNvalueChangedCallback ,
                (XtCallbackProc) _set_overlay, ( XtPointer ) M_ROADS ) ;

  _create_menu_item_toggle ( map_menu_items [ map_overlay_highways ] ,
                      & map_menu_items [ map_overlay_highways_only ] ,
                      "Highways" , 'g' , NULL , NULL ) ;
 
  XtAddCallback(map_menu_items [ map_overlay_highways_only ] ,
                XmNvalueChangedCallback ,
                (XtCallbackProc) _set_overlay, ( XtPointer ) M_HIGHWAYS ) ;
 
  _create_menu_item_toggle ( map_menu_items [ map_overlay_highways ] ,
                      & map_menu_items [ map_overlay_no_highways ] ,
                      "None" , 'n' , NULL , NULL ) ; 

  XtAddCallback(map_menu_items [ map_overlay_no_highways ] ,
                XmNvalueChangedCallback ,
                (XtCallbackProc) _set_overlay, ( XtPointer ) M_NOHIGHWAYS ) ;
 
  _create_menu_separator(map_menu_items[map_overlay_pane]);

  // Create the HRAP menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_hrap_grid],
			   "HRAP" , 'P', NULL, NULL );

  XtAddCallback(map_menu_items[map_hrap_grid],XmNvalueChangedCallback,
                (XtCallbackProc) _set_overlay,(XtPointer)M_HRAP_GRID);


  // Create the Latitude and Longitude overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_lat_lon],
			   "Lat/Lon Lines" , 'L', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_lat_lon],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_LAT_LON_LINES);

  // Create the Time Zone overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_timezones],
			   "Time Zones" , 'T', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_timezones],XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_TIMEZONE);

  _create_menu_separator(map_menu_items[map_overlay_pane]);

  // Create the Radar Locations overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_radar_locations],
			   "Radar Locations" , 'd', NULL, NULL );
  XtAddCallback(map_menu_items[map_overlay_radar_locations],
		XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_RADAR_LOCATIONS);

  // Create the Radar Rings overlay menu item. 
  _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_radar_rings],
			   "Radar Rings" , 'n', "Alt<Key>U","Alt+U" );
  XtAddCallback(map_menu_items[map_overlay_radar_rings],
		XmNvalueChangedCallback,
		(XtCallbackProc) _set_overlay,(XtPointer)M_RADAR_RINGS);

  // Create the Maps Foreground option. 
    _create_menu_item_toggle(map_menu_items[map_overlay_pane],
			   &map_menu_items[map_overlay_foreground],
			   "Maps Foreground" , 'M', NULL, NULL );
  // XtAddCallback(map_menu_items[map_overlay_foreground],
//		XmNvalueChangedCallback,
//		(XtCallbackProc) _set_overlay,(XtPointer)M_RADAR_RINGS);
  //if ( _get_overlay_status ( M_RADAR_RINGS ) == M_ON )
   // XmToggleButtonSetState(map_menu_items[map_overlay_radar_rings],True,False);
  // This menu option has not yet been implemented.  DeSensitize for future
    // development. 
  DeSensitize ( map_menu_items [ map_overlay_foreground ] );
}

/*****************************************************************************
 *
 * Routine: _create_menu_help()
 *
 * Description: routine creates the help menu options
 *
 *****************************************************************************/

void _create_menu_help( )
{
  _create_cascade(map_menubar,&map_menu_items[map_help_pane],
		  &map_menu_items[map_help_cascade],"Help", 'H');
/*  _create_menu_item(map_menu_items[map_help_pane],
		    &map_menu_items[map_help_topics],"Help Topics" , 
		    	   'H',NULL, NULL );*/
  _create_menu_item(map_menu_items[map_help_pane],
		    &map_menu_items[map_help_screen],"About" , 'A',NULL, NULL );
  _create_menu_separator(map_menu_items[map_help_pane]);
  _create_menu_item_toggle(map_menu_items[map_help_pane],
			   &map_menu_items[map_help_legend],"Map Legend" , 
                           'M', NULL, NULL ) ;

  /* Check to see if the menu is initially "on" or "off" */
  if ( mGetLegendStatus ( ) == ( int ) M_ON )
  {
     _set_legend_state ( M_ON ) ;
  }
  
//  XtAddCallback(map_menu_items[map_help_topics],XmNactivateCallback,
//  		popup_help_window,"MAIN");
  XtAddCallback(map_menu_items[map_help_screen],XmNactivateCallback,
  		_about_cb,NULL);
  XtAddCallback(map_menu_items[map_help_legend],XmNvalueChangedCallback,
		_show_legend,NULL);
}

/*****************************************************************************
 *
 * Routine: _set_legend_state ( )
 *
 * Description: Sets the activation state of the "Legend" toggle button
 *              on the "Help" menu. 
 *
 *****************************************************************************/

void _set_legend_state ( enum MapState state ) 
{ 
   if ( state == M_ON )
   {
     XmToggleButtonSetState ( map_menu_items [ map_help_legend ] ,
                              True ,
                              True ) ;
   }
   else
   {
     XmToggleButtonSetState ( map_menu_items [ map_help_legend ] ,
                              False ,
                              True ) ;
   }
}

/*****************************************************************************
 *
 * Routine: create_map_menubar()
 *
 * Description: routine creates the menubar for the map screen
 *
 *****************************************************************************/


void _create_map_menubar(Widget form)
{

  map_menubar = (Widget) XmCreateMenuBar(form,"map_menubar",NULL,0);

  _create_menu_file();
  _create_menu_tools();
  _create_menu_projections();
  _create_menu_overlay();
  _create_menu_help();

  XtVaSetValues(map_menubar,
		XmNrightAttachment,XmATTACH_FORM,
		XmNrightOffset,1,
		XmNleftAttachment,XmATTACH_FORM,
		XmNleftOffset,1,
		XmNmenuHelpWidget, map_menu_items [ map_help_cascade ] ,
		XmNforeground,_get_color("Black"),
		XmNbackground,_get_color("CadetBlue"),
		NULL);
}


/*****************************************************************************
 *
 * Routine: _manage_menubar()
 *
 * Description: routine manages the menubar items
 *
 *****************************************************************************/
void _manage_menubar ( )
{
  /* Manage the menubar. */
  XtManageChild ( map_menubar ) ;

}

/*****************************************************************************
 *
 * Routine: mCreateMenu
 *
 * Description: routine creates a menu on the menubar
 *
 *****************************************************************************/

void mCreateMenu(Widget *pane,Widget *cascade,char *name, char mnemonic)
{
  _create_cascade(map_menubar,pane,cascade,name , mnemonic);
}

/*****************************************************************************
 *
 * Routine: mCreateMenuItem
 *
 * Description: routine creates a menu item on the given menu
 *
 *****************************************************************************/

void mCreateMenuItem ( Widget pane , Widget * item , char * name , 
                       char mnemonic, char *accel, char *accel_text )
{
  _create_menu_item ( pane , item , name , mnemonic , accel , accel_text) ;
}

/*****************************************************************************
 *
 * Routine: mCreateMenuToggle
 *
 * Description: routine creates a toggle button on the given menubar
 *
 *****************************************************************************/

void mCreateMenuToggle(Widget pane,Widget *item,char *name , char mnemonic , 
			char *accel, char *accel_text )
{
  _create_menu_item_toggle ( pane , item , name , mnemonic , accel , accel_text ) ;
}

/*****************************************************************************
 *
 * Routine: mCreateMenuSeparator
 *
 * Description: routine creates a separator on the given menubar
 *
 *****************************************************************************/

void mCreateMenuSeparator(Widget pane)
{
  _create_menu_separator(pane);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

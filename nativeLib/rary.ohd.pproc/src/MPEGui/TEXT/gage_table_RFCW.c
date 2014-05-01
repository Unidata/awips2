/*=========================================================================*/
/*                         FILE NAME:  gage_table_RFCW                     */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:  create_gage_table_RFCW              */
/*                                     table_data_RFCW                     */
/*                                     change_rc_RFCW                      */
/*                                     sort_RFCW                           */
/*                                     v_scrollbar_moved_RFCW              */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/CascadeB.h>

#include "display_field_data_RFCW.h"
#include "drawa.h"
#include "gage_table_RFCW.h"
#include "libXs.h"
#include "map_library.h"
#include "menus.h"
#include "mpe_log_utils.h"
#include "post_functions.h"
#include "read_precip_data.h"
#include "read_xmrg.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "xs_create_menu_buttons.h"

#define SIZE 7    /* number of elements in enum DisplayFieldData type array */

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

cb_struct	      cb_data;
gage_struct         * gage;
enum DisplayFieldData display_array [SIZE] = { display_Xmrg,
	                                       display_mMosaic,
					       display_rMosaic,
					       display_bMosaic,
					       display_lMosaic,
					       display_gageOnly,
					       display_satPrecip };
int addition_flag, gages;

static xs_menu_struct Control_menu_struct[] =
   {
   {"Quit", popdown_gagetable, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}
   };

static xs_menu_struct Sort_menu_struct[] =
   {
   {"by Gage Value", sort_RFCW, (caddr_t)0, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"by Gage ID",    sort_RFCW, (caddr_t)1, TRUE, PUSH_BUTTON, NULL, 0, NULL},
   {"by Radar ID",    sort_RFCW, (caddr_t)2, TRUE, PUSH_BUTTON, NULL, 0, NULL}
   };

Widget grtable = 0 ;

/***************************************************************************/
/*  FUNCTION NAME:   init_gage_table_memory()                              */
/*       FUNCTION:   Initialize the cb_data structure elements to NULL.    */
/*                   This routine should only be called once before the    */
/*                   gage table is launched.                               */
/***************************************************************************

Function type:
   void

Called by function:
   create_gage_table_RFCW

Functions called:
   None
******************************************** BEGIN init_gage_table_memory ****/

void init_gage_table_memory ( )
{
   cb_data.text_w = NULL ;
   cb_data.labels = NULL ;
   cb_data.gage_table_children = NULL ;
   cb_data.data_array = NULL ;
   cb_data.headings = NULL ;
   cb_data.tdata = NULL ;
   cb_data.h_labels = NULL ;
}

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   create_gage_table_RFCW                                */
/*       FUNCTION:   create gage table display                             */
/***************************************************************************

Function type:
   void

Called by function:
   show_gage_table_RFCW

Functions called:
   display_no_gage
   (callback) v_scrollbar_moved_RFCW
   (callback) edit_gage_value

****************************************** BEGIN create_gage_table_RFCW *******/
void create_gage_table_RFCW ( draw_struct * data )
{
   Atom         wmAtom;
   char * text = NULL ;
   Widget       form_w, rc, v_scrollbar;
   XmString     xmstr;
   Arg          wargs[10];
   int          i, j, m, n;
   int          longest_heading_length;
   extern int   display_gage_table ;
   Dimension    rc_width, rc_height;
   Dimension    v_scrollbar_width;
   Position     rc_y;
   Widget       Control_gtMenuItem, Sort_gtMenuItem, gt_cascade[2];
   Widget       menuBar, timeGg;
   //Widget       Help_mainMenuItem;
  /* static int   gageTableFlag = 0;*/
   XmFontList   font_list;
   XmString     xstr ;
   int  **data_array_field = NULL;

   char datetimeGg[40];
   memset(datetimeGg, '\0', 40);

 /*--------------------------------------------------------------*/
 /*     if no gages are available display message                */
 /*--------------------------------------------------------------*/

 if ( display_gage_table == 1 )
 {
    return;
 }

 if (ngages == 0)
 {
    display_no_gage();
    return;
 }

 display_gage_table = 1 ;
 /*----------------------------------------------------------------*/
 /*     Set overall size of arrays and visible portion to defaults */
 /*----------------------------------------------------------------*/

 cb_data.visible_columns = 11;
 cb_data.visible_rows = 15;
 cb_data.maximum_columns = 11;
 cb_data.maximum_rows = ngages + 1;

 if(cb_data.visible_rows > cb_data.maximum_rows)
    cb_data.visible_rows = cb_data.maximum_rows;

 /*--------------------------------------------------------------*/
 /*     Allocate space for arrays in cb_data structure           */
 /*--------------------------------------------------------------*/

 cb_data.text_w =
    ( Widget ** ) malloc ( cb_data.maximum_columns * sizeof ( Widget * ) ) ;

 if ( cb_data.text_w == NULL )
 {
    flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                       "Could not allocate memory for \"cb_data.text_w\".\n"
                       "Cannot display the gage table.\n" ) ;
    return ;
 }

 for ( i = 0 ; i < cb_data.maximum_columns ; i++ )
 {
    cb_data.text_w [ i ] =
       ( Widget * ) malloc ( cb_data.maximum_rows * sizeof ( Widget ) ) ;

    if ( cb_data.text_w [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                          "Could not allocate memory for element % d of\n"
                          "\"cb_data.text_w\". Cannot display the gage\n"
                          "table.\n" , i ) ;
       free_gage_table_memory ( ) ;
       return ;
    }
 }

 cb_data.labels =
    ( Widget ** ) malloc ( cb_data.maximum_columns * sizeof ( Widget * ) ) ;

 if ( cb_data.labels == NULL )
 {
    flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                       "Could not allocate memory for \"cb_data.labels\".\n"
                       "Cannot display the gage table.\n" ) ;
    free_gage_table_memory ( ) ;
    return ;
 }

 for(i=0; i < cb_data.maximum_columns ; i++ )
 {
    cb_data.labels [ i ] =
    	( Widget * ) malloc ( cb_data.maximum_rows * sizeof ( Widget ) ) ;

    if ( cb_data.labels [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                          "Could not allocate memory for element % d of\n"
                          "\"cb_data.labels\". Cannot display the gage\n"
                          "table.\n" , i ) ;
       free_gage_table_memory ( ) ;
       return ;
    }
 }

 cb_data.gage_table_children =
    ( Widget * ) malloc ( cb_data.maximum_columns * cb_data.maximum_rows *
                          sizeof ( Widget ) ) ;

 if ( cb_data.gage_table_children == NULL )
 {
    flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                       "Could not allocate memory for "
                       "\"cb_data.gage_table_children\".\n"
                       "Cannot display the gage table.\n" ) ;
    free_gage_table_memory ( ) ;
    return ;
 }

 cb_data.data_array =
     ( float ** ) malloc ( ( cb_data.maximum_columns ) * sizeof ( int * ) ) ;

 if ( cb_data.data_array == NULL )
 {
    flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                       "Could not allocate memory for \"cb_data.data_array\".\n"
                       "Cannot display the gage table.\n" ) ;
    free_gage_table_memory ( ) ;
    return ;
 }

 for ( i = 0 ; i < cb_data.maximum_columns ; i++ )
 {
    cb_data.data_array [ i ] = ( float * ) malloc ( ( cb_data.maximum_rows )
                                               * sizeof ( int ) ) ;
    if ( cb_data.data_array [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                          "Could not allocate memory for element % d of\n"
                          "\"cb_data.data_array\". Cannot display the gage\n"
                          "table.\n" , i ) ;
       free_gage_table_memory ( ) ;
       return ;
    }
 }

 cb_data.headings =
    ( char ** ) malloc ( ( cb_data.maximum_columns ) * sizeof ( char * ) ) ;

 if ( cb_data.headings == NULL )
 {
    flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                       "Could not allocate memory for \"cb_data.headings\".\n"
                       "Cannot display the gage table.\n" ) ;
    free_gage_table_memory ( ) ;
    return ;
 }

 for ( i = 0 ;  i < cb_data.maximum_columns ; i++ )
 {
    cb_data.headings [ i ] = ( char * ) malloc ( 14 * sizeof ( char ) ) ;

    if ( cb_data.headings [ i ] == NULL )
    {
       flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                          "Could not allocate memory for element % d of\n"
                          "\"cb_data.headings\". Cannot display the gage\n"
                          "table.\n" , i ) ;
       free_gage_table_memory ( ) ;
       return ;
    }
 }

 /*--------------------------------------------------------------*/
 /*     All space in cb_data structure now allocated             */
 /*     Alloc space for horizontal label widgets                 */
 /*--------------------------------------------------------------*/

 cb_data.h_labels = (Widget *)malloc((cb_data.maximum_columns-1) *
                                     sizeof(Widget));
 if ( cb_data.h_labels == NULL )
 {
    flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                       "Could not allocate memory for \"cb_data.h_labels\".\n"
                       "Cannot display the gage table.\n" ) ;
    free_gage_table_memory ( ) ;
    return ;
 }
 data_array_field = ( int ** )
                    malloc ( ( rad_data[0].maximum_columns )
                    * sizeof ( int * ) ) ;

 if ( data_array_field == NULL )
 {
     flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                        "An error was encountered while attempting to\n"
                        "dynamically allocate memory for temporary\n"
                        "data array \"data_array_field\".\n" );

     data_array_free_memory ( & data_array_field ) ;
     return ;
 }
 for ( i = 0 ; i < rad_data[0].maximum_columns ; ++ i )
 {
     data_array_field [ i ] = NULL ;
     data_array_field [ i ] = ( int * ) malloc (
                              ( rad_data[0].maximum_rows )
                               * sizeof ( int ) ) ;         /* ? */

     if ( data_array_field [ i ] == NULL )
     {
         flogMessage ( stderr , "In routine \"create_gage_table_RFCW\":\n"
                            "An error was encountered while attempting to\n"
                            "dynamically allocate memory for temporary\n"
                            "data array \"data_array_field\".\n" );

         data_array_free_memory ( & data_array_field ) ;
         return ;
     }

 }

 for ( m = 0; m < SIZE; ++m )
 {
	 /*
	  * defaulting to 0 since this source is not used in
	  * AWIPS II due to the existence of the X functions.
	  */
     display_field_data_RFCW ( display_array[m] ,
    		  data_array_field , date_st3 ,
	          addition_flag , 0, 0);

     lookup_mosaic_data ( data_array_field ,
                   gage ,
		   display_array [m] );
 }

 lookup_radar_index ( data_array_field ,
                      gage );

 strcpy(cb_data.headings[0], "GageID");
 strcpy(cb_data.headings[1], "Gage");
 strcpy(cb_data.headings[2], "BestQPE");
 strcpy(cb_data.headings[3], "MultiSensor");
 strcpy(cb_data.headings[4], "RadarID");
 strcpy(cb_data.headings[5], "RadarMosaic");
 strcpy(cb_data.headings[6], "FieldBias");
 strcpy(cb_data.headings[7], "LocalBias");
 strcpy(cb_data.headings[8], "GageOnly");
 strcpy(cb_data.headings[9], "Satellite");
 strcpy(cb_data.headings[10], "Edit");


 cb_data.first_row_visible = 0;
 cb_data.first_col_visible = 0;

 grtable = XtVaCreatePopupShell("Gage Table",
			      transientShellWidgetClass,
			      toplevel,
			      XmNdeleteResponse, XmDO_NOTHING,
			      NULL);

 /* Add window manager callbacks. */
 wmAtom = XmInternAtom ( ( XtDisplay ( grtable ) ), "WM_DELETE_WINDOW", False );
 XmAddWMProtocolCallback ( grtable, wmAtom, popdown_gagetable, NULL );

 /*--------------------------------------------------------------*/
 /*     Do not map the main shell when it is managed (initially  */
 /*     realized) because we then change the size based on the   */
 /*     size of the row column widget.  If we let it be mapped   */
 /*     when realized and then change size, the shell flashes    */
 /*     twice on the screen, once in the original and again in   */
 /*     the changed size.                                        */
 /*     Note that we map it just before calling XtMainLoop().    */
 /*--------------------------------------------------------------*/

 n=0;
 form_w = XtCreateManagedWidget("form_w",
	     xmFormWidgetClass, grtable, wargs, n);

 n=0;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 menuBar = XmCreateMenuBar(form_w, "gage_table_menuBar", wargs, n);
 XtManageChild(menuBar);

 xstr = XmStringCreate (" Help", XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[0], XmNlabelString, xstr ) ;
 XmStringFree ( xstr ) ;

 XtSetValues(menuBar, wargs, 1);

 timeGg = XtVaCreateManagedWidget("",
				    xmTextWidgetClass,
				    form_w,
				    XmNwidth, 150,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNtopOffset, 5,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset, 40,
				    XmNeditable, FALSE,
				    XmNcursorPositionVisible, FALSE,
				    XmNshadowThickness,0,
				    XmNtraversalOn, FALSE,
				    NULL);

 /* Retrieve the fontlist. */
 n = 0;
 XtSetArg(wargs[n], XmNfontList, &font_list); n++;
 XtGetValues(timeGg, wargs, n);

 /* Determine the displayed length in pixels of the longest
    column heading. */
 xstr = XmStringCreateLocalized ( cb_data.headings[5] );
 longest_heading_length = XmStringWidth ( font_list,  xstr );
 XmStringFree ( xstr );

 gageTableDate = timeGg;
 sprintf(datetimeGg,"%s", date_st3.lldate);
 XmTextSetString(timeGg, datetimeGg);

 /*--------------------------------------------------------------*/
 /*     Create the pulldown main menus...                        */
 /*--------------------------------------------------------------*/

 Control_gtMenuItem = XmCreatePulldownMenu(menuBar, "Control_gtMenuItem", NULL, 0);

 Sort_gtMenuItem = XmCreatePulldownMenu(menuBar, "Sort_gtMenuItem", NULL, 0);

 XtSetArg(wargs[0], XmNsubMenuId, Control_gtMenuItem);
 xstr = XmStringCreate ( "Control" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[1], XmNlabelString , xstr ) ;
 gt_cascade[0] = XmCreateCascadeButton(menuBar, "gtControl_cascade", wargs, 2);
 XmStringFree ( xstr ) ;

 XtSetArg(wargs[0], XmNsubMenuId, Sort_gtMenuItem);
 xstr = XmStringCreate ( "Sort Gages" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[1], XmNlabelString , xstr ) ;
 gt_cascade[1] = XmCreateCascadeButton(menuBar, "gtSort_cascade", wargs, 2);
 XmStringFree ( xstr ) ;
 XtManageChildren(gt_cascade,2);

 Control_menu_struct[0].data =  (caddr_t) grtable;
 xs_create_menu_buttons("", Control_gtMenuItem,
	 Control_menu_struct, XtNumber(Control_menu_struct));
 xs_create_menu_buttons("", Sort_gtMenuItem,
	 Sort_menu_struct, XtNumber(Sort_menu_struct));

 n=0;
 XtSetArg(wargs[n], XmNorientation, XmVERTICAL); n++;
 XtSetArg(wargs[n], XmNsliderSize, cb_data.visible_rows-1); n++;
 XtSetArg(wargs[n], XmNincrement, 1); n++;
 XtSetArg(wargs[n], XmNmaximum, cb_data.maximum_rows-1); n++;
 XtSetArg(wargs[n], XmNpageIncrement, cb_data.visible_rows-1); n++;

 v_scrollbar = XtCreateManagedWidget("v_scrollbar",
		    xmScrollBarWidgetClass, form_w , wargs, n);

 XtAddCallback(v_scrollbar, XmNvalueChangedCallback, v_scrollbar_moved_RFCW, &cb_data);
 XtAddCallback(v_scrollbar, XmNdragCallback,         v_scrollbar_moved_RFCW, &cb_data);
 XtAddCallback(v_scrollbar, XmNtoTopCallback,        v_scrollbar_moved_RFCW, &cb_data);
 XtAddCallback(v_scrollbar, XmNtoBottomCallback,     v_scrollbar_moved_RFCW, &cb_data);

 n=0;

 n=0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, menuBar); n++;
 XtSetArg(wargs[n], XmNtopOffset, 10); n++;
 rc = XtCreateManagedWidget("rc",
	     xmRowColumnWidgetClass, form_w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     Set the packing and number of columns for the rc widget  */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNpacking, XmPACK_COLUMN); n++;
 XtSetArg(wargs[n], XmNnumColumns, cb_data.visible_columns); n++;
 XtSetValues(rc, wargs, n);

 cb_data.rc = rc;

 cb_data.num_children = 0;

 for(i=0; i<cb_data.visible_columns; i++)
    {
    for (j=0; j<cb_data.visible_rows; j++)
       {
       if(j == 0)
	  {
	  /*--------------------------------------------------------------*/
	  /*     Fill zeroth (top) row with column identifiers            */
	  /*                                                              */
	  /*     First create frame widget as child or rc                 */
	  /*     Then put label widget into frame                         */
	  /*--------------------------------------------------------------*/
	  n=0;
	  XtSetArg(wargs[n], XmNshadowType, XmSHADOW_ETCHED_OUT); n++;
          XtSetArg(wargs[n], XmNwidth, 10); n++;
          XtSetArg(wargs[n], XmNresizePolicy, XmRESIZE_NONE ); n++;
	  cb_data.text_w[i][j] = XtCreateManagedWidget("text",
			    xmFrameWidgetClass, rc, wargs, n);
	  cb_data.gage_table_children[cb_data.num_children++] =
		   cb_data.text_w[i][j];
	  n=0;
	  xmstr = XmStringCreate(cb_data.headings[i],
			   XmSTRING_DEFAULT_CHARSET);
	  XtSetArg(wargs[n], XmNlabelString, xmstr); n++;
          XtSetArg(wargs[n], XmNwidth, 10); n++;
          XtSetArg(wargs[n], XmNrecomputeSize, False ); n++;
	  cb_data.h_labels[i] = XtCreateManagedWidget("text2",
			        xmLabelGadgetClass,
			        cb_data.text_w[i][j], wargs, n);
          XmStringFree ( xmstr ) ;
	  }
       else
	  {
	  if(i < cb_data.visible_columns-1)
	     {
	     /*--------------------------------------------------------------*/
	     /*     Fill row/column widget with gage structure values        */
	     /*                                                              */
	     /*     First create frame widget as child or rc                 */
	     /*     Then put label widget into frame                         */
	     /*--------------------------------------------------------------*/
	     n=0;
	     XtSetArg(wargs[n], XmNshadowType, XmSHADOW_ETCHED_OUT); n++;
             XtSetArg(wargs[n], XmNwidth, 10); n++;
          XtSetArg(wargs[n], XmNresizePolicy, XmRESIZE_NONE ); n++;
	     cb_data.text_w[i][j] = XtCreateWidget("text",
			       xmFrameWidgetClass, rc, wargs, n);

	     cb_data.gage_table_children[cb_data.num_children++] =
		      cb_data.text_w[i][j];

	     n=0;
             text =  table_data_RFCW(i,j-1) ;

             xmstr = XmStringCreate(text,XmSTRING_DEFAULT_CHARSET);
             XtSetArg(wargs[n], XmNlabelType, XmSTRING); n++;
	     XtSetArg(wargs[n], XmNlabelString, xmstr); n++;
             XtSetArg(wargs[n], XmNwidth, 10); n++;
          XtSetArg(wargs[n], XmNrecomputeSize, False ); n++;
	     cb_data.labels[i][j] = XtCreateManagedWidget("text",
			       xmLabelWidgetClass,
			       cb_data.text_w[i][j], wargs, n);
             XmStringFree ( xmstr ) ;
	     }
	  else
	     {
	     /*--------------------------------------------------------------*/
	     /*     Fill edit columns with text widget and data              */
	     /*--------------------------------------------------------------*/
	     n=0;
             XtSetArg (wargs[n], XmNwidth, longest_heading_length + 9); n++;
	     cb_data.text_w[i][j] = XtCreateWidget("gage_edit_text",
			       xmTextWidgetClass, rc, wargs, n);
	     cb_data.gage_table_children[cb_data.num_children++] =
		      cb_data.text_w[i][j];
             text =  table_data_RFCW(i,j-1) ;
	     XmTextSetString(cb_data.text_w[i][j], text);
	     XtAddCallback(cb_data.text_w[i][j], XmNvalueChangedCallback,
		 edit_gage_value, &cb_data);
	     }
	  }
       }
    }

 XtManageChildren(cb_data.gage_table_children,cb_data.num_children);

 XtRealizeWidget(grtable);

 /*--------------------------------------------------------------*/
 /*     Get width and height of row/column widget                */
 /*     Width and height aren't set until widget is realized     */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &rc_width); n++;
 XtSetArg(wargs[n], XmNheight, &rc_height); n++;
 XtSetArg(wargs[n], XmNy,&rc_y);n++;
 XtGetValues(rc, wargs, n);

 /*--------------------------------------------------------------*/
 /*     Set width and height of main shell widget based on rc size*/
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, rc_width+60); n++;
 XtSetArg(wargs[n], XmNheight, rc_height * 1.15); n++;
 XtSetValues(grtable, wargs, n);

 /*--------------------------------------------------------------*/
 /*     Get the width  of the vertical and height of the         */
 /*     horizontal scrollbars for use in positioning the         */
 /*     scrollbars in the form widget                            */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &v_scrollbar_width); n++;
 XtGetValues(v_scrollbar, wargs, n);

 /*--------------------------------------------------------------*/
 /*     Place the vertical scrollbar to the right of the         */
 /*     row/column widget                                        */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNleftWidget, rc); n++;
 XtSetArg(wargs[n], XmNleftOffset, 20 ); n++;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNtopOffset, rc_height/cb_data.visible_rows); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomOffset, 10 ); n++;

 XtSetValues(v_scrollbar, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNresizeHeight, FALSE); n++;
 XtSetArg(wargs[n], XmNresizeWidth, FALSE); n++;
 XtSetValues(rc,wargs,n);

 /*--------------------------------------------------------------*/
 /*     Map the main shell widget and all its children           */
 /*     Didn't do this earlier because changing the size         */
 /*     of the shell after it was realized caused it to be       */
 /*     drawn twice.  This way it isn't visible until it has     */
 /*     the correct dimensions                                   */
 /*--------------------------------------------------------------*/

  sort_RFCW(grtable, (XtPointer)1, NULL);
  XtPopup(grtable,XtGrabNone);
  data_array_free_memory ( & data_array_field );
}

/******************************************* END create_gage_table_RFCW *******/


/***************************************************************************/
/*  FUNCTION NAME:   table_data_RFCW                                       */
/*       FUNCTION:   populate gage table                                   */
/***************************************************************************

Function type:
   deref character

Called by function:

Functions called:
   none

******************************************** BEGIN table_data_RFCW **************/

char * table_data_RFCW ( int i , int j )
{
  static char value [ 10 ] ;

      if (i==0)   strcpy ( value , gage[j].id ) ;

 else if (i==1)   sprintf ( value , "%.2f" , gage[j].gval ) ;
 else if (i==2)   sprintf ( value , "%.2f" , gage[j].xmrg_val ) ;
 else if (i==3)   sprintf ( value , "%.2f" , gage[j].mval ) ;
 else if (i==4)   sprintf ( value , "%s" , gage[j].rid ) ;
 else if (i==5)   sprintf ( value , "%.2f" , gage[j].rval ) ;
 else if (i==6)   sprintf ( value , "%.2f" , gage[j].bval ) ;
 else if (i==7)   sprintf ( value , "%.2f" , gage[j].loc_val ) ;
 else if (i==8)   sprintf ( value , "%.2f" , gage[j].gage_only ) ;
 else if (i==9)   sprintf ( value , "%.2f" , gage[j].sat_val ) ;
 else if (i==10)   strcpy ( value , gage[j].edit ) ;

 if ( value [ 0 ] == '-' ) strcpy ( value , "M" ) ;

 return value ;

}

/******************************************* END table_data_RFCW **************/


/***************************************************************************/
/*  FUNCTION NAME:   change_rc_RFCW                                        */
/*       FUNCTION:   modify data displayed in gage table                   */
/***************************************************************************

Function type:
   void

Called by function:
   sort_RFCW

Functions called:
   table_data_RFCW

Local variables:
   i - integer; visible column incrementor
   j - integer; visible row incrementor
   n - integer; index (incrementor) for wargs array
   text - dereferenced character; 14 characters of text; value
      assigned in function table_data(), which draws text from
      structure cb_struct based on row and column position

************************************** BEGIN change_rc_RFCW ***************/

void change_rc_RFCW ( cb_struct * cb_data )
{
   int  i, j, n ;
   char * text  = NULL ;
   Arg  wargs [ 5 ] ;
   XmString xstr ;

 /*-------------------------------------------------------------------------*/
 /*     get values for all columns except edit column                       */
 /*-------------------------------------------------------------------------*/

 for(i=0; i<= cb_data->visible_columns-1; i++)
 {
    if (i != cb_data->visible_columns-1)
    {
       for ( j=0 ; j<cb_data->visible_rows-1; j++)
       {
	  n = 0 ;
	  text=table_data_RFCW(i,j+cb_data->first_row_visible);
          xstr = XmStringCreate ( text , XmSTRING_DEFAULT_CHARSET ) ;
	  XtSetArg ( wargs [ n ] , XmNlabelString , xstr ) ; n++ ;
	  XtSetValues(cb_data->labels[i][j+1], wargs, n);
          XmStringFree ( xstr ) ;
       }
    }

 /*-------------------------------------------------------------------------*/
 /*     set data for edit column                                            */
 /*-------------------------------------------------------------------------*/

    else if (i == cb_data->visible_columns-1)
    {
       for (j=0; j<cb_data->visible_rows-1; j++)
       {
	  n = 0;
	  text=table_data_RFCW(i,j+cb_data->first_row_visible);
	  XmTextSetString(cb_data->text_w[i][j+1],text);
       }
    }
  }

}

/**************************************** END change_rc_RFCW ***************/

/***************************************************************************/
/*  FUNCTION NAME:   sort_RFCW                                             */
/*       FUNCTION:   reorder the gage table by user selected option        */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   qsort
   change_rc_RFCW

***************************************** BEGIN sort_RFCW ******************/

void sort_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata )
{
    int data = ( int ) clientdata ;

//    if (data == 0)
//    qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct),
 //   		sort_by_gageval);

// else if (data == 1)
 //   qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct),
   // 		sort_by_gageid);

 //else if (data == 2)
   // qsort((void *)gage, (size_t) ngages, (size_t)sizeof(gage_struct),
    //		sort_by_radar);

// change_rc_RFCW(&cb_data);
}

/******************************************* END sort_RFCW *****************/


/***************************************************************************/
/*  FUNCTION NAME:   v_scrollbar_moved_RFCW()                              */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   change_rc_RFCW

***************************************** BEGIN v_scrollbar_moved_RFCW *******/

void v_scrollbar_moved_RFCW ( Widget w , XtPointer clientdata ,
                              XtPointer calldata )
{
   Arg wargs[5];
   cb_struct * cb_data = ( cb_struct * ) clientdata ;
   int n;
   XmScrollBarCallbackStruct * call_data = ( XmScrollBarCallbackStruct * )
                                             calldata ;

 /*--------------------------------------------------------------*/
 /*     If a toTop or toBottom scrollbar event, move the         */
 /*     scrollbar to the top or bottom of the scroll area        */
 /*--------------------------------------------------------------*/

 if ( call_data->reason == 7 || call_data->reason == 8 )
 {
    n = 0;
    XtSetArg(wargs[n], XmNvalue, call_data->value); n++;
    XtSetValues(w, wargs, n);
 }

 cb_data->first_row_visible = call_data->value;

 /*--------------------------------------------------------------*/
 /*     Move data into 1st through maximum rows and cols of      */
 /*     rc widget                                                */
 /*--------------------------------------------------------------*/

   change_rc_RFCW(cb_data);

}

/***************************************** END v_scrollbar_moved_RFCW *******/

/***************************************************************************/
/*  FUNCTION NAME:   free_gage_table_memory                                */
/*       FUNCTION:   Releases dynamically allocated memory for elements of */
/*                   the cb_data structure back to the operating system.   */
/***************************************************************************

Function type:
   void

Called by function:
   free_dynamic_memory ( in hmap_mpe ).

Functions called:
   None
***************************************** BEGIN v_scrollbar_moved_RFCW *******/


void free_gage_table_memory ( )
{
   int i ;

   if ( cb_data.text_w != NULL )
   {
      for ( i = 0 ;
            ( i <  cb_data.maximum_columns ) &&
            ( cb_data.text_w [ i ] != NULL ) ;
            ++ i )
      {
         free ( cb_data.text_w [ i ] ) ;
      }

      free ( cb_data.text_w ) ;
      cb_data.text_w = NULL ;
   }

   if ( cb_data.labels != NULL )
   {
      for ( i = 0 ;
            ( i < cb_data.maximum_columns ) &&
            ( cb_data.labels [ i ] != NULL ) ;
            ++ i )
      {
         free ( cb_data.labels [ i ] ) ;
      }

      free ( cb_data.labels ) ;
      cb_data.labels = NULL ;
   }

   if ( cb_data.gage_table_children != NULL )
   {
      free ( cb_data.gage_table_children ) ;
      cb_data.gage_table_children = NULL ;
   }

   if ( cb_data.data_array != NULL )
   {
      for ( i = 0 ;
            ( i < cb_data.maximum_columns ) &&
                  ( cb_data.data_array [ i ] != NULL ) ;
            ++ i )
      {
         free ( cb_data.data_array [ i ] ) ;
      }

      free ( cb_data.data_array ) ;
      cb_data.data_array = NULL ;
   }

   if ( cb_data.headings != NULL )
   {
      for ( i = 0 ;
            ( i < cb_data.maximum_columns ) &&
            ( cb_data.headings [ i ] != NULL)  ;
            ++ i )
      {
         free ( cb_data.headings [ i ] ) ;
      }

      free ( cb_data.headings ) ;
      cb_data.headings = NULL ;
   }

   if ( cb_data.h_labels != NULL )
   {
      free ( cb_data.h_labels ) ;
      cb_data.h_labels = NULL ;
   }

}

/***************************************************************************/
/*  FUNCTION NAME:   lookup_mosaic_data                                    */
/*       FUNCTION:   Fills row/column widget with gage structure (mosaics, */
/*                   xmrg, gage_only) values                               */
/***************************************************************************
Function type:
   void

Called by function:
   create_gage_table_RFCW

Functions called:
   None
***************************************** BEGIN lookup_mosaic_data *********/

void lookup_mosaic_data ( int ** data_array_tmp , gage_struct *gage ,
			  enum DisplayFieldData  display_array )
{
   int k;
   for(k=0; k<ngages; k++)
   {
     switch ( display_array ) {
         case display_mMosaic:
            if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
           	 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
                gage[k].mval = (float)
	    	data_array_tmp[ ( int ) gage[k].hrap_loc.x]
                              [ ( int ) gage[k].hrap_loc.y] / 2540. ;
            else
	        gage[k].mval =  -999.;
	    break;

	 case display_Xmrg:
	    if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
           	 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
                gage[k].xmrg_val = (float)
	    	data_array_tmp [ ( int ) gage[k].hrap_loc.x ]
                               [ ( int ) gage[k].hrap_loc.y ] / 2540. ;
            else
	        gage[k].xmrg_val =  -999.;
	    break;

	 case display_rMosaic:
	    if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
           	 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
                gage[k].rval = ( float )
	    	data_array_tmp[ ( int ) gage[k].hrap_loc.x ]
                              [ ( int ) gage[k].hrap_loc.y ]/2540. ;
            else
	        gage[k].rval =  -999.;
	    break;

         case display_bMosaic:
	    if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
           	 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
                gage[k].bval = (float)
	    	data_array_tmp[ ( int ) gage[k].hrap_loc.x]
                              [ ( int ) gage[k].hrap_loc.y]/2540.;
            else
	        gage[k].bval =  -999.;
	    break;

	 case display_lMosaic:
	    if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
           	 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
                gage[k].loc_val = (float)
	    	data_array_tmp[ ( int ) gage[k].hrap_loc.x]
                              [ ( int ) gage[k].hrap_loc.y]/2540.;
            else
	        gage[k].loc_val =  -999.;
	    break;

	 case display_gageOnly:
	    if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
           	 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
                gage[k].gage_only = (float)
	    	data_array_tmp[ ( int ) gage[k].hrap_loc.x]
                              [ ( int ) gage[k].hrap_loc.y]/2540.;
            else
	        gage[k].gage_only =  -999.;
	    break;

	 case display_satPrecip:
            if ( ( int ) gage[k].hrap_loc.x > 0 &&
                 ( int ) gage[k].hrap_loc.x < MAXX &&
                 ( int ) gage[k].hrap_loc.y > 0 &&
                 ( int ) gage[k].hrap_loc.y < MAXY )
            {
                gage[k].sat_val = (float)
                data_array_tmp [ ( int ) gage[k].hrap_loc.x ]
                               [ ( int ) gage[k].hrap_loc.y ]/2540.;
            }
	    else
                {
               logMessage("in else\n");
		gage[k].sat_val =  -999.;
                }
	   break;

         default:
	    return;
      }
   }

}
/************************************** END lookup_mosaic_data ***************/

/*****************************************************************************/
/*  FUNCTION NAME:   lookup_radar_index                                      */
/*       FUNCTION:   Fills row/column widget with radar igentifiers values   */
/*  									     */
/*****************************************************************************
Function type:
   void

Called by function:
   create_gage_table_RFCW

Functions called:
   None
***************************************** BEGIN lookup_mosaic_data *********/

void lookup_radar_index ( int ** data_array_tmp ,
                          gage_struct * gage )
{
   int                  len, len_fname ;
   char                 dirname [ 100 ] ;
   char                 fname [ 128 ] ;
   int 			k , n;
   char                 str[ 10 ];

   len = strlen("rfcwide_index_dir");
	 get_apps_defaults("rfcwide_index_dir",&len,dirname,&len);
         strcpy(cv_use_tmp,"INDEX");

   sprintf(fname,"%s/%s%sz",dirname,cv_use_tmp,date_st3.cdate);
   len_fname = strlen(fname);

   display_field_read_xmrg ( data_array_tmp , fname, addition_flag , 0, 0 );

   for(k=0; k<ngages; k++)
   {
      len = 3;
      gage[k].rid[3] = '\0';

      if ( ( int ) gage[k].hrap_loc.x > 0 &&
           ( int ) gage[k].hrap_loc.x < MAXX &&
           ( int ) gage[k].hrap_loc.y > 0 &&
           ( int ) gage[k].hrap_loc.y < MAXY )
         {
	    n = data_array_tmp[ ( int ) gage[k].hrap_loc.x]
                          [ ( int ) gage[k].hrap_loc.y] - 1;

	    if ( n >= 0)
	    {
	       sprintf ( str , "%s" , nexrad[n].id ) ;
	       strncpy ( gage[k].rid, nexrad[n].id, len);
	    }
	    else
	       strcpy ( gage[k].rid, "M");

         }
   }
   return ;
}
/************************************* END lookup_radar_index **************/

/***************************************************************************/
/*  FUNCTION NAME:   data_array_free_memory()                              */
/*       FUNCTION:   This routine frees dynamically allocated memory used  */
/*                   for data_array_field array in create_gage_table_RFCW  */
/*                   routine.                                              */
/***************************************************************************

Function type:
   void

Called by function:
   create_gage_table_RFCW

Functions called
   only standard library routines

************************************** BEGIN data_array_free_memory *******/

void data_array_free_memory ( int *** data_array_tmp )
{
   int i ;

   /* Free the data array. */

   if ( * data_array_tmp != NULL )
   {
      for ( i = 0 ; ( ( * data_array_tmp ) [ i ] != NULL )
                      && ( i < rad_data[0].maximum_columns ) ; ++ i )
      {
         free ( ( *  data_array_tmp ) [ i ] ) ;
          ( * data_array_tmp ) [ i ] =  NULL ;
      }

      free ( * data_array_tmp ) ;
   }
}

/************************************** END data_array_free_memory *******/

/***************************************************************************/
/*  FUNCTION NAME:   popdown_gagetable                                     */
/*       FUNCTION:   popdown gage table shell and check for any edited     */
/*                   gages;                                                */
/*                   if any edited gages are found, then turn on           */
/*                   sensitivity of Run StageII option                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) quit button of gage table

Functions called:
   None

**************************************** BEGIN popdown_gagetable ***********/
void popdown_gagetable(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{
   extern int display_gage_table ;
   if ( grtable != 0 )
   {
      XtPopdown ( grtable ) ;
      grtable = 0 ;
   }

   display_gage_table = 0 ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/gage_table_RFCW.c,v $";
 static char rcs_id2[] = "$Id: gage_table_RFCW.c,v 1.25 2007/02/22 16:05:46 lawrence Exp $";}
/*  ===================================================  */

}

/***************************************** END popdown_gagetable ***********/



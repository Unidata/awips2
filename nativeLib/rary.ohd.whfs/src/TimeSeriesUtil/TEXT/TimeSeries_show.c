/* ******************************************** */
/* 	File:           TimeSeries_show.c       */
/* 	Date:           April 1999              */
/* 	Author:         Sung Vo                 */
/* 	Purpose:        Provide support for the */
/*      Precipitation Accumalitions Dialog.     */
/* ******************************************** */
/* **************************************************************/
/*	Modification History										*/
/*  Date:		Author:			Purpose:						*/
/*  04-2004		Guoxian Zhou    add TSBatchScaleStages          */
/*                              functions to support batch      */
/*                              scale stages                    */
/*  05-2004		Guoxian Zhou    Modify some push buttons'       */
/*                              functions to radion buttons'    */
/*                              functions                       */
/* **************************************************************/


/* ***************************************** */
/* ************* Header files ************** */
/* ***************************************** */

#include <X11/IntrinsicP.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>

#include "TSGRAPH.h"
#include "GeneralUtil.h"
#include "TimeSeries_show.h"
#include "TimeSeries.h"
#include "TSControl.h"
#include "TSedit.h"
#include "tabular_info.h"

static KeySym keysym1 [ ] = { XK_Page_Up , XK_Page_Down , XK_Up , XK_Down } ;
static KeyCode keycode[XtNumber(keysym1)];

extern int display_width;
/* ********************************************* */
/* Global data structures are defined here       */
/* ********************************************* */

 PAGE_INFO        TSpageinfo[MAX_PAGE_INFO];
 PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

 PAGE_MGR        *PageMgr   = NULL;
 GROUP_INFO      *GroupInfo = NULL;
 PAGE_INFO       *PageInfo  = NULL;
 GRAPH_INFO      *GraphInfo = NULL;
 TRACE_INFO      *TraceInfo = NULL;

 COLORS  TScolors[MAX_COLORS] =
 {
        { 0, "DodgerBlue"} ,
        { 0, "Yellow"} ,     
        { 0, "Cyan"} ,       
        { 0, "Green"} ,         
        { 0, "Purple"} ,
        { 0, "Magenta"} ,    
        { 0, "Red"} ,        
        { 0, "Aquamarine"} , 
        { 0, "SeaGreen"} ,      
        { 0, "maroon"} ,
        { 0, "BlueViolet"} , 
        { 0, "Coral"} ,      
        { 0, "HotPink"} ,    
        { 0, "MediumPurple"} ,  
        { 0, "DeepPink"} ,
        { 0, "ForestGreen"} ,
        { 0, "LimeGreen"} ,  
        { 0, "Orange"} ,     
        { 0, "AliceBlue"} ,     
        { 0, "SkyBlue"} ,
        { 0, "White"} ,      
        { 0, "DarkGreen"} ,  
        { 0, "ForestGreen"} ,
        { 0, "LightBlue"} ,     
        { 0, "Wheat"} ,
        { 0, "DarkGreen"} ,  
        { 0, "DarkOrange"} , 
        { 0, "DeepPink"} ,   
        { 0, "Gold"} ,          
        { 0, "GreenYellow"} ,
        { 0, "DarkViolet"} , 
        { 0, "IndianRed"} ,  
        { 0, "LawnGreen"} ,  
        { 0, "CornflowerBlue"} ,
        { 0, "LimeGreen"} ,
        { 0, "MediumBlue"} , 
        { 0, "DarkGreen"} ,  
        { 0, "PaleGreen"} ,  
        { 0, "MediumOrchid"} ,  
        { 0, "SteelBlue" }
 };


/* ********************************************* */
/* End   global data structures are defined here */
/* ********************************************* */

	Widget  TS_fileSB,
		TSTracesDS, 
		TSTracesClosePB,
		TSTracesFO,
		TSTracesW[40];

/* *********************************** */
/* Activate Time Series Display        */
/* *********************************** */
void    show_TimeSeries(Widget w, XtPointer ptr, XtPointer cbs)
{

	static int first = 1;
	int * manage_gui = ( int * ) ptr;

	if (! TimeSeriesDS)
    	{
       		create_TimeSeriesDS(GetTopShell(w));
		create_TSTracesDS(GetTopShell(w));
       		TimeSeries_callbacks();
    	}


 	if ( ! XtIsManaged(TimeSeriesDS) && 
	      ( manage_gui != NULL ) && 
	      ( * manage_gui == 1 ) )
   	{
      		XtManageChild(TSFO);
      		XtManageChild(TimeSeriesDS);
		XtMoveWidget(TimeSeriesDS, 280,0);

	        if( first)TSsetup_gc();
	        first =0;
   	}

   return;

}


/* *********************************************************** */
/* Adding callbacks for Time Series Display - All options on   */
/* the menu bar                                                */
/* *********************************************************** */
void TimeSeries_callbacks()
{

	extern void save_cancel_edit ();

	Atom    atom;

	atom = XmInternAtom(XtDisplay(TimeSeriesDS),"WM_DELETE_WINDOW",False);

	/* *********************************************************** */
	/* Adding Callbacks for TSoptions when mouse button 2 activate */
	/* *********************************************************** */

	XtAddCallback(TSGridLinesTB,	XmNvalueChangedCallback, TSgrid_CB, NULL);
	XtAddCallback(TSShowFcstTB,		XmNvalueChangedCallback, TSfcstonly_CB, NULL);

	XtAddCallback(TSTracePB,     XmNactivateCallback, TSTracePB_CB, NULL);
	XtAddCallback(TSAddPB,       XmNactivateCallback, TSAddPB_CB, NULL);
	XtAddCallback(TSDeletePB,    XmNactivateCallback, TSDeletePB_CB, NULL);
	XtAddCallback(TSMovePB,      XmNactivateCallback, TSMovePB_CB, NULL);
        XtAddCallback(TSSetMissingPB, XmNactivateCallback, TSSetMissingPB_CB, NULL);
	XtAddCallback(TSEditDonePB,  XmNactivateCallback, TSEditDonePB_CB, NULL);
	XtAddCallback(TSEditCancelPB,XmNactivateCallback, save_cancel_edit, (XtPointer) 2);

	XtAddCallback(TSPgUp,         XmNactivateCallback, TSpageUp_CB,    NULL);
	XtAddCallback(TSPgDn,         XmNactivateCallback, TSpageDown_CB,  NULL);
       
	XtAddCallback (TSSetTB,XmNvalueChangedCallback,(XtCallbackProc) set_graph, 
                       (XtPointer ) 0) ;
        XtAddCallback (TSResetTB,XmNvalueChangedCallback,(XtCallbackProc) set_graph,
                      (XtPointer ) 1 ) ;
        XtAddCallback (TSOffTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_graph_show, ( XtPointer ) NO_PC_TO_PP) ;
        XtAddCallback (TSInterpolateTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_graph_show, ( XtPointer ) INTERPOLATE) ;
        XtAddCallback (TSAssignTB, XmNvalueChangedCallback, 
                       ( XtCallbackProc ) set_graph_show, ( XtPointer ) ASSIGN) ;
        XmToggleButtonSetState ( TSInterpolateTB ,  True , False ) ; 

        XtAddCallback (TSScaleDataOnlyTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_graph_scale, ( XtPointer ) 0) ;
        XtAddCallback (TSScaleDataShowCategTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_graph_scale, ( XtPointer ) 1) ; 
        XtAddCallback (TSScaleDataCategoriesTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_graph_scale,( XtPointer ) 2) ; 
	XmToggleButtonSetState ( TSScaleDataShowCategTB ,  True , False ) ;

        XtAddCallback (TSPointsTB,XmNvalueChangedCallback,(XtCallbackProc) set_plot,
                       (XtPointer ) 0) ;
        XtAddCallback (TSLinesTB,XmNvalueChangedCallback,(XtCallbackProc) set_plot,
                       (XtPointer ) 1) ;
        XtAddCallback (TSBothTB,XmNvalueChangedCallback,(XtCallbackProc) set_plot,
                       (XtPointer ) 2) ;
        XmToggleButtonSetState ( TSBothTB,  True , False ) ;

        XtAddCallback (TSBatchDataOnlyTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_batch_scale, (XtPointer )0 ) ;
        XtAddCallback (TSBatchShowCategTB, XmNvalueChangedCallback,
		       ( XtCallbackProc ) set_batch_scale, (XtPointer )1 ) ;
        XtAddCallback (TSBatchCategoriesTB, XmNvalueChangedCallback,
                       ( XtCallbackProc ) set_batch_scale, (XtPointer )2 ) ;
        XmToggleButtonSetState ( TSBatchShowCategTB,  True , False ) ;

	XtAddCallback(TSDrawArea,     XmNexposeCallback, TSexpose_resize_CB, NULL);

	XtAddEventHandler(TSDrawArea, ButtonPressMask,   False,  (XtEventHandler)TSpress_CB,   NULL);
	XtAddEventHandler(TSDrawArea, ButtonReleaseMask, False,  (XtEventHandler)TSrelease_CB, NULL);
	XtAddEventHandler(TSDrawArea, Button1MotionMask,  False,  (XtEventHandler)TSdrag_CB,    NULL);

	XtAddEventHandler(TSDrawArea, ButtonPressMask,   False,  (XtEventHandler)TSpressEdit_CB,   NULL);
	XtAddEventHandler(TSDrawArea, ButtonReleaseMask, False,  (XtEventHandler)TSreleaseEdit_CB, NULL);
	XtAddEventHandler(TSDrawArea, Button1MotionMask,  False,  (XtEventHandler)TSdragEdit_CB,    NULL);

	XtAddEventHandler(TSDrawArea, KeyPressMask,  False,  (XtEventHandler)TSkey_CB,    NULL);

	XtAddCallback(TSCDialogPB, XmNactivateCallback, Manage_TimeSeries, NULL);


	XtAddCallback(TSSavePB,XmNactivateCallback, TSsave_CB,  NULL);

	XtAddCallback(PnonInversePB,XmNactivateCallback, TSprint_noinverse_CB,  NULL);
	XtAddCallback(PInversePB,   XmNactivateCallback, TSprint_inverse_CB,    NULL);

	XtAddCallback(TSClosePB,XmNactivateCallback, TSClose_CB,  NULL);

	XmAddWMProtocolCallback(TimeSeriesDS, atom,TSClose_CB, NULL);

	XtAddCallback(TSTracesClosePB,XmNactivateCallback, TSTracesClose_CB,  NULL);

	return;
}

/* **************************************************** */
/* Set up graphical context and initialize page manager */
/* **************************************************** */
void TSsetup_gc()
{

	static int first = 1;

	/* ******************* */
	/* Set up gc for page  */
	/* ******************* */

	if ( first )
	{

		PageMgr->page_width  	= getWidgetWidth(TSDrawArea);
		PageMgr->page_height 	= getWidgetHeight(TSDrawArea);
		PageMgr->widget  	= TSDrawArea;
        	PageMgr->display 	= XtDisplay(TSDrawArea);
		PageMgr->window  	= XtWindow(TSDrawArea);
		PageMgr->gc_line       	= XCreateGC(PageMgr->display, PageMgr->window, 0, NULL);
		PageMgr->gc_point    	= XCreateGC(PageMgr->display, PageMgr->window, 0, NULL);
		PageMgr->gc_reference	= XCreateGC(PageMgr->display, PageMgr->window, 0, NULL);
		PageMgr->gc_grid	= XCreateGC(PageMgr->display, PageMgr->window, 0, NULL);
		PageMgr->gc_background	= XCreateGC(PageMgr->display, PageMgr->window, 0, NULL);
		PageMgr->gc_header	= XCreateGC(PageMgr->display, PageMgr->window, 0, NULL);
		PageMgr->gc_Xor   	= xs_create_xor_gc(TSDrawArea);

        	if(( PageMgr->display_fs = XLoadQueryFont(PageMgr->display, "7x13")) == NULL)
		{
			printf("Error in loading TSFont...\n");
			return;
		} 

		XSetLineAttributes(PageMgr->display,PageMgr->gc_grid,     1,LineOnOffDash,CapRound, JoinMiter);
		XSetLineAttributes(PageMgr->display,PageMgr->gc_reference,2,LineOnOffDash,CapRound, JoinMiter);

		XSetLineAttributes(PageMgr->display,PageMgr->gc_line, 1,LineSolid,CapRound, JoinRound);
		XSetLineAttributes(PageMgr->display,PageMgr->gc_point,1,LineSolid,CapRound, JoinRound);

		SetColor(PageMgr->gc_reference, TSDrawArea, "Cyan");

		XSetFont(PageMgr->display, PageMgr->gc_line,  PageMgr->display_fs->fid);
		XSetFont(PageMgr->display, PageMgr->gc_header,PageMgr->display_fs->fid);

		SetColor(PageMgr->gc_grid, TSDrawArea, "white");
		SetColor(PageMgr->gc_line,      TSDrawArea, "white");
		SetColor(PageMgr->gc_grid,      TSDrawArea, "white");
		SetColor(PageMgr->gc_background,TSDrawArea, "black");
		SetColor(PageMgr->gc_header,    TSDrawArea, "Cyan");  
		SetColor(PageMgr->gc_point,     TSDrawArea, "Red");  


		/* Check the apps_default file for timeseries_linewidth token value */
		char 	gad_value[128], apps_str[] = "timeseries_linewidth";

		int    	gad_token_len=0, gad_value_len=0, rcfad=0, tmpLinewidth;

		gad_token_len = strlen(apps_str);
		rcfad = get_apps_defaults(apps_str, &gad_token_len, gad_value, &gad_value_len);

		if ( rcfad == 0 )
		{
			tmpLinewidth = atoi(gad_value);

			if(tmpLinewidth <= 0 || tmpLinewidth > 5)
			{
				fprintf(stderr, "WARNING: The current value \"%s\" set for the token \"timeseries_linewidth\"\n", gad_value); 
				fprintf(stderr, "         is not valid.\n"); 
			}
			else
				XSetLineAttributes(PageMgr->display, PageMgr->gc_line, tmpLinewidth, LineSolid, CapRound, JoinRound);
		}
		else
		{
			fprintf(stderr,"STATUS: There is no value currently set for the token \"timeseries_linewidth\".\n"); 
		}



		PageMgr->display_crosshairs   = 1;
		PageMgr->active_graph         = 0;
		PageMgr->last_active_graph    = 0;
		GroupInfo->current_page       = 0;
		PageMgr->symbol_type          = CIRCLE;

		GroupInfo->grid_lines 	      = 1; /* grid line on as default           */
		GroupInfo->trace_mode         = 2; /* trace_mode: 0=Point,1=Line,2=Both */

		/*Initialize menu so that the corresponding item is selected.
		 * Added by guoxian zhou 05-2004
		 */

		Widget widget;
		char buttonIndex[10];
		int i;

		for(i = 0; i < 3; i++ )
		{
			sprintf(buttonIndex, "button_%d", i);

			if(i == GroupInfo->trace_mode)
			{
				if((widget = XtNameToWidget(TSPlotMO, buttonIndex)) != NULL )
					XtVaSetValues(widget, XmNset, TRUE, NULL);
			}
			else
			{
				if((widget = XtNameToWidget(TSPlotMO, buttonIndex)) != NULL )
					XtVaSetValues(widget, XmNset, FALSE, NULL);
			}

		}

		first = 0;

		DeSentitize_edit_options();

		PageMgr->Edit_count = 0;
		PageMgr->Editmode   = 0;
		init_TScolors();

		/* Initialize the key mappings. */
		for(i=0;i<XtNumber(keysym1);i++)
		{
			keycode[i] = XKeysymToKeycode(PageMgr->display,keysym1[i]);
		}

	}

}


/* ************************************ */
/* Manage the TimeSeries Display Window */
/* ************************************ */
void Manage_TimeSeries ( Widget w, XtPointer ptr, XtPointer cbs)
{
	XtManageChild(TSC_FO);
	XtManageChild(TSControlDS);
	XRaiseWindow(XtDisplay(TSControlDS), XtWindow(TSControlDS));
}

/* ******************************************* */
/* Handle the page down selected from menu bar */
/* ******************************************* */
void TSpageDown_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
	if (  GroupInfo->GroupSelected == GROUP)
			display_TSnext_page ();	

	return;
}

/* ******************************************* */
/* Handle the page up   selected from menu bar */
/* ******************************************* */
void TSpageUp_CB (Widget w, XtPointer ptr, XtPointer cbs)
{

	if (  GroupInfo->GroupSelected == GROUP)
			display_TSprev_page ();	

	return;
}

/* ********************************************** */
/* Handle the scale stages selected from menu bar */
/* ********************************************** */
void TSFloodCat_CB ( Widget w , XtPointer client_data , XtPointer call_data )
{

	PAGE_INFO       *pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
	GRAPH_INFO      *ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];

	PAGE_DATA  	*pptr  = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA 	*gptr;
	int item_no = ( int ) client_data ;

	if ( pptr->num_graphs == 1 ||  PageMgr->active_graph < 0)
						PageMgr->active_graph = 0;

	gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

	/* ************************************************************* */
	/* 	item_no=0 scale by data only;                             	 */
    /*	item_no=1 scale by data, show categories if within data;  	 */
    /*	item_no=2 scale by data and categories;                   	 */
	/* ************************************************************* */

	ginfo->showcat = item_no;

	/* *************************  */
	/* Display the active graph   */
	/* *************************  */
	display_TSgraph_data( PageMgr->active_graph );

}

/* ******************************************* */
/* Return if display flood categories is valid */
/* ******************************************* */
int display_floodcat()
{
	int	n,
		valid_graph;
	float	ymin, ymax;

	PAGE_INFO       *pinfo;
	GRAPH_INFO      *ginfo;

	PAGE_DATA	*pptr;
	GRAPH_DATA 	*gptr;
	TRACE_DATA	*tptr;

	pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
	ginfo = (GRAPH_INFO *)&pinfo->graph[PageMgr->active_graph];
	pptr = (PAGE_DATA  *) &TSpagedata[0];
	gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

	if ( pptr->num_graphs == 1 ||  PageMgr->active_graph < 0)
						PageMgr->active_graph = 0;
	
	valid_graph = 0;
	for ( n = 0; n< gptr->num_traces; n++)
	{
		if ( gptr->trace_on[n] )
		{
			tptr = (TRACE_DATA *) &gptr->traces[n];

			if ( strncmp ("H", tptr->trace_info.name, 1 ) == 0)
			{
				valid_graph = 1;
				break;

			}

			if ( strncmp ("Q", tptr->trace_info.name, 1 ) == 0)
			{
				valid_graph = 1;
				break;
			}
		}

	}

	if ( valid_graph == 0 || gptr->hdataPtr.valid == 0) 
	{
		return ( 0 );

	}

	/* ************************************************************  */
	/* Adjust yaxis to new max min when scale to catgories selected  */
	/* ************************************************************  */

	ymin = gptr->hdataPtr.ymin;
	ymax = gptr->hdataPtr.ymax;

 	if (PageMgr->zoom == 1 && gptr->hdataPtr.valid == 1)
			return (1);

 	if ( PageMgr->Editmode )
			return (1);

	if ( gptr->hdataPtr.valid )
	{
		switch ( ginfo->showcat )
		{
			case 0:
				gptr->ymin = gptr->old_ymin;
				gptr->ymax = gptr->old_ymax;
				gptr->data_inc = gptr->old_data_inc;
				return (0);
			break;

			case 1:
				gptr->ymin = gptr->old_ymin;
				gptr->ymax = gptr->old_ymax;
				gptr->data_inc = gptr->old_data_inc;
				return ( 1);
			break;

			case 2:
				if ( ymin >= gptr->ymin &&  ymax <= gptr->ymax)
				{
					/* may not work in one case ??? 
					gptr->ymin = gptr->old_ymin;
					gptr->ymax = gptr->old_ymax;
					gptr->data_inc = gptr->old_data_inc;
					*/
					return( 1 );
				}
				else
				{

					if ( ymin > gptr->ymin ) ymin = gptr->ymin;
					if ( ymax < gptr->ymax ) ymax = gptr->ymax;
					if ( ymax == 0.0) gptr->ymin = 0.0;
					if ( ymax < gptr->old_ymax) ymax = gptr->old_ymax;
					adjust_ymaxmin(ymin, ymax, &gptr->ymin, &gptr->ymax, &gptr->data_inc);
					return ( 1 );
				}
			break;

		}

	}

	return (0);
}

/* ************************************ */
/* Handle show PP derived from PC event */
/* ************************************ */
void TSpcAspp_CB (Widget w, XtPointer client_data , XtPointer call_data )
{
	PAGE_INFO	*pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
	GRAPH_INFO	*ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];
	int item_no = ( int ) client_data ;

	/* ************************************* */
	/* 0 = off ; 1 = INTERPOLATE; 2 = ASSIGN */
	/* ************************************* */

	PageMgr->pcAspp_intervals = 3600;

        ginfo->derivepp = item_no;

        /* Check to see if 'off' has been selected. */
        if ( item_no == NO_PC_TO_PP )
        {
           ginfo->showpp = 0;
        }
        else
        {
           ginfo->showpp = 1;
        }

	display_TSgraph_data( PageMgr->active_graph );

}

/* ******************************************************** */
/* Handle "Select Trace" select from Time Series Menu Bar   */
/* ******************************************************** */
void TSTracePB_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
	/* ******************************************************* */ 
	/* If modification has made but not saved then do nothing  */
	/* ******************************************************* */ 

	if (  PageMgr->Edit_count > 0 ) return;

	/* ******************************************************* */ 
	/* Disable Zoom/crosshair , clear Edit_mode and set Edit   */
	/* is active.                                              */
	/* ******************************************************* */ 

	PageMgr->zoom = 0;

	PageMgr->display_crosshairs = 0;

	PageMgr->Editmode = 0;

	PageMgr->Edit_active = EDIT_START;

	highlight_select_options();
	SetCursor( PageMgr->widget, XC_hand2);

}

/* ******************************************************* */ 
/* Add option is selected from MenuBar                     */
/* ******************************************************* */ 
void TSAddPB_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
	PageMgr->Editmode = ADD;
	SetCursor( PageMgr->widget, XC_hand2);

	highlight_select_options();
	show_active_trace_label ();

}

/* ******************************************************* */ 
/* Delet option is selected from MenuBar                   */
/* ******************************************************* */ 
void TSDeletePB_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
	PageMgr->Editmode = DELETE;
	SetCursor( PageMgr->widget, XC_hand2);

	highlight_select_options();
	show_active_trace_label ();
}

/* ******************************************************* */ 
/* Move  option is selected from MenuBar                   */
/* ******************************************************* */ 
void TSMovePB_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
	PageMgr->Editmode = MOVE;
	SetCursor( PageMgr->widget, XC_hand2);

	highlight_select_options();
	show_active_trace_label ();
}

/* ******************************************************* */
/* Set Miss option is selected from MenuBar                */
/* ******************************************************* */
void TSSetMissingPB_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
        PageMgr->Editmode = SETMISSING;
        SetCursor( PageMgr->widget, XC_hand2);
 
        highlight_select_options();
        show_active_trace_label ();
}



/* ******************************************************* */ 
/* Done  option is selected from MenuBar                   */
/* ******************************************************* */ 
void TSEditDonePB_CB (Widget w, XtPointer ptr, XtPointer cbs)
{
	extern void save_confirmation ();
	save_confirmation ( w );

}

/* ******************************************************* */ 
/* Hightlight selected Edit options in Red                 */
/* ******************************************************* */ 
void highlight_select_options()
{
	int 	ac;
	Arg	arg[5];

	static int fg, bg, fg_color, first = 1;

	if ( first )
	{
		XtVaGetValues(TSMovePB, XmNforeground, &fg, 
				 	XmNbackground, &bg, 
				 	NULL);
		fg_color =  getMyColor ( XtDisplay(TSAddPB), "Red");
		first = 0;
	}


	if ( PageMgr->Edit_active == EDIT_RESET )
	{
		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg);ac++;
		XtSetValues(TSTracePB, arg, ac);
		XtSetValues(TSAddPB,   arg, ac);
		XtSetValues(TSMovePB,  arg, ac);
                XtSetValues(TSSetMissingPB,  arg, ac);
		XtSetValues(TSDeletePB,arg, ac);
		XtSetValues(TSEditCancelPB,arg, ac);
		XtSetValues(TSEditDonePB,  arg, ac);


		DeSensitize(TSTracePB);
		DeSensitize(TSAddPB);
		DeSensitize(TSMovePB);
                DeSensitize(TSSetMissingPB);
		DeSensitize(TSDeletePB);
		DeSensitize(TSEditDonePB);
		DeSensitize(TSEditCancelPB);
		Sensitize(TSTracePB);

		PageMgr->Edit_count  = 0;
		PageMgr->Edit_active = 0;
		PageMgr->display_crosshairs = 1;
		UnsetCursor( PageMgr->widget );

		return;
	
	}

	if ( PageMgr->Edit_active == EDIT_START )
	{
		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg_color);ac++;
		XtSetValues(TSTracePB,arg, ac);
		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg);ac++;
		XtSetValues(TSAddPB,   arg, ac);
		XtSetValues(TSDeletePB,arg, ac);
		XtSetValues(TSMovePB,  arg, ac);
                XtSetValues(TSSetMissingPB,  arg, ac);
		XtSetValues(TSEditDonePB,    arg, ac);
		XtSetValues(TSEditCancelPB,  arg, ac);
		return;

	}

	if ( PageMgr->Editmode == ADD)
	{
	
		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg_color);ac++;
		XtSetValues(TSAddPB,arg, ac);

		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg);ac++;
		XtSetValues(TSTracePB, arg, ac);
		XtSetValues(TSMovePB,  arg, ac);
		XtSetValues(TSDeletePB,arg, ac);
                XtSetValues(TSSetMissingPB,arg, ac);
		return;
	
	}

	if ( PageMgr->Editmode == DELETE)
	{
	
		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg_color);ac++;
		XtSetValues(TSDeletePB,arg, ac);

		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg);ac++;
		XtSetValues(TSTracePB,arg, ac);
		XtSetValues(TSAddPB,  arg, ac);
		XtSetValues(TSMovePB, arg, ac);
                XtSetValues(TSSetMissingPB,arg, ac);
		return;
	}

	if ( PageMgr->Editmode == MOVE)
	{
	
		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg_color);ac++;
		XtSetValues(TSMovePB,arg, ac);

		ac = 0;
		XtSetArg(arg[ac], XtNbackground, bg);ac++;
		XtSetArg(arg[ac], XtNforeground, fg);ac++;
		XtSetValues(TSTracePB, arg, ac);
		XtSetValues(TSAddPB,   arg, ac);
		XtSetValues(TSDeletePB,arg, ac);
                XtSetValues(TSSetMissingPB,arg, ac);
		return;
	
	}

        if ( PageMgr->Editmode == SETMISSING)
        {
         
                ac = 0;
                XtSetArg(arg[ac], XtNbackground, bg);ac++;
                XtSetArg(arg[ac], XtNforeground, fg_color);ac++;
                XtSetValues(TSSetMissingPB,arg, ac);

                ac = 0;
                XtSetArg(arg[ac], XtNbackground, bg);ac++;
                XtSetArg(arg[ac], XtNforeground, fg);ac++;
                XtSetValues(TSTracePB,arg, ac);
                XtSetValues(TSAddPB,  arg, ac);
                XtSetValues(TSDeletePB,arg, ac);
                XtSetValues(TSMovePB, arg, ac);
                return;
        }


}

/* ******************************************************* */ 
/* Display previous page when selected from MenuBar        */
/* ******************************************************* */ 
void display_TSprev_page ()
{


	if(GroupInfo->current_page-- <= 0)
			GroupInfo->current_page=(GroupInfo->page_count-1);

	get_TSpage_data ( GroupInfo->current_page );
	display_TSpage_data( GroupInfo->current_page );


}

/* ******************************************************* */ 
/* Display next     page when selected from MenuBar        */
/* ******************************************************* */ 
void display_TSnext_page ()
{


	if(GroupInfo->current_page++ >= (GroupInfo->page_count-1))
			GroupInfo->current_page = 0;

	get_TSpage_data ( GroupInfo->current_page );
	display_TSpage_data( GroupInfo->current_page );
}


/* ******************************************************* */ 
/* Handle exposure events when window moved or resized     */
/* ******************************************************* */ 
void TSexpose_resize_CB (Widget w, XtPointer ptr, XtPointer cbs)
{

	static time_t tsec, last_t=0;


	XmDrawingAreaCallbackStruct *cbst = (XmDrawingAreaCallbackStruct *)cbs;

	if ( PageMgr->BeginTime >=  PageMgr->EndTime )
	{
		return;

	}

	time(&tsec);

	/* Only repaint if it has been more than a second */
	if ( tsec > last_t )
	{
		if ( cbst->reason == XmCR_EXPOSE ) 
		{

			update_pageDimesion (GroupInfo->current_page);

			display_TSpage_data( GroupInfo->current_page );

			last_t = tsec;

			PageMgr->active_graph = PageMgr->last_active_graph;

		}
	}

}


/* ******************************************************* */ 
/* switch grid option to on/off when selected under Options menu  */
/* on MenuBar                                              */
/* ******************************************************* */ 
void TSgrid_CB ( Widget w , XtPointer client_data , XtPointer call_data ) 
{

	XmToggleButtonCallbackStruct * toggle_data = 
		                ( XmToggleButtonCallbackStruct * ) call_data ;

	if(toggle_data->set)
		GroupInfo->grid_lines = 1;
	else
		GroupInfo->grid_lines = 0;
	

	display_TSpage_data( GroupInfo->current_page);

}

/* ******************************************************* */ 
/* Set batch scale stages option when                      */
/* selected under options menu on MenuBar                  */
/* created by guoxian zhou 05-2004                         */
/* ******************************************************* */ 
void TSBatchScaleStages_CB(Widget w, XtPointer client_data ,
                           XtPointer call_data )
{

	int i;
        int item_no = ( int ) client_data ;
	int tmpActiveGraph = PageMgr->active_graph;

	PAGE_DATA  	*pptr  = (PAGE_DATA  *) &TSpagedata[0];

	PAGE_INFO       *pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
	GRAPH_INFO      *ginfo ;

	if ( pptr->num_graphs == 1 || PageMgr->active_graph < 0)
		PageMgr->active_graph = 0;

	/* pick up the scale stages value from the active graph */
	ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];

	for(i = 0; i < pptr->num_graphs; i++)
	{
		PageMgr->active_graph = i;

		ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];

		ginfo->showcat = item_no;

		display_TSgraph_data( PageMgr->active_graph);
	}

	/* reset the active graph */
	PageMgr->active_graph = tmpActiveGraph; 
	
}

/* ************************************************************** */ 
/* Set zoom option to set/reset  when selected under Options menu */
/* on MenuBar                                                     */
/* ************************************************************** */ 
void TSZoom_CB (Widget w, XtPointer client_data , XtPointer call_data )
{
        int item_no = ( int ) client_data ;
	int status;

        XmToggleButtonCallbackStruct * pButtonStruct = 
                          ( XmToggleButtonCallbackStruct * ) call_data;

        status = pButtonStruct->set;

	if(status == 1)
	{
		if(item_no == 0)  /*For set function*/
		{
			XtUnmanageChild(TSTracesFO);
			XtUnmanageChild(TSTracesDS);

			PageMgr->option_menuUp = 0;
 			PageMgr->zoom          = 1;
			PageMgr->Editmode      = 0;
 			PageMgr->display_crosshairs = 0;  /* disable crossschairs function */

			SetCursor( PageMgr->widget, XC_hand2);
		}
		else  if(item_no == 1) /*For reset function*/
		{
			PAGE_DATA  	*pptr  = (PAGE_DATA  *) &TSpagedata[0];
			GRAPH_DATA 	*gptr  = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

			gptr->xmin = gptr->org_xmin;
			gptr->xmax = gptr->org_xmax;

			gptr->old_xmin = gptr->org_xmin;
			gptr->old_xmax = gptr->org_xmax;

			gptr->ymin = gptr->org_ymin;
			gptr->ymax = gptr->org_ymax;

			gptr->old_ymin = gptr->org_ymin;
			gptr->old_ymax = gptr->org_ymax;

			gptr->data_inc = gptr->org_data_inc;
			gptr->old_data_inc = gptr->org_data_inc;

			find_TSgraph_minmax( gptr  , PageMgr->active_graph, 0);
			display_TSgraph_data( PageMgr->active_graph );
		}
	}

}


/* ******************************************************* */ 
/* Set plot option when selected under Options menu        */
/* on MenuBar                                              */
/* ******************************************************* */ 
void TSPlot_CB (Widget w, XtPointer client_data , XtPointer call_data )
{



	GroupInfo->trace_mode = ( int ) client_data ;

	display_TSpage_data( GroupInfo->current_page );

}


/* ********************************************************* */ 
/* Handle on/off selection for showing latest forecast data  */
/* only when selected under Graph option on MenuBar          */
/* ********************************************************* */ 

void TSfcstonly_CB(Widget w, XtPointer client_data , XtPointer call_data )
{


	PAGE_INFO  	*pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
	GRAPH_INFO  *ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];

	PAGE_DATA  	*pptr  = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA 	*gptr  = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph];

	XmToggleButtonCallbackStruct * toggle_data = 
               ( XmToggleButtonCallbackStruct * ) call_data ;
	if(toggle_data->set)
		ginfo->latestfcstonly =  1;
	else
		ginfo->latestfcstonly =  0;
	
	setTraceLabel ( PageMgr->active_graph );
	
	find_TSgraph_minmax( gptr, PageMgr->active_graph, 0);

	update_pageDimesion (GroupInfo->current_page);

	display_TSpage_data( GroupInfo->current_page );

/*
	display_TSgraph_data( PageMgr->active_graph );
*/
}


/* ************************************************************** */ 
/* Handle Close option when Toggle Time Series dialog             */
/* ************************************************************** */ 
void TSTracesClose_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{


	XtUnmanageChild(TSTracesDS);

	display_TSgraph_data( PageMgr->active_graph );
	XFlush(PageMgr->display);
	PageMgr->option_menuUp = 0;

	PageMgr->display_crosshairs = 1;
	
	return;

}

/* ************************************************************** */ 
/* Handle Close option when selected from File  option on MenuBar */
/* ************************************************************** */ 
void TSClose_CB( Widget w, XtPointer ptr, XtPointer cbs)
{

	/*
	time_t 	t;
	int	n;

	TAB_INFO ptr;

	ptr.nitems = 1;

	time(&t);
	ptr.Begin_time  = (t - 48*3600);
	ptr.End_time    = (t + 38*3600);
	ptr.buf = (RussText *)malloc (sizeof(RussText) * (ptr.nitems + 1));
	for ( n = 0; n < ptr.nitems; n++)
	sprintf(ptr.buf[n],"%s %s %s %s %s","ADSO2","TD","0","RZ","Z");

	display_TSgraph( w, ptr);
	*/

	XtUnmanageChild( TimeSeriesDS );
	return;

}

/* ******************************** */ 
/* Handle mouse press event         */
/* ******************************** */ 
void TSpress_CB (Widget w, XtPointer ptr, XEvent *event)
{


	int	active_graph;

	PAGE_DATA 	*pptr  = (PAGE_DATA  *) &TSpagedata[0];
	
	if ( PageMgr->option_menuUp  && event->xbutton.button == Button1) 
	{
		XtUnmanageChild(TSTracesDS);
		return;
	}

	PageMgr->off_focus = 0;

	/* ******************************************* */
	/* check if pointer is in current_active graph */
	/* ******************************************* */

	active_graph = find_active_graph( pptr, event->xbutton.x, event->xbutton.y);

	if ( active_graph < 0 ) 
	{
		PageMgr->off_focus = 1;
		PageMgr->active_graph = PageMgr->last_active_graph;
		XBell(XtDisplay(w), 50);
		return;
	}
	else
	{
		PageMgr->active_graph = PageMgr->last_active_graph = active_graph;

		show_active_graph_label( active_graph);

	}


	pptr  = (PAGE_DATA  *) &TSpagedata[0];
	
   	if (event->xbutton.button == Button1) 
	{
                /* Store the number of the button that was pressed into 
                   the page manager structure. */ 

   		if ( PageMgr->display_crosshairs ) 
   		{

			PageMgr->zoom = 0; 

			PageMgr->last_x = event->xbutton.x;
			PageMgr->last_y = event->xbutton.y;

			draw_crosshairs ( );

			return;
   		}

   		if ( PageMgr->zoom ) 
   		{

			/* **************************************** */
			/* store first xy-coordinate on first click */
			/* **************************************** */

			PageMgr->x1 = PageMgr->start_x = PageMgr->last_x = event->xbutton.x;
			PageMgr->y1 = PageMgr->start_y = PageMgr->last_y = event->xbutton.y;

			draw_rubber_band();

			return;
   		}

	}

	/* ******************************** */
	/* Check if option menu is selected */
	/* ******************************** */

   	if (event->xbutton.button == Button2) 
   	{
                /* Store the number of the button that was pressed into 
                   the page manager structure. */ 

		active_graph = find_active_graph( pptr, event->xbutton.x, 
                                                  event->xbutton.y);

		if( PageMgr->active_graph < 0) 
		{
			PageMgr->off_focus    = 1;
			PageMgr->active_graph = PageMgr->last_active_graph;
			return;
			
		} 
		else
		{
			PageMgr->active_graph = active_graph;
		}

		XtManageChild(TSTracesFO);
		XtManageChild(TSTracesDS);

		setTraceLabel( PageMgr->active_graph );

		/* ************************************************ */
		/* Move the toggle window to the top of its parent  */
		/* window whenever user presses option mouse key.   */
		/* added by guoxian zhou 12-2004                    */
		/* ************************************************ */

/*
		XtMoveWidget(TSTracesDS, event->xbutton.x+280, event->xbutton.y);
*/
		XtMoveWidget(TSTracesDS, event->xbutton.x+20, event->xbutton.y);

		XRaiseWindow(XtDisplay(TSTracesDS), XtWindow(TSTracesDS));

		PageMgr->last_active_graph = PageMgr->active_graph;

		PageMgr->display_crosshairs = 0;
		PageMgr->option_menuUp = 1;

   	}


	return;
}


/* ******************************** */ 
/* Handle mouse release event         */
/* ******************************** */ 
void TSrelease_CB (Widget w, XtPointer ptr, XEvent *event)
{

	
	PAGE_DATA	*pptr = NULL ;
	GRAPH_DATA	*gptr = NULL ;
	
   	if ( PageMgr->off_focus == 1 || event->xbutton.button != Button1)
		return;

	if ( PageMgr->option_menuUp )
	{
		display_TSpage_data ( GroupInfo->current_page );
		PageMgr->display_crosshairs = 1;

		PageMgr->option_menuUp = 0;
		return;

	}

	/* ******************* */
	/* Erase the XOR image */
	/* ******************* */

   	if ( PageMgr->zoom )
	{

		pptr = (PAGE_DATA *)&TSpagedata[0];
		
		/* ********************************************* */
		/* Redraw rubber_band to erase the previous  one */
		/* ********************************************* */

		draw_rubber_band( );

		/* ************************** */
		/* store second xy-coordinate */
		/* ************************** */

		PageMgr->x2 = PageMgr->last_x = event->xbutton.x;
		PageMgr->y2 = PageMgr->last_y = event->xbutton.y;

		/* *************************************** */
		/* To avoid click and release at same time */
		/* *************************************** */

		check_points(&PageMgr->x1,&PageMgr->y1,&PageMgr->x2,&PageMgr->y2);
		if ( (PageMgr->x2 - PageMgr->x1) >= 10  && (PageMgr->y2 - PageMgr->y1) >= 10)
		{
			update_graph (PageMgr->x1,PageMgr->y1,PageMgr->x2,PageMgr->y2);
		}

		PageMgr->zoom = 0; 
		UnsetCursor( PageMgr->widget );

	}

   	if ( PageMgr->display_crosshairs )
	{
		pptr = (PAGE_DATA  *) &TSpagedata[0];
		gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph]; 
	

		if ( PageMgr->option_menuUp )
		{
			PageMgr->option_menuUp = 0;
			return;
		}

		/* ********************************** */
		/* Erase crosshairs from privous draw */
		/* ********************************** */

		draw_crosshairs ( );

		/* ************************ */
		/* Clear display value area */
		/* ************************ */
/*
		XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, gptr->x+55, gptr->y-55,290,15);
*/
		if (  GroupInfo->GroupSelected == GROUP )
			XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, gptr->x+5, gptr->y-45,display_width,15);
		else
			XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, gptr->x+5, gptr->y-65,display_width,35);
		

	}

	if ( PageMgr->zoom == 0);
		PageMgr->display_crosshairs = 1;
}

/* ******************************** */
/* Handle mouse drags event         */
/* ******************************** */
void TSdrag_CB (Widget w, XtPointer ptr, XEvent *event)
{

	int	in_bounds;


	PAGE_DATA  *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->active_graph]; 

   	if ( PageMgr->off_focus != 0 )
		return;

	/* ************************************************ */
	/* Check if mouse is dragging off from active graph */
	/* ************************************************ */

	if ( PageMgr->zoom == 0 && PageMgr->display_crosshairs == 0) return;

	in_bounds = check_graph_bound( gptr, event->xbutton.x, event->xbutton.y);

   	if ( PageMgr->zoom )
	{

		pptr = (PAGE_DATA *) &TSpagedata[0];

		draw_rubber_band( );
		PageMgr->last_x = event->xbutton.x;
		PageMgr->last_y = event->xbutton.y;
		draw_rubber_band(  );

	}

   	if ( PageMgr->display_crosshairs )
	{

		/* ************************************ */
		/* Keep crosshairs in graph's boundary  */
		/* ************************************ */

		if ( ! in_bounds  ) return;

		/* ********************************** */
		/* Erase crosshairs from privous draw */
		/* ********************************** */

		draw_crosshairs ( );

	        search_and_display_trace( event->xbutton.x, event->xbutton.y, 0);

		PageMgr->last_x = event->xbutton.x;
		PageMgr->last_y = event->xbutton.y;

		/* ******************** */
		/* Draw new cross chair */
		/* ******************** */
		draw_crosshairs ( );

	}

}



/* ***************************** */ 
/* Handle keyboard event         */
/* ***************************** */ 
void TSkey_CB (Widget w, XtPointer ptr, XEvent *event)
{
	KeyCode code;
	
	code = event->xkey.keycode;
	
	if ( code == keycode[0] || code == keycode[2] )
		display_TSprev_page();
		
	if ( code == keycode[1] || code == keycode[3] )
		display_TSnext_page();

	return;

}



/* ******************************************************* */ 
/* Handle Save option selected from File option on MenuBar */
/* ******************************************************* */ 
void    TSsave_CB(Widget w, XtPointer ptr, XtPointer cbs)
{
	Arg          args[10];
	int          ac, gad_token_len=0, gad_value_len=0, rcfad=0;
	char   	     buf[200],
		     gad_value[128];
	XmString     str;


	if(! TS_fileSB )
	{
		TS_fileSB = XmCreateFileSelectionDialog(w, "fileSB", NULL, 0);

		XtAddCallback(TS_fileSB, XmNcancelCallback, TSclosefile_CB, NULL);

		XtAddCallback(TS_fileSB, XmNokCallback,     TSselectfile_CB, NULL);

		XtUnmanageChild(XmSelectionBoxGetChild(TS_fileSB, XmDIALOG_HELP_BUTTON));
	}


	gad_token_len  =  strlen ("whfs_image_dir");
	rcfad = get_apps_defaults("whfs_image_dir", &gad_token_len, gad_value, &gad_value_len);
	sprintf(buf,"%s/*.gif",gad_value);

	str = XmStringCreateSimple(buf);

	ac = 0;
	XtSetArg(args[ac], XmNpattern, str); ac++;
	XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
	XtSetValues(TS_fileSB, args, ac);
 	XtFree((char*) str);

	SetTitle(TS_fileSB,"File Selection");
	if(! XtIsManaged(TS_fileSB))
		XtManageChild(TS_fileSB);
	 return;



}

/* ******************************************************** */ 
/* Handle Print option selected from File option on MenuBar */
/* ******************************************************** */ 
void    TSprint_noinverse_CB(Widget w, XtPointer ptr, XtPointer cbs)
{
	char 		buf[200];
	char            lpr_print[128];
	char            whfs_bin_dir[128];
	char            cmd_str[200];
	int             gad_token_len=0,gad_value_len=0;
        int             inverse = 0 ; /* Print the image in "normal" color. */

        /* Set the lpr_print and whfs_bin_dir "C" strings to contain
           null characters just to be safe. */
        memset ( lpr_print , '\0' , 128 ) ;
        memset ( whfs_bin_dir , '\0' , 128 ) ;

        /*token for the print command*/
	gad_token_len = strlen("whfs_printcommand");
        get_apps_defaults( "whfs_printcommand", &gad_token_len,lpr_print, 
                           &gad_value_len );

        if(strlen(lpr_print) > 0)
        {
          strcpy(cmd_str,lpr_print);
        }
        else
        {
          strcpy(cmd_str, "lp");
        }
 
        /* token for the whfs bin directory. */
	gad_token_len = strlen ( "whfs_bin_dir" ) ;
        get_apps_defaults( "whfs_bin_dir" , & gad_token_len , whfs_bin_dir ,
                           & gad_value_len ) ;
     
        /* Test for the success of the token retrieval. */
        if ( strlen ( whfs_bin_dir ) == 0 )
        {
           strcpy ( whfs_bin_dir , "." ) ;  
        }
        
        /* This section of code was updated by Bryon Lawrence on
           September 26, 2001.  The "print_image" script is now called,
           and this script takes into account the differences between
           the HP-UX and Linux operating systems when it comes to
           capturing and processing images. */
        sprintf ( buf , "%s/print_image \"%s\" \"%s\" %d " , whfs_bin_dir ,
                  "Time Series Display" , cmd_str , inverse ) ;

	SetCursor( PageMgr->widget, XC_watch);
	system(buf);
	UnsetCursor( PageMgr->widget );

}

/* **************************************************************** */ 
/* Handle Print Inverse option selected from File option on MenuBar */
/* **************************************************************** */ 
void    TSprint_inverse_CB(Widget w, XtPointer ptr, XtPointer cbs)
{
	char 		buf[200];
	char            lpr_print[128];
	char            cmd_str[200];
        char            whfs_bin_dir[128] ; 
	int             gad_token_len=0,gad_value_len=0;
        int             inverse = 1 ; /* Print the image in inverse color. */
	
        memset ( lpr_print , '\0' , 128 ) ;
        memset ( whfs_bin_dir , '\0' , 128 ) ;

        /*token for the print command*/
	gad_token_len = strlen("whfs_printcommand");
        get_apps_defaults("whfs_printcommand" , &gad_token_len , lpr_print , 
                          &gad_value_len);

        if(strlen(lpr_print) > 0)
        {
          strcpy(cmd_str,lpr_print);
        }
        else
        {
          strcpy(cmd_str, "lp");
        }

        /* token for the whfs bin directory. */
	gad_token_len = strlen ( "whfs_bin_dir" ) ;
        get_apps_defaults( "whfs_bin_dir" , & gad_token_len , whfs_bin_dir ,
                           & gad_value_len ) ;
     
        /* Test for the success of the token retrieval. */
        if ( strlen ( whfs_bin_dir ) == 0 )
        {
           strcpy ( whfs_bin_dir , "." ) ;  
        }
        
        /* This section of code was updated by Bryon Lawrence on
           September 26, 2001.  The "print_image" script is now called,
           and this script takes into account the differences between
           the HP-UX and Linux operating systems when it comes to
           capturing and processing images. */
        sprintf ( buf , "%s/print_image \"%s\" \"%s\" %d " , whfs_bin_dir ,
                  "Time Series Display" , cmd_str , inverse ) ;

	SetCursor( PageMgr->widget, XC_watch);
	system(buf);
	UnsetCursor( PageMgr->widget );

}

/* **************************************** */ 
/* Handle Save a selected file in the .gif  */
/* **************************************** */ 
void	TSselectfile_CB(Widget w, XtPointer ptr, XtPointer cbs) 
{

        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
	char            *pathName;
	char 		buf[200];
        char            whfs_bin_dir[128];
	int             gad_token_len=0,gad_value_len=0;

	XmFileSelectionBoxCallbackStruct *fscbs = 
                                     (XmFileSelectionBoxCallbackStruct *)cbs;

	XmStringGetLtoR(fscbs->value, charset, &pathName);
	SetCursor(w, XC_watch);
        memset ( whfs_bin_dir , '\0' , 128 ) ;

        /* token for the whfs bin directory. */
	gad_token_len = strlen ( "whfs_bin_dir" ) ;
        get_apps_defaults( "whfs_bin_dir" , & gad_token_len , whfs_bin_dir ,
                           & gad_value_len ) ;
     
        /* Test for the success of the token retrieval. */
        if ( strlen ( whfs_bin_dir ) == 0 )
        {
           strcpy ( whfs_bin_dir , "." ) ;  
        }
        
        /* This section of code was updated by Bryon Lawrence on
           September 27, 2001.  The "save_image" script is now called,
           and this script takes into account the differences between
           the HP-UX and Linux operating systems when it comes to
           capturing and processing images. */
        sprintf ( buf , "%s/save_image \"%s\" \"%s\" " , whfs_bin_dir ,
                  "Time Series Display" , pathName ) ;
	system(buf);
	XtFree(pathName);
   	XtUnmanageChild(TS_fileSB);
	UnsetCursor( w );

	return;
}

/* ************************************************ */ 
/* Handle Close Time Series Display when selected   */
/* under File option from MenuBar                   */
/* ************************************************ */ 
void    TSclosefile_CB(Widget w, XtPointer ptr, XtPointer cbs)
{

	XtUnmanageChild(TS_fileSB);
	return;

}


/* ************************************************ */ 
/* Create a dialog for Toggle traces in Time Series */
/* When second mouse button activated               */
/* ************************************************ */ 
void create_TSTracesDS (Widget parent)
{
	Arg al[64];
	register int ac = 0;
	int	n;
	XmString xmstring;
	Widget rowcol = (Widget)NULL;
	Widget form1  = (Widget)NULL;

	ac=0;
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
 	XtSetArg(al[ac], XmNresizePolicy, XmRESIZE_ANY); ac++;
	XtSetArg(al[ac], XmNtitle, "Toggle Time Series"); ac++;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	TSTracesDS = XmCreateDialogShell ( parent, "TSTracesDS", al,ac);


	ac = 0;
	XtSetArg(al[ac],XmNresizePolicy, XmRESIZE_ANY);ac++;
	XtSetArg(al[ac],XmNautoUnmanage, FALSE);ac++;
	TSTracesFO = XmCreateForm ( TSTracesDS, "form", al, ac );

	ac=0;
	XtSetArg(al[ac],XmNnumColumns, 2); ac++;
	XtSetArg(al[ac],XmNorientation, XmVERTICAL);ac++;
	XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN); ac++;
	rowcol = XmCreateRowColumn ( TSTracesFO, "rowcol", al, ac );

	ac=0;
	XtSetArg(al[ac],XmNindicatorType, XmONE_OF_MANY);ac++;
	for ( n = 0 ; n < 40; n++)
		TSTracesW[n]  =  XmCreateToggleButton ( rowcol,
                                                        " ", 
                                                        al, ac );

	ac=0;
	XtSetArg(al[ac],XmNresizePolicy, XmRESIZE_ANY); ac++;
	XtSetArg(al[ac],XmNautoUnmanage, FALSE); ac++;
	form1 = XmCreateForm ( TSTracesFO, "form1", al, ac );

	ac = 0;
	xmstring = XmStringCreateLocalized("Close");
	XtSetArg(al[ac], XmNresizePolicy, XmRESIZE_ANY); ac++;
	XtSetArg(al[ac], XmNx, 50); ac++;
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	TSTracesClosePB  = XmCreatePushButton (form1, "TSTracesClosePB", al, ac );
	XmStringFree(xmstring);

	ac=0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 10); ac++;
	XtSetValues ( rowcol,al, ac );
	
	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, rowcol); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 10); ac++;
	XtSetValues ( form1,al, ac );

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetValues ( TSTracesClosePB,al, ac );


	XtManageChild(TSTracesClosePB);
	XtManageChild(form1);
	XtManageChild(rowcol);
}

/* ***************************************** */
/* Initialize or assign colors to traces     */
/* ***************************************** */
void init_TScolors()
{
	int n;

	for ( n = 0; n< MAX_COLORS; n++)
	{
		TScolors[n].fg_color = getMyColor ( PageMgr->display, TScolors[n].colorname);

	}

}

/*****************************************************************************
 *
 * Routine: set_graph
 *
 * Description: this routine is callback for Zoom toggle buttons.  It sets
 * the status of the Zoom toggle buttons.
 *
 ****************************************************************************/

void set_graph(Widget wid , XtPointer ptr, XtPointer call_data)
{

   int option = ( int ) ptr ;

   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) call_data ;

   if ( state->set )
   {
     if ( option == 0 )
     {
        XmToggleButtonSetState ( TSResetTB , False , False ) ;
        TSZoom_CB(TSSetTB, (XtPointer) 0, (XtPointer) state);
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSSetTB , False , False ) ;
        TSZoom_CB(TSResetTB, (XtPointer) 1, (XtPointer) state);
     }
   }
   else  
   {
     if ( option == 0 )
     {
        XmToggleButtonSetState (TSSetTB , True ,True ) ;
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSResetTB , True , True ) ;
     }
   }
}
/*****************************************************************************
 *
 * Routine: set_graph_show
 *
 * Description: this routine is callback for Show PC as 1Hr toggle buttons.  
 * It sets the status of the toggle buttons.
 *
 ****************************************************************************/

void set_graph_show(Widget wid , XtPointer ptr, XtPointer call_data)
{

   int option = ( int ) ptr ;

   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) call_data ;

   if ( state->set ) 
   { 
     if ( option == 0 )
     { 
        XmToggleButtonSetState ( TSInterpolateTB , False , False ) ;
        XmToggleButtonSetState (TSAssignTB , False , False ) ;
        TSpcAspp_CB(TSOffTB, (XtPointer) 0, (XtPointer) state);
     } 
     else if ( option == 1 )
     { 
        XmToggleButtonSetState (TSOffTB , False , False ) ;
        XmToggleButtonSetState (TSAssignTB , False , False ) ;
        TSpcAspp_CB(TSInterpolateTB, (XtPointer) 1, (XtPointer) state);
     } 
     else if ( option == 2 ) 
     { 
        XmToggleButtonSetState (TSOffTB , False , False ) ;
        XmToggleButtonSetState (TSInterpolateTB , False , False ) ;
        TSpcAspp_CB(TSAssignTB , (XtPointer) 2, (XtPointer) state ) ;
     } 
   } 
   else   
   { 
     XmToggleButtonSetState ( TSInterpolateTB ,  False , False ) ;
     if ( option == 0 )
     { 
        XmToggleButtonSetState (TSOffTB , True ,True ) ;
     } 
     else if ( option == 1 )
     { 
        XmToggleButtonSetState (TSInterpolateTB , True , True ) ;
     } 
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSAssignTB , True , True ) ;
     }

   } 

}
/*****************************************************************************
 *
 * Routine: set_graph_scale
 *
 * Description: this routine is callback for Show PC as 1Hr toggle buttons.  
 * It sets the status of the toggle buttons.
 *
 ****************************************************************************/

void set_graph_scale(Widget wid , XtPointer ptr, XtPointer call_data)
{

   int option = ( int ) ptr ;

   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) call_data ;

   if ( state->set )
   {
     if ( option == 0 )
     {
        XmToggleButtonSetState ( TSScaleDataShowCategTB , False , False ) ;
        XmToggleButtonSetState (TSScaleDataCategoriesTB , False , False ) ;
        TSFloodCat_CB(TSScaleDataOnlyTB, (XtPointer) 0, (XtPointer) state);
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSScaleDataOnlyTB , False , False ) ;
        XmToggleButtonSetState (TSScaleDataCategoriesTB , False , False ) ;
        TSFloodCat_CB(TSScaleDataShowCategTB, (XtPointer) 1, (XtPointer) state);
     }
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSScaleDataOnlyTB , False , False ) ;
        XmToggleButtonSetState ( TSScaleDataShowCategTB , False , False ) ;
        TSFloodCat_CB (TSScaleDataCategoriesTB ,(XtPointer) 2, (XtPointer) state  ) ;
     }
   }
   else
   {
     XmToggleButtonSetState ( TSScaleDataShowCategTB ,  False , False ) ;
     if ( option == 0 )
     {
        XmToggleButtonSetState (TSScaleDataOnlyTB, True ,True ) ;
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSScaleDataShowCategTB , True , True ) ;
     }
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSScaleDataCategoriesTB , True , True ) ;
     }
   }

}
/*****************************************************************************
 *
 * Routine: set_Options
 *
 * Description: this routine is callback for Plot toggle buttons.  
 * It sets the status of the toggle buttons.
 *
 ****************************************************************************/

void set_plot(Widget wid , XtPointer ptr, XtPointer call_data)
{

   int option = ( int ) ptr ;

   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) call_data ;

   if ( state->set )
   {
     if ( option == 0 )
     {
        XmToggleButtonSetState (TSLinesTB  , False , False ) ;
        XmToggleButtonSetState (TSBothTB , False , False ) ;
        TSPlot_CB(TSPointsTB, (XtPointer) 0, (XtPointer) state);
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSPointsTB , False , False ) ;
        XmToggleButtonSetState (TSBothTB , False , False ) ;
        TSPlot_CB(TSLinesTB, (XtPointer) 1 , (XtPointer) state);
     }
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSPointsTB , False , False ) ;
        XmToggleButtonSetState (TSLinesTB , False , False ) ;
        TSPlot_CB(TSBothTB , (XtPointer) 2 , (XtPointer) state  ) ;
     }
   }
   else
   {
     XmToggleButtonSetState ( TSBothTB,  False , False ) ;
     if ( option == 0 )
     {
        XmToggleButtonSetState (TSPointsTB , True ,True ) ;
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState ( TSLinesTB, True , True ) ;
     }
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSBothTB , True , True ) ;
     }
   }

}
/*****************************************************************************
 *
 * Routine: set_batch_scale
 *
 * Description: this routine is callback for Batch Scale Stages toggle buttons.  
 * It sets the status of the toggle buttons.
 *
 ****************************************************************************/
 
void set_batch_scale(Widget wid , XtPointer ptr, XtPointer call_data)
{
   int option = ( int ) ptr ;

   XmToggleButtonCallbackStruct * state =
   ( XmToggleButtonCallbackStruct * ) call_data ;

   if ( state->set )
   {
     if ( option == 0 )
     {
        XmToggleButtonSetState (TSBatchShowCategTB  , False , False ) ;
        XmToggleButtonSetState (TSBatchCategoriesTB , False , False ) ;
        TSFloodCat_CB(TSBatchDataOnlyTB, (XtPointer) 0, (XtPointer) state);
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSBatchDataOnlyTB , False , False ) ;
        XmToggleButtonSetState (TSBatchCategoriesTB , False , False ) ;
        TSFloodCat_CB(TSBatchShowCategTB, (XtPointer) 1, (XtPointer) state);
     }
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSBatchDataOnlyTB , False , False ) ;
        XmToggleButtonSetState (TSBatchShowCategTB , False , False ) ;
        TSFloodCat_CB (TSBatchCategoriesTB ,(XtPointer) 2, (XtPointer) state  ) ;
     }
   }
   else
   {
     XmToggleButtonSetState ( TSBatchShowCategTB,  False , False ) ;
     if ( option == 0 )
     {
        XmToggleButtonSetState (TSBatchDataOnlyTB, True ,True ) ;
     }
     else if ( option == 1 )
     {
        XmToggleButtonSetState (TSBatchShowCategTB , True , True ) ;
     }
     else if ( option == 2 )
     {
        XmToggleButtonSetState (TSBatchCategoriesTB , True , True ) ;
     }
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/TimeSeriesUtil/RCS/TimeSeries_show.c,v $";
 static char rcs_id2[] = "$Id: TimeSeries_show.c,v 1.15 2007/08/17 13:58:06 loubna Exp $";}
/*  ===================================================  */

}

/* ******************************************** */
/* End of  File:           TimeSeries_show.c    */
/* ******************************************** */


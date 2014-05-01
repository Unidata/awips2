/* ********************************************* */
/*	File:           TSedit.c                 */
/*	Date:           September 1999           */
/*	Author:         Sung Vo                  */
/*	Purpose:        Provide support Time     */
/*      Series Display plot's utility functions. */
/* ********************************************* */

#define  FOUND 		1
#define  NOT_FOUND 	0
#define  ADD_LEFT	1
#define  ADD_MIDDLE	2
#define  ADD_RIGHT 	3

/* ***************************************** */
/* ************* Header files ************** */
/* ***************************************** */

#include "CurPC.h"
#include "CurPP.h"
#include "load_maxfcst.h"
#include "TimeSeries_show.h"
#include "TSControl_show.h"
#include "TSedit.h"

/* ***************************************** */
/* Variables are globally defined to this    */
/* Module only                               */
/* ***************************************** */
int	click_valid = 1;

time_t	obstime,
	validtime,
	basistime;

float	oldValue;

time_t	ppdur = (-1);

/* ***************************************** */
/* structure need to hold move information * */
/* ***************************************** */
struct 
{
	int 	valid;

	int	x, y,
	    	x1,y1, 
		x2, y2;

	int	left_limit, 
		right_limit,
		left_index,
		right_index;

	int	trace_index,
		add_flag,
		num_legs;

	time_t	xvalue;

} move_info;

extern  COLORS  TScolors[MAX_COLORS];

/* ********************************************************** */
/* handle press event when Edit mode active                   */
/* ********************************************************** */
void TSpressEdit_CB (Widget w, XtPointer ptr, XEvent *event)
{
	extern GROUP_INFO     *GroupInfo;
	extern PAGE_MGR       *PageMgr;

	extern  PAGE_DATA  TSpagedata[MAX_PAGE_DATA];
	extern  PAGE_INFO  TSpageinfo[MAX_PAGE_INFO];

	PAGE_INFO       *pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
	GRAPH_INFO      *ginfo = (GRAPH_INFO *) &pinfo->graph[PageMgr->active_graph];


	PAGE_DATA  *pptr = (PAGE_DATA  *) &TSpagedata[0];

	GRAPH_DATA	*gptr;

	int 	num_traces_on,
		active_graph,
		n;

	if(event->xbutton.button != Button1 || PageMgr->zoom == 1 || PageMgr->Edit_active == 0) return;

	click_valid = 1;

	if ( PageMgr->Edit_count > 0)
	{

		active_graph = find_active_graph( pptr, event->xbutton.x, event->xbutton.y);

		if ( active_graph < 0 )
		{
			click_valid  = 0;
			XBell(XtDisplay(w), 50);
			return;

		}
		else if ( active_graph !=  PageMgr->edit_graph )
		{
			click_valid = 0;
			show_active_trace_label ();
			XBell(XtDisplay(w), 50);
			return;
		}

	}

	if ( PageMgr->Edit_active == EDIT_START )
	{

		active_graph = find_active_graph( pptr, event->xbutton.x, event->xbutton.y);
		if ( active_graph < 0)
		{
			click_valid  = 0;
			XBell(XtDisplay(w), 50);
			return;
		}

		/* *************************************** */
		/* Active graph and active trace are found */
		/* *************************************** */

		PageMgr->edit_graph = active_graph;
		PageMgr->edit_trace = find_edit_trace ( event->xbutton.x, event->xbutton.y );

		PageMgr->Edit_count  = 0;
		PageMgr->Edit_active = EDIT_ACTIVE;

		GroupInfo->trace_mode = 2;

		gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

		ppdur = conv_ppdur2min ( gptr, PageMgr->edit_trace);

		ginfo->latestfcstonly = 2; /* don't show either latest/all forcast */

		display_TSgraph_data( PageMgr->edit_graph );
		show_active_trace_label ();
		Sentitize_edit_options();
		reset_fg_button( TSTracePB );

		/* ***************************************** */
		/* Keep track traces that are on during edit */
		/* ***************************************** */
		for ( n = 0; n < gptr->num_traces; n++)
		{
			PageMgr->traces_on_edit[n] = 0;

			if ( gptr->trace_on[n] )
				PageMgr->traces_on_edit[n] = 1;

		}

		return;
	}


	/* ***************************************** */
	/* Check if one or more traces are on        */
	/* ***************************************** */
	if ( PageMgr->Editmode == ADD || PageMgr->Editmode == DELETE 
             || PageMgr->Editmode == MOVE  || PageMgr->Editmode == SETMISSING) 
	{

		PageMgr->zoom = 0;
		PageMgr->display_crosshairs = 0;

		num_traces_on = check4valid_select();

		if ( num_traces_on == 0)
		{
			TSErrorDialog(w,"Error: All traces are turned off...");
			return;

		}

	}

	/* ***************************************** */
	/* Trace is in add editing mode              */
	/* ***************************************** */
	if( PageMgr->Editmode == ADD)
	{

		move_info.valid = TSadd_checkEdit( event->xbutton.x, event->xbutton.y);

		if ( ! move_info.valid )
				return;

		if ( move_info.add_flag == ADD_MIDDLE)
			find_TSinsert_point (  event->xbutton.x, event->xbutton.y );

		move_info.x = event->xbutton.x;
		move_info.y = event->xbutton.y;
		draw_add( );
	}

	/* ***************************************** */
	/* Trace is in move editing mode             */
	/* ***************************************** */
	if( PageMgr->Editmode == MOVE)
	{
		move_info.valid = find_TSmove_index ( event->xbutton.x, event->xbutton.y);

		if ( move_info.valid )
		{
			move_info.y = event->xbutton.y;
			draw_move( );
		}

	}

	/* *********************************************** */
	/* Trace is in delete or set missing editing mode  */
	/* *********************************************** */
	if( PageMgr->Editmode == DELETE || PageMgr->Editmode == SETMISSING)
	{
		move_info.valid = 1;
		move_info.x1 = move_info.x2 = event->xbutton.x;
		move_info.y1 = move_info.y2 = event->xbutton.y;
		draw_box();
	}

}

/* ************************************************************ */
/* handle release events during editing mode  - release event   */
/* here is to share with release event from zoom so only one    */
/* is allowed at the time.a                                     */
/* ************************************************************ */
void TSreleaseEdit_CB (Widget w, XtPointer ptr, XEvent *event)
{
	extern PAGE_MGR         *PageMgr;

        /* ************************************************************ */
        /* Do nothing if zoom on or mouse button is not active  or      */
	/* Editing mode is turned off for some reasons                  */
        /* ************************************************************ */
	if ( event->xbutton.button != Button1 || PageMgr->zoom == 1 || PageMgr->Edit_active == 0) return;

	if ( click_valid == 0) return;

        /* *************************************************  */
        /* Check for ADD/MOVE/DELETE/SETMISSING are valid     */
        /* ************************************************  */

	if( PageMgr->Editmode == ADD && move_info.valid == 1)
	{
		draw_add();
		update_TSadd_data(); 
		display_TSgraph_data( PageMgr->edit_graph );
		PageMgr->Edit_count++;
		DeSensitize(TSTracePB);
		show_active_trace_label ();
	}

	if( PageMgr->Editmode == MOVE && move_info.valid == 1)
	{
		draw_move();
		update_TSmove_data();
		display_TSgraph_data( PageMgr->edit_graph );
		PageMgr->Edit_count++;
		DeSensitize(TSTracePB);
		show_active_trace_label ();
	}

	if( PageMgr->Editmode == DELETE && move_info.valid == 1)
	{
		draw_box();
		update_TSdelete_data();
		display_TSgraph_data( PageMgr->edit_graph );
		PageMgr->Edit_count++;
		DeSensitize(TSTracePB);
		show_active_trace_label ();
	}

        if( PageMgr->Editmode == SETMISSING && move_info.valid == 1)
        {
                draw_box();
                update_TSMissing_data();
                display_TSgraph_data( PageMgr->edit_graph );
                PageMgr->Edit_count++;
                DeSensitize(TSTracePB);
                show_active_trace_label ();
        }


	/* **************************************************** */
	/* Show graph active label and store graph's dimensions */
	/* **************************************************** */

	show_active_graph_label( PageMgr->edit_graph );
}

/* ********************************************************** */
/* handle drags event during Editing mode - this event is     */
/* also shared with drags event from crosshairs event         */
/* ********************************************************** */
void TSdragEdit_CB (Widget w, XtPointer ptr, XEvent *event)
{

	extern PAGE_MGR         *PageMgr;

	int	in_bounds;
	extern  PAGE_DATA  TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA  *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];


        /* ************************************************************ */
        /* Do nothing if zoom on or mouse button is not active  or      */
	/* Editing mode is turned off for some reasons                  */
        /* ************************************************************ */
	if ( PageMgr->zoom == 1 || PageMgr->Edit_active == 0) return;

	if ( click_valid == 0) return;

	/* *********************************************** */
	/* Check for insert postion - Left Right or Middle */
	/* *********************************************** */
	if( PageMgr->Editmode == ADD && move_info.valid == 1)
	{
		in_bounds = check_graph_bound( gptr, event->xbutton.x, event->xbutton.y);

		if ( ! in_bounds  ) 
				return;

		if ( move_info.add_flag == ADD_LEFT  &&  event->xbutton.x >=  move_info.left_limit)
				return;
		if ( move_info.add_flag == ADD_RIGHT &&  event->xbutton.x <=  move_info.right_limit)
				return;

		if ( move_info.add_flag == ADD_MIDDLE  &&  (event->xbutton.x <  move_info.left_limit ||
		     event->xbutton.x >  move_info.right_limit))
				return;
		draw_add();
		move_info.x =  event->xbutton.x; 
		move_info.y =  event->xbutton.y; 
		draw_add();
		search_and_display_trace( move_info.x, move_info.y, 0);

	}

	/* *********************************************** */
	/* Check for valid move during aditing             */
	/* *********************************************** */
	if( PageMgr->Editmode == MOVE && move_info.valid == 1)
	{
		in_bounds = check_graph_bound( gptr, event->xbutton.x, event->xbutton.y);

		if ( ! in_bounds  ) 
				return;
		draw_move();
		move_info.y =  event->xbutton.y; 
		draw_move();
		search_and_display_trace( move_info.x, move_info.y, move_info.xvalue);
	}

	/* ***************************************************** */
	/* Check for valid move during deleting or set missing   */
	/* ***************************************************** */
	if(( PageMgr->Editmode == DELETE ||PageMgr->Editmode == SETMISSING) 
		&& move_info.valid == 1)
	{
		in_bounds = check_graph_bound( gptr, event->xbutton.x, event->xbutton.y);

		if ( ! in_bounds  ) 
				return;
		draw_box();
		move_info.x2 =  event->xbutton.x; 
		move_info.y2 =  event->xbutton.y; 
		draw_box();
	}

}

/* ********************************************** */
/* find adjacent index of the point being moved   */
/* ********************************************** */
int find_TSmove_index ( int x, int y )
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	n,
		xpix,
		ypix,
		valid_graph, 
		left_index,
		right_index,
		active_trace,
		npts;

	TRACE_DATA   *tptr;

	valid_graph = 0;

	for (n = 0; n < gptr->num_traces; n++)
		 if ( gptr->trace_valid[n] ) valid_graph = 1;

	if ( !  valid_graph )
			return NOT_FOUND ;

	active_trace = PageMgr->edit_trace;

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];

	npts   =  tptr->npts;

	for ( n = 0; n < npts; n++)
	{
		if( tptr->TSdata[n].mode == DELETE || tptr->TSdata[n].mode == SETMISSING) continue;

		if (tptr->TSdata[n].x <  gptr->xmin || tptr->TSdata[n].x >  gptr->xmax ||
		tptr->TSdata[n].y < gptr->ymin || tptr->TSdata[n].y > gptr->ymax) continue;

		xpix = x2pixel ( gptr , tptr->TSdata[n].x);
		ypix = y2pixel ( gptr , tptr->TSdata[n].y);


		if ( (xpix <= (x+5) && xpix >= (x-5)) && (ypix <= (y+5) && ypix >= (y-5)) )
		{
			move_info.trace_index = n;
			move_info.x           = xpix; 
			move_info.xvalue      = tptr->TSdata[n].x;

			if ( n == 0) 
			{
				right_index  = find_right_index ( tptr , n);
				move_info.x2 = x2pixel ( gptr , tptr->TSdata[right_index].x);
				move_info.y2 = y2pixel ( gptr , tptr->TSdata[right_index].y);
				move_info.num_legs = 1;

				return FOUND ;
					
			}
			else if ( n == (npts - 1))
			{

				left_index = find_left_index ( tptr , n);
				move_info.x2 = x2pixel ( gptr , tptr->TSdata[left_index].x);
				move_info.y2 = y2pixel ( gptr , tptr->TSdata[left_index].y);
				move_info.num_legs = 1;

				return FOUND ;

			}
			else
			{

				left_index  = find_left_index  ( tptr , n);
				right_index = find_right_index ( tptr , n);

				move_info.x1 = x2pixel ( gptr , tptr->TSdata[left_index].x);
				move_info.y1 = y2pixel ( gptr , tptr->TSdata[left_index].y);
				move_info.x2 = x2pixel ( gptr , tptr->TSdata[right_index].x);
				move_info.y2 = y2pixel ( gptr , tptr->TSdata[right_index].y);
				move_info.num_legs = 2;

				/* **************************************************** */
				/* Check if one leg is off graph then draw only one leg */
				/* **************************************************** */

				if (  move_info.x1 <= (gptr->x-5) ||  move_info.y1 <= (gptr->y - 5) || 
				      move_info.y1 >= (gptr->y + gptr->h + 5) )
				{    

					move_info.num_legs = 1;
				}

				if (  move_info.x2 >= (gptr->x + gptr->w +5) ||  move_info.y2 <= (gptr->y - 5) || 
			      		move_info.y2 >= (gptr->y + gptr->h + 5) )
				{    

					move_info.x2 = move_info.x1;
					move_info.y2 = move_info.y1;
					move_info.num_legs = 1;
				}


				return ( FOUND );
			}
		} 
	} 
					
	return ( NOT_FOUND );
}


/* ********************************************* */
/* find the index of the first valid right point */
/* ********************************************* */
int find_TSinsert_point ( int x, int y )
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	n, 
		xpix,
		ypix,
		right_index, 
		left_index,
		valid_graph, 
		active_trace,
		npts;

	TRACE_DATA   *tptr;

	valid_graph = 0;

	for (n = 0; n < gptr->num_traces; n++)
		 if ( gptr->trace_valid[n] ) valid_graph = 1;

	if ( !  valid_graph )
			return NOT_FOUND ;

	active_trace = PageMgr->edit_trace;

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];

	npts   =  tptr->npts;

	for ( n = 0; n < npts; n++)
	{
		if( tptr->TSdata[n].mode == DELETE ||     tptr->TSdata[n].mode == SETMISSING) continue;

		if (tptr->TSdata[n].x <  gptr->xmin || tptr->TSdata[n].x >  gptr->xmax ||
		tptr->TSdata[n].y < gptr->ymin || tptr->TSdata[n].y > gptr->ymax) continue;

		xpix = x2pixel ( gptr , tptr->TSdata[n].x);
		ypix = y2pixel ( gptr , tptr->TSdata[n].y);

		/* **************************** */
		/* Find first valid right point */
		/* **************************** */

		if ( xpix > x)
		{
			move_info.trace_index = n;

			right_index = n;
			left_index = find_left_index ( tptr, n );

			move_info.x1 = x2pixel ( gptr , tptr->TSdata[left_index].x);
			move_info.y1 = y2pixel ( gptr , tptr->TSdata[left_index].y);

			move_info.x2 = x2pixel ( gptr , tptr->TSdata[right_index].x);
			move_info.y2 = y2pixel ( gptr , tptr->TSdata[right_index].y);

			move_info.left_limit  = move_info.x1;
			move_info.right_limit = move_info.x2;
			move_info.left_index  = left_index;
			move_info.right_index = right_index;

			return FOUND ;

		}


	}

	return NOT_FOUND ;
}



/* ************************************************************* */
/* check for valid move - can't move beyond two  adjacent points */
/* ************************************************************* */
int TSadd_checkEdit ( int x, int y )
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	n, 
		valid_graph, 
		active_trace,
		npts;

	TRACE_DATA   *tptr;

	valid_graph = 0;

	for (n = 0; n < gptr->num_traces; n++)
		 if ( gptr->trace_valid[n] ) valid_graph = 1;

	if ( !  valid_graph )
			return NOT_FOUND ;

	active_trace = PageMgr->edit_trace;

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];

	npts   =  tptr->npts;

	move_info.left_limit  = x2pixel ( gptr , tptr->TSdata[0].x);
	move_info.right_limit = x2pixel ( gptr , tptr->TSdata[npts-1].x);

	if ( x >  move_info.left_limit &&  x < move_info.right_limit)
	{
		move_info.add_flag  = ADD_MIDDLE;
		return FOUND ;

	}
	else if ( x <  move_info.left_limit)
	{
		move_info.add_flag  = ADD_LEFT;
		move_info.x1 = x2pixel ( gptr , tptr->TSdata[0].x);
		move_info.y1 = y2pixel ( gptr , tptr->TSdata[0].y);
		return FOUND ;
	}
	else if ( x > move_info.right_limit)
	{

		move_info.add_flag  = ADD_RIGHT;
		move_info.x1 = x2pixel ( gptr , tptr->TSdata[tptr->npts-1].x);
		move_info.y1 = y2pixel ( gptr , tptr->TSdata[tptr->npts-1].y);
		return FOUND ;
	}
	else  
	{
		move_info.add_flag = 0;
		return NOT_FOUND ;

	}


	return FOUND ;
}


/* ************************** */
/* update when delete is done */
/* ************************** */
void	update_TSdelete_data()
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	n, 
		xpix,
		ypix,
		valid_graph, 
		active_trace,
		npts;

	TRACE_DATA   *tptr;


	valid_graph = 0;

	for (n = 0; n < gptr->num_traces; n++)
		 if ( gptr->trace_valid[n] ) valid_graph = 1;

	if ( !  valid_graph )
			return;

	active_trace = PageMgr->edit_trace;

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];

	npts   =  tptr->npts;

	for ( n = 0; n <tptr->npts; n++)
	{
		xpix = x2pixel ( gptr , tptr->TSdata[n].x);
		ypix = y2pixel ( gptr , tptr->TSdata[n].y);

		if ( xpix >= (move_info.x1-5) && xpix <= (move_info.x2+5) && 
		     ypix >= (move_info.y1-5) && ypix <= (move_info.y2+5)) 
		{
			tptr->TSdata[n].mode = DELETE;
				
		}


	}

	return;
}

void    update_TSMissing_data()
{

        extern PAGE_MGR         *PageMgr;
        extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

        PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
        GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

        int     n,
                xpix,
                ypix,
                valid_graph,
                active_trace,
                npts;

        TRACE_DATA   *tptr;


        valid_graph = 0;

        for (n = 0; n < gptr->num_traces; n++)
                 if ( gptr->trace_valid[n] ) valid_graph = 1;

        if ( !  valid_graph )
                        return;

        active_trace = PageMgr->edit_trace;

        tptr = (TRACE_DATA *) &gptr->traces[active_trace];

        npts   =  tptr->npts;

        for ( n = 0; n <tptr->npts; n++)
        {
                xpix = x2pixel ( gptr , tptr->TSdata[n].x);
                ypix = y2pixel ( gptr , tptr->TSdata[n].y);

                if ( xpix >= (move_info.x1-5) && xpix <= (move_info.x2+5) &&
                     ypix >= (move_info.y1-5) && ypix <= (move_info.y2+5))
                {
                        tptr->TSdata[n].mode = SETMISSING;

                }


        }

        return;
}


/* ************************** */
/* update when add    is done */
/* ************************** */
void update_TSadd_data ( )
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	nmove,
		active_trace,
		snap_value,
		r_index,
		l_index,
		nduration,
		ngaps;

	float	yvalue;
	time_t	xvalue,
		ppdur_secs = ( time_t ) 0 ;

	TRACE_DATA   *tptr;

	xvalue = pixel2x ( gptr, move_info.x);
	yvalue = pixel2y ( gptr, move_info.y);

	active_trace = PageMgr->edit_trace;

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];


	/* *****************************************  */
	/* Check for duplicate points in three cases  */
	/*	First, Middle, and End point          */
	/* *****************************************  */

	snap_value = 300; /* covert 5 minutes to secoconds */

	if ( ppdur > 0)
		ppdur_secs   = ppdur*60;
	else
		xvalue+=250;      /* 250 seconds */


	if ( move_info.add_flag == ADD_LEFT)
	{

		/* ******************************* */
		/* PP trace is treated differently */
		/* ******************************* */
		if ( ppdur > 0 )
		{
			 ngaps  = (xvalue -  tptr->TSdata[0].x)/ppdur_secs;
			 xvalue = tptr->TSdata[0].x - ngaps*ppdur_secs;
		}
		else
			xvalue = (xvalue/snap_value)*snap_value;

		if ( xvalue >=  tptr->TSdata[0].x)
		{
			return;
		}

		nmove = (tptr->npts)*sizeof(POINT);
		memmove((char *)&tptr->TSdata[1],(char *)&tptr->TSdata[0],nmove);
		tptr->TSdata[0].x = xvalue;
		tptr->TSdata[0].y = yvalue;
		tptr->TSdata[0].mode = ADD;
		tptr->npts+=1;
	}
	else if ( move_info.add_flag == ADD_RIGHT )
	{
		/* ******************************* */
		/* PP trace is treated differently */
		/* ******************************* */
		if ( ppdur > 0 )
		{
			 ngaps  = (xvalue -  tptr->TSdata[tptr->npts-1].x)/ppdur_secs;
			 xvalue = tptr->TSdata[tptr->npts-1].x + ngaps*ppdur_secs;
		}
		else
			xvalue = (xvalue/snap_value)*snap_value;

		if ( xvalue <=  tptr->TSdata[tptr->npts-1].x)
		{
			return;
		}

		tptr->TSdata[tptr->npts].x = xvalue;
		tptr->TSdata[tptr->npts].y = yvalue;
		tptr->TSdata[tptr->npts].mode = ADD;
 		tptr->npts+=1;
	}
	else if ( move_info.add_flag == ADD_MIDDLE)
	{

		xvalue = (xvalue/snap_value)*snap_value;

		r_index = move_info.right_index;
		l_index = move_info.left_index;


		if ( xvalue <=  tptr->TSdata[l_index].x ||  xvalue >=  tptr->TSdata[r_index].x)
			return;

		if ( ppdur > 0 ) /* PP trace is treated differently */
		{

			ngaps = (tptr->TSdata[r_index].x - tptr->TSdata[l_index].x)/ppdur_secs;
			if ( ngaps == 1)
				return;
			if ( ngaps == 2 ) 
				 xvalue = tptr->TSdata[l_index].x + ppdur_secs;
			else
			{
				nduration = (xvalue - tptr->TSdata[l_index].x)/ppdur_secs;
				xvalue = tptr->TSdata[l_index].x + nduration*ppdur_secs;
			}

			if ( xvalue >  tptr->TSdata[r_index].x)
			return;

		}
		else
			xvalue = (xvalue/snap_value)*snap_value;


		nmove = (tptr->npts - r_index)*sizeof(POINT);

		memmove((char *)&tptr->TSdata[r_index+1],(char *)&tptr->TSdata[r_index],nmove);

		tptr->TSdata[r_index].x = xvalue;
		tptr->TSdata[r_index].y = yvalue;
		tptr->TSdata[r_index].mode = ADD;
		tptr->npts+=1;
	}

	return;
}

/* ************************** */
/* update when move   is done */
/* ************************** */
void update_TSmove_data ( )
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	active_trace,
		trace_index;

	float	yvalue;

	TRACE_DATA   *tptr;

	
	active_trace = PageMgr->edit_trace;
	trace_index  = move_info.trace_index;

	yvalue= pixel2y ( gptr, move_info.y);

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];

	tptr->TSdata[trace_index].y = yvalue;
	tptr->TSdata[trace_index].mode = MOVE;

	return;
}



/* *********************************************** */
/* find an index of the selected trace for editing */
/* *********************************************** */
int find_edit_trace (int x, int y)
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	n,
		xpix,
		ypix,
		valid_graph, 
		ntrace;

	TRACE_DATA   *tptr;

	valid_graph = 0;

	for (n = 0; n < gptr->num_traces; n++)
		 if ( gptr->trace_valid[n] ) valid_graph = 1;

	if ( !  valid_graph )
   	   return NOT_FOUND ;

	for (ntrace = 0; ntrace < gptr->num_traces; ntrace++)
	{
		 if ( gptr->trace_on[ntrace] )
		 {
			tptr = (TRACE_DATA *) &gptr->traces[ntrace];
			for ( n = 0; n <tptr->npts; n++)
			{

				if( tptr->TSdata[n].mode == DELETE || tptr->TSdata[n].mode == SETMISSING) continue;

				if (tptr->TSdata[n].x <  gptr->xmin || tptr->TSdata[n].x >  gptr->xmax ||
				tptr->TSdata[n].y < gptr->ymin || tptr->TSdata[n].y > gptr->ymax) continue;

				xpix = x2pixel ( gptr , tptr->TSdata[n].x);
				ypix = y2pixel ( gptr , tptr->TSdata[n].y);


				if ( (xpix <= (x+5) && xpix >= (x-5)) && (ypix <= (y+5) && ypix >= (y-5)) )
				{
					return ( ntrace );
					
				} 
					
			}

		 }
	}

	return ( NOT_FOUND );
}



/* *********************************************************** */
/* check for valid select - number of traces on must be >=1    */
/* *********************************************************** */
int check4valid_select()
{

	extern PAGE_MGR         *PageMgr;

	extern  PAGE_DATA  TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA  *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int 	ntrace,
		ntrace_on = 0;

	for (ntrace = 0; ntrace < gptr->num_traces; ntrace++)
		 if ( gptr->trace_on[ntrace] )ntrace_on++;

	return ( ntrace_on );

}

/* ************************* */
/*  draw move must use gcXOR */
/* ************************* */
void draw_move()
{
	extern PAGE_MGR         *PageMgr;

	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x2, move_info.y2, move_info.x, move_info.y);
	if ( move_info.num_legs == 2)
	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x, move_info.y, move_info.x1, move_info.y1);
	XDrawArc( PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x-2, move_info.y-2, 4, 4,  0, 360*64);
}

/* ************************* */
/*  draw add  must use gcXOR */
/* ************************* */
void draw_add()
{
	extern PAGE_MGR         *PageMgr;

	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x1, move_info.y1, move_info.x, move_info.y);
	if ( move_info.add_flag == ADD_MIDDLE )
	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x, move_info.y, move_info.x2, move_info.y2);
	XDrawArc( PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x-2, move_info.y-2, 4, 4,  0, 360*64);
}

/* *************************************** */
/*  draw box (rubber band)  must use gcXOR */
/* *************************************** */
void draw_box()
{
	extern PAGE_MGR         *PageMgr;

	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x1, move_info.y1, move_info.x2, move_info.y1);
	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x2, move_info.y1, move_info.x2, move_info.y2);
	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x2, move_info.y2, move_info.x1, move_info.y2);
	XDrawLine(PageMgr->display, PageMgr->window, PageMgr->gc_Xor, move_info.x1, move_info.y2, move_info.x1, move_info.y1);
}

/* ************************************************************* */
/*  find left index of a point being add/move/delete/set missing */
/*  case it is a first/last point of a trace         		 */
/* ************************************************************* */
int  find_left_index ( TRACE_DATA *tptr , int n)
{
	int m;
	for( m = (n-1); m > 0; m--)
		if( tptr->TSdata[m].mode != DELETE || 
		    tptr->TSdata[m].mode != SETMISSING) break;

	return ( m );

}

/* ************************************************** */
/*  find right index of a point being add/move/delete */
/*  case it is a first/last point of a trace          */
/* ************************************************** */
int  find_right_index ( TRACE_DATA *tptr , int n)
{
	int m;
	for( m = (n+1); m < tptr->npts; m++)
		if( tptr->TSdata[m].mode != DELETE || 
		    tptr->TSdata[m].mode != SETMISSING) break;

	return ( m );

}

/* ************************************************** */
/*  Show active page  label with page number at the   */
/*  top left  corner of the page                      */
/*                                                    */
/*  Add page title to the page label                  */
/*  if it is available for GROUP mode.                */
/*  Added by guoxian zhou                             */
/* ************************************************** */
void show_active_page_label( )
{
	extern GROUP_INFO     *GroupInfo;
	extern PAGE_MGR  	*PageMgr;

	extern  PAGE_INFO  TSpageinfo[MAX_PAGE_INFO];

	PAGE_INFO       *pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];

	char buf[200], tmp[80];
	char* tmpptr;
	
	memset(buf, '\0', 200);
	memset(tmp, '\0', 80);
	
	if((GroupInfo->GroupSelected == GROUP) && (pinfo->title != NULL))
	{
		/*I need remove the '\n' at the end of title string*/
		tmpptr = strpbrk(pinfo->title, "\n");
		
		if(tmpptr == NULL)
			strcpy(tmp, pinfo->title);
		else
			strncpy(tmp, pinfo->title, strlen(pinfo->title)-1); 
		sprintf(buf,"Page %d of %d               %s", GroupInfo->current_page+1,GroupInfo->page_count, tmp);
	}
	else
	{
		sprintf(buf,"Page %d of %d", GroupInfo->current_page+1,GroupInfo->page_count);
	}

	XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, 5, 10,50,20);
	SetColor(PageMgr->gc_header, PageMgr->widget, "Cyan");

	XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_header, 5, 10, buf, strlen(buf));
	XFlush(PageMgr->display);
	return;
}

/* ************************************************** */
/*  Show active graph label with letter 'A' at the    */
/*  top right corner of the graph                     */
/* ************************************************** */
void show_active_graph_label( int active_graph )
{
	extern PAGE_MGR  	*PageMgr;
	extern PAGE_DATA    TSpagedata[MAX_PAGE_DATA];
	extern  GROUP_INFO    *GroupInfo;
	
	extern  PAGE_INFO	TSpageinfo[MAX_PAGE_INFO];

	PAGE_DATA    *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA   *gptr;

	int	n,
		xpos,
		ypos;

	SetColor(PageMgr->gc_header, PageMgr->widget, "Green");

	for ( n = 0; n < pptr->num_graphs; n++)
	{
		gptr = (GRAPH_DATA *) &pptr->graph[n];
		xpos = gptr->x + gptr->w - 15;
		ypos = gptr->y + 25;
		XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, xpos-5, ypos-10, 20, 20);
		if ( n == active_graph)
		{
			XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_header, xpos, ypos, "A",1);

			/* update all radio buttons' mode */
			Widget widget;
			char buttonIndex[10];
			int i;
			PAGE_INFO	*pinfo;
			GRAPH_INFO	*ginfo;

			pinfo = (PAGE_INFO *) &TSpageinfo[GroupInfo->current_page];
			ginfo = (GRAPH_INFO *) &pinfo->graph[active_graph];
			
			for(i = 0; i < 3; i++ )
			{
				sprintf(buttonIndex, "button_%d", i);

				if(i == ginfo->showcat)
				{
					if((widget = XtNameToWidget(TSscaleMO, buttonIndex)) != NULL)
						XtVaSetValues(widget, XmNset, TRUE, NULL);
				}
				else
				{
					if((widget = XtNameToWidget(TSscaleMO, buttonIndex)) != NULL)
						XtVaSetValues(widget, XmNset, FALSE, NULL);
				}

				if(i == ginfo->showpp)
				{
					if((widget = XtNameToWidget(TSShowPCAsPPMO, buttonIndex)) != NULL )
						XtVaSetValues(widget, XmNset, TRUE, NULL);
				}
				else
				{
					if((widget = XtNameToWidget(TSShowPCAsPPMO, buttonIndex)) != NULL )
						XtVaSetValues(widget, XmNset, FALSE, NULL);
				}
			}
			if(ginfo->latestfcstonly == 1)
				XtVaSetValues(TSShowFcstTB, XmNset, TRUE, NULL);
			else
				XtVaSetValues(TSShowFcstTB, XmNset, FALSE, NULL);
			
			
			
		}

	}


	XFlush(PageMgr->display);

	return;
}

/* **************************** */
/* show label for active trace  */
/* **************************** */
void show_active_trace_label ()
{

	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	int	active_trace;

	char	buf[80];

	TRACE_DATA   *tptr;

	active_trace = PageMgr->edit_trace;

	tptr = (TRACE_DATA *) &gptr->traces[active_trace];

	sprintf(buf,"ACTIVE TRACE: %s %s %d %s",tptr->trace_info.pe, tptr->trace_info.ts, 
						tptr->trace_info.dur, tptr->trace_info.extremum);

	XFillRectangle(PageMgr->display, PageMgr->window, PageMgr->gc_background, gptr->x + gptr->w/2,  gptr->y-35,150,15);

	XSetForeground(PageMgr->display, PageMgr->gc_line, tptr->trace_color);
	XDrawString(PageMgr->display,PageMgr->window, PageMgr->gc_line, gptr->x +  gptr->w/2, gptr->y-30, 
									  buf, strlen(buf));

	XFlush(PageMgr->display);
}


/* ************************************************** */
/* confirmation dialog - save data to database        */
/* ************************************************** */
void save_confirmation( Widget w )
{
	Widget 	qstDS,
		okPB,
		cancelPB;

	char         	buf[80];

		sprintf(buf, "Do you wish to save changes to Database?");
		qstDS = QuestionDialog(w, buf);
		SetTitle(qstDS,"Apply Confirmation");
		okPB     = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
		cancelPB = XmMessageBoxGetChild(qstDS, XmDIALOG_CANCEL_BUTTON);
		XtAddCallback(okPB,               XmNactivateCallback, save_cancel_edit, (XtPointer) 1);
		XtAddCallback(cancelPB,           XmNactivateCallback, save_cancel_edit, (XtPointer) 3);

	XtManageChild(qstDS);

	return;

}

/* ************************************************** */
/* handle cancel saving to database option from users */
/* ************************************************** */
void  save_cancel_edit ( Widget w, XtPointer ptr, XtPointer cbs)
{

	extern GROUP_INFO     *GroupInfo;
	extern PAGE_MGR         *PageMgr;
	extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

	int	n, 
		edit_trace,
		option;

	float	graph_ymin,
	 	graph_ymax,
		graph_data_inc,
		old_graph_ymin,
	 	old_graph_ymax,
		old_graph_data_inc;

	 time_t	graph_xmin,
		graph_xmax,
		old_graph_xmin,
		old_graph_xmax;


	PAGE_DATA       *pptr = (PAGE_DATA  *) &TSpagedata[0];
	GRAPH_DATA      *gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	TRACE_DATA	*tptr;



	option = (int) ptr;

	graph_ymin     = gptr->ymin;
	graph_ymax     = gptr->ymax;
	graph_xmin     = gptr->xmin;
	graph_xmax     = gptr->xmax;
	graph_data_inc = gptr->data_inc;

	old_graph_ymin     = gptr->old_ymin;
	old_graph_ymax     = gptr->old_ymax;
	old_graph_xmin     = gptr->old_xmin;
	old_graph_xmax     = gptr->old_xmax;
	old_graph_data_inc = gptr->old_data_inc;

	if (  option == 1 ) /* Update and save changes to database */ 
	{
		PageMgr->Edit_active = EDIT_RESET;
		highlight_select_options();
		SetCursor( PageMgr->widget, XC_watch);

		edit_trace = PageMgr->edit_trace;
		tptr = (TRACE_DATA *) &gptr->traces[edit_trace];
		Update_database(  tptr );
		get_TSpage_data( GroupInfo->current_page );
	}
	else if ( option == 2 ) /* Restore database */
	{
		PageMgr->Edit_active = EDIT_RESET;
		highlight_select_options();
		SetCursor( PageMgr->widget, XC_watch);
		get_TSpage_data( GroupInfo->current_page );

	}
	else if ( option == 3 ) /* Do nothing but redraw graph */
	{
 		;;
	}



	pptr = (PAGE_DATA  *) &TSpagedata[0];
	gptr = (GRAPH_DATA *) &pptr->graph[PageMgr->edit_graph];

	gptr->ymin = graph_ymin;
	gptr->ymax = graph_ymax;
	gptr->xmin = graph_xmin;
	gptr->xmax = graph_xmax;
	gptr->data_inc =  graph_data_inc;

	gptr->old_ymin = old_graph_ymin;
	gptr->old_ymax = old_graph_ymax;
	gptr->old_xmin = old_graph_xmin;
	gptr->old_xmax = old_graph_xmax;
	gptr->old_data_inc =  old_graph_data_inc;

	for ( n = 0; n < gptr->num_traces; n++)
	{
		gptr->trace_on[n]=0; 
		if (  PageMgr->traces_on_edit[n] )
		{
			gptr->trace_on[n] = 1;

		}
	}

	display_TSgraph_data( PageMgr->edit_graph );

	UnsetCursor( PageMgr->widget );
	PageMgr->display_crosshairs = 1;

	if ( option == 3)
	{
		DeSensitize(TSTracePB);
		Sentitize_edit_options();
	}
	else
		PageMgr->Editmode = 0;

}

/* ************************************************** */
/* handle update changes to data base                 */
/* ************************************************** */
void Update_database( TRACE_DATA *tptr)
{
	int	n,
		mod_obs_success,
		mod_fcst_success;

	long 	result;

	time_t	current_time;

	dtime_t posting_time,
		producttime;

	char 	tablename[BUFSIZ],
		where[BUFSIZ],
		command[BUFSIZ],
		product_id[PRODUCT_LEN+1];


	Observation	oPtr;
	RejectedData 	rejectedData;
	Forecast       	fPtr;

	memset(&oPtr, '\0', sizeof(Observation));
	memset(&fPtr, '\0', sizeof(Forecast));

	getTableName(tptr->trace_info.pe,tptr->trace_info.ts, tablename);


	if ( tptr->trace_info.isForecast )
	{
		strcpy(fPtr.lid,tptr->trace_info.lid);
		strcpy(fPtr.pe ,tptr->trace_info.pe);
		strcpy(fPtr.ts ,tptr->trace_info.ts);
		strcpy(fPtr.extremum,tptr->trace_info.extremum);
		fPtr.dur   = tptr->trace_info.dur;
		strcpy(fPtr.shef_qual_code, "M");

		validtime    = tptr->TSdata[0].x;
		basistime    = tptr->basistime;

		timet_to_yearsec_dt (validtime, &fPtr.validtime);
		timet_to_yearsec_dt (basistime, &fPtr.basistime);

		createUpdDelWhereFcst (where, &fPtr);
	        getFcstProd_idtime(where, tablename, product_id, &producttime);
		strcpy(fPtr.product_id,product_id);
		fPtr.producttime  = producttime;

	}
	else
	{
		strcpy(oPtr.lid,tptr->trace_info.lid);
		strcpy(oPtr.pe ,tptr->trace_info.pe);
		strcpy(oPtr.ts ,tptr->trace_info.ts);
		strcpy(oPtr.extremum,tptr->trace_info.extremum);
		oPtr.dur   = tptr->trace_info.dur;
		strcpy(oPtr.shef_qual_code, "M");

		obstime  = tptr->TSdata[0].x;
		timet_to_yearsec_dt (obstime, &oPtr.obstime);
		createUpdDelWhereObs ( where, &oPtr);
	        getObsProd_idtime(where, tablename, product_id, &producttime);
		strcpy(oPtr.product_id,product_id);
		oPtr.producttime  = producttime;
	 }


	/* ******************************* */
	/* set postingtime to current time */
	/* ******************************* */

	time(&current_time);
	timet_to_yearsec_dt(current_time, &posting_time);
	fPtr.postingtime  = posting_time;
	oPtr.postingtime  = posting_time;

	mod_obs_success  = 0;
	mod_fcst_success = 0;


	for ( n = 0; n < tptr->npts; n++)
	{
		if ( tptr->TSdata[n].mode == DELETE)
		{

			if ( tptr->trace_info.isForecast )
			{
				oldValue     = tptr->TSdata[n].old_value;
				validtime    = tptr->TSdata[n].x;
				basistime    = tptr->basistime;
				fPtr.quality_code= tptr->TSdata[n].quality_code; 
				fPtr.revision= tptr->TSdata[n].revision; 
				timet_to_yearsec_dt (validtime, &fPtr.validtime);
				timet_to_yearsec_dt (basistime, &fPtr.basistime);
				createUpdDelWhereFcst (where, &fPtr);
				result = DeleteForecast(where, tablename); 
				if ( result >= 0)
					mod_fcst_success = 1;
				else
					break;

				setRejectedDataFcst(&fPtr, &rejectedData, oldValue);
				result = PutRejectedData(&rejectedData);
				if ( result < 0 )
				{
				   printf( "Error in PutRejectedData %ld\n" ,
                                           result ) ;
				   break;
				}

			}
			else
			{
				oldValue     = tptr->TSdata[n].old_value;
				obstime      = tptr->TSdata[n].x;
				oPtr.quality_code = tptr->TSdata[n].quality_code; 
				oPtr.revision= tptr->TSdata[n].revision; 
				timet_to_yearsec_dt (obstime, &oPtr.obstime);
				createUpdDelWhereObs ( where, &oPtr);
				result = DeleteObservation(where, tablename);
				if (  result < 0)
				{
					printf("Error in Delete %ld\n",result);
					break;

				}
				else
					mod_obs_success = 1;

				if ((oPtr.pe[0] == 'P' ) && (oPtr.pe[1] != 'A' && oPtr.pe[1] != 'D' &&
									 oPtr.pe[1] != 'E' && oPtr.pe[1] != 'L'))

 				{
			           /* ************************************ */
				   /* if precip then delete from either    */
                                   /* the CurPC or CurPP tables as well.   */
				   /* ************************************ */
                                   if ( oPtr.pe [ 1 ] == 'P' )
                                   {
					 DeleteCurPP(where);
                                   }
                                   else
                                   {
                                         DeleteCurPC(where);
                                   }
                                
				}

				setRejectedDataObs(&oPtr, &rejectedData, oldValue);
				result = PutRejectedData(&rejectedData);

				if (  result < 0)
				{
				   printf ( "Error in PutRejectedData %ld\n" ,
                                            result ) ;
				   break;

				}

			}

		 }

                if ( tptr->TSdata[n].mode == SETMISSING)
                {

                        if ( tptr->trace_info.isForecast )
                        {
                                oldValue     = tptr->TSdata[n].old_value;
				fPtr.value   = MISSING;
                                validtime    = tptr->TSdata[n].x;
                                basistime    = tptr->basistime;
				
                                fPtr.quality_code= tptr->TSdata[n].quality_code;
				set_qccode(QC_MANUAL_PASSED, &fPtr.quality_code);
                                fPtr.revision = 1;
                                timet_to_yearsec_dt (validtime, &fPtr.validtime);
                                timet_to_yearsec_dt (basistime, &fPtr.basistime);
                                createUpdDelWhereFcst (where, &fPtr);
                                result = UpdateForecast(&fPtr, where, tablename);
                                if ( result >= 0)
                                        mod_fcst_success = 1;
                                else
                                        break;

                                setRejectedDataFcst(&fPtr, &rejectedData, oldValue);
                                result = PutRejectedData(&rejectedData);
                                if ( result < 0 )
                                {
                                   printf( "Error in PutRejectedData %ld\n" ,
                                           result ) ;
                                   break;
                                }

                        }
                        else
                        {
                                oldValue     = tptr->TSdata[n].old_value;
                                oPtr.value   = MISSING;
                                obstime      = tptr->TSdata[n].x;
				timet_to_yearsec_dt (obstime, &oPtr.obstime);
                                oPtr.quality_code = tptr->TSdata[n].quality_code;
				set_qccode(QC_MANUAL_PASSED, &oPtr.quality_code);
                                oPtr.revision = 1;
                                createUpdDelWhereObs ( where, &oPtr);
                                getObsProd_idtime(where, tablename, product_id, &producttime);
                                strcpy(oPtr.product_id,product_id);
                                oPtr.producttime  = producttime;
                                result = UpdateObservation(&oPtr,where, tablename);
                                if (  result < 0)
                                {
                                        printf("Error in Set Missing %ld\n",result);
                                        break;

                                }
                                else /* successful update of observation */
                                        mod_obs_success = 1;

                                setRejectedDataObs(&oPtr, &rejectedData, oldValue);
                                result = PutRejectedData(&rejectedData);
 
                                if (  result < 0)
                                {
                                   printf ( "Error in PutRejectedData %ld\n" ,
                                            result ) ;
                                   break;
 
                                }
 
                        }

                }

		if ( tptr->TSdata[n].mode == ADD)
		{
			if ( tptr->trace_info.isForecast )
			{
				fPtr.value   = tptr->TSdata[n].y;
				validtime    = tptr->TSdata[n].x;
				basistime    = tptr->basistime;
				fPtr.quality_code = tptr->TSdata[n].quality_code;
				set_qccode(QC_MANUAL_NEW, &fPtr.quality_code);
				fPtr.probability    = (-1.0);
				fPtr.revision       = 0;

				timet_to_yearsec_dt (validtime, &fPtr.validtime);
				timet_to_yearsec_dt (basistime, &fPtr.basistime);

				createUpdDelWhereFcst( where, &fPtr);
				result = PutForecast ( &fPtr, tablename );
				if ( result == 0) mod_fcst_success = 1;

			}
			else
			{

				oPtr.value    = tptr->TSdata[n].y;
				obstime       = tptr->TSdata[n].x;
				oPtr.revision     = tptr->TSdata[n].revision;
				oPtr.quality_code = tptr->TSdata[n].quality_code;
				set_qccode(QC_MANUAL_NEW, &oPtr.quality_code);
				oPtr.revision       = 0;

				timet_to_yearsec_dt(obstime, &oPtr.obstime);
				createUpdDelWhereObs( where, &oPtr);

				result = PutObservation ( &oPtr, tablename );
				if ( result < 0)
				{

				   printf ( "Error in PutObservation: %ld\n" ,
                                            result ) ;
				   break;
				}
			}

		}

		if ( tptr->TSdata[n].mode == MOVE)
		{

			if ( tptr->trace_info.isForecast )
			{
				oldValue     = tptr->TSdata[n].old_value;
				fPtr.value   = tptr->TSdata[n].y;
				validtime    = tptr->TSdata[n].x;
				basistime    = tptr->basistime;
				fPtr.revision     = 1;    
				fPtr.probability  =  tptr->TSdata[n].probability; 
				fPtr.quality_code =  tptr->TSdata[n].quality_code;

				timet_to_yearsec_dt (validtime, &fPtr.validtime);
				timet_to_yearsec_dt (basistime, &fPtr.basistime);
				set_qccode(QC_MANUAL_PASSED, &fPtr.quality_code);

				createUpdDelWhereFcst( where, &fPtr);
				result = UpdateForecast ( &fPtr, where, tablename );
				if ( result == 0)
					mod_fcst_success = 1;
				else
					break;

				setRejectedDataFcst(&fPtr, &rejectedData, oldValue);
				result = PutRejectedData(&rejectedData);

				if( result < 0)
				{
				   printf( "Error in PutRejectedData: %ld\n" ,
                                           result ) ;
			  	   break;
				}

			}
			else
			{
				
				oldValue     = tptr->TSdata[n].old_value;
				oPtr.value   = tptr->TSdata[n].y;
				obstime      = tptr->TSdata[n].x;
				oPtr.revision     = 1; 
				oPtr.quality_code = tptr->TSdata[n].quality_code;
				set_qccode(QC_MANUAL_PASSED, &oPtr.quality_code);

				timet_to_yearsec_dt (obstime, &oPtr.obstime);
				createUpdDelWhereObs( where, &oPtr);

				result = UpdateObservation ( &oPtr, where, tablename );
				if ( result < 0 )
				{
				   printf( "Error in UpdateObservation: %ld\n" ,
                                           result);
				   break;
				}

				setRejectedDataObs(&oPtr, &rejectedData, oldValue);

				result = PutRejectedData(&rejectedData);

				if ( result < 0 )
				{
				   printf( "Error in PutRejectedData: %ld\n" ,
                                           result);
				   break;
				}
			}

		}
	}


	/*  **********************************************  */
	/*  if height then calculate new curheight as well  */
	/*  **********************************************  */

	if ( mod_obs_success == 1 && ( oPtr.pe[0] == 'H' || oPtr.pe[0] == 'Q' ))
	{
		sprintf(command, "load_obs_river('%s', '%s', '%s')",
		oPtr.lid, oPtr.pe, oPtr.ts);
		result = execFunction(command);
		if ( result != 0 )
		printf("Error in execFunction(command).\n");
	} 
	else if ( mod_fcst_success == 1 && ( fPtr.pe[0] == 'H' || fPtr.pe[0] == 'Q' ))
	{
		load_maxfcst_item ( fPtr.lid, fPtr.pe, fPtr.ts);
	}


}

/* ************************************** */
/* set button's background/foreground     */
/* to its original  background/foreground */
/* ************************************** */
void reset_fg_button( Widget w)
{
	int     ac,
		fg,
		bg;
	Arg     arg[5];

	XtVaGetValues(TSAddPB,  XmNforeground, &fg,
				XmNbackground, &bg,
				NULL);
	ac = 0;
	XtSetArg(arg[ac], XtNbackground, bg);ac++;
	XtSetArg(arg[ac], XtNforeground, fg);ac++;
	XtSetValues( w, arg, ac);

	return;
}

/* ************************************** */
/* sentitize edit options                 */
/* ************************************** */
void	Sentitize_edit_options()
{

	Sensitize(TSAddPB);
	Sensitize(TSMovePB);
	Sensitize(TSDeletePB);
        Sensitize(TSSetMissingPB);
	Sensitize(TSEditDonePB);
	Sensitize(TSEditCancelPB);

}

/* ************************************** */
/* desentitize edit options               */
/* ************************************** */
void	DeSentitize_edit_options()
{
	DeSensitize(TSAddPB);
	DeSensitize(TSMovePB);
	DeSensitize(TSDeletePB);
        DeSensitize(TSSetMissingPB);
	DeSensitize(TSEditDonePB);
}



/* ************************************** */
/* convert pp duration to minutes         */
/* ************************************** */
time_t	conv_ppdur2min ( GRAPH_DATA *graph, int edit_trace)
{
	int	dur,
		ivalue,
		jhrs,
		jdays,
		jmos,
		jyrs;

	time_t	durin_minute = 0;

	TRACE_DATA *tptr;
	
	
	tptr = (TRACE_DATA *) &graph->traces[edit_trace];

	if ( strncmp ("PP",tptr->trace_info.pe,2) != 0)
	return ( -1 );

	dur = tptr->trace_info.dur;

	ivalue = dur/1000;

	switch ( ivalue )
	{
		case 0: 
			durin_minute = 0;
		break;

		case 1: 
			jhrs = dur - (dur/1000)*1000;
			durin_minute = jhrs*60;
		break;

		case 2: 
			jdays = dur - (dur/1000)*1000;
			durin_minute = jdays*60*24;
		break;

		case 3: 
			jmos = dur - (dur/1000)*1000;
			durin_minute = jmos*60*24*30;
		break;

		case 4: 
			jyrs = dur - (dur/1000)*1000;
			durin_minute = jyrs*60*24*30*12;
		break;

		default:
			durin_minute = 0;

	}

	return ( durin_minute );

}


/* ************************************** */
/* get forecast product time              */
/* ************************************** */
void getFcstProd_idtime (char *where, char *tablename, char *product_id, dtime_t *producttime)
{

	Forecast        *fHead, 
			*fPtr;

	int             count = 0;

	fHead  = GetForecast(where, tablename);

	if ( fHead ) 
		count = ListCount(&fHead->list);

	if ( count == 0) 
	{ 
		strcpy(product_id,"CCCCNNNXXX");
		producttime  = 0;
		return;
	}

	fPtr = (Forecast *) ListFirst(&fHead->list);
	strcpy(product_id, fPtr->product_id);
	*producttime = fPtr->producttime;

	if ( fHead )
	       FreeForecast(fHead);

}


/* ************************************** */
/* get observed product time              */
/* ************************************** */
void getObsProd_idtime (char *where, char *tablename, char *product_id, dtime_t *producttime)
{

	Observation  	*oHead, 
			*oPtr;

	int             count = 0;

	oHead  = GetObservation(where, tablename);

	if ( oHead ) 
		count = ListCount(&oHead->list);

	if ( count == 0) 
	{ 
		strcpy(product_id,"CCCCNNNXXX");
		producttime  = 0;
		return;
	}

	oPtr = (Observation *) ListFirst(&oHead->list);
	strcpy(product_id, oPtr->product_id);
	*producttime = oPtr->producttime;

	if ( oHead )
	       FreeObservation(oHead);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
/* ********************************************* */
/* End of File:           TSedit.c               */
/* ********************************************* */


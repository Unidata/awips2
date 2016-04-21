/*
	File:		rate_show.c
	Date:		February 1995
	Author:		Dale Shelton

        History:

        Bryon L - 1/25/2005    Modified to use the date_t_to_USA_date and
                               USA_date_to_date_t routines for the PostGres 
                               conversion.
	
	Purpose:	Provide support for the Rating Curve DS.
*/

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>
/******* POSTGRES
#include <sqlhdr.h>
*******/
#include "Xtools.h"
#include "time_convert.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "HwStages.h"
#include "Rating.h"
#include "RatingShift.h"
#include "Riverstat.h"
#include "rate_cbs.h"
#include "rateds.h"

#include "ParamDefs.h" 
#include "List.h"
#include "rc_crosshairs.h"
#include "set_window_title.h"

/*
	Globals.
*/
Widget		fileSB = (Widget) NULL;

char		rate_lid[LOC_ID_LEN + 1];

HwStages	*hw = NULL ;

Rating		*rating = NULL ;

RatingShift	*ratingShift = NULL ;

int		drawCurve ;

float		shiftValue = 0;

void	ShowRateDs(Widget w, char *lid, Boolean editable)
{
   char		where[MAX_WHERE_LEN];
   char		flood[50], record[50];
   char		datestring[DATE_LEN+1], rate_date[30], usgs_num[30];
   Riverstat	*rvrPtr = NULL ;
	
	
   if (! rateDS)
   {
      create_rateDS(GetTopShell(w));
      rate_callbacks();
   }
   
   
   if (! XtIsManaged(rateDS))
   {
      strcpy(rate_lid,lid);
      sprintf(where, " WHERE lid = '%s' ", rate_lid);
      
      sprintf(flood, "Flood:         ");
      sprintf(record, "Record:         ");
      if ( ( hw = GetHwStages(where) ) != NULL )
      {
	 sprintf(flood, "Flood: %8.2f", hw->fs);
	 sprintf(record, "Record: %8.2f", hw->ms);
      }
      SetLabel(rtfldstgLbl, flood);
      SetLabel(rtforLbl, record);
      
      if ( ( rvrPtr = GetRiverstat(where) ) != NULL )
      {
        date_t_to_USA_date ( rvrPtr->ratedat, datestring );
	sprintf( rate_date, "Date of Rating: %s", datestring);
	sprintf(usgs_num, "USGS Rating No.: %s", rvrPtr->usgs_ratenum);
	FreeRiverstat(rvrPtr);
      }
      else
      {
	 sprintf( rate_date, "Date of Rating:           ");
	 sprintf(usgs_num, "USGS Rating No.:      ");
      }
      SetLabel(rtdateLbl, rate_date);
      SetLabel(rtusgsnumLbl, usgs_num);
      
      sprintf(where, " WHERE lid = '%s' ORDER BY date DESC ", rate_lid);
      ratingShift = GetRatingShift(where);

      if ( ratingShift != NULL )
	load_shift_list(ratingShift);
      else
	shiftValue = 0;
      
      sprintf(where, " WHERE lid = '%s' ORDER BY stage ASC ", rate_lid);
      rating = GetRating(where);

      if ( rating != NULL )
      { 
         load_list(rating);
         drawCurve = True;
      }
      else
      {
         drawCurve = False;
      }
      
      set_title(rateDS, "Rating Curve", rate_lid);
      XtManageChild(rateFM);
      XtManageChild(rateDS);
      rate_sensitize();
      shift_sensitize();

      if ( ! editable )
      {
	XtUnmanageChild(rtimpPB);
	XtUnmanageChild(rtmodifyPB);
	XtUnmanageChild(rtremPB);
	XtUnmanageChild(rtclearPB);
	XtUnmanageChild(shiftapplyPB);
	XtUnmanageChild(shiftdelPB);
	XtUnmanageChild(rtokPB);
	XtUnmanageChild(rtapplyPB);
      }

      DeSensitize(rateresultLB); /* Gray out listbox of resultant rating curve */

   }
   
   return;
}


void	rate_callbacks(void)
{
	Atom	wmAtom;
	
	wmAtom = XmInternAtom(XtDisplay(rateDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(rateDS, wmAtom, close_rate, NULL);

	XtAddCallback(rateLB,     XmNbrowseSelectionCallback, current_stage, NULL);
	XtAddCallback(rateDA,     XmNexposeCallback, draw_curve, NULL);
	XtAddCallback(rtimpPB,    XmNactivateCallback, import_file, NULL);
	XtAddCallback(rtclearPB,  XmNactivateCallback, rate_clear_conf, NULL);
	XtAddCallback(rtremPB,    XmNactivateCallback, rate_remove_conf, NULL);
	XtAddCallback(rtmodifyPB, XmNactivateCallback, rate_modify, NULL);

	XtAddCallback(rshiftLB,   XmNbrowseSelectionCallback, current_shift, NULL);
	XtAddCallback(shiftdelPB, XmNactivateCallback, shift_remove_conf, NULL);
	XtAddCallback(shiftapplyPB, XmNactivateCallback, shift_modify, NULL);

	XtAddCallback(rtokPB,     XmNactivateCallback, rate_ok, NULL);
	XtAddCallback(rtapplyPB,  XmNactivateCallback, rate_save, NULL);
	XtAddCallback(rtclosePB,  XmNactivateCallback, close_rate,  NULL);

	rc_ch_data.gc = rc_setup_crosshairs(rateDA);
	
	XtAddEventHandler(rateDA, ButtonPressMask, FALSE, 
		  rc_start_crosshairs, &rc_ch_data);
	XtAddEventHandler(rateDA, ButtonMotionMask, FALSE,
		  rc_track_crosshairs, &rc_ch_data);
	XtAddEventHandler(rateDA, ButtonReleaseMask, FALSE,
		  rc_stop_crosshairs, &rc_ch_data);
	
	
	XtAddCallback(rtstgTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(rtqTxt,     XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(rs_dateTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(rs_valueTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);


	return;
}


void	draw_curve(Widget w, XtPointer ptr, XtPointer cbs)
{
	draw_rating(w);
	return;
}


void	import_file(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	list;
   Arg		args[10];
   int		ac;
   XmString	str;
   char		import_dir[128];
   int		gad_token_len=0, gad_value_len=0;
   
   
   if(! fileSB)
   {
      fileSB = XmCreateFileSelectionDialog(GetTopShell(w), "fileSB", NULL, 0);
      
      
      /* set XmNpattern & XmNdialogStyle */
      str = XmStringCreateSimple("*.rating");
      ac = 0;
      XtSetArg(args[ac], XmNpattern, str); ac++;
      XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;

      gad_token_len = strlen("whfs_import_dir");
      get_apps_defaults("whfs_import_dir", &gad_token_len, import_dir, &gad_value_len);
      str = XmStringCreateSimple(import_dir);
      XtSetArg(args[ac], XmNdirectory, str); ac++;
      XtSetValues(fileSB, args, ac);
      XmStringFree(str);
      
      
      XtAddCallback(fileSB, XmNcancelCallback, close_file, NULL);
      XtAddCallback(fileSB, XmNokCallback, select_file, NULL);
      
      list = XmFileSelectionBoxGetChild(fileSB, XmDIALOG_LIST);
      XtAddCallback(list, XmNdefaultActionCallback, select_file, NULL);
   }   
   
   SetTitle(fileSB,"Import File Selection");
   if(! XtIsManaged(fileSB))
      XtManageChild(fileSB);
   
   return;
}



void	select_file(Widget w, XtPointer ptr, XmFileSelectionBoxCallbackStruct *cbs)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        FILE		*fp;
	char		*name,
			*str,
			*s1,
			*s2,
			buf[MAX_BUF_LEN];
	int		i ;
	Rating		* rate = NULL ;
	
	
	if(XtIsManaged(fileSB))
	{
	   XtDestroyWidget(fileSB);
	   fileSB = NULL;
	}
	
	if (! XmStringGetLtoR(cbs->value, charset, &name))
		return;
		
	if (! *name)
	{
		XtFree(name);
		return;
	}

	if ((fp = fopen(name, "r")) == (FILE *) NULL)
	{
		return;
	}	
	
	
	/*
		Make sure it is free before malloc any more.
	*/
	if (rating)
	{
	   FreeRating(rating);
	   rating = NULL;
	}	
	rating = (Rating *) malloc(sizeof(Rating));
	ListInit(&rating->list);
	
	rating->node.next = NULL;
	rating->node.prev = NULL;
	
	for (i = 0; fgets(buf, sizeof(buf), fp); i++)
	{
		if (i > 4)
		{
			str = buf;						
			s1 = strtok(str, " \t");
			str =  NULL;
			s2 = strtok(str, " \n");

			/* if either string is missing then skip that line */
			if ((s1 == NULL) || (s2 == NULL))
				continue;

			if ((rate = (Rating *) malloc(sizeof(Rating))) == NULL)
			{
				return;
			}
			
			
			memset(rate, '\0', sizeof(Rating));
			strcpy(rate->lid, (char *)rate_lid);
			rate->stage  = atof(s1);
			rate->discharge = atof(s2);
			ListAdd(&rating->list, &rate->node);
		}	
	}
	
	
	load_list(rating);
	set_curve(NULL, NULL, NULL);
	XtFree(name);	
	fclose(fp);
	
	return;
}


void	close_file(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(fileSB))
   {
      XtDestroyWidget(fileSB);
      fileSB = NULL;
   }
   
   return;
}


void	set_curve(Widget w, XtPointer ptr, XtPointer cbs)
{
   
   if (w == rtclearPB)
      drawCurve = False;	
   else 
      drawCurve = True;
   
   
   draw_curve(rateDA, NULL, NULL);	
   rate_sensitize();
   
   return;
}


void	load_list(Rating *rating)
{
	XmStringTable	xmStr;
	Arg		arg[5];
	char		buf[MAX_BUF_LEN];
	int		cnt = 0 ,
			i;
	Rating		*rtPtr = NULL;
	float		resultantStage = 0;
			
	XmTextSetString(rtstgTxt, "");
	XmTextSetString(rtqTxt, "");
	
        if ( rating != NULL )
        {
   	   if ((cnt = ListCount(&rating->list)) >= 0)
	   {
	   	   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
		   rtPtr = (Rating *) ListFirst(&rating->list);
		   if (rtPtr != NULL)
		   {
		     /* load the BASE Rating Curve */
		     for (i = 0; rtPtr; i++)
	   	     {
		   	   sprintf(buf, "%8.2f        %8.1f",
				   rtPtr->stage, rtPtr->discharge);
			   xmStr[i] = XmStringCreateSimple(buf);
			   rtPtr = (Rating *) ListNext(&rtPtr->node);			
		     }
		
		
		     XtSetArg(arg[0], XmNitemCount, cnt);
		     XtSetArg(arg[1], XmNitems, xmStr);
		     XtSetValues(rateLB, arg, 2);

		     for(i = 0; i < cnt; i++)
		   	XmStringFree(xmStr[i]);
		     XtFree((char *) xmStr);

		     /* load the Shifted Rating Curve */
	   	     xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
		     rtPtr = (Rating *) ListFirst(&rating->list);
		     for (i = 0; rtPtr; i++)
	   	     {
			resultantStage = rtPtr->stage + shiftValue;
		   	   sprintf(buf, "%8.2f        %8.1f",
				   resultantStage, rtPtr->discharge);
			   xmStr[i] = XmStringCreateSimple(buf);
			   rtPtr = (Rating *) ListNext(&rtPtr->node);			
		     }
		
		
		     XtSetArg(arg[0], XmNitemCount, cnt);
		     XtSetArg(arg[1], XmNitems, xmStr);
		     XtSetValues(rateresultLB, arg, 2);

		     for(i = 0; i < cnt; i++)
		   	XmStringFree(xmStr[i]);
		     XtFree((char *) xmStr);

		  } /* if rtPtr NOT Null */

	   } /* if cnt greater than Zero */

        } /* if rating is NOT Null */
	
	XmListSelectPos(rateLB,1,True);
	rate_sensitize();
	
	return;
}


void	load_shift_list(RatingShift *ratingShift)
{
	XmStringTable	xmStr;
	Arg		arg[5];
	char		buf[MAX_BUF_LEN],
	      		datestring[DATE_LEN+1];
	int		cnt=0, i=0, shiftFound=0, position_at=0;
	RatingShift	*rsPtr = NULL;
			
	XmTextSetString(rs_dateTxt, "");
	XmTextSetString(rs_valueTxt, "");
	
        shiftValue = 0.0; /* initialize shift value */
	shiftFound = 0;        
	
        if ( ratingShift != NULL )
        {
   	   if ((cnt = ListCount(&ratingShift->list)) > 0)
	   {
	   	   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
		   rsPtr = (RatingShift *) ListFirst(&ratingShift->list);
		   if (rsPtr != NULL)
		   {
		     /* load the Rating Shifts */
		     for (i = 0; rsPtr; i++)
	   	     {
	   	     	   if (rsPtr->active[0] == 'T' && shiftFound == 0)
	   	     	   {
				shiftValue = rsPtr->shift_amount;
				shiftFound = 1;
				position_at = i+1;
	   	     	   }

                           date_t_to_USA_date ( rsPtr->date, datestring);
		   	   sprintf(buf, "%10s      %5.1f       %1s", datestring, rsPtr->shift_amount, rsPtr->active);
			   xmStr[i] = XmStringCreateSimple(buf);
			   rsPtr = (RatingShift *) ListNext(&rsPtr->node);			
		     }
		
		
		     XtSetArg(arg[0], XmNitemCount, cnt);
		     XtSetArg(arg[1], XmNitems, xmStr);
		     XtSetValues(rshiftLB, arg, 2);

		     for(i = 0; i < cnt; i++)
		   	XmStringFree(xmStr[i]);
		     XtFree((char *) xmStr);

		  } /* if rsPtr NOT Null */

	   } /* if cnt greater than Zero */

        } /* if ratingShift is NOT Null */
	
	XmListSelectPos(rshiftLB, position_at, True);
	shift_sensitize();
	
	return;
}


void 	rate_modify(Widget w, XtPointer ptr, XtPointer cbs)
{

	Rating	*rPtr = NULL , *oldPtr = NULL , *newPtr = NULL ;
	char	c_stage[BUFSIZ],
		c_flow[BUFSIZ];
	double	stage,
		flow;
	int	success = 0,
		cnt = 0,
		pos,
		*poslist;
	
	/*
		Get strings from XmTexts.
		Check for empty strings.
	*/
	
	strcpy(c_stage,XmTextGetString(rtstgTxt));
	strcpy(c_flow,XmTextGetString(rtqTxt));
	
	if ( (strlen(c_stage) == 0) || (strlen(c_flow) == 0) )
	{
		ErrorDialog(rateDS,"Null fields may not be entered.");
		return;
	}


	/*
		Convert strings to doubles and check for negativity.
	*/
	
	stage = atof(c_stage);
	flow  = atof(c_flow);
	
	if (flow < 0)
	{
		ErrorDialog(rateDS,"Negative flow values may not be entered.");
		return;
	}

	/*
		malloc new node,check for success, assign entered values
	*/
	
	newPtr = (Rating *) malloc(sizeof(Rating));
	if (newPtr)
	{	
		strcpy(newPtr->lid,rate_lid);
		newPtr->stage = stage;
		newPtr->discharge = flow;
	}
	else
	{
		ErrorDialog(rateDS,"Unable to allocate memory for new stage\n");
		return;
	}
	
	/*
		Find the selected record, check for errors, and then find the
		associated linked-list node.
	*/
	
	XmListGetSelectedPos(rateLB,&poslist,&cnt);
	if (cnt == 1)
	{
		pos = poslist[0];
		XtFree( (char *) poslist);
	
                if ( rating != NULL )
			oldPtr = (Rating*)ListNth(&rating->list,pos);
	}
	else /* NO list yet so this will be the ONLY entry */
	{
		/* Make sure it is free before malloc any more.	*/
		if (rating)
		{
		   FreeRating(rating);
		   rating = NULL;
		}	
		rating = (Rating *) malloc(sizeof(Rating));
		ListInit(&rating->list);
		rating->node.next = NULL;
		rating->node.prev = NULL;

		ListAdd(&rating->list, &newPtr->node);

		load_list(rating);
		set_curve(NULL,NULL,NULL);
		rate_sensitize();
		return;
	}
	
	/*
		Go through list, looking for place to insert new node.
	*/

        /* First check to make certain that the "rating" pointer is 
           not NULL. */
	if ( rating != NULL )
           rPtr = (Rating *) ListFirst(&rating->list);
        else
           return;

	while(rPtr)
	{
		if (stage == rPtr->stage) 
		{
			if (rPtr == oldPtr)  /* replacing record with same stage */
			{
				ListInsert(&rating->list,(Node *)oldPtr,(Node *)newPtr);
				ListDelete(&rating->list,(Node *) oldPtr);
				success = 1;
				break;
			}
			else 
			{
				ErrorDialog(rateDS,
					    "Duplicate stages may not be entered.");
				free(newPtr);
				return;
			}
		}
		else if (stage < rPtr->stage)
		{	
			ListInsert(&rating->list,(Node *)rPtr,(Node *)newPtr);
			success = 1;
			break;
		}
		rPtr = (Rating *) ListNext(&rPtr->node);
	}
	
	
	/* 
	    if (rPtr == NULL ), new node must be last in list. 
	    So can't use ListInsert, must use ListAdd instead
	*/
	
	if (rPtr == NULL)
	{
		ListAdd(&rating->list, &newPtr->node);
		success = 1;
	}
	
	
	/*
		check and free newPtr.
	*/
	
	if (! success)
	{
		free(newPtr);
		newPtr = NULL;
		return;
	}

	if (rating != NULL)
		load_list(rating);
   
	set_curve(NULL,NULL,NULL);
	rate_sensitize();
}




void 	shift_modify(Widget w, XtPointer ptr, XtPointer cbs)
{

	RatingShift	*rPtr = NULL , *oldPtr = NULL , *newPtr = NULL ;
	char	c_value[BUFSIZ],
		c_date[BUFSIZ],
		c_active[2];
	double	value;
	int	success = 0,
		cnt = 0,
		pos,
		*poslist;
	long	date=0;
	char 	*buf, msg[MAX_WHERE_LEN];
	
	/*	Get strings from XmTexts and check for empty strings.	*/
	
	strcpy(c_value,XmTextGetString(rs_valueTxt));
	strcpy(c_date, XmTextGetString(rs_dateTxt));
	
	if ( (strlen(c_value) == 0) || (strlen(c_date) == 0) )
	{
		ErrorDialog(rateDS,"Null fields may not be entered.");
		return;
	}

	if ( (buf = XmTextGetString(rs_dateTxt)) )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(rateDS, msg);
		  /* leave function, do not save data */
		  return;
		}
		else
		{
		  if ( USA_date_to_date_t(buf, &date) != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
			    buf);
		    ErrorDialog(rateDS, msg);
		    /* leave function, do not save data */
		    return;
		  }
		}
		XtFree(buf);
	}

	/* Convert string to double */
	value = atof(c_value);
	
	if (XmToggleButtonGetState(rs_activeTB))
		strcpy(c_active, "T");
	else
		strcpy(c_active, "F");
	

	/* malloc new node,check for success, assign entered values */
	
	newPtr = (RatingShift *) malloc(sizeof(RatingShift));
	if (newPtr)
	{	
		strcpy(newPtr->lid,rate_lid);
		strcpy(newPtr->active, c_active);
		newPtr->shift_amount = value;
		newPtr->date  = date;
	}
	else
	{
		ErrorDialog(rateDS,"Unable to allocate memory for new shift\n");
		return;
	}
	
	/*
		Find the selected record, check for errors, and then find the
		associated linked-list node.
	*/
	
	XmListGetSelectedPos(rshiftLB,&poslist,&cnt);
	if (cnt > 0)
	{
		pos = poslist[0];
		XtFree( (char *) poslist);
	
                if ( ratingShift != NULL )
                {
	  	   oldPtr = (RatingShift*)ListNth(&ratingShift->list,pos);
                }
	}
	else
	{
                if ( ratingShift == NULL ) /* No Rating Shift linked list exists */
			ratingShift = (RatingShift *) malloc(sizeof(RatingShift));

		ListInit(&ratingShift->list);

		strcpy(ratingShift->lid, newPtr->lid);
		strcpy(ratingShift->active, newPtr->active);
		ratingShift->shift_amount = newPtr->shift_amount;
		ratingShift->date = newPtr->date;
		oldPtr = (RatingShift*)ListFirst(&ratingShift->list);
	}
	
	/* Go through list, looking for place to insert new node. */

        /* First check to make certain that the pointer is not NULL. */
	if ( ratingShift != NULL )
        {
           rPtr = (RatingShift *) ListFirst(&ratingShift->list);
        }

	while(rPtr)
	{
		if (date == rPtr->date) 
		{
			if (rPtr == oldPtr)  /* replacing record with same date */
			{
				ListInsert(&ratingShift->list,(Node *)oldPtr,(Node *)newPtr);
				ListDelete(&ratingShift->list,(Node *) oldPtr);
				success = 1;
				break;
			}
			else 
			{
				ErrorDialog(rateDS,
					    "Duplicate dates may not be entered.");
				free(newPtr);
				return;
				
			}
		}
		else if (date > rPtr->date)
		{	
			ListInsert(&ratingShift->list,(Node *)rPtr,(Node *)newPtr);
			success = 1;
			break;
		}
		rPtr = (RatingShift *) ListNext(&rPtr->node);
	}
	
	
	
	/* 
	    if (rPtr == NULL ), new node must be last in list. 
	    So can't use ListInsert, must use ListAdd instead
	*/
	
	if (rPtr == NULL)
	{
		ListAdd(&ratingShift->list, &newPtr->node);
		success = 1;
	}
	
	
	/*
		If have never put newnode in list, free the newPtr.
	*/
	
	if (! success )
	{
		free(newPtr);
		newPtr = NULL;
		return;
	}
		
	if (ratingShift != NULL)  load_shift_list(ratingShift);
 	if (rating != NULL)  load_list(rating);
  
	set_curve(NULL,NULL,NULL);
	shift_sensitize();
}


void	current_stage(Widget w, XtPointer ptr, XtPointer cbs)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        XmString        *xmStr = NULL;
        Arg             arg[2];
        char            *text = NULL,
                        *stg = NULL,
                        *flow = NULL;

        /*
                Get the currently selected list item.
        */
        XtSetArg(arg[0], XmNselectedItems, &xmStr);
        XtGetValues(rateLB, arg, 1);


        /*
                Parse the selected items to get the
                station identifier.  If successful,
                show the Precip dialog, else show
                the error dialog.
        */
        stg  = 0;
        flow = 0;
        if (XmStringGetLtoR(xmStr[0], charset, &text))
        {
                stg  = strtok(text, " \t");
                text = NULL;
                flow = strtok(text, " \n");
                
                XmTextSetString(rtstgTxt, stg);
                XmTextSetString(rtqTxt, flow);
        }      	
	return;
}


void	current_shift(Widget w, XtPointer ptr, XtPointer cbs)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        XmString        *xmStr = NULL;
        Arg             arg[2];
        char            *text = NULL,
                        *date = NULL,
                        *value = NULL,
                        *active = NULL;

        /*
                Get the currently selected list item.
        */
        XtSetArg(arg[0], XmNselectedItems, &xmStr);
        XtGetValues(rshiftLB, arg, 1);


        /*
                Parse the selected items to get the
                station identifier.
        */
        date  = 0;
        value = 0;
        if (XmStringGetLtoR(xmStr[0], charset, &text))
        {
                date  = strtok(text, " \t");
                text = NULL;
                value = strtok(text, " \n");
                text = NULL;
                active = strtok(text, " \n");
                
                XmTextSetString(rs_dateTxt, date);
                XmTextSetString(rs_valueTxt, value);
		if (active[0] == 'T')
			XmToggleButtonSetState(rs_activeTB, True, False);
		else
			XmToggleButtonSetState(rs_activeTB, False, False);
        }      	

	return;
}


void	rate_remove_conf(Widget w, XtPointer ptr, XtPointer cbs) 
{
        Widget	qstDS,
		okPB;
        char	buf[MAX_BUF_LEN];

	
        sprintf(buf, "This will remove the highlighted pair.");
        qstDS = QuestionDialog(rateDS, buf);	                
        SetTitle(qstDS, "Remove Base Rating Point Confirmation");

	
        /*
                Get the XmMessageBox ok button,
                and associate a callback to it.
        */       
	okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);              
        XtAddCallback(okPB, XmNactivateCallback, rate_remove, NULL);              		
        rate_sensitize();
	
        return;
}



void	rate_remove(Widget w,  XtPointer ptr, XtPointer cbs) 
{
   Rating	*rPtr = NULL ;
   
   int		pos = 0,
      		*poslist = NULL ,
      		cnt = 0;
   
   XmListGetSelectedPos(rateLB, &poslist, &cnt);
   
   if (cnt > 0)
   {
      pos = poslist[0];
      if ( rating != NULL )
      {
         rPtr = (Rating *) ListNth(&rating->list, pos);
         ListDelete(&rating->list, &rPtr->node);
      }
   }

   if ( rating != NULL ) load_list ( rating ) ;
   
   if (pos > ListRsrcGetCount(rateLB))
   {
      pos = pos - 1;
   }

   XmListSetPos(rateLB, pos);
   XmListSelectPos(rateLB, pos, True);

   set_curve(NULL,NULL,NULL);

   XtFree((char *) poslist); 
   rate_sensitize();
   
   return;
}


void	shift_remove_conf(Widget w, XtPointer ptr, XtPointer cbs) 
{
        Widget	qstDS,
		okPB;
        char	buf[MAX_BUF_LEN];

	
        sprintf(buf, "This will remove the highlighted shift.");
        qstDS = QuestionDialog(rateDS, buf);	                
        SetTitle(qstDS, "Remove Shift Confirmation");
	
        /*
                Get the XmMessageBox ok button,
                and associate a callback to it.
        */       
	okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);              
        XtAddCallback(okPB, XmNactivateCallback, shift_remove, NULL);
	
        return;
}



void	shift_remove(Widget w,  XtPointer ptr, XtPointer cbs) 
{
   RatingShift	*rsPtr = NULL ;
   
   int		pos = 0,
      		*poslist = NULL ,
      		cnt = 0;
   
   XmListGetSelectedPos(rshiftLB, &poslist, &cnt);
   
   if (cnt > 0)
   {
      pos = poslist[0];
      if ( ratingShift != NULL )
      {
         rsPtr = (RatingShift *) ListNth(&ratingShift->list, pos);
         ListDelete(&ratingShift->list, &rsPtr->node);
      }
   }

   if ( ratingShift != NULL ) load_shift_list ( ratingShift ) ;
   if ( rating != NULL ) load_list ( rating ) ;
  
   if (pos > ListRsrcGetCount(rshiftLB))
   {
      pos = pos - 1;
   }

   XmListSetPos(rshiftLB, pos);
   XmListSelectPos(rshiftLB, pos, True);

   XtFree((char *) poslist); 
   shift_sensitize();
   
   return;
}


void	rate_clear_conf(Widget w, XtPointer ptr, XtPointer cbs) 
{
        Widget		qstDS,
			okPB;
        char		buf[MAX_BUF_LEN];


        sprintf(buf, "This will clear the list for %s.", rate_lid);
        qstDS = QuestionDialog(rateDS, buf);
        SetTitle(qstDS, "Clear Confirmation");

	
        /*
	         Get the XmMessageBox ok button,
                 and associate a callback to it.
        */
        okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);                
        XtAddCallback(okPB, XmNactivateCallback, rate_clear, NULL); 
       
	
	if(! XtIsManaged(qstDS))
	   XtManageChild(qstDS);

        return;
}


void	rate_clear(Widget w, XtPointer ptr, XtPointer cbs)
{
	clear_list();
	if (rating)
	{
	   FreeRating(rating);
	}
	rating = (Rating *) malloc(sizeof(Rating));
	ListInit(&rating->list);
	
	rating->node.next = NULL;
	rating->node.prev = NULL;
	
	set_curve(NULL,NULL,NULL);
	rate_sensitize();
	
	return;
}


void	clear_list(void)
{
	Arg	arg[2];
	int	cnt;
	
	XtSetArg(arg[0], XmNitemCount, &cnt);
	XtGetValues(rateLB, arg, 1);	
	if (cnt) 
	{
		XmListDeleteAllItems(rateLB);
		DeSensitize(rtimpPB);
	}
	XmTextSetString(rtstgTxt,"");
	XmTextSetString(rtqTxt,"");
	
	if (rating)
	{
	   FreeRating(rating);
	   rating = NULL;
	}
	rate_sensitize();
	
	return;
}


void	rate_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   rate_save(NULL, NULL, NULL);
   close_rate(NULL, NULL, NULL);
   
   return;
}


void	rate_save(Widget w, XtPointer ptr, XtPointer cbs)
{
   Rating	* rtPtr = NULL ;
   RatingShift	* rsPtr = NULL ;
   char		where[MAX_WHERE_LEN];
   
   
   sprintf(where, " WHERE lid = '%s' ", rate_lid);	
   
   DeleteRating(where);
   if ( rating != NULL )
   {
      if (ListCount(&rating->list) > 0 )
      {
         rtPtr = (Rating *) ListFirst(&rating->list);
         while (rtPtr)
         {
	     PutRating(rtPtr);
	     rtPtr = (Rating *) ListNext(&rtPtr->node);
         }
      }
   }
   
   DeleteRatingShift(where);
   if ( ratingShift != NULL )
   {
      if (ListCount(&ratingShift->list) > 0 )
      {
         rsPtr = (RatingShift *) ListFirst(&ratingShift->list);
         while (rsPtr)
         {
	     PutRatingShift(rsPtr);
	     rsPtr = (RatingShift *) ListNext(&rsPtr->node);
         }
      }
   }

   return;
}


void	rate_del_conf(Widget w, XtPointer ptr, XtPointer cbs) 
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "This will delete the rating curve for %s.", rate_lid);
   qstDS = QuestionDialog(rateDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, rate_delete, NULL);          
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	rate_delete(Widget w, XtPointer ptr, XtPointer cbs)
{        
	char	where[MAX_WHERE_LEN];
	
	
	SetCursor(rateFM, XC_watch);

	
	sprintf(where, " WHERE lid = '%s' ", rate_lid);
	DeleteRating(where);
	
	if (rating)
	{
	   FreeRating(rating);
	   rating = NULL;
	}
	rating = (Rating *) malloc(sizeof(Rating));
	ListInit(&rating->list);
	
	/*Make sure the "prev" and "next" pointers of the node memberof the
	rating are initially set to NULL (01/2002)  */

	rating->node.next = NULL;
	rating->node.prev = NULL;
	
	clearForm(rateFM);
	XmListDeleteAllItems(rateLB);
        rate_sensitize();
	
	
	UnsetCursor(rateFM);
	
	
	return;
}

void	close_rate(Widget w, XtPointer ptr, XtPointer cbs)
{
   clear_list();
   if(XtIsManaged(rateDS))
   {
      XtDestroyWidget(rateDS);
      rateDS = NULL;
   }
   if (hw)
   {
      FreeHwStages(hw);
      hw = NULL;
   }
   if (rating)
   {
      FreeRating(rating);
   }
   
   return;
}


void	rate_sensitize(void)
{
	int	count = 0 ;
	
        if ( rating != NULL ) count = ListCount(&rating->list);
	
	if (count > 0)
	{
		DeSensitize(rtimpPB);
		Sensitize(rtclearPB);
		Sensitize(rtremPB);
		Sensitize(rtmodifyPB);
	}	
	else
	{	
		Sensitize(rtimpPB);
		DeSensitize(rtclearPB);
		DeSensitize(rtremPB);
		XmListDeleteAllItems(rateLB);
		XmListDeleteAllItems(rateresultLB);
		XmTextSetString(rtstgTxt,"");
		XmTextSetString(rtqTxt,"");
	}
}


void	shift_sensitize(void)
{
	int	count = 0 ;
	
        if ( ratingShift != NULL ) count = ListCount(&ratingShift->list);
	
	if (count > 0)
	{
		Sensitize(shiftdelPB);
		Sensitize(shiftapplyPB);
	}	
	else
	{	
		DeSensitize(shiftdelPB);
		XmListDeleteAllItems(rshiftLB);
		XmTextSetString(rs_dateTxt,"");
		XmTextSetString(rs_valueTxt,"");
		shiftValue = 0;
	}
}

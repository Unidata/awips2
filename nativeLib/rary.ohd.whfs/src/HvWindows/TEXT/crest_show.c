/*****************************************************************
	File:		crest_show.c
	Date:		April 1995
	Author:		Dale Shelton, Chip Gobs
	
	Purpose:	Provide support for the Crest History DS.
******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <X11/cursorfont.h>
/*******
#include <sqlhdr.h>
*******/

#include "DbmsDefs.h"
#include "Crest.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Xtools.h"

#include "crest_show.h"
#include "GeneralUtil.h"
#include "crest.h"
#include "ParamDefs.h"
#include "ToolDefs.h"
#include "time_convert.h"
#include "set_window_title.h"

/*************************
 The crest_show() function 
 *************************/

void	crest_show(const Widget w, const char *lid, Boolean editable)

{  /* Opening brace of crest_show() function */

	crest = (Crest *) NULL;	

	if (! crestDS)
	{
		create_crestDS(GetTopShell(w));
		crest_callbacks();
	}

	if (! XtIsManaged(crestDS))
	{
		strcpy(crst_lid, lid);		
                set_title(crestDS, "Crest History", crst_lid);
		XtManageChild(crestFM);
		XtManageChild(crestDS);	

           if ( ! editable )
           {
		XtUnmanageChild(crstokPB);
		XtUnmanageChild(crstapplyPB);
		XtUnmanageChild(crstnewPB);
		XtUnmanageChild(crstdelPB);
           }
		/* initially set sort menu option to Stage */ 
		SetMenuPos(crstsortOM, CREST_STAGE);
		crest_call_load(crstslctOM, NULL, NULL);	
	}
	
	return;

}  /* Closing brace of crest_show() function */ 

/******************************
 The crest_callbacks() function 
 ******************************/

void	crest_callbacks(void)

{  /* Opening brace of crest_callbacks() function */ 

	Atom	wmAtom;
	
       /*************************
        Window manager callbacks.
        *************************/

	wmAtom = XmInternAtom(XtDisplay(crestDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(crestDS, wmAtom, crest_close, NULL);
	
       /*****************
        Widget callbacks.
	*****************/

	XtAddCallback(crstdrwDA, XmNexposeCallback, crest_draw_cb, NULL);
	
       /**************************
        Fill the widgets with data 
	**************************/

	XtAddCallback(crestLB,   XmNdefaultActionCallback,   crest_import, NULL);
	XtAddCallback(crestLB,   XmNbrowseSelectionCallback, crest_import, NULL);
	
       /**********************************************************************
	When a record in the list is picked, it is highlighted in the picture.
	So redraw the picture.
	**********************************************************************/

	XtAddCallback(crestLB,     XmNdefaultActionCallback,   crest_draw_cb,  NULL);
	XtAddCallback(crestLB,     XmNbrowseSelectionCallback, crest_draw_cb,  NULL);
	XtAddCallback(crstokPB,    XmNactivateCallback,        crest_ok,       NULL);
	XtAddCallback(crstapplyPB, XmNactivateCallback,        crest_apply,    NULL);
	XtAddCallback(crstnewPB,   XmNactivateCallback,        crest_new,      NULL);
	XtAddCallback(crstdelPB,   XmNactivateCallback,        crest_del_conf, NULL);
	XtAddCallback(crstclsPB,   XmNactivateCallback,        crest_close,    NULL);	
	
       /**********************
        Option menu callbacks.
        **********************/

	XtAddCallback(crstallPB,   XmNactivateCallback, crest_call_load, NULL);
	XtAddCallback(crsthwPB,    XmNactivateCallback, crest_call_load, NULL);
	XtAddCallback(crstlwPB,    XmNactivateCallback, crest_call_load, NULL);
        XtAddCallback(crstdatePB,  XmNactivateCallback, crest_call_load, NULL); 
        XtAddCallback(crstflowPB,  XmNactivateCallback, crest_call_load, NULL); 
        XtAddCallback(crststagePB, XmNactivateCallback, crest_call_load, NULL); 
	
       /**************************************************************
        Event Handler for pressing the button on the main drawing area
	**************************************************************/

	XtAddEventHandler(crstdrwDA, ButtonPressMask, FALSE, select_crest, NULL);
	
       /*********************
        TextFilter callbacks.
        *********************/

	crest_add_textfilter_callbacks();
	
	return;

}  /* Closing brace of crest_callbacks() function */ 

/*********************************************
 The crest_add_textfilter_callbacks() function 
 *********************************************/

void	crest_add_textfilter_callbacks(void)

{  /* Opening brace of crest_add_textfilter_callbacks() */

	XtAddCallback(crststgTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(crstqTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	XtAddCallback(crstdatTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(crsttimTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_COLONS);

	return;

}  /* Closing crest_add_textfilter_callbacks() function */ 

/************************************************
 The crest_remove_textfilter_callbacks() function 
 ************************************************/

void	crest_remove_textfilter_callbacks(void)
 
{  /* Opening brace of crest_remove_textfilter_callbacks() function */

	XtRemoveCallback(crststgTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtRemoveCallback(crstqTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	XtRemoveCallback(crstdatTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtRemoveCallback(crsttimTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_COLONS);

	return;

}  /* Closing brace of crest_remove_textfilter_callbacks() function */ 

/***********************
 The crest_ok() function 
 ***********************/

void	crest_ok(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of crest_ok() function */ 

	if (crest_save(w))
            crest_close(w, NULL, NULL);

	return;

}  /* Closing brace of crest_ok() function */ 

/**************************
 The crest_apply() function 
 **************************/

void	crest_apply(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of crest_apply() function */ 

   	(void) crest_save(w);

	return;

}  /* Closing brace of crest_apply() function */

/*************************
 The crest_save() function 
 *************************/

int	crest_save(Widget w)

{  /* Opening brace of crest_save() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	Crest		crest;
	char		key[MAX_BUF_LEN];
        char      	msg[MAX_BUF_LEN];
        char            tmpstage[BUFSIZ], tmpq[BUFSIZ];
        char           *buf;
        long  		date;
        int             error;
        int             pos;
        int             rtn;
        int             button_selected;

       /**********************************
        Associate the correct location id.
        **********************************/

	memset(&crest,'\0',sizeof(crest));
	strcpy(crest.lid, crst_lid);
	
      /************************************
       Ensure a good date value is entered,
       else display an error message.
       ************************************/

	if ( (buf = XmTextGetString(crstdatTxt)) )
        {    
		if (four_digit_year(buf) != 0)
		{
                    sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
		            buf);

		    ErrorDialog(crestDS, msg);

		    return(False);  /* Exit function.  Do not save data. */ 
		}

		else
		{
                    /* Modified by BryonL, Jan 25, 2005. */
		    if ( USA_date_to_date_t (buf, &date) != 0)
		    {
		        sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
		                buf);

		        ErrorDialog(crestDS, msg);

		        return(False);  /* Exit function.  Do not save data. */
		    }

		    else
        	         crest.datcrst = date;
         	}
        			
		XtFree(buf);
	}
	
       /***********************************************************************
        Determine if XmToggleButton widgets are set to true. If so, associate a 
	value of 'X' with the related field.	
        ***********************************************************************/

	if (XmToggleButtonGetState(crsthwTB))
            strcpy(crest.hw, "X");
		
	if (XmToggleButtonGetState(crsticeTB))
	    strcpy(crest.jam, "X");
		
	if (XmToggleButtonGetState(crstodTB))
	    strcpy(crest.olddatum, "X");
		
	if (XmToggleButtonGetState(crstsuprsTB))
	    strcpy(crest.suppress, "X");
	
	button_selected = GetMenuPos(crestcatOM);

	switch (button_selected)
	{

	   case PRELIM_CREST:
			strcpy(crest.prelim, "P");
			break;

	   case RECORD_CREST:
			strcpy(crest.prelim, "R");
			break;
	      
	   case OFFICIAL_CREST:
	   default:
			strcpy(crest.prelim, "O");
			break;

	}


       /*******************************
	Get values from XmText widgets.
        *******************************/

        strcpy (tmpstage, XmTextGetString(crststgTxt));
        strcpy (tmpq, XmTextGetString(crstqTxt));
        if ( (!IsBlankStr(tmpstage))  || (!IsBlankStr(tmpq)) )   //Checks to see if at least one or the other string has some value in it
        {
	   if (!IsBlankStr(tmpstage))
	       crest.stage = atof(tmpstage);                     //If stage is not empty, then sets crest.stage to tmpstage else sets it to NULL
	   else
              (void) SetNull(DOUBLE, (void *) &crest.stage); 
           if (!IsBlankStr(tmpq))
              crest.q = atol(tmpq);                              //If flow is not empty, then sets crest.q to tmpq else sets it to NULL
           else
              (void) SetNull(INT, (void *) &crest.q); 
	}
        else
        {
           sprintf(msg, "You must enter either a stage value or a flow value...\n");
           ErrorDialog(crestDS, msg);
           return(False);
        }	

       /***********************************************************
 	Check for a valid time (hh:mm) entered.
	If blank or "UNDEF", then set to "UNDEF".
	If string is less than 5 characters, then reject.
	If the third charater is not a colon or the first, second, 
	fourth or fifth character is a colon, then reject. 
        If 00 > hh > 23 or 00 > mm > 59, then reject.
        ***********************************************************/
	
        if ( (buf = XmTextGetString(crsttimTxt)) )
        {
	    if (strlen(buf) == 0 || strcmp(buf, "UNDEF") == 0)
        	strcpy(crest.timcrst, "UNDEF");      		

            else if ( (strlen(buf) < 5 ) || (buf[2] != ':')
       		       || (buf[0] == ':') || (buf[1] == ':')
       		       || (buf[3] == ':') || (buf[4] == ':') )
            {
			sprintf(msg, "Invalid Time format '%s' entered.\nFormat should be 'hh:mm'.", buf);
			ErrorDialog(crestDS, msg);
			return(False);
	    }

            else if ( ( (strncmp(buf, "00", 2) >= 0 && strncmp(buf, "09", 2) <= 0)
		         || (strncmp(buf, "10", 2) >= 0 && strncmp(buf, "19", 2) <= 0)
		         || (strncmp(buf, "20", 2) >= 0 && strncmp(buf, "23", 2) <= 0) )
		        &&
		         ( (buf[3] >= '0' && buf[3] <= '5') && (buf[4] >= '0' && buf[4] <= '9') ) )

        		 strcpy(crest.timcrst, buf);      		

           else
	   {
	        sprintf(msg, "Invalid Hour and/or Minute '%s' entered.", buf);
		ErrorDialog(crestDS, msg);
		return(False);
	   }

		XtFree(buf);
	}
		
	if ( (buf = XmTextGetString(crstrmkTxt)) )
	{
            memset(crest.cremark, '\0', sizeof(crest.cremark));

	    if (IsNull(CHAR, buf) == NOTNULL)
		strcpy(crest.cremark, buf);

            XtFree(buf);
	}
	
       /*****************************************
	Insert/Update the entered dbms record, 
	based on the value of the state variable.
        *****************************************/

	rtn=True;

	if (crst_state)
	{
            crest_key(key, &pos);

            if (strlen(key))
	    {
		if ((error = UpdateCrest(&crest, key)) != 0)
		{
	            sprintf(msg, "Unable to update record: %d\n", error);
	            ErrorDialog(crestDS, msg);
	            rtn = False;
		}
	    }
	}

	else
	{
	    if ((error = PutCrest(&crest)) != 0)
	    {
	 	sprintf(msg, "Unable to insert record: %d\n", error);
		ErrorDialog(crestDS, msg);
		rtn = False;
	    }
	}
	
       /*****************************
	Set state to True and return.
        *****************************/

	crest_call_load(w, NULL, NULL);

	return(rtn);

}  /* Closing brace of crest_save() function */ 

/************************
 The crest_new() function 
 ************************/

void	crest_new(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of crest_new() function */

	crest_clear();
	Sensitize(crstokPB);
	Sensitize(crstapplyPB);
	crst_state = False;
	return;

}  /* Closing brace of crest_new() function */ 

/*****************************
 The crest_del_conf() function 
 *****************************/

void	crest_del_conf(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of crest_del_conf() function */ 

  /******************************************
   Declaration of automatic (local) variables
   ******************************************/

   Widget	qstDS;
   Widget       okPB;
   char		buf[MAX_BUF_LEN];
   
   sprintf(buf, "Do you wish to delete this entry?");

   qstDS = QuestionDialog(crestDS, buf);

   SetTitle(qstDS,"Delete Confirmation");
   
  /***************************************************************
   Get the XmMessageBox ok button, and associate a callback to it.
   ***************************************************************/

   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);

   XtAddCallback(okPB, XmNactivateCallback, crest_delete, NULL);
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;

}  /* Closing brace of crest_del_conf() function */ 

/***************************
 The crest_delete() function 
 ***************************/

void	crest_delete(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of crest_delete() function */ 

  /******************************************
   Declaration of automatic (local) variables
   ******************************************/

   char		key[MAX_BUF_LEN];
   int		pos;
   
  /********************************************************
   Delete record from dbms, and reload list and select pos.
   ********************************************************/

   SetCursor(crestFM, XC_watch);
   crest_key(key, &pos);

   if (strlen(key))
   {
      DeleteCrest(key);
      (void) crest_call_load(w, NULL, NULL);
      XmListSelectPos(crestLB, pos, True);
   }

   UnsetCursor(crestFM);
   
   return;

}  /* Closing brace of crest_delete() function */ 

/**************************
 The crest_close() function 
 **************************/

void	crest_close(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace crest_close() function */

   if (crest)
   {   
      FreeCrest(crest);
      crest = NULL;
   }
   
   if(XtIsManaged(crestDS))
   {
      XtDestroyWidget(crestDS);
      crestDS = NULL;
   }
   
   return;

}  /* Closing brace of crest_close() function */ 

/**************************
 The crest_clear() function 
 **************************/

void	crest_clear(void)

{  /* Opening brace of crest_clear() function */ 

   clearForm(stageFM);   
   return;

}  /* Closing brace of crest_clear() function */ 

/************************
 The crest_key() function 
 ************************/

void	crest_key(char *key, int *pos)

{  /* Opening brace of crest_key() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	Crest          *crstPtr;
        char            where[MAX_WHERE_LEN];
        char 	        buf[12];
        int            *poslist = NULL;
        int             cnt=0;

        XmListGetSelectedPos(crestLB, &poslist, &cnt);

        if(cnt > 0)
	{
           crstPtr = (Crest *) ListNth(&crest->list, poslist[0]);

           if (crstPtr)
           {
               /* Modified BryonL - 1/25/2005. */
               date_t_to_USA_date ( crstPtr->datcrst, buf);

       	       memset(where, '\0', sizeof(where));

               sprintf(where, " WHERE lid = '%s' "
               	       " AND datcrst = '%ld'  AND timcrst = '%s' ",
                       	crstPtr->lid, crstPtr->datcrst, crstPtr->timcrst);

               *pos = poslist[0];

               strcpy(key, where);

	       if (poslist)
                   XtFree((char *)poslist);
           }
	
	}

	return;

}  /* Closing brace of crest_key() function */ 

/******************************
 The crest_call_load() function 
 ******************************/

void	crest_call_load(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace for crest_call_load() function */

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	char		where[MAX_WHERE_LEN],
	   		subquery[MAX_WHERE_LEN];
	int		pos;
	
       /*************************************************************
        Menu handling for the Crest option menus.
   	Get the menu position and build the appropriate where clause.
	*************************************************************/

	sprintf(where, " WHERE lid = '%s' ", crst_lid);

	pos = GetMenuPos(crstslctOM);

	switch (pos)

	{  /* Opening brace of switch(pos) structure */ 

	   case CREST_ALL:

	        break;
	      
	   case CREST_HI_WATER:

	        sprintf(subquery, " AND stage >= "
		        " (select wstg from riverstat "
		        "  where lid = '%s' )", crst_lid);

                strcat(where, subquery);

	        break;
	      
	   case CREST_LO_WATER:

	        sprintf(subquery, " AND stage < "
	                " (select wstg from riverstat "
		        "  where lid = '%s' )", crst_lid);

	        strcat(where, subquery);

	        break;
	      
	   default:

                break;

	}  /* Closing brace of switch(pos) structure */

       /**********************************************************************
        Special handling for the ordering of data queried from the Crest table
	**********************************************************************/

	pos = GetMenuPos(crstsortOM);

	switch (pos)

	{  /* Opening brace of switch(pos) structure */ 

	   case CREST_DATE:

          	strcat(where, " ORDER BY datcrst DESC ");

	        break;
	      
	   case CREST_FLOW:

          	strcat(where, " ORDER BY q DESC ");

	        break;
	      
	   case CREST_STAGE:

	        strcat(where, " ORDER BY stage DESC ");

	        break;
	      
	   default:

                break;

	}  /* Closing brace of switch(pos) structure */

       /********************************
        Free crest if it already exists. 
        ********************************/
 
	if (crest)
	{   
	   FreeCrest(crest);
	   crest = NULL;
	}
	
       /********************************************
	Get crests from database.  If there are any 
	crests, Load the form widgets and draw the 
	associated graphic.
        *******************************************/

	crest = GetCrest(where);	  
	crst_state = crest_load(crest);	
	crest_draw(crstdrwDA, crest);	

	return;

}  /* Closing of crest_call_load() function */ 

/*************************
 The crest_load() function 
 *************************/

int 	crest_load(Crest *crest)

{  /* Opening brace of crest_load() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	XmStringTable	xmStr;
	Crest	       *crstPtr;
	char		buf[MAX_BUF_LEN];
        char		date[DATE_LEN + 2];
        char		stage[9];
        char		flow[9];
        char		crestType[7];
	int	        state;
        int		cnt;
        int 		i;

       /***********
	Clear form.
        ***********/

	XmListDeleteAllItems(crestLB);

	crest_clear();

	state = False;	
	
	if ( crest && (cnt = ListCount(&crest->list)) )
	{	
	    xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));		

	    crstPtr = (Crest *) ListFirst(&crest->list);
	
           /*************************************
            Iterate through the list of elements.
            *************************************/

		 for (i  = 0; crstPtr; i++)
	    {
			DataToString(&crstPtr->stage, DOUBLE, stage, "%7.2lf", "MSG");
			DataToString(&crstPtr->q, INT, flow, "%8d", "MSG");

                        /* Modified BryonL, Jan 25, 2005. */
                        date_t_to_USA_date ( crstPtr->datcrst, date );

			if (crstPtr->prelim[0] == 'X' || crstPtr->prelim[0] == 'P')
				strcpy(crestType, "Prelim");
			else if (crstPtr->prelim[0] == 'R')
				strcpy(crestType, "Record");
			else
				strcpy(crestType, "      ");
         
			sprintf(buf, "%10s %10s  %-11s  %-5s   %-6s",
		         stage, flow, date, crstPtr->timcrst, crestType);
		
			xmStr[i] = XmStringCreateSimple(buf);
			crstPtr  = (Crest *) ListNext(&crstPtr->node);
       }

           XmListAddItems(crestLB, xmStr, cnt, 1);
	   XmListSelectPos(crestLB, 1, True);
		
           Sensitize(crstokPB);
	   Sensitize(crstapplyPB);
	   Sensitize(crstdelPB);

	   state = True;
		
	   for (i = 0; i < cnt; i++)
		XmStringFree(xmStr[i]);

           XtFree((char *) xmStr);
	}

     else
	{
            DeSensitize(crstokPB);
	    DeSensitize(crstapplyPB);
	    DeSensitize(crstdelPB);
	}
		
	return(state);

}  /* Closing brace of crest_load() function */ 

/***************************
 The crest_import() function 
 ***************************/

void	crest_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)

{  /* Opening brace of crest_import() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	Crest		*crstPtr;
	char		buf[DATE_LEN + 1];
	char		temp[BUFSIZ];
			
       /*********************************
	Declaration of external variables
        *********************************/
 
	crest_remove_textfilter_callbacks();
	
       /********************************************
        Retrieve data for the current lid from the
	dbms, and search for the currently selected
	offset in the XmList widget.  Update 
	Xm widgets on XmForm work area with
	appropriate values.
        *******************************************/

	crest_clear();	
	
       /*********************************************
        Get the appropriate records for this location
	and iterate through the list.
        *********************************************/

	crstPtr = (Crest *) ListNth(&crest->list, cbs->item_position);

	if (crstPtr)
	{	
           /***********************************************
            Set XmText values to values stored in the dbms.
            ***********************************************/

            memset(&temp, '\0', sizeof(temp));
	    DataToString(&crstPtr->stage, DOUBLE, temp, "%8.2lf", "");
            XmTextSetString(crststgTxt, temp);
		
	    memset(&temp, '\0', sizeof(temp));
	    DataToString(&crstPtr->q, INT, temp, "%8ld", "");
	    XmTextSetString(crstqTxt, temp);
		
            /* Modified Bryon L, Jan 25, 2005. */
            date_t_to_USA_date ( crstPtr->datcrst, buf);
	    XmTextSetString(crstdatTxt, buf);
		
	    XmTextSetString(crsttimTxt, crstPtr->timcrst);
		
            if (IsNull(CHAR, crstPtr->cremark) == NOTNULL)
		XmTextSetString(crstrmkTxt, crstPtr->cremark);
		
           /***************************************************
            Set XmToggleButton state to True if a value exists.
            ***************************************************/

	    if (crstPtr->hw[0] == 'X')
                XmToggleButtonSetState(crsthwTB, True, False);
			
	    if (crstPtr->jam[0] == 'X')
	        XmToggleButtonSetState(crsticeTB, True, False);
			
	    if (crstPtr->olddatum[0] == 'X')
	        XmToggleButtonSetState(crstodTB, True, False);
			
	    if (crstPtr->suppress[0] == 'X')
	        XmToggleButtonSetState(crstsuprsTB, True, False);
		
           /***************************************************
            Set Option Menu Button based on value of prelim field.
            ***************************************************/

	    if (crstPtr->prelim[0] == 'X' || crstPtr->prelim[0] == 'P')
           SetMenuPos(crestcatOM, PRELIM_CREST);
       else if (crstPtr->prelim[0] == 'R')
           SetMenuPos(crestcatOM, RECORD_CREST);
       else
           SetMenuPos(crestcatOM, OFFICIAL_CREST);


            /******************************
            Delete operations are enabled.
            ******************************/

            Sensitize(crstdelPB);
	}
	
	crest_add_textfilter_callbacks();
	
	return;

}  /* Closing brace of crest_import() function */ 


/****************************
 The crest_draw_cb() function 
 ****************************/

void	crest_draw_cb(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of crest_draw_cb() function */ 

 	crest_draw(crstdrwDA, crest);	

	return;

}  /* Closing brace of crest_draw_cb() function */ 

/****************************
 The crest_draw_cb() function 
 ****************************/

void	select_crest(Widget w, XtPointer client_data, XEvent *event, Boolean *callem)

{  /* Opening brace of select_crest() function */ 
   
       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

   	Position	x;
	Position        y;
	Position	min_win_x;
	Position        max_win_x;
	Position        min_win_y;
	Position        max_win_y;
	double	        min_year;
	double          max_year;
	double	        min_stage;
	double          max_stage;
	double	        stage;
	long		year;
	long		pos;
	
	x = event -> xbutton.x;
	y = event -> xbutton.y;
		
       /***************************************************************
	Get previously stored x and y coordinates for data and windows.
        ***************************************************************/

	get_x_coords(&min_year, &max_year, &min_win_x, &max_win_x);
	get_y_coords(&min_stage, &max_stage, &min_win_y, &max_win_y);
	
	year  = round(GetDataCoord(x, min_year, max_year, min_win_x, max_win_x));
	stage = GetDataCoord(y, min_stage, max_stage, min_win_y, max_win_y);
	
       /***********************
	Get list pos and select
        ***********************/

	pos = find_closest_record(year, stage);
	
	XmListSelectPos(crestLB, pos, TRUE);

	XmListSetPos(crestLB, pos);
	
	return;   

}  /* Closing brace of select_crest() function */

/**********************************
 The find_closest_record() function 
 **********************************/

long	find_closest_record(long year, double stage)

{  /* Opening brace of find_closest_record() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	Crest	       *crestPtr=NULL;
	long		pos;
        long      	closest_pos;
	long		curyear;
	double		curstage;
        double		total_diff;
        double		smallest_diff;
	
	if (crest != NULL)
		crestPtr = (Crest *) ListFirst(&crest->list); 
	
	pos = 1;

	closest_pos = pos;
	
	smallest_diff = 99999999;
	
	while (crestPtr)

	{  /* Opening brace of while(crestPtr) loop */ 
	
               curstage = crestPtr->stage;

	       curyear = get_year(crestPtr->datcrst);
		
	       total_diff = sqrt(pow((curstage - stage),2) +
	                         pow((curyear - year),2));
		
		if (total_diff < smallest_diff)
		{
 		    closest_pos = pos;
		    smallest_diff = total_diff;		
		}	
							
		crestPtr = (Crest *) ListNext(&crestPtr->node);
		pos++;

	}  /* Closing brace of while(crestPtr) loop */ 	
		
	return (closest_pos);	

}  /* Closing brace of find_closest_record() function */ 


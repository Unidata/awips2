/*
	File:		res_show.c
	Date:		11/2/1994
	Author:		Dale Shelton, Paul Taylor (Jan 1998)
	
	Purpose:	Provide support for the Reservoir DS,
			and the child window (Associate Reservoir DS).
*/

#include <time.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
/**** POSTGRES
#include <sqlhdr.h>
****/

#include "callbacks.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "Filter.h"
#include "loc_cbs.h"
#include "Xtools.h"
   
#include "Reservoir.h"
#include "Location.h"
#include "DamTypes.h"
#include "ResOwner.h"

#include "user_prefs.h"
#include "res_cbs.h"
#include "resvr.h"
#include "cvt_latlon.h"
#include "hybase.h"

#include "time_convert.h"
   
#define	RESASSOC_DEFAULT_RADIUS	"0.5"
#define RESASSOC_INITCOUNT	"( 0 Dams )                              "
   
Widget		rdownerPB;
Widget		rdtypePB;
char		res_lid[LOC_ID_LEN + 1];
int		res_update_state = 0;
int		des_update_state = 0;



void	ShowResDs(Widget w, char *lid)
{	
	if (! resDS)
	{
		create_resDS(GetTopShell(w));
		res_callbacks();
		res_add_btns();
	}

	
	if (! XtIsManaged(resDS))
	{
		strcpy(res_lid, lid);
		res_update_state = res_load(lid);
		set_window_title(resDS, "Reservoir", res_lid);
		XtManageChild(resFM);
		XtManageChild(resDS);
	}
	
	return;
}


void	res_callbacks(void)
{
	Atom	wmAtom;
	

	/*
		Window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(resDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(resDS, wmAtom, res_close, NULL);
	

	/*
		Widget callbacks.
	*/
	XtAddCallback(rokPB,    XmNactivateCallback,     res_save,   NULL);
	XtAddCallback(rsdelPB,  XmNactivateCallback,     res_del_conf, NULL);
	XtAddCallback(rclosePB, XmNactivateCallback,     res_close,  NULL);	
	
	
	XtAddCallback(impdtTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
	XtAddCallback(gatesTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	
	XtAddCallback(srchgTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(fullTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(sillTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(reselevTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

	XtAddCallback(fldTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(spwayTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(consTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(deadTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

	
	return;
}


void	res_add_btns(void)
{
	DamTypes	*dam,
			*damPtr;
	ResOwner	*owner,
			*ownerPtr;
	
	
	/*
		Load all available dam types
		into the damtype option menu.
	*/		
	if ((dam = GetDamTypes(" ORDER BY type ")) != NULL)
	{
		damPtr = (DamTypes *) ListFirst(&dam->list);
		while (damPtr)
		{
			rdtypePB = XtVaCreateManagedWidget(damPtr->type,
					xmPushButtonWidgetClass,
					rtypePDM,
					NULL);
			damPtr = (DamTypes *) ListNext(&damPtr->node);
		}
		FreeDamTypes(dam);
	}
	
	
	if ((owner = GetResOwner(" ORDER BY owner ")) != NULL)
	{
		ownerPtr = (ResOwner *) ListFirst(&owner->list);
		while (ownerPtr)
		{
			rdownerPB = XtVaCreateManagedWidget(ownerPtr->owner,
					xmPushButtonWidgetClass,
					rownerPDM,
					NULL);
			ownerPtr = (ResOwner *) ListNext(&ownerPtr->node);
		}
		FreeResOwner(owner);
	}
	return;
}

void	res_save(Widget w, XtPointer ptr, XtPointer cbs)
{
   	WidgetList	widgetList;
	Arg		arg[2];
	Reservoir	res;
	char		where[MAX_WHERE_LEN],
	   		msg[MAX_BUF_LEN],
			* buf = NULL ;
	long		date;
	int		cnt,
			returnValue,
			i,
                        status;
	
	/*
		Associate currently managed location
		with the to be updated/inserted in
		the database.
	*/
	memset(&res,'\0',sizeof(res));
	strcpy(res.lid, res_lid);
	
	/*
		Associate XmText widget values with
		the appropriate field in the 
		Reservoir struct.
	*/
        /* This logic has changed in ob4.  The associate dam 
           on the main reservoir gui, the associated nid field
           has been broken into two text fields, one for the 
           state abbreviation of the dam, and one for the 
           id of the dam. */
	if ( ( buf = XmTextGetString(resstateTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	   {
	      strncpy(res.damids, buf,             STATE_LEN);
	   }
	   XtFree(buf);
	}

	if ( ( buf = XmTextGetString(resnidTxt) ) != NULL )
	{
	   if (IsNull(CHAR, buf) == NOTNULL)
	   {
	      strncpy(res.damidn, buf, DAM_ID_NUM_LEN);
	   }
	   XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(rnameTxt) ) != NULL)
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(res.name, buf);
		XtFree(buf);
	}


	if ( ( buf = XmTextGetString(impdtTxt) ) != NULL )
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year\n"
                               "required.\n", buf);
		  ErrorDialog(resDS, msg);
		  /* leave function, do not save data */
		  return;
		}
		else
		{
                  status = USA_date_to_date_t ( buf, & date );

		  if ( status != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and\n"
                                 "day.\n", buf);
		    ErrorDialog(resDS, msg);
		    /* leave function, do not save data */
		    return;
		  }
		  else
		    res.impounded = date;
		}

		XtFree(buf);
	}

        if (rownerOM) 
        {
  	  buf = GetLabel(GetMenuHistory(rownerOM));
	  strcpy(res.owner, buf);
        }
        
        if (rtypeOM)
        {	
	  buf = GetLabel(GetMenuHistory(rtypeOM));
	  strcpy(res.type, buf);
        }	
		
	if ( ( buf = XmTextGetString(gatesTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.gates = atoi(buf);
                else
                        (void) SetNull(INT, (void *) &res.gates);
                XtFree(buf);
        }
	
	
	if ( ( buf = XmTextGetString(srchgTxt) ) != NULL )
	{		
                if (!IsBlankStr(buf))
                        res.surchg = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.surchg);
                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(fullTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.top = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.top);
                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(sillTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.sill = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.sill);
                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(reselevTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.elev = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.elev);
                XtFree(buf);
	}
	

	if ( ( buf = XmTextGetString(fldTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.floodpool = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.floodpool);
                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(spwayTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.spillway = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.spillway);
                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(consTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.conserpool = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.conserpool);
                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(deadTxt) ) != NULL )
	{
                if (!IsBlankStr(buf))
                        res.deadpool = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &res.deadpool);
                XtFree(buf);
	}
	

	/*
		Determine if the hydroTB
		XmToggle widget is set to 
		true, if yes, store an X
		in the field.
	*/
	XtSetArg(arg[0], XmNchildren, &widgetList);
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(resopsRC, arg, 2);
	for (i = 0; i < cnt; i++)
	   	if (XmToggleButtonGetState(widgetList[i]))
		{
		 	buf = GetLabel(widgetList[i]);
			res.uses[strlen(res.uses)] = buf[0];
		}
	res.uses[strlen(res.uses)] = '\0';
		

	/*
		Insert/Update the entered dbms
		record, based on the state of 
		the res_update_state variable.
	*/
	sprintf(where, " WHERE lid = '%s' ", res_lid);	
	if (res_update_state)
	{
		returnValue = UpdateReservoir(&res, where);
	}
	else
	{
		PutReservoir(&res);
	}
	
	
	Loc_UpdateStationClassFields(res_lid);	
	

	/*
		Close the dialog.
	*/
	res_close(w, NULL, NULL);

	return;
}


void	res_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(resDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, res_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);	
   
   return;
}


void	res_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char		buf[MAX_BUF_LEN],
	   		msg[MAX_BUF_LEN];
	int		error;


	SetCursor(resFM, XC_watch);
	
	
	/*
		Build the SQL procedure statement.
	*/
	sprintf(buf, " delete_resvr ( '%s' ) ", res_lid);
	if ((error = execFunction(buf)) != 0)
	{
	   	sprintf(msg, "Unable to delete reservoir - %s: %d\n", 
			res_lid, error);
		ErrorDialog(resDS, msg);
	}
	
	Loc_UpdateStationClassFields(res_lid);

	res_close(w, NULL, NULL);
	
	UnsetCursor(resFM);
	
	
	return;
}


void	res_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(resDS))
   {
	XtDestroyWidget(resDS);
	resDS = NULL;
   }
   
   return;
}


int	res_load(const char *lid)
{
   	WidgetList	widgetList;
	Arg		arg[5];
	Reservoir	*res;
	char		where[80],
			buf[20],
			*str;
	int		state,
	   		cnt,
			i,
			j;
	
	/*
                Set state variable to False,
                pending retrieval of a record
                from the database.  If a record is
                retrieved, state variable will
                be set to true.
        */
        state = False;


        /*
                Clear fields if values are
                currently loaded.
        */
	clearForm(resFM);
 	DeSensitize(rsdelPB);
        
        /*
        	Set XmRowColumn and XmToggles to an
        	initial starting state.
        */
        SetMenuPos(rownerOM, 0);
        SetMenuPos(rtypeOM, 0);

	/*
		Set the XmToggles to an initial starting
		state.
	*/
	XtSetArg(arg[0], XmNchildren, &widgetList); 
	XtSetArg(arg[1], XmNnumChildren, &cnt);
	XtGetValues(resopsRC, arg, 2);
	for (i = 0; i < cnt; i++)
	   	XmToggleButtonSetState(widgetList[i], False, False);

	
	/*
		Read in data and load it into the
		associated XmText widgets.  Free data
		after use.  How this is done has changed
                in build ob4.  The gui used to associate
                a dam with a reservoir has been removed.
                Now, there are two text fields on the
                Reservoir GUI.  One is for the state of the
                dam and one is for the id of the dam.
	*/
	sprintf(where, " WHERE lid = '%s' ", lid);

	if ((res = GetReservoir(where)) != NULL)
        {
	   if ( res->damids )
	   {
	      memset(&buf, '\0', sizeof(buf));
	      strcpy(buf, res->damids);
	      XmTextSetString(resstateTxt, buf);
	   }
	   else
	   {
	      XmTextSetString(resstateTxt, "");
	   }

	   if ( res->damidn )
	   {
	      memset(&buf, '\0', sizeof(buf));
	      strcat(buf, res->damidn);
	      XmTextSetString(resnidTxt, buf);
	   }
	   else
	   {
	      XmTextSetString(resnidTxt, "");
	   }
	   
	   
	   if (res->name)
	      XmTextSetString(rnameTxt, res->name);
	   
	   if (res->type)
	      SetMenuHistory(rtypeOM, res->type);
	   
	   if (res->owner)
	      SetMenuHistory(rownerOM, res->owner);
           
           date_t_to_USA_date ( res->impounded, buf );

	   if ( buf )
           {
	      XmTextSetString ( impdtTxt, buf );
           }
	   
	   if(! IsNull(INT, (void*)&res->gates))
		DataToString(&res->gates, INT, buf, "%d", "");
	   XmTextSetString(gatesTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->surchg))
		DataToString(&res->surchg, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(srchgTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->top))
		DataToString(&res->top, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(fullTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->sill))
		DataToString(&res->sill, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(sillTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->elev))
		DataToString(&res->elev, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(reselevTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->floodpool))
		DataToString(&res->floodpool, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(fldTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->spillway))
		DataToString(&res->spillway, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(spwayTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->conserpool))
		DataToString(&res->conserpool, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(consTxt, buf);
	   
	   if(! IsNull(DOUBLE, (void*)&res->deadpool))
		DataToString(&res->deadpool, DOUBLE, buf, "%8.1lf", "");
	   else
		strcpy(buf, "");
	   XmTextSetString(deadTxt, buf);
	   
	   
	   /*
	   	Set the uses XmToggles
	   	state to True if a value is
	   	found.
	   */
	   XtSetArg(arg[0], XmNchildren, &widgetList); 
	   XtSetArg(arg[1], XmNnumChildren, &cnt);
	   XtGetValues(resopsRC, arg, 2);
	   for (i = 0; i < cnt; i++)
	   {
	      str = GetLabel(widgetList[i]);
	      for (j = 0; j < strlen(res->uses); j++)
		 if (str[0] == res->uses[j])
		    XmToggleButtonSetState(widgetList[i], True, False);
	   }
	   
	   
	   /*
	   	A record is available, so sensitize
	   	the Delete XmPushButton.
	   */
	   Sensitize(rsdelPB);
	   
	   
	   /*
	   	Free any allocated memory.
	   */
	   FreeReservoir(res);
	   
	   
	   /*
	   	Set state variable to True.
	   */
	   state = True;
	}

	
	/*
		Return the state variable to provide
		an indication whether to Insert 
		or Update a record in the dbms.
	*/
	return(state);	
}

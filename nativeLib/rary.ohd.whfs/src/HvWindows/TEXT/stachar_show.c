/*
	File:		stachar_show.c
	Date:		2/13/96
	Author:		Paul Taylor
	
	Purpose:	Provides support for Data Sources DS
			(formerly Station Characteristics DS).
	
	Revision Date:  03-04-2003
                        - Replaced Owner, State, Sponsor, Recip,
		          Telemetry, Owner, and Payor with
		          Scrolled Lists.
			- Added freelist function  

                        04-03-2003	
			- Added null checks to prevent core dumps

                        10-14-2003
                        - Set initial default values for all 
                          scrolled lists to the first item in 
                          the lists.
			- Added null checks on values being added
			  to the database.

        Bryon Lawrence  03-09-2004
	                - Added the email field to the Observer form.
        Bryon Lawrence  01-25-2005 
                        - Changed to use the date_t_to_USA_date and
                          USA_date_to_date_t routines for the PostGres
                          conversion.

        Cham Pham       09/05/07
			- Changed to display address1 properly in "observer"
		        table.	
*/

#include <stdio.h>
#include <stdlib.h>
/**** POSTGRES
#include <sqlhdr.h>
******/
#include <unistd.h>
#include <time.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>

#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "ParamDefs.h"
#include "CoopSpons.h"
#include "CoopRecip.h"
#include "CoopComms.h"
#include "Observer.h"
#include "State.h"
#include "DcpOwner.h"
#include "Dcp.h"
#include "TelmOwner.h"
#include "TelmPayor.h"
#include "TelmType.h"
#include "Telem.h"
#include "stachar.h"
#include "stachar_show.h"
#include "time_convert.h"
#include "set_stnclass.h"
#include "set_window_title.h"

#define DCP	0
#define OBS	1
#define TELM	2

/*
	Globals.
*/
static char	current_lid[LOC_ID_LEN + 1];

Widget		commPB;
Widget		sponsPB;
Widget		recipPB;
Widget		statePB;
Widget		telmOwnerPB;
Widget		telmPayorPB;
Widget		telmTypePB;


void	ShowStaCharDs(Widget w, char *lid, Boolean editable)
{
   Atom		wmAtom;
   
   if (! stacharDS)
   {
      create_stacharDS(GetTopShell(w));
      dcp_btns();	/* create buttons for Dcp. */
      obs_btns();	/* create buttons for Observer. */
      telm_btns();	/* create buttons for Telemetry. */
      
      wmAtom = XmInternAtom(XtDisplay(stacharDS), "WM_DELETE_WINDOW", False);
      XmAddWMProtocolCallback(stacharDS, wmAtom, stachar_close, NULL);
      
      /*
      		Add callbacks for PBs.
      */
      XtAddCallback(scobsPB, XmNactivateCallback, stachar_checkPB, NULL);
      XtAddCallback(scdcpPB, XmNactivateCallback, stachar_checkPB, NULL);
      XtAddCallback(sctelmPB,XmNactivateCallback, stachar_checkPB, NULL);
      
      XtAddCallback(dosTB, XmNvalueChangedCallback, obs_loaddate, NULL);
      
      XtAddCallback(scapplyPB, XmNactivateCallback, (XtCallbackProc)stachar_apply, scOM);
      XtAddCallback(sccancelPB, XmNactivateCallback, stachar_close, NULL);
      XtAddCallback(scdeletePB, XmNactivateCallback, (XtCallbackProc)stachar_del_conf, scOM);
      
      
      /*
      		Add TextFilter callbacks.
      */
      stachar_addTextFilterCallbacks();
   }
   
   
   if (! XtIsManaged(stacharDS))
   {
      /*
      		Setup Station Characterstics window.
      */
      strcpy(current_lid, lid);

      /*
          Set title
      */
      set_title(stacharDS, "Data Sources", lid);

      SetMenuPos(scOM, 0);
      stachar_checkPB(scdcpPB, NULL, NULL); /* default is dcp */
      
      XtManageChild(scFM);
      XtManageChild(stacharDS);

      if ( ! editable)
      {    
         XtUnmanageChild(scapplyPB);
         XtUnmanageChild(scdeletePB);
      }

   }
   
   return;
}


void	stachar_addTextFilterCallbacks(void)
{
   /* dcp */
   XtAddCallback(goesTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtAddCallback(timeTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_COLONS);
   XtAddCallback(freqTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   /* obs */
   XtAddCallback(ozipTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtAddCallback(dosTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtAddCallback(hfonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtAddCallback(wfonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtAddCallback(ossnTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtAddCallback(orateTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
   XtAddCallback(cd404Txt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtAddCallback(otaskTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE_AND_HYPHENS);

   /* telem */
   XtAddCallback(tphoneTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtAddCallback(tcostTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
   XtAddCallback(tsensorTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtAddCallback(trptfreqTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   
   return;
}


void	stachar_removeTextFilterCallbacks(void)
{
   /* dcp */
   XtRemoveCallback(goesTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtRemoveCallback(timeTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_COLONS);
   XtRemoveCallback(freqTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   /* obs */
   XtRemoveCallback(ozipTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtRemoveCallback(dosTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
   XtRemoveCallback(hfonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtRemoveCallback(wfonTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtRemoveCallback(ossnTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtRemoveCallback(orateTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
   XtRemoveCallback(cd404Txt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtRemoveCallback(otaskTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE_AND_HYPHENS);

   /* telem */
   XtRemoveCallback(tphoneTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
   XtRemoveCallback(tcostTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
   XtRemoveCallback(tsensorTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
   XtRemoveCallback(trptfreqTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   
   return;
}



void	stachar_apply(Widget w, Widget om, XtPointer cbs)
{
	/*
		Apply changes based on the current window.
	*/
	switch(GetMenuPos(om))
	{
	   case DCP:	dcp_apply(w, NULL, NULL);
			stachar_checkPB(scdcpPB, NULL, NULL);
			break;

	   case OBS:	obs_apply(w, NULL, NULL);
			stachar_checkPB(scobsPB, NULL, NULL);
			break;

	   case TELM:	telm_apply(w,NULL, NULL);
			stachar_checkPB(sctelmPB, NULL, NULL);
			break;

	   default:	fprintf(stderr,"ERROR: Unknown menu selection.\n");
	      		break;
	}
	set_stnclass(current_lid);

	return;
}


void	stachar_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
   
      stachar_removeTextFilterCallbacks();
      
      
      if(w == scdcpPB)		/* DCP */
      {
	   XtSetMappedWhenManaged(scobsFrm, False);	/* turn off Obs */
	   XtSetMappedWhenManaged(scadminFrm, False);
	   XtSetMappedWhenManaged(sctlinfoFrm, False);	/* turn off Telm */
	   XtSetMappedWhenManaged(sctlcritFrm, False);

	   clearForm(dcpinfoFM);
	   clearForm(dcpcritFM);
	   XmToggleButtonSetState(dnotifyTB, True, False);	/* default=T */
	   dcp_load(current_lid);
	   
	   XtSetMappedWhenManaged(scdcpinfoFrm, True);	/* turn on DCP */
	   XtSetMappedWhenManaged(scdcpcritFrm, True);
      }

      else if(w == scobsPB)	/* OBSERVER */
      {
	   XtSetMappedWhenManaged(scdcpinfoFrm, False);	/* turn off DCP */
	   XtSetMappedWhenManaged(scdcpcritFrm, False);
	   XtSetMappedWhenManaged(sctlinfoFrm, False);	/* turn off Telm */
	   XtSetMappedWhenManaged(sctlcritFrm, False);

	   clearForm(obsFM);
	   clearForm(oadminFM);
	   XmToggleButtonSetState(instTB, True, False);		/* default=I */
	   obs_load(current_lid);

	   XtSetMappedWhenManaged(scobsFrm, True);	/* turn on Obs */
	   XtSetMappedWhenManaged(scadminFrm, True);
      }

      else if(w == sctelmPB)	/* TELEMETRY */
      {
	   XtSetMappedWhenManaged(scobsFrm, False);	/* turn off Obs */
	   XtSetMappedWhenManaged(scadminFrm, False);
	   XtSetMappedWhenManaged(scdcpinfoFrm, False);	/* turn off DCP */
	   XtSetMappedWhenManaged(scdcpcritFrm, False);

	   clearForm(tlinfoFM);
	   clearForm(tcritFM);	   
	   XmToggleButtonSetState(tnotifyTB, True, False);	/* default=T */
	   telm_load(current_lid);

	   XtSetMappedWhenManaged(sctlinfoFrm, True);	/* turn on Telm */
	   XtSetMappedWhenManaged(sctlcritFrm, True);
      }
      
      else
	fprintf(stderr,"ERROR: Unknown widget.\n");

      
      stachar_addTextFilterCallbacks();

      
      return;
}


void	stachar_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(stacharDS))
   {
      XtDestroyWidget(stacharDS);
      stacharDS = NULL;
   }
   
   return;
}


void	stachar_del_conf(Widget w, Widget om, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(stacharDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, (XtCallbackProc)stachar_delete, om);

      
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);

   
   return;
}


void	stachar_delete(Widget w, Widget om, XtPointer cbs)
{
   char		where[MAX_WHERE_LEN];
   

   SetCursor(scFM, XC_watch);
   
   
   /*
   	Apply changes based on the current window.
   	(The OM will either be set to Observer, DCP, or Telemetry.)
   */
   sprintf(where," WHERE lid = '%s' ",current_lid);
   switch(GetMenuPos(om))
   {
      case DCP:		DeleteDcp(where);
			stachar_checkPB(scdcpPB, NULL, NULL);
	 		break;
	 
      case OBS:		DeleteObserver(where);
			stachar_checkPB(scobsPB, NULL, NULL);
	 		break;
	 
      case TELM:	DeleteTelem(where);
			stachar_checkPB(sctelmPB, NULL, NULL);
	 		break;
	 
      default:		fprintf(stderr,"ERROR: Unknown menu selection.\n");
	 		break;
   }
   set_stnclass(current_lid);

   
   UnsetCursor(scFM);

   return;
}



/*
	Support for Observer.
*/

void	obs_btns(void)
{
	CoopComms	*comm, *commPtr;
	CoopSpons	*spons, *spPtr;
	CoopRecip	*recip, *rcPtr;
	State		*st, *stPtr;
        XmStringTable   xmStr;
        int             cnt = 0,
                        i = 0;
	
	
	comm = GetCoopComms(" ORDER BY comm ");
        if (comm)
		commPtr = (CoopComms *) ListFirst(&comm->list);
        else
        	commPtr = NULL;
	while (commPtr)
	{
		commPB = XtVaCreateManagedWidget(commPtr->comm,
				xmPushButtonWidgetClass,
				ocommsPDM, NULL);
		commPtr = (CoopComms *) ListNext(&commPtr->node);
	}
	
	
	spons = GetCoopSpons(" ORDER BY spons ");
        if (spons)
        {
		spPtr = (CoopSpons *) ListFirst(&spons->list);
                cnt = ListCount(&spPtr->list);
        }
        else
        {     
        	spPtr = NULL;
                cnt = 0;
        }

	/* Adding sponsors to the Scrolled List */
        i = 0;
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (spPtr)
	{
                xmStr[i] = XmStringCreateSimple(spPtr->spons);
                i++;
		spPtr = (CoopSpons *) ListNext(&spPtr->node);
	}
        XmListAddItems(osponsLI, xmStr, cnt, 1);
        XmListSelectPos(osponsLI, 1, 0);        // Selects the first item in the
	                                        // sponsor list by default
        freelist(xmStr, cnt);
	
        /* Adding recips to the scrolled list */
	recip = GetCoopRecip(" ORDER BY recip ");
        if (recip)
        { 
		rcPtr = (CoopRecip *) ListFirst(&recip->list);
        	cnt = ListCount(&rcPtr->list);
        }
        else
	{
		rcPtr = NULL;
		cnt = 0;
        }
        i = 0;
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (rcPtr)
	{
                xmStr[i] = XmStringCreateSimple(rcPtr->recip);
                i++;
		rcPtr = (CoopRecip *) ListNext(&rcPtr->node);
	}
        XmListAddItems(orecpLI, xmStr, cnt, 1);
        XmListSelectPos(orecpLI, 1, 0);       // Selects the first item in the
	                                      // Recip list by default
        freelist(xmStr, cnt);
	
	st = GetState(" ORDER BY state ");
        if (st)
	{
		stPtr = (State *) ListFirst(&st->list);
        	cnt = ListCount(&stPtr->list);
        }
	else
	{
		stPtr = NULL;
		cnt = 0;
        }
        i = 0;
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (stPtr)
	{
                xmStr[i] = XmStringCreateSimple(stPtr->state);
                i++;
		stPtr = (State *) ListNext(&stPtr->node);
	}
        XmListAddItems(ostateLI, xmStr, cnt, 1);
        XmListSelectPos(ostateLI, 1, 0);      // Selects the first state in the
	                                      // State list by default
        freelist(xmStr, cnt);
	
	if (comm)  FreeCoopComms(comm);
	if (spons) FreeCoopSpons(spons);
	if (recip) FreeCoopRecip(recip);
	if (st)    FreeState(st);
	return;
}


void	obs_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	Observer 	obs;
	char		where[MAX_WHERE_LEN];
	long		count;	
        int             status;
        char            errormsg[80];
	
	/*
		Apply changes to the DBMS.
	*/
	memset(&obs, '\0', sizeof(obs));
	memset(&where, '\0', sizeof(where));

	if (!obs_unload(&obs))
	  return;
	  
	sprintf(where, " WHERE lid = '%s' ", current_lid);


	count = recordCount("Observer", where);
	if (count > 0)
        {
	   if (( status = UpdateObserver(&obs, where) ) != 0 )
           {
              if ((status == -239) || (status == -268))
                 sprintf(errormsg,
                      "Information not added. \nSpecified record already defined.");
              else if (status == -391)
                 sprintf(errormsg,
                      "Information not added. \nMissing one or more fields.");
              else
                 sprintf(errormsg, "Add of record failed; err= %d", status);
              ErrorDialog( stacharDS, errormsg );
           }
        }
	else
        {
           if (( status = PutObserver(&obs) ) != 0 )
           {
              if ((status == -239) || (status == -268))
                  sprintf(errormsg,
                  "Information not added. \nSpecified record already defined.");
              else if (status == -391)
                  sprintf(errormsg,
                  "Information not added. \nMissing one or more fields.");
              else
                  sprintf(errormsg, "Add of record failed; err= %d", status);
              sprintf( errormsg, errormsg );
              ErrorDialog(stacharDS , errormsg );
           }
        }
	return;
}


void    obs_load(char *lid)
{
	char    	where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
	  		address[MAX_BUF_LEN],
                        address2[MAX_BUF_LEN],
                        address3[MAX_BUF_LEN];			
	Observer 	*obs;
        XmString        item;
        int             pos = 0;
	
	
	/*
		Init.
	*/
	memset(&where, '\0', sizeof(where));
	memset(&buf, '\0', sizeof(buf));
	memset(&address, '\0', sizeof(address));
        memset(&address2, '\0', sizeof(address2));
        memset(&address3, '\0', sizeof(address3));

	
	sprintf(where," WHERE lid = '%s' ",lid);
	obs = GetObserver(where);	
	if ( ( obs != NULL ) && ListCount ( &obs->list ) )
	{
		XmTextSetString(ofnameTxt, obs->firstname);
		XmTextSetString(olnameTxt, obs->lastname);
		XmTextSetString(hfonTxt,   obs->hphone);
		XmTextSetString(wfonTxt,   obs->phone);
		XmTextSetString(ossnTxt,   obs->ssn);
		XmTextSetString(cd404Txt,  obs->ornr);
		XmTextSetString(otaskTxt,  obs->tsk);
		XmTextSetString(oreptTxt,  obs->rprt);
		
		/*
			set date
		*/
		if (obs->dos)
                {
                   date_t_to_USA_date ( obs->dos, buf );
                }
		else
			strcpy(buf,"");		
		XmTextSetString(dosTxt,buf);
		
		
		/*
			set money values
		
		*/
		DataToString(&obs->rate,DOUBLE,buf,"%5.2lf","");
		XmTextSetString(orateTxt,buf);

		
		/*
			Set Address
		*/
		
		strcpy(address,obs->a1);
		strcat(address,"\n");

                strcat(address2,obs->a2);
		strcat(address2,"\n");
		
		strcat(address3,obs->a3);
		strcat(address3,"\n");

		XmTextSetString(oaddrTxt, address);
		XmTextSetString(oaddrTxt2, address2);
		XmTextSetString(oaddrTxt3, address3);
		

		/*
			Set the 3 widgets for city, state, zip
		
		*/
		XmTextSetString(ocityTxt, obs->city);
		XmTextSetString(ozipTxt,  obs->zip);		
		
                /*
                        Set the email field
                */
                XmTextSetString(emailTxt, obs->email) ;
		
		/*
			gender
		*/
		if (strcmp(obs->gn,"M") == 0)
		{
		   XmToggleButtonSetState(instTB, False, False);
		   XmToggleButtonSetState(maleTB, True, False);
		}
		else if (strcmp(obs->gn,"F") == 0)
		{
		   XmToggleButtonSetState(instTB, False, False);
		   XmToggleButtonSetState(femaleTB, True, False);
		}

	
			
		/*
			Set Option Menus.
		*/
		SetMenuHistory(ocommsOM,  obs->comm);
 
                /* Selects the current recip in the scrolled list */
                item = XmStringCreateSimple(obs->recip);          
                pos = XmListItemPos(orecpLI, item);
                XmListSetPos(orecpLI, pos);
                XmListSelectPos(orecpLI, pos, True);

                /* Selects the current sponsor in the scrolled list */
                item = XmStringCreateSimple(obs->spons); 
                pos = XmListItemPos(osponsLI, item);
                XmListSetPos(osponsLI, pos);
                XmListSelectPos(osponsLI, pos, True);
                  
                /* Selects the current state in the scrolled list */
                item = XmStringCreateSimple(obs->state); 
                pos = XmListItemPos(ostateLI, item);
                XmListSetPos(ostateLI, pos);
                XmListSelectPos(ostateLI, pos, True);
                XtFree((char *)item);
  
		
		/*
			Free any allocated memory.
		*/
		FreeObserver(obs);
		
		Sensitize(scdeletePB);
	}
	else
	{
	   DeSensitize(scdeletePB);
	}

	return;
}


void    obs_loaddate(Widget w, XtPointer ptr, XtPointer cbs)
{
        char    	sdate[DATE_LEN + 2],
        		where[MAX_WHERE_LEN];
        struct tm       *tm_ptr;
	time_t		starttime;

	Observer	*obs;

	
	stachar_removeTextFilterCallbacks();
	
	
	/*
		Init.
	*/
	memset(&sdate, '\0', sizeof(sdate));
	memset(&where, '\0', sizeof(where));

	
	if (XmToggleButtonGetState(dosTB))
	{
		time(&starttime);
       		tm_ptr = localtime(&starttime);

       		strftime(sdate,DATE_LEN+2,"%m/%d/%Y",tm_ptr);
       		XmTextSetString(dosTxt,sdate);
        }       
        else
        {
        	sprintf(where," WHERE lid = '%s' ", current_lid);     	
        	obs = GetObserver(where);

        	if (current_lid[0] && obs)
                {
                   date_t_to_USA_date ( obs->dos, sdate );
                }
        	else
                {
        	   strcpy(sdate,"");
                }
 
        	
        	XmTextSetString(dosTxt, sdate);        		
        	FreeObserver(obs);
        }
	
	
	stachar_addTextFilterCallbacks();

	
 	return;       
}


int	obs_unload(Observer *obs)
{
   char  		*buf,
	   		msg[MAX_BUF_LEN],
                        *SLdata = NULL;
   long			date;
   int                  cnt = 0, 
                        *pos = NULL;
   XmString             *strlist;
	   		
   
   
   strcpy(obs->lid, current_lid);
   
   
   if ( ( buf = XmTextGetString(ofnameTxt) ) )
   {
      strcpy(obs->firstname, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(olnameTxt) ) )
   {
      strcpy(obs->lastname, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(hfonTxt)))
   {
      strcpy(obs->hphone, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(wfonTxt) ) )
   {
      strcpy(obs->phone, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(ossnTxt) ) )
   {
      strcpy(obs->ssn, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(cd404Txt) ) ) 
   {
      strcpy(obs->ornr, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(otaskTxt)) )
   {
      strcpy(obs->tsk, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(oreptTxt)) )
   {
      strcpy(obs->rprt, buf);
      XtFree(buf);
   }
   
   
   if ( ( buf = XmTextGetString(dosTxt)) )
   {
	if (four_digit_year(buf) != 0)
	{
	  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
		  buf);
	  ErrorDialog(stacharDS, msg);
	  /* leave function, do not save data */
	  return(False);
	}
	else
	{
	  if ( USA_date_to_date_t ( buf, &date ) != 0 )
	  {
	    sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
		    buf);
	    ErrorDialog(stacharDS, msg);
	    /* leave function, do not save data */
	    return(False);
	  }
	  else
	    obs->dos = date;
	}
			
	XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(orateTxt) ) )
   {
      obs->rate = atof(buf);
      XtFree(buf);
   }
   
   
   /*
   	copy address from widget to fields.
   */
   if ( ( buf = XmTextGetString(oaddrTxt)) )
   {
      strcpy(obs->a1, buf);
      XtFree(buf);
   }
   if ( ( buf = XmTextGetString(oaddrTxt2)) )
   {
      strcpy(obs->a2, buf);
      XtFree(buf);
   }
   if ( ( buf = XmTextGetString(oaddrTxt3)) )
   {
      strcpy(obs->a3, buf);
      XtFree(buf);
   }
   
   
   /*
   	copy city, state, zip from widgets to field.
   */	
   if ( ( buf = XmTextGetString(ocityTxt) ) )
   {
      strcpy(obs->city, buf);
      XtFree(buf);
   }
   
   
   if ( ( buf = XmTextGetString(ozipTxt) ) )
   {
      strcpy(obs->zip, buf);
      XtFree(buf);
   }
   
   if ( ( buf = XmTextGetString(emailTxt) ) )
   {
      strcpy(obs->email, buf);
      XtFree(buf);
   }
   
   XmListGetSelectedPos(ostateLI, &pos, &cnt);
   if (cnt)
   {
     XtVaGetValues(ostateLI, XmNitems, &strlist, NULL);
     XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
     strcpy(obs->state, SLdata);
   }

   /*
   	pull down menus
   */
   
   buf = GetLabel(GetMenuHistory(ocommsOM));
   strcpy(obs->comm, buf);
   
   /* 
        scrolled lists 
   */

   XmListGetSelectedPos(osponsLI, &pos, &cnt);
   if (cnt)
   {
     XtVaGetValues(osponsLI, XmNitems, &strlist, NULL);
     XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
     strcpy(obs->spons, SLdata);
   } 
   
   XmListGetSelectedPos(orecpLI, &pos, &cnt);
   if (cnt)
   {
     XtVaGetValues(orecpLI, XmNitems, &strlist, NULL);
     XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
     strcpy(obs->recip, SLdata);
   } 
   
   /*
   	gender	
   */
   if (XmToggleButtonGetState(maleTB))
      strcpy(obs->gn, "M");		
   else if (XmToggleButtonGetState(femaleTB))
      strcpy(obs->gn, "F");
   else
      strcpy(obs->gn, "I");
   
   return(True);
}




/*
	Support for Dcp.
*/

void	dcp_btns(void)
{
	DcpOwner	*dcp,
			*dcpPtr;
        XmStringTable   xmStr;
        int             cnt = 0, 
                        i = 0;
	
	
	if ( ( dcp = GetDcpOwner("") ) )
	{   
		dcpPtr = (DcpOwner *) ListFirst(&dcp->list);
                cnt = ListCount(&dcpPtr->list);
                xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
		while (dcpPtr)
		{
                        xmStr[i] = XmStringCreateSimple(dcpPtr->owner);
                        i++;
			dcpPtr = (DcpOwner *) ListNext(&dcpPtr->node);
		}
                XmListAddItems(downerLI, xmStr, cnt, 1);
                XmListSelectPos(downerLI, 1, 0);
                freelist(xmStr, cnt);
		FreeDcpOwner(dcp);
	}
	return;
}


void	dcp_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	Dcp		dcp;
	char		where[MAX_WHERE_LEN],
			*buf,
                        *SLdata = NULL;
	long		count;
        int             cnt = 0,
                        *pos = NULL;
        XmString        *strlist;


	/*
		Associate appropriate lid with
		Dcp lid field.
	*/
	memset(&dcp, '\0', sizeof(dcp));
	memset(&where, '\0', sizeof(where));
	strcpy(dcp.lid, current_lid);
		

	/*
		Retrieve the values from the XmText widgets.
	*/
	if ( ( buf = XmTextGetString(goesTxt) ) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(dcp.goes, buf);
		XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(freqTxt) ) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(dcp.rptfreq, buf);
		XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(timeTxt) ) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(dcp.rptime, buf);
		XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(dcritTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(dcp.criteria, buf);
		XtFree(buf);
	}
	
       
        /* Retrieves the value from the owner scrolled list */	
        XmListGetSelectedPos(downerLI, &pos, &cnt);
        if (cnt)
        {
          XtVaGetValues(downerLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(dcp.owner, SLdata);
        }
        
	
	/*
		Get the notify value.
	*/
	if (XmToggleButtonGetState(dnotifyTB))
	   	strcpy(dcp.notify, "X");
	else
		strcpy(dcp.notify, " ");
	
	
        /*
                Insert/Update the entered dbms
                record, based on the state of
                the dcp_state variable.
        */
        sprintf(where, " WHERE lid = '%s' ", current_lid);
	
	count = recordCount("DCP", where);
        if (count > 0)
                UpdateDcp(&dcp, where);
        else
                PutDcp(&dcp);

	return;
}


void	dcp_load(char *lid)
{
	Dcp		*dcp;
	char		where[MAX_WHERE_LEN];
        XmString        item;
        int             pos = 0;
	

	/*
		Get data from the DBMS.
	*/
	memset(&where, '\0', sizeof(where));
	sprintf(where, " WHERE lid = '%s' ", lid);
	if ( ( dcp = GetDcp(where) ) )
	{
		/*
			Load values into Xm display fields.
		*/
		XmTextSetString(goesTxt,  dcp->goes);
		XmTextSetString(freqTxt,  dcp->rptfreq);
		XmTextSetString(timeTxt,  dcp->rptime);
		XmTextSetString(dcritTxt, dcp->criteria);

                /* Selects the owner in the scrolled list */
                item = XmStringCreateSimple(dcp->owner);
                pos = XmListItemPos(downerLI, item);
                XmListSetPos(downerLI, pos);
                XmListSelectPos(downerLI, pos, True);
                XtFree((char *)item);

		/*
			Get the notify TB value.
		*/
		if (dcp->notify[0] != 'X')
		   XmToggleButtonSetState(dnotifyTB, False, False);
		
	
		/*
			Cleanup and return.
		*/
		FreeDcp(dcp);
		
		Sensitize(scdeletePB);
	}
	else
	{
	   DeSensitize(scdeletePB);
	}

	return;
}









/*
	Support for Telemetry.
*/

void	telm_btns(void)
{
	TelmOwner	*to,
	   		*toPtr;
	TelmPayor	*tp,
	   		*tpPtr;
	TelmType	*tt,
	   		*ttPtr;
        XmStringTable   xmStr;
        int             cnt = 0,
                        i = 0;

	
	if ( ( to = GetTelmOwner(" ORDER BY owner ") ) )
	{   
		toPtr = (TelmOwner *) ListFirst(&to->list);
                cnt = ListCount(&toPtr->list);
                i = 0;
                xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
		while (toPtr)
		{
                        xmStr[i] = XmStringCreateSimple(toPtr->owner);
                        i++;
			toPtr = (TelmOwner *) ListNext(&toPtr->node);
		}
                XmListAddItems(townerLI, xmStr, cnt, 1);
                XmListSelectPos( townerLI, 1, 0);
                freelist(xmStr, cnt);
	
		FreeTelmOwner(to);
	}
	
	
	if ( ( tp = GetTelmPayor(" ORDER BY payor ") ) )
	{   
		tpPtr = (TelmPayor *) ListFirst(&tp->list);
                cnt = ListCount(&tp->list);
                i = 0;
                xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
		while (tpPtr)
		{
                        xmStr[i] = XmStringCreateSimple(tpPtr->payor);
                        i++;
			tpPtr = (TelmPayor *) ListNext(&tpPtr->node);
		}
                XmListAddItems(tpayorLI, xmStr, cnt, 1);
                XmListSelectPos(tpayorLI, 1, 0);
                freelist(xmStr, cnt);
		FreeTelmPayor(tp);
	
	}
	
	
	if ( ( tt = GetTelmType(" ORDER BY type ") ) )
	{   
		ttPtr = (TelmType *) ListFirst(&tt->list);
                cnt = ListCount(&tt->list);
                i = 0;
                xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
		while (ttPtr)
		{
                        xmStr[i] = XmStringCreateSimple(ttPtr->type); 
                        i++;
			ttPtr = (TelmType *) ListNext(&ttPtr->node);
		}
                XmListAddItems(telmLI, xmStr, cnt, 1);
                XmListSelectPos(telmLI, 1, 0);
                freelist(xmStr, cnt);
	
		FreeTelmType(tt);
	}

	
	return;
}



void	telm_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	Telem		telem;
	char		where[MAX_WHERE_LEN],
			*buf,
                        *SLdata = NULL;
	long		count;		
        int             cnt = 0,
                        *pos = NULL;
        XmString        *strlist;

			
	/*
                Associate currently managed location
                with the to be updated/inserted in
                the database.
        */
  	memset(&telem,'\0',sizeof(telem));
	memset(&where, '\0', sizeof(where));
      	strcpy(telem.lid, current_lid);


	/*
		Associate scrolled list widget values
		with the appropriate field in the 
		Telem struct.
	*/
        XmListGetSelectedPos(telmLI, &pos, &cnt);
        if (cnt)
        {
          XtVaGetValues(telmLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(telem.type, SLdata);
        }
        
        XmListGetSelectedPos(townerLI, &pos, &cnt);
        if (cnt)
        {
          XtVaGetValues(townerLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(telem.owner, SLdata);
        }

        XmListGetSelectedPos(tpayorLI, &pos, &cnt);
        if (cnt)
        {
          XtVaGetValues(tpayorLI, XmNitems, &strlist, NULL);
          XmStringGetLtoR(strlist[pos[0]-1], XmFONTLIST_DEFAULT_TAG, &SLdata);
          strcpy(telem.payor, SLdata);
        }

	
	
        /*
                Associate XmText widget values with
                the appropriate field in the
                Reservoir struct.
        */
        if ( ( buf = XmTextGetString(tphoneTxt) ) )
        {
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(telem.phone, buf);
  		XtFree(buf);
  	}
  	
  	
	if ( ( buf = XmTextGetString(tsensorTxt)) )
	{
	   	if (IsNull(CHAR, buf) == NOTNULL)
		   	strcpy(telem.sensorid, buf);
		XtFree(buf);
	}
	
	
        if ( ( buf = XmTextGetString(tcritTxt) ) )
        {
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(telem.criteria, buf);
		XtFree(buf);
	}
	
	if ( ( buf = XmTextGetString(tcostTxt) ) )
	{
                if (!IsBlankStr(buf))
                        telem.cost = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &telem.cost);

                XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString(trptfreqTxt) ) )
	{
	 	if (IsNull(CHAR, buf) == NOTNULL)
		   	strcpy(telem.rptfreq, buf);
		XtFree(buf);
	}
	
	
	/*
		Get the TBG state for the notify option,
		and set the value appropriately.
	*/
	if (XmToggleButtonGetState(tnotifyTB))
		strcpy(telem.notify, "X");
	else
		strcpy(telem.notify, " ");
	
	
        /*
                Insert/Update the entered dbms
                recordCount
        */
        sprintf(where, " WHERE lid = '%s' ", current_lid);
	
	count = recordCount("Telem", where);
	
        if (count > 0)
		UpdateTelem(&telem, where);
        else
		PutTelem(&telem);

        return;
}


void	telm_load(char *lid)
{
	char		where[MAX_WHERE_LEN],
			buf[MAX_WHERE_LEN];
	Telem		*telem;
        XmString        item;
        int             pos = 0;


	/*
		Read data from the DBMS.
	*/
	memset(&where, '\0', sizeof(where));
	memset(&buf, '\0', sizeof(buf));
	sprintf(where, " WHERE lid = '%s' ", lid);
	if ( ( telem = GetTelem(where) ) )
	{
                /*
                        Set Scrolled list values
                */
                
                item = XmStringCreateSimple(telem->type);
                pos = XmListItemPos(telmLI, item);
                XmListSetPos(telmLI, pos);
                XmListSelectPos(telmLI, pos, True);
 
                item = XmStringCreateSimple(telem->owner);
                pos = XmListItemPos(townerLI, item);
                XmListSetPos(townerLI, pos);
                XmListSelectPos(townerLI, pos, True);
  
                item = XmStringCreateSimple(telem->payor);
                pos = XmListItemPos(tpayorLI, item);
                XmListSetPos(tpayorLI, pos);
                XmListSelectPos(tpayorLI, pos, True);
                XtFree((char *)item);

		/*
			Load values into Xm display fields.
		*/
		XmTextSetString(tphoneTxt, telem->phone);
		XmTextSetString(tcritTxt, telem->criteria);
		XmTextSetString(tsensorTxt, telem->sensorid);
		XmTextSetString(trptfreqTxt, telem->rptfreq);
		
		if(! IsNull(DOUBLE, (void*)&telem->cost))
		   DataToString(&telem->cost, DOUBLE, buf, "%8.2lf", "");
		else
		   strcpy(buf, "");
		XmTextSetString(tcostTxt, buf);

		if (telem->notify[0] != 'X')
		   XmToggleButtonSetState(tnotifyTB, False, False);
		
		
		/*
			Cleanup and return.
		*/
		FreeTelem(telem);
		
		Sensitize(scdeletePB);
	}
	else
	{
	   DeSensitize(scdeletePB);
	}
	
	return;	
}



void	obs_set_3line_address(char *address,
			      char *line1, char *line2, char *line3)
{
   char		buf[MAX_BUF_LEN];
   int		i, j;
   
   
   /*
   	Init.
   */
   memset(&buf, '\0', sizeof(buf));
   strncpy(line1, buf, 30);  /* set to NULL */
   strncpy(line2, buf, 30);  /* set to NULL */
   strncpy(line3, buf, 30);  /* set to NULL */
   
   strcpy(buf, address);  /* work with a copy of the address */

   
   /*
   	Get line 1.
   */
   for (i=0,j=0;   i < 30;  i++,j++)
   {
      if (buf[i] != '\n')
	 line1[j] = buf[i];
      
      if ((buf[i] == '\n') || (buf[i] == '\0'))
      {
	 i = i + 1;
	 break;
      }
   }

   
   /*
   	Get line 2.
   */
   for (    j=0;   i < 60;  i++,j++)
   {
      if (buf[i] != '\n')
	 line2[j] = buf[i];
      
      if ((buf[i] == '\n') || (buf[i] == '\0'))
      {
	 i = i + 1;
	 break;
      }
   }

   
   /*
   	Get line 3.
   */
   for (    j=0;   i < 90;  i++,j++)
   {
      if (buf[i] != '\n')
	 line3[j] = buf[i];
      
      if ((buf[i] == '\n') || (buf[i] == '\0'))
      {
	 i = i + 1;
	 break;
      }
   }


   return;
}


XmStringTable freelist(XmStringTable xmStr, int cnt)
/******************
* Function : freelist
* Purpose  : takes a XmStringTable variable and releases the memory
******************/

{
  int i = 0;
  
  for (i = 0; i < cnt; i++)
    XmStringFree (xmStr[i]);
  XtFree((char *) xmStr);
  return (xmStr);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

/*
	File:		datum_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Datum DS.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "datum.h"
#include "Datum.h"
#include "datum_cbs.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "hybase.h"
#include "time_convert.h"
#include "user_prefs.h"



char		datum_lid[LOC_ID_LEN + 1];
Datum		*datumHead = NULL;


void	datum_show(Widget w, const char *lid)
{
	if (! datumDS)
	{	
		create_datumDS(GetTopShell(w));
		datum_callbacks();
	}
	
	if (! XtIsManaged(datumDS))
	{
		strcpy(datum_lid,lid);
		datum_load(datum_lid);
		set_window_title(datumDS, "Datum", (char *)datum_lid);
		XtManageChild(datumFM);
		XtManageChild(datumDS);
	}
	
	return;
}


void	datum_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(datumDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(datumDS, atom, datum_close, NULL);
	
	
	/*
		General widget callbacks.
	*/
   XtAddCallback(datumLB,   XmNdefaultActionCallback,  datum_import,  NULL);
   XtAddCallback(datumLB,   XmNbrowseSelectionCallback,datum_import,  NULL);
   XtAddCallback(dtapplyPB, XmNactivateCallback,       datum_apply,   NULL);
   XtAddCallback(dtclosePB, XmNactivateCallback,       datum_close,   NULL);
   XtAddCallback(dtdelPB,   XmNactivateCallback,       datum_del_conf,NULL);

	
	/*
		TextFilter callbacks.
	*/
	XtAddCallback(dtdateTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);

	/* Modified by guoxian zhou 07-2004*/
	XtAddCallback(dtelevTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);

	
	return;
}


void	datum_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   (void) datum_save();
   return;
}


int	datum_save(void)
{
   Datum datum;
   char  *buf,
         msg[MAX_BUF_LEN];
   int   error,
         rtn;
   date_t datet;


	/*
		Associate lid to field entry.
	*/
	memset(&datum, '\0', sizeof(datum));
	strcpy(datum.lid, datum_lid);

	/*
		Get values from XmText widgets.
	*/
	if ( (buf = XmTextGetString(dtdateTxt)) )
	
	{
		if (four_digit_year(buf) != 0)
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(datumDS, msg);
		  /* leave function, do not save data */
		  return(False);
		}
		else
		{
        if ( USA_date_to_date_t ( buf, &datet ) != 0 )
		  {
		    sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
			    buf);
		    ErrorDialog(datumDS, msg);
		    /* leave function, do not save data */
		    return(False);
		  }
		  else
		    datum.ddate = datet;
		}
		
		XtFree(buf);
	}

	
	if ( (buf = XmTextGetString(dtelevTxt)) )
	{
                if (!IsBlankStr(buf))
                        datum.elev = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &datum.elev);

                XtFree(buf);
	}
		

	/*
      Insert/Update the entered dbms record
   */
	rtn = True;
   if ( (error = InsertOrUpdateDatum(&datum) ) != 0)
   {
      sprintf(msg, "Unable to update record: %d\n", error);
      ErrorDialog(datumDS, msg);
      rtn = False;
   }
	
   datum_load(datum_lid);
   
   return(rtn);
}


void	datum_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (datumHead != NULL)
   {
      FreeDatum (datumHead);
      datumHead = NULL;
   }

   if(XtIsManaged(datumDS))
   {
      XtDestroyWidget(datumDS);
      datumDS = NULL;
   }

   return;
}


void	datum_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(datumDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, datum_delete, NULL);
   
   
   if (! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	datum_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	Datum	*datumPtr;
	int	pos;

	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(datumFM, XC_watch);

	datumPtr = datum_selected ( &pos );
   DeleteDatumByRecord ( datumPtr );
   (void) datum_load(datum_lid);
   XmListSelectPos(datumLB, pos, True);

	UnsetCursor(datumFM);
	
	return;
}


Datum * datum_selected(int *pos)
{
   Datum	*dtPtr;
   int   *poslist,
         cnt = 0;

	XmListGetSelectedPos(datumLB, &poslist, &cnt);
   dtPtr = (Datum *) ListNth(&datumHead->list, poslist[0]);
   if (dtPtr)
   {
         *pos = poslist[0];
         XtFree((char *)poslist);
   }

	return (dtPtr);
}


void	datum_load(const char *lid)
{
	XmStringTable	xmStr;
	Datum	*dtPtr = NULL;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
			datebuf[DATE_LEN + 1],
			elevbuf[9];
	int   cnt,
			i;
 
	
         /*
                Set state variable to False,
                pending retrieval of a record
                from the database.  If a record is
                retrieved, state variable will
                be set to true.
        */
	clearForm(datumFM);
	XmListDeleteAllItems(datumLB);
   
        if (datumHead != NULL)
        {
           FreeDatum (datumHead);
           datumHead = NULL;
        }
        sprintf(where, " WHERE lid = '%s' ORDER BY ddate DESC ", lid);
        if ((datumHead = GetDatum(where)) != NULL)
        {
                cnt     = ListCount(&datumHead->list);
                xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
                dtPtr   = (Datum *) ListFirst(&datumHead->list);
                for (i = 0; dtPtr; i++)
                {
			date_t_to_USA_date ( dtPtr->ddate, datebuf );
			if(! IsNull(DOUBLE, (void*)&dtPtr->elev))
                	   DataToString(&dtPtr->elev, DOUBLE, elevbuf, "%8.3lf", "");
			else
			   strcpy(elevbuf, "");
			sprintf(buf, "%-11s %25s %14s",
                                datebuf, " ", elevbuf);
                        xmStr[i] = XmStringCreateSimple(buf);
                        dtPtr = (Datum *) ListNext(&dtPtr->node);
                }


		XmListAddItems(datumLB, xmStr, cnt, 1);
                XmListSelectPos(datumLB, 1, True);
                      

        	/*
                	cleanup.
        	*/
        	for (i = 0; i < cnt; i++)
                	XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);
                
                
		Sensitize(dtapplyPB);
                Sensitize(dtdelPB);
        }
	else
	{
		DeSensitize(dtdelPB);
	}
	
	
        return;	
}


void    datum_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        Datum *datumPtr;
        int   pos;
        char  buf[DATE_LEN + 1];

         datumPtr = datum_selected ( &pos );
        	if (datumPtr)
        	{
            date_t_to_USA_date ( datumPtr->ddate, buf );
            XmTextSetString(dtdateTxt, buf);

            if (! IsNull(DOUBLE,(void*)&datumPtr->elev) )
               DataToString(&datumPtr->elev, DOUBLE, buf, "%8.3lf", "");
            else
               strcpy(buf, "");
            XmTextSetString(dtelevTxt, buf);
          }

          return;
}

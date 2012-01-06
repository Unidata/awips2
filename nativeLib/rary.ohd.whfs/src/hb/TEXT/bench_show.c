/*
	File:		bench_show.c
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Benchmark DS.
	
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
#include "bench.h"
#include "bench_cbs.h"
#include "Benchmark.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "hybase.h"
#include "user_prefs.h"
#include "Xtools.h"

char		bench_lid[LOC_ID_LEN + 1];
int		bench_state;


void	bench_show(Widget w, const char *lid)
{
	if (! benchDS)
	{	
		create_benchDS(GetTopShell(w));
		bench_callbacks();
	}

	
	if (! XtIsManaged(benchDS))
	{
		strcpy(bench_lid,lid);
		bench_state = bench_load(bench_lid);
		set_window_title(benchDS, "Benchmark", bench_lid);
		XtManageChild(benchFM);
		XtManageChild(benchDS);
	}
	
	return;
}


void	bench_callbacks(void)
{
	Atom		atom;
	
	/*
		Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(benchDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(benchDS, atom, bench_close, NULL);
	
	
	/*
		General widget callbacks.
	*/
        XtAddCallback(benchLB,   XmNdefaultActionCallback, bench_import, NULL);
        XtAddCallback(benchLB,   XmNbrowseSelectionCallback, bench_import, NULL);
	XtAddCallback(bmokPB,    XmNactivateCallback, bench_ok, NULL);
	XtAddCallback(bmapplyPB, XmNactivateCallback, bench_apply, NULL);
	XtAddCallback(bmclosePB, XmNactivateCallback, bench_close, NULL);
	XtAddCallback(bmnewPB,   XmNactivateCallback, bench_new, NULL);
	XtAddCallback(bmdelPB,   XmNactivateCallback, bench_del_conf, NULL);

	
	/*
		TextFilter callbacks.
	*/
	/* DO NOT USE
	XtAddCallback(bmTxt,     XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);
	*/
	XtAddCallback(bmelevTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	
	
	return;
}


void	bench_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (bench_save())
      bench_close(w, NULL,NULL);
   return;
}


void	bench_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   	(void) bench_save();
	return;
}


int	bench_save(void)
{
	Benchmark	bench;
	char		*buf,
			key[MAX_WHERE_LEN],
			msg[MAX_BUF_LEN];
	int		error,
	   		pos,
			rtn;
			
			
	/*
		Associate lid to field entry.
	*/
	memset(&bench, '\0', sizeof(bench));
	strcpy(bench.lid, bench_lid);
	
	
	/*
		Get values from XmText widgets.
	*/
	if ( (buf = XmTextGetString(bmTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(bench.bnum, buf);
		XtFree(buf);
	}
	
		
	if ( (buf = XmTextGetString(bmelevTxt)) )
	{
                if (!IsBlankStr(buf))
                        bench.elev = atof(buf);
                else
                        (void) SetNull(DOUBLE, (void *) &bench.elev);

                XtFree(buf);
	}
	
			
	if ( (buf = XmTextGetString(bmdescTxt)) )
	{
		if (IsNull(CHAR, buf) == NOTNULL)
			strcpy(bench.remark, buf);	
		XtFree(buf);
	}
	

	/*
                Insert/Update the entered dbms
                record, based on the state of
                the state variable.
        */
	rtn = True;
        if (bench_state)
       	{
		bench_key(key, &pos);
		if (strlen(key))
		{
			if ((error = UpdateBenchmark(&bench, key)) != 0)
			{
				sprintf(msg, "Unable to update record: %d\n", error);
				ErrorDialog(benchDS, msg);
				rtn = False;
			}
		}
       	}
        else
	{
        	if ((error = PutBenchmark(&bench)) != 0)
		{
		 	sprintf(msg, "Unable to insert record: %d\n", error);
			ErrorDialog(benchDS, msg);
			rtn = False;
		}
        }	
        	
	
        /*
        	Reset Insert/Update state to 
        	True, and return.
        */
        bench_state = bench_load(bench_lid);
	return(rtn);
}


void	bench_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(benchDS))
   {
      XtDestroyWidget(benchDS);
      benchDS = NULL;
   }

   return;
}


void	bench_new(Widget w, XtPointer ptr, XtPointer cbs)
{
	clearForm(benchFM);
	Sensitize(bmokPB);
	Sensitize(bmapplyPB);
	bench_state = False;
	return;
}


void	bench_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
   Widget	qstDS,
      		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(benchDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, bench_delete, NULL);

   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	bench_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[MAX_WHERE_LEN];
	int	pos;


	/*
		Delete record from dbms,
		and reload list and select pos.
	*/
	SetCursor(benchFM, XC_watch);
	bench_key(key, &pos);
	if (strlen(key))
	{
		DeleteBenchmark(key);
		(void) bench_load(bench_lid);
		XmListSelectPos(benchLB, pos, True);
	}
	UnsetCursor(benchFM);
	
	return;
}


void	bench_key(char *key, int *pos)
{
   Benchmark	*bench,
      		*bmPtr;
   
   char		where[MAX_WHERE_LEN];
   
   int		*poslist,
      		cnt;
   
   
   XmListGetSelectedPos(benchLB, &poslist, &cnt);
   sprintf(where, " WHERE lid = '%s' ORDER BY elev DESC ", bench_lid);
   if ((bench = GetBenchmark(where)) != NULL)
   {
      bmPtr = (Benchmark *) ListNth(&bench->list, poslist[0]);
      if (bmPtr)
      {
	 memset(where, '\0', sizeof(where));
	 sprintf(where, " WHERE lid = '%s' AND bnum = '%s' ",
		 bmPtr->lid, bmPtr->bnum);
	 
	 *pos = poslist[0];
	 strcpy(key, where);
	 XtFree((char *)poslist);
      }
      
      FreeBenchmark(bench);
   }
   
   return;
}


int	bench_load(const char *lid)
{
	XmStringTable	xmStr;
	Benchmark	*bench,
			*bmPtr;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN],
			elevbuf[9];
	int		state,
			cnt,
			i;
 
         /*
                Set state variable to False,
                pending retrieval of a record
                from the database.  If a record is
                retrieved, state variable will
                be set to true.
        */
	XmListDeleteAllItems(benchLB);
	clearForm(benchFM);
        state = False;    
        cnt   = 0;
        
        
        sprintf(where, " WHERE lid = '%s' ORDER BY elev DESC ", lid);
        if ((bench = GetBenchmark(where)) != NULL)
        {
	   cnt     = ListCount(&bench->list);
	   xmStr   = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
	   bmPtr   = (Benchmark *) ListFirst(&bench->list);
	   for (i = 0; bmPtr; i++)
	   {
	      if(! IsNull(DOUBLE, (void*)&bmPtr->elev))
	         DataToString(&bmPtr->elev, DOUBLE, elevbuf, "%8.3lf", "");
	      else
		 strcpy(elevbuf, "");
	      sprintf(buf, "%-6s %47s %14s",
		      bmPtr->bnum, " ", elevbuf);
	      xmStr[i] = XmStringCreateSimple(buf);
	      bmPtr = (Benchmark *) ListNext(&bmPtr->node);
	   }
	   
	   
	   /*
	   	Load the list.
	   */
	   XmListAddItems(benchLB, xmStr, cnt, 1);
	   XmListSelectPos(benchLB, 1, True);
	   
	   
	   /*
	   	cleanup.
	   */
	   for (i = 0; i < cnt; i++)
	      XmStringFree(xmStr[i]);
	   XtFree((char *) xmStr);
	   FreeBenchmark(bench);
	   
	   
	   Sensitize(bmokPB);
	   Sensitize(bmapplyPB);
	   Sensitize(bmdelPB);
	   state = True;
        }
        else
        {
	   DeSensitize(bmokPB);
	   DeSensitize(bmapplyPB);
	   DeSensitize(bmdelPB);
        }


        return(state);	
}


void    bench_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{
        Benchmark            	*benchs,
                        	*bmPtr;
        char            	where[MAX_WHERE_LEN],
                        	buf[DATE_LEN + 1];

        /*
                External variable(s).
        */
        extern char   		bench_lid[LOC_ID_LEN + 1];


        /*
                Retrieve data for the current lid from the
                dbms, and search for the currently selected
                offset in the XmList widget.  Update
                Xm widgets on XmForm work area with
                appropriate values.
        */
        sprintf(where, " WHERE lid = '%s' ORDER BY elev DESC ", bench_lid);
        if ((benchs = GetBenchmark(where)) != NULL)
	{
	   bmPtr  = (Benchmark *) ListNth(&benchs->list, cbs->item_position);
	   if (bmPtr)
	   {
	      XmTextSetString(bmTxt, bmPtr->bnum);
	      if(! IsNull(DOUBLE, (void*)&bmPtr->elev))
	         DataToString(&bmPtr->elev, DOUBLE, buf, "%8.3lf", "");
	      else
		 strcpy(buf, "");
	      XmTextSetString(bmelevTxt, buf);
	      XmTextSetString(bmdescTxt, bmPtr->remark);
	   }
	   
	   FreeBenchmark(benchs);
	}
	
	
        return;
}

/*
	File:		fldcat_show.c
	Date:		August 1995
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Flood Category DS.
	
*/


#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Floodcat.h"
#include "GeneralUtil.h"
#include "user_prefs.h"
#include "fldcat_show.h"
#include "fldcat.h"
#include "hybase.h"



int		fdat_state;
char		fdat_lid[LOC_ID_LEN + 1];


void	ShowFdatDs(Widget w, char *lid)
{
	if (! fdatDS)
	{
		create_fdatDS(GetTopShell(w));
		fdat_callbacks();
	}
	
	if (! XtIsManaged(fdatDS))
	{	
		strcpy(fdat_lid, lid);
		fdat_state = fdat_load(fdat_lid);
		set_window_title(fdatDS, "Flood Category", fdat_lid);
		XtManageChild(fdatFM);
		XtManageChild(fdatDS);
	}
	
	return;
}


void	fdat_callbacks(void)
{
	Atom	wmAtom;
	
	/*
		Window manager callbacks.
	*/
	wmAtom = XmInternAtom(XtDisplay(fdatDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(fdatDS, wmAtom, close_fdat, NULL);

	
	/*
		General widget callbacks.
	*/
	XtAddCallback(fdokPB,    XmNactivateCallback, save_fdat,     NULL);
	XtAddCallback(fdclosePB, XmNactivateCallback, close_fdat,    NULL);
	XtAddCallback(fddelPB,   XmNactivateCallback, fdat_del_conf, NULL);

	
	/*
		TextFilter callbacks.
	*/
	XtAddCallback(fcat1stageTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(fcat2stageTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(fcat3stageTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(fcat1flowTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(fcat2flowTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(fcat3flowTxt,  XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	
	
	return;
}


void	save_fdat(Widget w, XtPointer ptr, XtPointer cbs)
{
	Floodcat	fc;
	char		where[MAX_WHERE_LEN],
			*buf = NULL ;
	
	
	/*
		Associate currently managed location,
		with the entry to updated/inserted in
		the database.	
	*/	
	memset(&fc, '\0', sizeof(fc));
	strcpy(fc.lid, fdat_lid);
	
	
	/*
		Associate XmText widget values with
		the appropriate field in the 
		Floodcat struct.
	*/
	if ( ( buf = XmTextGetString ( fcat3stageTxt ) ) != NULL )
	{
		if (!IsBlankStr(buf))
			fc.major_stage = atof(buf);
		else
			(void) SetNull(DOUBLE, (void *) &fc.major_stage);

		XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString ( fcat2stageTxt ) ) != NULL )
	{
		if (!IsBlankStr(buf))
			fc.moderate_stage = atof(buf);
		else
			(void) SetNull(DOUBLE, (void *) &fc.moderate_stage);

		XtFree(buf);
	}
	
	
	if ( ( buf = XmTextGetString ( fcat1stageTxt ) ) != NULL )
	{
		if (!IsBlankStr(buf))
			fc.minor_stage = atof(buf);
		else
			(void) SetNull(DOUBLE, (void *) &fc.minor_stage);

		XtFree(buf);
	}
	
	if ( (buf = XmTextGetString(fcat3flowTxt)) != NULL )
	{
		if (!IsBlankStr(buf))
			fc.major_flow = atof(buf);
		else
			(void) SetNull(DOUBLE, (void *) &fc.major_flow);

		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(fcat2flowTxt)) )
	{
		if (!IsBlankStr(buf))
			fc.moderate_flow = atof(buf);
		else
			(void) SetNull(DOUBLE, (void *) &fc.moderate_flow);

		XtFree(buf);
	}
	
	
	if ( (buf = XmTextGetString(fcat1flowTxt)) != NULL )
	{
		if (!IsBlankStr(buf))
			fc.minor_flow = atof(buf);
		else
			(void) SetNull(DOUBLE, (void *) &fc.minor_flow);

		XtFree(buf);
	}
	
	
	
	/*
		Update the database.	
	*/
	sprintf(where, " WHERE lid = '%s' ", fdat_lid);	
	if (fdat_state)
		UpdateFloodcat(&fc, where);
	else
		PutFloodcat(&fc);
		
		
	/*
		Close the dialog.
	*/
	close_fdat(w, NULL);
	return;
}


void	close_fdat(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(fdatDS))
   {
      XtDestroyWidget(fdatDS);
      fdatDS = NULL;
   }
   
   return;
}


void	fdat_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete this entry?");
   qstDS = QuestionDialog(fdatDS, buf);
   SetTitle(qstDS,"Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, fdat_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	fdat_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	where[MAX_WHERE_LEN];
	
	
	sprintf(where, " WHERE lid = '%s' ", fdat_lid);
	
	SetCursor(fdatFM, XC_watch);
	DeleteFloodcat(where);
	UnsetCursor(fdatFM);
	
	close_fdat(w, NULL, NULL);	
	return;
}



int     fdat_load(const char *lid)
{
        Floodcat        * fc = NULL ;
        char            where[MAX_WHERE_LEN],
                        buf[MAX_BUF_LEN];
        int             state;

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
        clearForm(fdatFM);
        DeSensitize(fddelPB);

        /*
                Read in data and load it into the
                associated text widgets.  Free data
                after use.
        */
        sprintf(where, " WHERE lid = '%s' ", lid);
        fc = GetFloodcat(where);

        if ( fc != NULL )
        {
           if (ListCount(&fc->list))
           {
		if(! IsNull(DOUBLE, (void*)&fc->major_stage))
                      DataToString(&fc->major_stage, DOUBLE, buf, "%5.1lf", "");
		else
		   strcpy(buf, "");
                XmTextSetString(fcat3stageTxt, buf);

		if(! IsNull(DOUBLE, (void*)&fc->moderate_stage))
                   DataToString(&fc->moderate_stage, DOUBLE, buf, "%5.1lf", "");
		else
		   strcpy(buf, "");
                XmTextSetString(fcat2stageTxt, buf);

		if(! IsNull(DOUBLE, (void*)&fc->minor_stage))
                   DataToString(&fc->minor_stage, DOUBLE, buf, "%5.1lf", "");
		else
		   strcpy(buf, "");
                XmTextSetString(fcat1stageTxt, buf);

		if(! IsNull(DOUBLE, (void*)&fc->major_flow))
                   DataToString(&fc->major_flow, DOUBLE, buf, "%.0lf", "");
		else
		   strcpy(buf, "");
                XmTextSetString(fcat3flowTxt, buf);

		if(! IsNull(DOUBLE, (void*)&fc->moderate_flow))
                   DataToString(&fc->moderate_flow, DOUBLE, buf, "%.0lf", "");
		else
		   strcpy(buf, "");
                XmTextSetString(fcat2flowTxt, buf);

		if(! IsNull(DOUBLE, (void*)&fc->minor_flow))
                   DataToString(&fc->minor_flow, DOUBLE, buf, "%.0lf", "");
		else
		   strcpy(buf, "");
                XmTextSetString(fcat1flowTxt, buf);



                /*
                        Set Delete XmPushButton state
                        to True, allow delete.
                */
                Sensitize(fddelPB);


                /*
                        Free any allocated memory.
                */
                FreeFloodcat(fc);
  

                /*
                        Set state variable to True.
                */
                state = True;
           }
       }

       /*
          Return the state variable to provide
          an indication whether to Insert
          or Update a record in the dbms.
       */
       return(state);
}

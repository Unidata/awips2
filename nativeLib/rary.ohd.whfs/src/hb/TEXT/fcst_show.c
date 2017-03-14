/*
	File:		fcst_show.c
	Date:		11/07/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Description DS.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>

#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "Xtools.h"

#include "Descrip.h"
#include "LocArea.h"
#include "Proximity.h"

#include "user_prefs.h"
#include "fcst_cbs.h"
#include "fcst.h"
#include "hybase.h"


/*
	Globals.
*/
Widget		prev;
Widget		proxPB;

int		fcst_state;
int		updateLocArea = 0;
char		fcst_lid[LOC_ID_LEN + 1];



void	fcst_show(Widget w, const char *lid)
{
        /* Test to make sure that lid is not "NULL" and
           that it is not equal to '\0' . */

        if ( ( lid != NULL ) &&
             ( * lid != '\0' ) )
        {

   	   if (! fcstDS)
	   {
		   create_fcstDS(GetTopShell(w));
		   fcst_btns();		/* create buttons for Proximity */
		   fcst_callbacks();
	   }
	
	   if (! XtIsManaged(fcstDS))
	   {
		   strcpy(fcst_lid,lid);
		   fcst_state = fcst_load(fcst_lid);
		   set_window_title(fcstDS, "Description",(char *) fcst_lid);
		   XtManageChild(fcstFM);
		   XtManageChild(fcstDS);
	   }
        }
	
	return ;
}



void	fcst_btns(void)
{
   Proximity	*prox = NULL , *proxPtr = NULL ;
   
   
   if ((prox = GetProximity(" ORDER BY proximity ")) != NULL)
   {
      proxPtr = (Proximity *) ListFirst(&prox->list);
      while (proxPtr)
      {
	 proxPB = XtVaCreateManagedWidget(proxPtr->proximity,
					  xmPushButtonWidgetClass,
					  fpproxPDM, NULL);
	 proxPtr = (Proximity *) ListNext(&proxPtr->node);
      }
      
      FreeProximity(prox);
   }
   
   
   return;
}


void	fcst_callbacks(void)
{
        Atom    wmAtom;


        /*
                Window manager callbacks.
        */
        wmAtom = XmInternAtom(XtDisplay(fcstDS), "WM_DELETE_WINDOW", False);
        XmAddWMProtocolCallback(fcstDS, wmAtom, fcst_close, NULL);


	/*
		Widget callbacks.
	*/
	XtAddCallback(fpokPB,      XmNactivateCallback, fcst_save,        NULL);
	XtAddCallback(fpdelPB,     XmNactivateCallback, fcst_del_conf,    NULL);

	XtAddCallback(fpareaokPB,  XmNactivateCallback, fcst_save2,        NULL);
	XtAddCallback(fpareadelPB, XmNactivateCallback, fcst_del_conf2,   NULL);

	XtAddCallback(fpcancelPB,  XmNactivateCallback, fcst_close,       NULL);
	
	return;
}


void	fcst_save(Widget w, XtPointer ptr, XtPointer cbs)
{
	Descrip		desc;
	char		where[MAX_WHERE_LEN],
			*buf = NULL ;
	
	        
	/*
                Associate currently managed location
                with the to be updated/inserted in
                the database.
        */
        memset(&desc, '\0', sizeof(desc));
        strcpy(desc.lid, fcst_lid);

        buf = GetLabel(GetMenuHistory(fpproxOM));
        if ( buf != NULL ) strcpy(desc.proximity, buf);

	
        /*
                Associate XmText widget values with
                the appropriate field in the
                Descrip struct.
	*/
	if ( (buf = XmTextGetString(fpbedTxt)) != NULL )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.bed, buf);
        	XtFree(buf);
	}

	if ( (buf = XmTextGetString(fpdivertTxt)) != NULL )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.divert, buf);
		XtFree(buf);
	}

 	if ( (buf = XmTextGetString(fprmkTxt)) != NULL )
        {
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.remark, buf);
 		XtFree(buf);
 	}

	if ( (buf = XmTextGetString(fpiceTxt)) != NULL )
        {
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.ice, buf);
 		XtFree(buf);
 	}

	if ( (buf = XmTextGetString(fpreachTxt)) != NULL )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.reach, buf);
        	XtFree(buf);
	}

 	if ( (buf = XmTextGetString(fpresTxt)) != NULL )
        {
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.res, buf);
 		XtFree(buf);
 	}

 	if ( (buf = XmTextGetString(fptopoTxt)) != NULL )
        {
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(desc.topo, buf);
 		XtFree(buf);
 	}

   /*
		Insert/Update the entered dbms
		record, based on the state of
		the fcst_state variable.
	*/
	sprintf(where, " WHERE lid = '%s' ", fcst_lid);
	if (fcst_state)
		UpdateDescrip(&desc, where);
	else
		PutDescrip(&desc);

	return;			
}


void	fcst_save2(Widget w, XtPointer ptr, XtPointer cbs)
{
	LocArea		larea;
	char		where[50],
			*buf = NULL ;
	
	        
   memset(&larea, '\0', sizeof(larea));
   strcpy(larea.lid, fcst_lid);

   /* area column of the LocArea table */
 	if ( (buf = XmTextGetString(fpareaTxt)) != NULL )
   {
      if (IsNull(CHAR, buf) == NOTNULL)
         strncpy(larea.area, buf, SHORT_LEN);
      XtFree(buf);
 	}

	/*	Insert/Update record, based on the state of the updateLocArea variable.	*/
	sprintf(where, " WHERE lid = '%s' ", fcst_lid);
	if (updateLocArea)
		UpdateLocArea(&larea, where);
	else
		PutLocArea(&larea);

	return;			
}


void	fcst_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete these Description entries?");
   qstDS = QuestionDialog(fcstDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, fcst_delete, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	fcst_del_conf2(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "Do you wish to delete the Affected Area entry?");
   qstDS = QuestionDialog(fcstDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, fcst_delete2, NULL);
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	fcst_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
        char	where[MAX_WHERE_LEN];

        sprintf(where, " WHERE lid = '%s' ", fcst_lid);
	
        SetCursor(fcstFM, XC_watch);
        DeleteDescrip(where);
        UnsetCursor(fcstFM);
	
        fcst_clear();
        fcst_load(fcst_lid);

        return;
}

void	fcst_delete2(Widget w, XtPointer ptr, XtPointer cbs)
{
        char	where[MAX_WHERE_LEN];

        sprintf(where, " WHERE lid = '%s' ", fcst_lid);
	
        SetCursor(fcstFM, XC_watch);
        DeleteLocArea(where);
        UnsetCursor(fcstFM);
	
        fcst_clear();
        fcst_load(fcst_lid);

        return;
}


void	fcst_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   	/*
   		Close any open windows.
	*/
	if (XtIsManaged(fcstDS))
	{
	   XtDestroyWidget(fcstDS);
	   fcstDS = NULL;
	}
	
	return;
}



int	fcst_load(const char *lid)
{
	Descrip		*desc = NULL ;
	LocArea		*larea = NULL ;
	char		where[MAX_WHERE_LEN];
	int		state;
	
	
	/*
		Set state to false, and clear
		XmForm widgets.
	*/
	clearForm(fcstFM);
	DeSensitize(fpdelPB);
	state = False;
	
	
	/*
		Get the appropriate record from the
		dbms, and load the XmForm fields.
	*/
	sprintf(where, " WHERE lid = '%s' ", lid);
	desc = GetDescrip(where);
        if ( desc != NULL )
        {
	   if (ListCount(&desc->list))
	   {
		SetMenuHistory(fpproxOM, desc->proximity);
		
		XmTextSetString(fpreachTxt, desc->reach);
		XmTextSetString(fpbedTxt, desc->bed);
		XmTextSetString(fpdivertTxt, desc->divert);
		XmTextSetString(fpiceTxt, desc->ice);
		XmTextSetString(fptopoTxt, desc->topo);
		XmTextSetString(fprmkTxt, desc->remark);
		XmTextSetString(fpresTxt , desc->res);


		/*
			Sensitize appropriate widgets.
		*/
		Sensitize(fpdelPB);
		
		
		/*
			Free alloc'd memory and set
			the retrieve state to true.
		*/
		FreeDescrip(desc);
		state = True;
	   }
        }

     updateLocArea = False;
	  larea = GetLocArea(where);
     if ( larea != NULL )
     {
        if (ListCount(&larea->list))
        {
           XmTextSetString(fpareaTxt , larea->area);
           Sensitize(fpdelPB);
		
           /* Free alloc'd memory and set	the LocArea state to true. */
           FreeLocArea(larea);
           updateLocArea = True;
	      }
      }


	/*
		Set the section XmOptionMenu state,
		and return.
	*/
	return(state);
}



void	fcst_clear(void)
{
	/*
		Clear text widget values.
	*/
	XmTextSetString(fpreachTxt,   "");
	XmTextSetString(fpbedTxt,     "");
	XmTextSetString(fpdivertTxt,  "");
	XmTextSetString(fpiceTxt,     "");
	XmTextSetString(fptopoTxt,    "");
	XmTextSetString(fprmkTxt,     "");
	XmTextSetString(fpresTxt,     "");
	
	/*
		Set widget state(s).
	*/
	SetMenuPos(fpproxOM, 0);
	DeSensitize(fpdelPB);
	
	
	/*
		We're out of here.
	*/	
	return;
}

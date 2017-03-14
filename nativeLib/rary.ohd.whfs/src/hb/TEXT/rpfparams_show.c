/*
	File:		rpfparams_show.c
	Date:		1/8/1998
	Author:		Paul Taylor
	
	Purpose:	Provide support for the RiverPro General Params DS.
*/


/************************************************************************
   
   Functions to handle the setup control of rpfparams information.
   
   ***********************************************************************/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "RpfParams.h"

#include "DbmsUtils.h"
#include "Xtools.h"
#include "ParamDefs.h"

#include "rpfparams.h"
#include "rpfparams_show.h"


   
/************************************************************************
   
   Display the rpfparams dialog shell.
   
   ***********************************************************************/

void	rpfparams_show(Widget w)
{
   if (! rpfparamsDS)
   {      
      /* create the dialog shell and add the callbacks */
      
      create_rpfparamsDS(GetTopShell(w));
      write_rpfparams_info();
      add_rpfparams_cbs();
   }
   
   
   if (! XtIsManaged(rpfparamsDS))
   {
      XtManageChild(rpfparamsFO);
      XtManageChild(rpfparamsDS);
   }
   
      
   return;
}


/************************************************************************
    
   Add the rpfparams selection callbacks.
   
   ************************************************************************/

void	add_rpfparams_cbs(void)
{
   
   Atom	wmAtom;
   
   
   /* callbacks on add, update, delete pushbuttons */
   
   XtAddCallback(rpfparams_closePB,  XmNactivateCallback, rpfparams_closeCB,  NULL);
   XtAddCallback(rpfparams_updatePB, XmNactivateCallback, rpfparams_updateCB, NULL);
   
   
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(rpfparamsDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(rpfparamsDS, wmAtom, rpfparams_closeCB, NULL);

   
   /* callbacks for TextFilter */   
   
   add_rpfparams_textfilter_cbs();
   
   
   return;
}


void	add_rpfparams_textfilter_cbs(void)
{
   XtAddCallback(obshrsTE,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(fcsthrsTE,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(rvsexphrsTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(flsexphrsTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtAddCallback(flwexphrsTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   
   return;
}


void	remove_rpfparams_textfilter_cbs(void)
{
   XtRemoveCallback(obshrsTE,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtRemoveCallback(fcsthrsTE,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtRemoveCallback(rvsexphrsTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtRemoveCallback(flsexphrsTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
   XtRemoveCallback(flwexphrsTE, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);

   return;
}


/************************************************************************
   
  Update the database using the most-recently-entered user-data.
   
   **********************************************************************/

void	rpfparams_updateCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   RpfParams 	paraminfo;
   RpfParams	*paramPtr;
   
   char		buf[MAX_BUF_LEN];
   
   int		error = -1;  /* assume failure */
   
   
   memset(&paraminfo, 0, sizeof(RpfParams));
   read_rpfparams_info(&paraminfo);
   
   
   if ((paramPtr = (RpfParams *) GetRpfParams(NULL)) != NULL)
   {
      error = UpdateRpfParams(&paraminfo, NULL);
      if (error < 0)
      {
	 sprintf(buf, "Unable to update general parameters: %-i\n", error);
	 ErrorDialog(rpfparamsDS, buf);
      }
      
      
      /*
      	  Cleanup.
      */
      FreeRpfParams(paramPtr);
   }
   else
   {
      sprintf(buf, "Unable to update general parameters...\n");
      ErrorDialog(rpfparamsDS, buf);
   }
   
   
   return;
}


/************************************************************************
   
   Get data from the display.
   
   **********************************************************************/

void	read_rpfparams_info(RpfParams *paraminfo)
{
   char		*valstr = NULL ;

   if ( ( valstr = XmTextGetString(obshrsTE) ) )
   {
      paraminfo->obshrs = atol(valstr);
      XtFree(valstr);
   }
   if ( ( valstr = XmTextGetString(fcsthrsTE) ) )
   {
      paraminfo->fcsthrs = atol(valstr);
      XtFree(valstr);
   }

   
   if ( ( valstr = XmTextGetString(missvalTE) ) )
   {
      strcpy(paraminfo->missval, valstr);
      XtFree(valstr);
   }
   if ( ( valstr = XmTextGetString(misscatTE) ) )
   {
      strcpy(paraminfo->misscat, valstr);
      XtFree(valstr);
   }
   if ( ( valstr = XmTextGetString(misstimTE) ) )
   {
      strcpy(paraminfo->misstim, valstr);
      XtFree(valstr);
   }
   
   
   if ( ( valstr = XmTextGetString(rvsexphrsTE) ) )
   {
      paraminfo->rvsexphrs = atol(valstr);
      XtFree(valstr);
   }
   if ( ( valstr = XmTextGetString(flsexphrsTE) ) )
   {
      paraminfo->flsexphrs = atol(valstr);
      XtFree(valstr);
   }
   if ( ( valstr = XmTextGetString(flwexphrsTE) ) )
   {
      paraminfo->flwexphrs = atol(valstr);
      XtFree(valstr);
   }
   
   
   return;
}


/************************************************************************
   
   Put data into the display.
   
   **********************************************************************/

void	write_rpfparams_info(void)
{
   RpfParams	*paraminfo = NULL;
   
   char		buf[MAX_BUF_LEN];
   
   
   if ((paraminfo = (RpfParams *) GetRpfParams(NULL)) != NULL)
   {
      sprintf(buf, "%-ld", paraminfo->obshrs);
      XmTextSetString(obshrsTE, buf);
      
      sprintf(buf, "%-ld", paraminfo->fcsthrs);
      XmTextSetString(fcsthrsTE, buf);

      XmTextSetString(missvalTE, paraminfo->missval);
      XmTextSetString(misscatTE, paraminfo->misscat);
      XmTextSetString(misstimTE, paraminfo->misstim);
      
      sprintf(buf, "%-ld", paraminfo->rvsexphrs);
      XmTextSetString(rvsexphrsTE, buf);
      
      sprintf(buf, "%-ld", paraminfo->flsexphrs);
      XmTextSetString(flsexphrsTE, buf);
      
      sprintf(buf, "%-ld", paraminfo->flwexphrs);
      XmTextSetString(flwexphrsTE, buf);
      
      
      /*
      	  Cleanup.
      */
      FreeRpfParams(paraminfo);
   }
   
   
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void rpfparams_closeCB(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(rpfparamsDS))
   {
      XtDestroyWidget(rpfparamsDS);
      rpfparamsDS = NULL;
   }
   
   return;
}



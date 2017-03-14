
#include "desiredProductAbs.h"
#include "desiredProductRel.h"
#include "desiredProduct_show.h"
#include "HvDisplayControl.h"    /* for getHvDisplayControl() call */
#include "display_control.h"     /* for access to ad_productLI widget var */
#include "display_control_show.h" /* for access to the dc_window_status
                                     routine. */


/**********************************************************************/

void	desiredProductShow(Widget w)
{
   DesiredProductRecord *dpr = NULL ;

   if (! desiredProductDS)
   {
   	dpr = getDesiredProductRecord();
   
	create_desiredProductDS(GetTopShell(w));
	addDesiredProductCallbacks(dpr);
   }
	
   if (! XtIsManaged(desiredProductDS))
   {      
	/*
	     Manage widget
	*/
	XtManageChild(desiredProductFO);
	XtManageChild(desiredProductDS);

	
	/*
	     load the 2 lists
	*/
	dpAbsLoadList(dpr);
	dpRelLoadList(dpr);

	
	/*
	     select a position in each list
	*/
	if (dpr->absHead)
	{
             XmListSelectPos(dp_absLI, 1, True);
        }
	if (dpr->relHead)
	{
             XmListSelectPos(dp_relLI, 1, True);
        }
	
	
	/*
	     fill the resulting desired list with data according to
	     what is in the database
        */
	updateDesiredList(dpr);
   }      
	
	return;
}

/**********************************************************************/

void addDesiredProductCallbacks(DesiredProductRecord *dpr)
{
   	Atom		atom;
	
	/*
	    Window manager callbacks.
	*/
	atom = XmInternAtom(XtDisplay(desiredProductDS),
			    "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(desiredProductDS, atom,
				desiredProductCloseCallback, dpr);
	XtAddCallback(dp_closePB, XmNactivateCallback,
		      desiredProductCloseCallback, dpr);

	XtAddCallback(dp_resultLI, XmNdefaultActionCallback,
		      dpResultSelectCallback, NULL);
	XtAddCallback(dp_resultLI, XmNbrowseSelectionCallback,
		      dpResultSelectCallback, NULL);

	/*
	     Add callbacks
	*/
	addAbsProductCallbacks(dpr);
	
	addRelProductCallbacks(dpr);

        return;   
}

/**********************************************************************/
void	dpResultSelectCallback(Widget w, XtPointer ptr, XtPointer cbs)
{

     int pos;
     
     pos = ListRsrcGetFirstSelectedPos(dp_resultLI);
     
     XmListDeselectPos(dp_resultLI, pos);
   
     return;
}
/**********************************************************************/   
   
void	desiredProductCloseCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     DesiredProductRecord *dpr = (DesiredProductRecord * ) ptr;
     HvDisplayControl *hdc = getHvDisplayControl();
     
     printf("desiredProductCloseCallback\n");
     
     
     /* update the raw desired info that may have just edited */ 
     
     hdc->displaySettings.areal.DesiredControls =
	readArealProductControlFromDb(&(hdc->displaySettings.areal.numDesiredControls));
     printf("numDesiredProducts re-read= %ld\n",
	    hdc->displaySettings.areal.numDesiredControls);
     
     if (XtIsManaged(desiredProductDS))
     {
	  XtDestroyWidget(desiredProductDS);
	  desiredProductDS = NULL;
	  
	  if (dpr->absHead)
	  {
	       /*FreeHvAbsDesiredProd(dpr->absHead); */
	       dpr->absHead = NULL;
	       
	  }
	  if (dpr->relHead)
	  {
	       /* FreeHvRelDesiredProd(dpr->relHead); */
	       dpr->relHead = NULL;
	  }
     }
     
     
     /* always reload the list of products if the 
	list is currently managed.  */ 
     
     if (dc_window_status(-1))
     {
	printf("updating list of areal products...\n");
	loadProductList(hdc, True);
     }

     
     return;   
}

/**********************************************************************/

void updateDesiredList(DesiredProductRecord *dpr)
{
   
     ArealProductSpecifier *specs = NULL;
     int numSpecs;
     int i = 0;
     LongText *text = NULL;
     struct tm *tmPtr;
     long hours = 0;
     char timeString[BUFSIZ];
     
	
     /*
          Get the desired specifiers from the database
     */
     specs = readArealProductControls(&numSpecs); 
   
     
     /*
     sort the array of specifiers
     */  
     qsort(specs, numSpecs,
	   sizeof(ArealProductSpecifier),
	   compareSpecifiers);
     
     
     /*
          allocate memory for the text
     */
     text = (LongText *) malloc (sizeof(LongText) * numSpecs);
     if (!text)
     {
         return;	
     }
     
     
     
     /*
          load the text 
     */
     for (i = 0; i < numSpecs; i++)
     {
	
	  tmPtr = gmtime(&specs[i].endTime);
	  hours =  specs[i].duration/SECONDS_PER_HOUR;
	  
	  
	  strftime(timeString, 20, "%a %m/%d %H:%M ",tmPtr);
	  
	  if (hours > 1)
	  {
	       sprintf(text[i], "%-4ld hours %s",
		       hours,
		       timeString);
	  }
	  
	  if (hours == 1)
	  {
	       sprintf(text[i], "%-4ld hour  %s",
		       hours,
		       timeString);
	  }
	  
	
     }
     
     
     /*
          load the XmList with text
     */
     loadXmList(dp_resultLI, text, numSpecs);
     
     
     /*
          free memory
     */
     if (text)
     {
	free(text);
	text = NULL;
     }
     
     return;   
}

/**********************************************************************/

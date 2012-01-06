/****************************************************************************

This is a generic dialog to select items.  The callback function and the list of contents
must be passed in.

Creator: Chip Gobs
Date: 12/19/05

*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>

#include "Xtools.h"
#include "Filter.h"

#include "ItemChooser.h"
#include "ItemChooserDialog.h"


static int *ic_selectedPositionArray = NULL;
static int ic_itemCount = 0;
static ApplyFunctionPtr ic_applyFunctionPtr;
static CloseFunctionPtr ic_closeFunctionPtr;
static int ic_allowMultipleSelection = 1;

/*****************************************************************************/

                              
void show_item_chooser(Widget w, 
			      char *dialogTitleString,
                              LongText * itemStringArray,
                              int *selectedPositionArray,
                              int itemCount,
                              int allowMultipleSelection,
                              ApplyFunctionPtr applyFunctionPtr,
                              CloseFunctionPtr closeFunctionPtr)
{
         
    Arg al[64];                    /* Arg List */
	register int ac = 0;           /* Arg Count */
	           
   /* create the dialog */
   
    printf("show_generic_item_chooser(): itemCount = %d\n", itemCount);
   
   
   
   if ( ! itemChooserDS)
   {
      create_itemChooserDS(GetTopShell(w));
      
      SetTitle(itemChooserDS, dialogTitleString);
      
      ic_allowMultipleSelection = allowMultipleSelection;
      
      if ( ! allowMultipleSelection)
      {
      	  XtSetArg(al[ac], XmNselectionPolicy, XmBROWSE_SELECT); ac++; 
      	  XtSetValues(ic_itemLI, al, ac );  
      		
      }
        
      /* set up the callbacks */
                  
      XtAddCallback(ic_applyPB,  XmNactivateCallback, ic_apply, NULL);
      XtAddCallback(ic_cancelPB, XmNactivateCallback, ic_close, NULL);
      
   } 
   
   /* Store these as file-global, so that I can get them from the apply function
    */
   ic_selectedPositionArray = selectedPositionArray;
   ic_itemCount = itemCount;
   ic_applyFunctionPtr = applyFunctionPtr;
   ic_closeFunctionPtr = closeFunctionPtr;
      
   /* load in the data for the three lists */
   loadList(itemStringArray, selectedPositionArray, itemCount);            

   if (! XtIsManaged(itemChooserDS))
   {
      /*  Manage dialog shell and form  */
      XtManageChild(itemChooserFO);
      XtManageChild(itemChooserDS);
   }
   else
   {
   	     printf("show_generic_item_chooser(): ERROR ----- I claim to be managed\n");
   }
   
   XRaiseWindow(XtDisplay(itemChooserDS), XtWindow(itemChooserDS));
   
   return;
}

/*****************************************************************************/

void loadList(LongText * itemStringArray, int selectedPositionArray[], int itemCount)
{
   int 			i = 0;
   XmStringTable	xmStr;
   
   XmListDeleteAllItems(ic_itemLI);
   
//   printf("loadList(): itemCount = %d\n", itemCount);
//   for (i = 0; i < itemCount; i++)
 //  {
 //      printf("loadList(): itemStringArray[%d] = :%s:\n", i, itemStringArray[i]);	
   	
 //  }
  
   /* load in the data needed for the three lists */
   
   
    xmStr = (XmStringTable) XtMalloc(itemCount * sizeof(XmString *));
  
    if (xmStr == NULL)
    {
        return;	
    }
    
    for (i = 0; i < itemCount; i++)
    {
        xmStr[i] = XmStringCreateSimple (itemStringArray[i]) ;
    }  
    	
   
   // printf("loadList(): 1 \n" );  

    /* Add and select any items that were in the selectedPositionArray. */
    for ( i = 0 ; i < itemCount ; i++ )
    {
    	XmListAddItem(ic_itemLI, xmStr[i], 0); 
    	
        if ( selectedPositionArray [i] == 1 )
        {
            XmListSelectPos ( ic_itemLI , i + 1 , False ) ;
        }
    }
    
  //  printf("loadList(): 2 \n" );  
    
    
    /* need free the XmString memory */    
    for (i=0; i < itemCount; i++)
    {
	    XmStringFree(xmStr[i]);
    }
    
    
 //   printf("loadList(): 3 \n" );  
    
    if (xmStr != NULL)
    {
        XtFree((char *)xmStr); 
    }
    
    
//    printf("loadList(): 4 \n" );  
    
   return;   
}


/*****************************************************************************/

void ic_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	//char header[] = "ic_apply(): ";
	int i = 0;
	int * positionArray = NULL;
	int posCount = 0;
	int position = 0;
		
	/* clear the selected position  array */
	for (i = 0; i < ic_itemCount; i++)
	{
	   ic_selectedPositionArray[i] = False;
		
	}
	
	/* get the array of selected  */
	XmListGetSelectedPos(ic_itemLI, &positionArray, &posCount);
	

 //   printf("\nic_apply() ------------------------------\n");	
/* 
	if (positionArray)
	{
		for (i = 0; i < posCount; i++)
		{
		    printf("ic_apply(): positionArray[%d] = %d\n", i, positionArray[i]);		
		}
	}
*/
//	printf("ic_apply() ------------------------------\n");	
	    
	
	if (positionArray != NULL)
	{
         for (i = 0 ; i < posCount; i++)
         {
         	 position = positionArray[i] - 1;
         //	 printf("%s position = %d\n", header, position);
             ic_selectedPositionArray[position] = True;
         //    printf("ic_apply(): selected position = %d \n", position);
         }
         
         XtFree((XtPointer) positionArray);
         positionArray = NULL;
	}
	
	if (ic_applyFunctionPtr != NULL)
	{
	    ic_applyFunctionPtr(ic_selectedPositionArray, ic_itemCount);
	}  
      
    return;
}


/*****************************************************************************/

void ic_close(Widget w, XtPointer ptr, XtPointer cbs)
{  
	
	if (ic_closeFunctionPtr != NULL)
	{
	    ic_closeFunctionPtr(ic_selectedPositionArray);
	}
	
    ic_cleanup();
   
    return;
}

/*****************************************************************************/
void ic_cleanup()
{
	 XtUnmanageChild(itemChooserDS);
  
     XtDestroyWidget(itemChooserDS);
     itemChooserDS = (Widget) NULL;
}

/*****************************************************************************/


#include "desiredProductRel.h"
#include "desiredProduct_show.h"

/**********************************************************************/

void addRelProductCallbacks(DesiredProductRecord *dpr)
{
        /*
             Add the callbacks for the relative section
        */
  
	/*
		list callbacks
	*/
        XtAddCallback(dp_relLI, XmNdefaultActionCallback, dpRelSelectItemCallback, dpr);
        XtAddCallback(dp_relLI, XmNbrowseSelectionCallback, dpRelSelectItemCallback, dpr);
	
	
	/*
	     push button callbacks
	*/
        XtAddCallback(dp_relApplyPB, XmNactivateCallback, dpRelApplyCallback, dpr);
	XtAddCallback(dp_relDeletePB, XmNactivateCallback, dpRelDeleteCallback, dpr);

	
	/*
	     text verification callbacks
	*/
        XtAddCallback(dp_relDurTE, XmNmodifyVerifyCallback,
		      (XtCallbackProc)num_filter,
		      (XtPointer)INTEGERS);
     
	XtAddCallback(dp_relEndHourOffsetTE, XmNmodifyVerifyCallback,
		      (XtCallbackProc)num_filter,
		      (XtPointer)INTEGERS);
	
        return;     
}

/**********************************************************************/

void dpRelApplyCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     /*
          save to the database
     */
     
     DesiredProductRecord *dpr = (DesiredProductRecord * ) ptr;
     HvRelDesiredProd prod;
     int pos;
     
     
     /*
          set the cursor while waiting
     */
     SetCursor(GetTopShell(w), XC_watch);
        
     
     printf("dpRelApplyCallback\n");
     
     
     /*
          Get the data from the widgets,
	  Save to database
	  reLoad the Relative list
     */
     dpRelUnloadWidgets(&prod);
     
     dpRelSave(&prod);
     
     dpRelLoadList(dpr);
     
     pos = getHvRelDesiredProdPos(dpr->relHead, &prod);
     
     if (pos != -1)
     {
	  printf("pos = %d\n",pos);
          XmListSelectPos(dp_relLI, pos, True);
     }
     
     updateDesiredList(dpr);

     
     /*
          Change the cursor back
     */
     UnsetCursor(GetTopShell(w));
   
     
     return;   
}

/**********************************************************************/

void dpRelDeleteCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     /*
           deletes the selected row   
     */
     
     DesiredProductRecord *dpr = (DesiredProductRecord *) ptr;
     HvRelDesiredProd *relPtr = NULL;
     int pos = -1;
     
     SetCursor(GetTopShell(w), XC_watch);
   
     
     printf("dpRelDeleteCallback\n");
     
   
     relPtr = (HvRelDesiredProd *) ListNth(&dpr->relHead->list,
					   dpr->relSelectedPos);
     
     if (relPtr)
     {
	  
	  /*
	      stored position of selected pos
	  */
	  pos = dpr->relSelectedPos;
	  
	  
	  /*
	       delete the row
	  */
          dpRelDelete(relPtr);
	  
	  
	  /*
	      re-load the list
	  */
	  dpRelLoadList(dpr);
     
	  
	  /*
	      select the old position 
	  */
	  if (pos != -1)
	  {
               XmListSelectPos(dp_relLI, pos, True);
	  }
     }
     
     updateDesiredList(dpr);

     UnsetCursor(GetTopShell(w));
   
     
     return;   
}

/**********************************************************************/

void	dpRelSelectItemCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     /*
          loads the edit widgets according to the selected item
	  in the list
     */
     
     DesiredProductRecord *dpr = (DesiredProductRecord *) ptr;
     HvRelDesiredProd *relPtr = NULL;
     
     printf("dpLoadWidgetsCallback\n");
     
     
     /*
          set the selected pos according to the XmList
     */
     dpr->relSelectedPos = ListRsrcGetFirstSelectedPos(dp_relLI);

     
     /*
          get the correct record and load the widgets
     */
     if (dpr->relHead)
     {
          relPtr = (HvRelDesiredProd *) ListNth(&dpr->relHead->list,
						dpr->relSelectedPos);
          dpRelLoadWidgets(relPtr);
     }
     
     return;   
}


/**********************************************************************/

int getHvRelDesiredProdPos(HvRelDesiredProd *relHead, HvRelDesiredProd *prod)
{
     /*
          Finds the position of prod in the list whose first node is
	  relHead.
     */
     
     
     int pos = -1;
     int i = 0;
     
     HvRelDesiredProd *relPtr = NULL;
     
     if (relHead)
     {
	  i = 1;
          relPtr = (HvRelDesiredProd *) ListFirst(&relHead->list);
	  
	  while (relPtr)
	  {
	      if (memcmp(relPtr, prod, sizeof(HvRelDesiredProd)) == 0)
	      {
	           pos = i;
		   break;	   
	      }
	       
	      
	      i++;
	      relPtr = (HvRelDesiredProd *) ListNext(&relPtr->node);    
	  }
	  
	  
     }
     
     
     return pos;
}

/**********************************************************************/

void dpRelLoadList(DesiredProductRecord *dpr)
{
     /*
          loads the list of relative products
     
     */
     
     char activityString[BUFSIZ];
     char where[BUFSIZ];
     RussText text[MAX_DPRODUCTS];
     
     long count = 0;
     long i = 0;
     
     HvRelDesiredProd *relPtr = NULL;
          
     /*
          Free old memory
     */
     if (dpr->relHead)
     { 
          /* FreeHvRelDesiredProd(dpr->relHead); */
     }
     
     
     /*
          Make database call and
	  create array of strings to put into XmList
     */
     
     sprintf(where," ORDER BY dur_hours, end_hour_offset ");
     
     /* if ( ( dpr->relHead = GetHvRelDesiredProd(where) ) != NULL ) */
     if ( dpr->relHead != NULL )
     {
          relPtr = (HvRelDesiredProd *) ListFirst(&dpr->relHead->list);	  
	  
	  while (relPtr && (count < MAX_DPRODUCTS))
	  {
	        if (relPtr->active[0] =='T')
		{
	             strcpy(activityString,"Active");	  
		}
		else
		{
		     strcpy(activityString, "Inactive");    
		}
		
	        sprintf(text[i], "%8ld hours %8ld hours %10s",
			relPtr->end_hour_offset, relPtr->dur_hours,
			activityString);   
		
		i++;
	       
	       relPtr = (HvRelDesiredProd *) ListNext(&relPtr->node);	     
	  }
	  
     }
     
     count = i;
     
     /*
          Put strings into XmList
     */
     loadXmList100(dp_relLI, text, count);
     
     return;     
}

/**********************************************************************/

void dpRelGetWhereClauseFromKey(HvRelDesiredProd *hPtr, char *where)
{
     
     /*
          Get the where clause that specifies the key of
	  the database row that corresponds to hPtr
     */
     
     sprintf(where, "WHERE end_hour_offset = %ld and dur_hours = %ld ",
	     hPtr->end_hour_offset,
	     hPtr->dur_hours);
     
     return;   
}

/**********************************************************************/

void dpRelLoadWidgets(HvRelDesiredProd *hPtr)
{
     
     /*
          Given a pointer that corresponds to a row in the
	  database, load the widgets with data
     */
     
     char buf[BUFSIZ];
     
     
     /*
          set end_hour_offset 
     */
     sprintf(buf,"%ld", hPtr->end_hour_offset);
     XmTextSetString(dp_relEndHourOffsetTE, buf);
     
     
     /*
          set duration
     */
     sprintf(buf,"%ld", hPtr->dur_hours);
     XmTextSetString(dp_relDurTE, buf);
     
     
     /*
          set active/inactive toggle buttons
     */
     if (hPtr->active[0] == 'T')
     {
          XmToggleButtonSetState(dp_relActiveTB, True, False);
	  XmToggleButtonSetState(dp_relInactiveTB, False, False); 	  
     }
     else
     {
	  XmToggleButtonSetState(dp_relInactiveTB, True, False); 
	  XmToggleButtonSetState(dp_relActiveTB, False, False); 	  
     }
     
     return;
}
/**********************************************************************/

void dpRelUnloadWidgets(HvRelDesiredProd *hPtr)
{
     
     /*
          Takes the data in the widgets and convert it to
	  hPtr
     */
     
     char * buf = NULL;
     Boolean btn_state;
     
     /*
     Get duration value
     */
     if ( ( buf = XmTextGetString(dp_relEndHourOffsetTE) ) != NULL  )
     {
	  if (IsNull(CHAR, buf) == NOTNULL)
	  {       
	       hPtr->end_hour_offset = atol(buf);
	  }
	  XtFree(buf);
     }
     
     
     /*
     Get duration value
     */
     if ( ( buf = XmTextGetString(dp_relDurTE) ) != NULL )
     {
	  if (IsNull(CHAR, buf) == NOTNULL)
	  {       
	       hPtr->dur_hours = atol(buf);
	       if (hPtr->dur_hours < 1)
	       {
	            hPtr->dur_hours = 1;
		    XmTextSetString(dp_relDurTE,"1");
	       }
	  }
	  XtFree(buf);
     }
     
     
     /*
     get activity boolean
     */
     btn_state = XmToggleButtonGetState(dp_relActiveTB);
     if (btn_state)
     {
	  strcpy(hPtr->active,"T");     
     }
     else
     {
	  strcpy(hPtr->active,"F");     
     }
     return;   
}

/**********************************************************************/

void dpRelSave(HvRelDesiredProd *hPtr)
{
     /*
          Inserts or Updates the database with a row
	  that corresponds to hPtr
     */
     
     
     char where[BUFSIZ];
     char msg[BUFSIZ];
     int count;
     int error = 0;
   
     dpRelGetWhereClauseFromKey(hPtr, where);
   
     count = recordCount("HvRelDesiredProd",where);	  
	  
	  
     if ( count > 0 )	  
     {
          /* if ((error = UpdateHvRelDesiredProd(hPtr, where)) != 0) */
	  if ( error != 0 )
	  {
		  sprintf(msg, "Unable to update record: %d\n", error);
		  ErrorDialog(desiredProductDS, msg);
	  }    
     }
     
     else
     {
          /* if ((error = PutHvRelDesiredProd(hPtr) != 0)) */
          if ( error != 0 ) 
	  {
	          sprintf(msg, "Unable to insert record: %d\n", error);
	          ErrorDialog(desiredProductDS, msg);	   
		   
	  }
     }
	 
    
     return;   
}

/**********************************************************************/

void dpRelDelete(HvRelDesiredProd *hPtr)
{
     
     /*
          Deletes the row that corresponds to hPtr
     */
     
     char where[BUFSIZ];
     char msg[BUFSIZ];
     int count;
     int error = 0;
  
     dpRelGetWhereClauseFromKey(hPtr, where);
   
     count = recordCount("HvRelDesiredProd",where);	  
	  
	  
     if ( count > 0 )	  
     {
          /* if ((error = DeleteHvRelDesiredProd(where)) != 0) */
          if ( error != 0 )
	  {
		  sprintf(msg, "Unable to delete record: %d\n", error);
		  ErrorDialog(desiredProductDS, msg);
	  }    
     }  
     
     return;
}

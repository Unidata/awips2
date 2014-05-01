#include "desiredProductAbs.h"
#include "desiredProduct_show.h"

/**********************************************************************/

void addAbsProductCallbacks(DesiredProductRecord *dpr)
{
     /*
     Add the callbacks for the absolute section
     */
     
     /*
     list callbacks
     */
     XtAddCallback(dp_absLI, XmNdefaultActionCallback,
		   dpAbsSelectItemCallback, dpr);
     XtAddCallback(dp_absLI, XmNbrowseSelectionCallback,
		   dpAbsSelectItemCallback, dpr);
     
     
     /*
     push button callbacks
     */
     XtAddCallback(dp_absApplyPB, XmNactivateCallback,
		   dpAbsApplyCallback, dpr);
     XtAddCallback(dp_absDeletePB, XmNactivateCallback,
		   dpAbsDeleteCallback, dpr);
     
     
     /*
     text verification callbacks
     */
     XtAddCallback(dp_absDurTE, XmNmodifyVerifyCallback,
		   (XtCallbackProc)num_filter,
		   (XtPointer)INTEGERS);
     
     XtAddCallback(dp_absEndHourTE, XmNmodifyVerifyCallback,
		   (XtCallbackProc)num_filter,
		   (XtPointer)INTEGERS);
     
     
     XtAddCallback(dp_absLookbackHoursTE, XmNmodifyVerifyCallback,
		   (XtCallbackProc)num_filter,
		   (XtPointer)INTEGERS);
     
     
     return;     
}

/**********************************************************************/

void dpAbsApplyCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     /*
     save to the database
     */
     
     DesiredProductRecord *dpr = (DesiredProductRecord * ) ptr;
     HvAbsDesiredProd prod;
     int pos;
     
     SetCursor(GetTopShell(w), XC_watch);
   
     
     printf("dpAbsApplyCallback\n");
     
     dpAbsUnloadWidgets(&prod);
     
     dpAbsSave(&prod);
     
     dpAbsLoadList(dpr);
     
     pos = getHvAbsDesiredProdPos(dpr->absHead, &prod);
     
     if (pos != -1)
     {
          XmListSelectPos(dp_absLI, pos, True);
     }
     
     updateDesiredList(dpr);
     
     UnsetCursor(GetTopShell(w));
   
     
     
     return;   
}

/**********************************************************************/

void dpAbsDeleteCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     /*
     deletes the selected row   
     */
     
     DesiredProductRecord *dpr = (DesiredProductRecord *) ptr;
     HvAbsDesiredProd *absPtr = NULL;
     int pos = -1;
     
     SetCursor(GetTopShell(w), XC_watch);
     
     
     printf("dpAbsDeleteCallback\n");
     
     
     absPtr = (HvAbsDesiredProd *) ListNth(&dpr->absHead->list,
					   dpr->absSelectedPos);
     
     if (absPtr)
     {
	  
	  /*
	  stored position of selected pos
	  */
	  pos = dpr->absSelectedPos;
	  
	  
	  /*
	  delete the row
	  */
          dpAbsDelete(absPtr);
	  
	  
	  /*
	  re-load the list
	  */
	  dpAbsLoadList(dpr);
	  
	  
	  /*
	  select the old position 
	  */
	  if (pos != -1)
	  {
               XmListSelectPos(dp_absLI, pos, True);
	  }
     }
     
     updateDesiredList(dpr);

     UnsetCursor(GetTopShell(w));
     
     return;   
}

/**********************************************************************/

void	dpAbsSelectItemCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     /*
     loads the edit widgets according to the selected item
     in the list
     */
     
     DesiredProductRecord *dpr = (DesiredProductRecord *) ptr;
     HvAbsDesiredProd *absPtr = NULL;
     
     printf("dpLoadWidgetsCallback\n");
     
     
     /*
     set the selected pos according to the XmList
     */
     dpr->absSelectedPos = ListRsrcGetFirstSelectedPos(dp_absLI);
     
     
     /*
     get the correct record and load the widgets
     */
     if (dpr->absHead)
     {
	  absPtr = (HvAbsDesiredProd *) ListNth(&dpr->absHead->list,
						dpr->absSelectedPos);
          dpAbsLoadWidgets(absPtr);
     }
     
     return;   
}


/**********************************************************************/

int getHvAbsDesiredProdPos(HvAbsDesiredProd *absHead, HvAbsDesiredProd *prod)
{
     /*
     Finds the position of prod in the list whose first node is
     absHead.
     */
     
     
     int pos = -1;
     int i = 0;
     
     HvAbsDesiredProd *absPtr = NULL;
     
     if (absHead)
     {
	  i = 1;
          absPtr = (HvAbsDesiredProd *) ListFirst(&absHead->list);
	  
	  while (absPtr)
	  {
	       if (memcmp(absPtr, prod, sizeof(HvAbsDesiredProd)) == 0)
	       {
		    pos = i;
		    break;	   
	       }
	       
	       
	       i++;
	       absPtr = (HvAbsDesiredProd *) ListNext(&absPtr->node);    
	  }
	  
	  
     }
     
     
     return pos;
}

/**********************************************************************/

void dpAbsLoadList(DesiredProductRecord *dpr)
{
     /*
     loads the list of desired products
     
     */
      
              
     char activityString[BUFSIZ];
     RussText text[MAX_DPRODUCTS];
     
     long count = 0;
     long i = 0;
     
     HvAbsDesiredProd *absPtr = NULL;
     
     /*
     Free old memory
     */
     if (dpr->absHead)
     { 
     /*     FreeHvAbsDesiredProd(dpr->absHead);	   */
     }
     
     
     /*
     Make database call and
     create array of strings to put into XmList
     */
      /*if ( ( dpr->absHead =
            GetHvAbsDesiredProd(" ORDER BY dur_hours, end_hour ") ) != NULL ) */
     if ( dpr->absHead != NULL )
     {
	  absPtr = (HvAbsDesiredProd *) ListFirst(&dpr->absHead->list);	  
	  
	  while (absPtr && (count < MAX_DPRODUCTS))
	  {
	       if (absPtr->active[0] =='T')
	       {
		    strcpy(activityString,"Active");	  
	       }
	       else
	       {
		    strcpy(activityString, "Inactive");    
	       }
	       
	       sprintf(text[i], "%6ld Z %6ld hours %6ld hours %8s",
		       absPtr->end_hour, absPtr->dur_hours,
		       absPtr->lookback_hours,
		       activityString);   
	       
	       i++;
	       
	       absPtr = (HvAbsDesiredProd *) ListNext(&absPtr->node);	     
	  }
	  
     }
     
     count = i;
     
     /*
     Put strings into XmList
     */
     loadXmList100(dp_absLI, text, count);
     
     return;     
}

/**********************************************************************/

void dpAbsGetWhereClauseFromKey(HvAbsDesiredProd *hPtr, char *where)
{
     
     /*
     Get the where clause that specifies the key of
     the database row that corresponds to hPtr
     */
     
     sprintf(where, "WHERE end_hour = %ld and dur_hours = %ld ",
	     hPtr->end_hour,
	     hPtr->dur_hours);
     
     return;   
}

/**********************************************************************/

void dpAbsLoadWidgets(HvAbsDesiredProd *hPtr)
{
     
     /*
     Given a pointer that corresponds to a row in the
     database, load the widgets with data
     */
     
     char buf[BUFSIZ];
     
     
     /*
     set end_hour_offset 
     */
     sprintf(buf,"%ld", hPtr->end_hour);
     XmTextSetString(dp_absEndHourTE, buf);
     
     
     /*
     set duration
     */
     sprintf(buf,"%ld", hPtr->dur_hours);
     XmTextSetString(dp_absDurTE, buf);
     
     
     /*
     set lookback hours
     */
     sprintf(buf,"%ld", hPtr->lookback_hours);
     XmTextSetString(dp_absLookbackHoursTE, buf);
     
     
     
     
     /*
     set active/inactive toggle buttons
     */
     if (hPtr->active[0] == 'T')
     {
          XmToggleButtonSetState(dp_absActiveTB, True, False);
	  XmToggleButtonSetState(dp_absInactiveTB, False, False); 	  
     }
     else
     {
	  XmToggleButtonSetState(dp_absInactiveTB, True, False); 
	  XmToggleButtonSetState(dp_absActiveTB, False, False); 	  
     }
     
     return;
}
/**********************************************************************/

void dpAbsUnloadWidgets(HvAbsDesiredProd *hPtr)
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
     if ( ( buf = XmTextGetString(dp_absEndHourTE) ) != NULL )
     {
	  if (IsNull(CHAR, buf) == NOTNULL)
	  {       
	       hPtr->end_hour = atol(buf);
	  }
	  XtFree(buf);
     }
     
     
     /*
     Get duration value
     */
     if ( ( buf = XmTextGetString(dp_absDurTE) ) != NULL )
     {
	  if (IsNull(CHAR, buf) == NOTNULL)
	  {       
	       hPtr->dur_hours = atol(buf);
	       if (hPtr->dur_hours < 1)
	       {
	            hPtr->dur_hours = 1;
		    XmTextSetString(dp_absDurTE,"1");
	       }
	  }
	  XtFree(buf);
     }
     
     
     /*
     Get lookback_hours value
     */
     if ( ( buf = XmTextGetString(dp_absLookbackHoursTE) ) != NULL )
     {
	  if (IsNull(CHAR, buf) == NOTNULL)
	  {       
	       hPtr->lookback_hours = atol(buf);
	  }
	  XtFree(buf);
     }
     
     
     /*
     get activity boolean
     */
     btn_state = XmToggleButtonGetState(dp_absActiveTB);
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

void dpAbsSave(HvAbsDesiredProd *hPtr)
{
     /*
     Inserts or Updates the database with a row
     that corresponds to hPtr
     */
     
     
     char where[BUFSIZ];
     /* char msg[BUFSIZ]; */
     int count;
     /* int error; */
     
     dpAbsGetWhereClauseFromKey(hPtr, where);
     
     count = recordCount("HvAbsDesiredProd",where);	  
     
     
     if ( count > 0 )	  
     {
         /*  if ((error = UpdateHvAbsDesiredProd(hPtr, where)) != 0)
	  {
	       sprintf(msg, "Unable to update record: %d\n", error);
	       ErrorDialog(desiredProductDS, msg);
	  } */    
     }
     
     else
     {
          /* if ((error = PutHvAbsDesiredProd(hPtr) != 0))
	  {
	       sprintf(msg, "Unable to insert record: %d\n", error);
	       ErrorDialog(desiredProductDS, msg);	   
	       
	  } */
     }
     
     
     return;   
}

/**********************************************************************/

void dpAbsDelete(HvAbsDesiredProd *hPtr)
{
     
     /*
     Deletes the row that corresponds to hPtr
     */
     
     char where[BUFSIZ];
     /* char msg[BUFSIZ]; */
     int count;
     /* int error; */
     
     dpAbsGetWhereClauseFromKey(hPtr, where);
     
     count = recordCount("HvAbsDesiredProd",where);	  
     
     
     if ( count > 0 )	  
     {
          /* if ((error = DeleteHvAbsDesiredProd(where)) != 0)
	  {
	       sprintf(msg, "Unable to delete record: %d\n", error);
	       ErrorDialog(desiredProductDS, msg);
	  }*/    
     }  
     
     return;
}

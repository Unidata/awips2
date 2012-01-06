/*
	File:		agencyoffice_show.c
	Date:		January 1998
	Author:		Paul Taylor
	Reworked:       Chip Gobs

	Purpose:	Provides support for the
			Cooperating Agencies/Offices DS.
*/


#include "agencyoffice_show.h"


/***********************************************************************/

void	show_agencyoffice(Widget w, const char *lid)
{
     AgencyOfficeRec *aor = getAgencyOfficeRec();
    
     if (! agcyoffDS) 
     {
	  
	  /*
	       init aor
	  */
	  strcpy(aor->lid, lid);
	  aor->availableHead = NULL;	  
	  aor->selectedHead = NULL;
	  
	  
	  create_agcyoffDS(GetTopShell(w));
	  
	  ao_AddCallbacks(aor);
	  
 
	  set_window_title(agcyoffDS,
			   "Cooperating Agencies/Offices", aor->lid);
	  
	  
	  /*
	  DeSensitize(agcyoff_addPB);
	  DeSensitize(agcyoff_delPB);
	  */
	  
	  ao_LoadAvailableList(aor);
	  ao_LoadSelectedList(aor);

	  
	  XtManageChild(agcyoffFM);
	  XtManageChild(agcyoffDS);
     }
     
     
     return;
}

/***********************************************************************/

AgencyOfficeRec * getAgencyOfficeRec(void)
{
     static AgencyOfficeRec agencyOfficeRec;   
     
     return &agencyOfficeRec;
}

/***********************************************************************/

void	ao_AddCallbacks(AgencyOfficeRec *aor)
{
     Atom	wmAtom;
     /*
     Window manager callbacks.
     */
     wmAtom = XmInternAtom(XtDisplay(agcyoffDS), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(agcyoffDS, wmAtom, ao_CloseCallback, aor);
     
     
     /*
     Button callbacks.
     */
     XtAddCallback(agcyoff_addPB, XmNactivateCallback, ao_ApplyCallback, aor);
     XtAddCallback(agcyoff_delPB, XmNactivateCallback, ao_DeleteCallback, aor);
     XtAddCallback(agcyoff_closePB, XmNactivateCallback, ao_CloseCallback, aor);
     
     
     /*
     List callbacks
     */
     XtAddCallback(agcyoffaL, XmNextendedSelectionCallback,
		              ao_LoadWidgetsCallback, aor);
      
     
     XtAddCallback(agcyoffaL, XmNdefaultActionCallback,
		              ao_ListApplyCallback, aor);
     
     XtAddCallback(agcyoffsL, XmNdefaultActionCallback,
		              ao_DeleteCallback, aor);
     
      
     /*
     Add TextFilter callbacks.
     */
     XtAddCallback(agcyoff_agencyTxt, XmNfocusCallback,
		   ao_ClearListSelectionsCallback, aor);

     XtAddCallback(agcyoff_officeTxt, XmNfocusCallback,
		   ao_ClearListSelectionsCallback, aor);
     
     
     agencyoffice_addTextFilterCallbacks();
     
     
     return;
}

/***********************************************************************/


void	agencyoffice_addTextFilterCallbacks(void)
{
     XtAddCallback(agcyoff_agencyTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE_AND_VALIDSYMBOLS+SPACES));
     XtAddCallback(agcyoff_officeTxt, XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)(MIXEDCASE_AND_VALIDSYMBOLS+SPACES));
     
     
     return;
}

/***********************************************************************/
void	ao_UnloadWidgets(AgencyOfficeUnique *aPtr)
{
     char *tempString;
     
     tempString = XmTextGetString(agcyoff_agencyTxt);
     if(tempString)
     {
          strcpy(aPtr->agency_code, tempString);
 	  free(tempString);
	  tempString = NULL;
     }
     
     tempString = XmTextGetString(agcyoff_officeTxt);
     if(tempString)
     {
          strcpy(aPtr->office, tempString);
 	  free(tempString);
	  tempString = NULL;
     }
     
     return;
}
/***********************************************************************/
void	ao_LoadWidgets(AgencyOfficeUnique *aPtr)
{
     	  

     if (aPtr)
     {
	  XmTextSetString(agcyoff_agencyTxt, aPtr->agency_code);
	  XmTextSetString(agcyoff_officeTxt, aPtr->office);
     }
     else
     {
	  ao_ClearWidgets();
     }
     
     
     return;
}

/***********************************************************************/

void ao_ClearWidgets(void)
{
     XmTextSetString(agcyoff_agencyTxt, "");
     XmTextSetString(agcyoff_officeTxt, "");
     
     return;
}
/***********************************************************************/

void ao_ClearWidgetsCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     ao_ClearWidgets();
     
     return;
}
/***********************************************************************/

void ao_ClearListSelectionsCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     XmListDeselectAllItems(agcyoffaL);
     
     return;     
}

/***********************************************************************/

void ao_LoadWidgetsCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     /*
          when a list item has been selected, finds the first selected
	  position and loads the edit widgets with the associated item's
	  information
     */
     AgencyOfficeRec *aor = (AgencyOfficeRec *) ptr;
     AgencyOfficeUnique *aPtr = NULL;
     
     int pos;
     pos = ListRsrcGetFirstSelectedPos(agcyoffaL);

     
     aPtr = (AgencyOfficeUnique *) ListNth(&aor->availableHead->list, pos);
     
     if (aPtr)
     {
          ao_LoadWidgets(aPtr); 	
     }
     
     
     return;    
}

/***********************************************************************/
void ao_CloseCallback(Widget w, XtPointer ptr, XtPointer cbs)
{ 
     AgencyOfficeRec *aor = (AgencyOfficeRec *) ptr;
     
     if (agcyoffDS)
     {
          XtDestroyWidget(agcyoffDS);
          agcyoffDS = NULL;
     }
     
     if (aor->availableHead)
     {
          FreeAgencyOfficeUnique(aor->availableHead);
	  aor->availableHead = NULL;
     }
     
     if (aor->selectedHead)
     {
          FreeLocExtAgency(aor->selectedHead);
	  aor->selectedHead = NULL;
     }
     
     
     Loc_LoadCooperators(aor->lid);

     
     return;
}

/***********************************************************************/
void	ao_ApplyCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
    int pos;
     
	 
     pos  = ListRsrcGetFirstSelectedPos(agcyoffaL);
     
    
     /*
          if there are no selected items in list, then apply any
	  items in the text widgets
     */
     if (pos == -1) 
     {
	  ao_TextApplyCallback(w, ptr, cbs);
     }
     else
     {
          ao_ListApplyCallback(w, ptr, cbs);	  
     }
     
    
     
}
/***********************************************************************/

void	ao_TextApplyCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     AgencyOfficeRec *aor = (AgencyOfficeRec *) ptr;
     LocExtAgency locExtAgency;
     AgencyOfficeUnique agencyOfficeUnique;
  
     ao_UnloadWidgets(&agencyOfficeUnique);
     copyAgencyInfo(&locExtAgency, &agencyOfficeUnique, aor->lid);
     ao_Save(&locExtAgency);
     
     ao_LoadAvailableList(aor);
     ao_LoadSelectedList(aor);     
     
     return;
}

/***********************************************************************/

void	ao_ListApplyCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     AgencyOfficeRec *aor = (AgencyOfficeRec *) ptr;
     AgencyOfficeUnique *aPtr = NULL;
     LocExtAgency locExtAgency;
     long i = 0;          
     int numSelected;
     int *posList = NULL;
     int pos;
 
     XmListGetSelectedPos(agcyoffaL, &posList, &numSelected);
     
     for (i = 0; i < numSelected ; i++)
     {
	  pos = posList[i];
	  aPtr = (AgencyOfficeUnique *) ListNth(&aor->availableHead->list, pos);
	  
	  if (aPtr)
	  {
	       copyAgencyInfo(&locExtAgency, aPtr, aor->lid);
	       ao_Save(&locExtAgency);
	  }
     }
     
     if (posList)
     {
          free(posList);
          posList = NULL;
     }
     
     ao_LoadAvailableList(aor);
     ao_LoadSelectedList(aor);
     
}

/***********************************************************************/

void    ao_DeleteCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     
     AgencyOfficeRec *aor = (AgencyOfficeRec *) ptr;
     LocExtAgency		*leaPtr;
     
     int			*posList = NULL;
     int			pos = -1;
     int			i;
     int			numSelected = 0;
     
 
     XmListGetSelectedPos(agcyoffsL, &posList, &numSelected);
        
     for (i = 0; i < numSelected; i++)
     {
	  pos = posList[i];
	  leaPtr = (LocExtAgency *) ListNth(&aor->selectedHead->list, pos);
	  
	  if (leaPtr)
	  {
	  	ao_Delete(leaPtr);     
	  }
           	
     }
  
     if (posList)
     {
	  free(posList);
	  posList = NULL;
     }
     
     /*  
     agencyoffice_SetSensitivity();
     */
     
     ao_LoadAvailableList(aor);
     ao_LoadSelectedList(aor);
     
     return;
}

/***********************************************************************/

void ao_Delete(LocExtAgency *leaPtr)
{
     
     char where[BUFSIZ];
     char msg[BUFSIZ];
     int error;
     
     ao_createKeyClause(leaPtr, where);
     
     error = DeleteLocExtAgency(where);     
     if (error < 0)
     {
          sprintf(msg, "%s", DbErrorString(error));
          ErrorDialog(agcyoffDS, msg);
     }	       
     
     return;     
}
/***********************************************************************/

void ao_createKeyClause(LocExtAgency *leaPtr, char *where)
{
     sprintf(where, " WHERE lid = '%s' AND agency_code = '%s' " 
	     " AND office = '%s' ", 
	     leaPtr->lid,
	     leaPtr->agency_code,
	     leaPtr->office);

     
     return;     
}
/***********************************************************************/

int  ao_Save(LocExtAgency *leaPtr)
{
     int count;
     char where[BUFSIZ];
     char msg[BUFSIZ];
     int error;
     
   
     ao_createKeyClause(leaPtr, where);
     
     count = recordCount("LocExtAgency", where);
     
     if (count > 0)
     {
	  if ((error = UpdateLocExtAgency(leaPtr, where)) != 0)
	  {
	       sprintf(msg, "%s", DbErrorString(error));
	       ErrorDialog(agcyoffDS, msg);
	  }
     }

     else
     {
	  if ((error = PutLocExtAgency(leaPtr)) != 0)
	  {
	       sprintf(msg, "%s", DbErrorString(error));
	       ErrorDialog(agcyoffDS, msg);
	       
	  }
     }
  
     return(1);  /* success */
}

/***********************************************************************/

void 	agencyoffice_SetSensitivity(void)
{
   
     return;
}

/***********************************************************************/
void  ao_LoadAvailableList(AgencyOfficeRec *aor)
{
     AgencyOfficeUnique *aPtr = NULL;
     char where[BUFSIZ];
     LongText *text;
     int count;
     int i;
     
     if (aor->availableHead)
     {
          FreeAgencyOfficeUnique(aor->availableHead);
	  aor->availableHead = NULL;
     }
     
     XmListDeleteAllItems(agcyoffaL);
     
     sprintf(where, " ORDER BY agency_code, office ");
     
     if ( (aor->availableHead = GetAgencyOfficeUnique(where)) )
     {
          count= ListCount(&aor->availableHead->list);
	  if (count)
	  {
	       text = (LongText *) malloc(sizeof(LongText) * count);
	       if (text == NULL)
	       {
		    aor->availableHead = NULL;
	            return;     	    
	       }
	       else
	       {
		    i = 0;
		    aPtr = (AgencyOfficeUnique *) ListFirst(&aor->availableHead->list);
	            while (aPtr)
		    {
			 sprintf(text[i], "%-10s  %-30s",
				 aPtr->agency_code,
				 aPtr->office);
			 	 
			 i++;
		         aPtr = (AgencyOfficeUnique *) ListNext(&aPtr->node);	 
		    }
		    
		    loadXmList(agcyoffaL, text, i);
		    free(text);
		    text = NULL;
	       }
	       
	  }
     }
     
     return;     
}

/***********************************************************************/

void  ao_LoadSelectedList(AgencyOfficeRec *aor)
{
     
     LocExtAgency *aPtr = NULL;
     char where[BUFSIZ];
     LongText *text;
     int count;
     int i;
     
     if (aor->selectedHead)
     {
          FreeLocExtAgency(aor->selectedHead);
	  aor->selectedHead = NULL;
     }
     
     XmListDeleteAllItems(agcyoffsL);
     
     
     sprintf(where, "WHERE lid ='%s' order by agency_code, office", aor->lid);
     
     
     
     if ( (aor->selectedHead = GetLocExtAgency(where)) )
     {
          count= ListCount(&aor->selectedHead->list);
	  if (count)
	  {
	       text = (LongText *) malloc(sizeof(LongText) * count);
	       if (text == NULL)
	       {
		    aor->selectedHead = NULL;
	            return;     	    
	       }
	       else
	       {
		    i = 0;
		    aPtr = (LocExtAgency *) ListFirst(&aor->selectedHead->list);
	            while (aPtr)
		    {
			 sprintf(text[i], "%-10s  %-30s",
				 aPtr->agency_code,
				 aPtr->office);
			 
			 i++;
		         aPtr = (LocExtAgency *) ListNext(&aPtr->node);	 
		    }
		    
		   
		    loadXmList(agcyoffsL, text, i);
		    free(text);
		    text = NULL;
	       }
		   
	  }
     }
     
     return;     
}
     


/***********************************************************************/


void copyAgencyInfo(LocExtAgency *dest, AgencyOfficeUnique *source, char *lid)
{
     
     strcpy(dest->lid, lid);
     
     strcpy(dest->agency_code, source->agency_code);
     strcpy(dest->office, source->office);
     
     return;
}

/***********************************************************************/


/*
	File:		rejectdata_show.c
	Date:		December 1999
	Author:		Russell Erb
        History         Bryon Lawrence  June 8, 2001
                        Moved the orderby_clause global variable definition
                        from the rejectdata_show.h file into this file.
                        This was done to remedy duplicate symbol 
                        problems that were arising while linking
                        with the gcc compiler/linker. 
	
	Purpose:        To give a viewing interface for the table, RejectedData.
*/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "rejectdata_show.h"
#include "rating_util.h"
#include "get_loc_info.h"

/* Declare orderby_clause as a global variable. Any routines 
   outside of this file will need to link to this global
   variable using a statement such as " extern char * orderby_clause[] " . */
static char * orderby_clause [] ={" ORDER BY lid, pe, ts, validtime DESC",
                                  " ORDER BY validtime DESC, lid, pe, ts "};

/************************************************************************
   
   Load the rejectdata dialog shell.
   
***********************************************************************/

void	rejectdata_show(Widget w, char *location)
{
   
   if (! trashDS)
   {
      /* create the dialog shell and add the callbacks */
      create_trashDS(GetTopShell(w));
      add_rejectdata_cbs();   
   }
   
   
   /* manage the windows now before doing the selections */   
   if(! XtIsManaged(trashDS))
   {
      /* loading pe and pe's name to the list. */ 
      loadpeLI();
      XtManageChild(trashFO);
      XtManageChild(trashDS);

      /* initialize data memory, and we load the scrolled list. */
      RejDataHead = (RejectedData *)(NULL); 
      
      /* load in ALL RejectedData records sorted by lid */
      load_rejectdata_list(orderby_clause[0]);
      
      /* set location filter OFF, sort string to Location and select 1st item in list */
      XmToggleButtonSetState(trash_filter_locTB,False,False);

      /* Added call to desensitize the PE scrolled list. Bryon L 
         - 1/31/2006. */ 
      DeSensitize(trash_peLS);
      XmTextSetString(trash_locationTE,location);
      XmListSetPos(trash_dataLS,1);

      XtUnmanageChild(trash_headerLA);
      SetLabel(trash_headerLA, HDRTRASH);
      XtManageChild(trash_headerLA);
   }
      
   return;
}

/*******************************************************************************

  Add the view rejectdata callbacks.

********************************************************************************/

void add_rejectdata_cbs()
{
    Atom wmAtom;
    
    /* callback for the scrolled list of all fields in RejectedData table. */

    XtAddCallback(trash_filter_peTB,XmNvalueChangedCallback ,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_peLS,XmNmultipleSelectionCallback   ,trashBinFilterSort_CB,NULL);

    XtAddCallback(trash_filter_locTB,XmNvalueChangedCallback,trashBinFilterSort_CB,NULL);

    XtAddCallback(trash_locationTE,XmNvalueChangedCallback,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_locationTE,XmNactivateCallback,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_locationTE, XmNlosingFocusCallback,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_locationTE,XmNmodifyVerifyCallback,(XtCallbackProc)alphanum_filter,(XtPointer)UPPERCASE);
    
    XtAddCallback(trash_sort_locPB   ,XmNactivateCallback,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_sort_recentPB,XmNactivateCallback,trashBinFilterSort_CB,NULL);

    XtAddCallback(trash_rejtype_allPB ,XmNactivateCallback,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_rejtype_autoPB,XmNactivateCallback,trashBinFilterSort_CB,NULL);
    XtAddCallback(trash_rejtype_manPB ,XmNactivateCallback,trashBinFilterSort_CB,NULL);

    wmAtom = XmInternAtom(XtDisplay(trashDS), "WM_DELETE_WINDOW",False);
    XmAddWMProtocolCallback(trashDS, wmAtom,          closeTrashBin_CB,NULL);
    XtAddCallback(trash_closePB ,XmNactivateCallback, closeTrashBin_CB,NULL);

    XtAddCallback(trash_emptyPB ,XmNactivateCallback, emptyTrashBin_CB,NULL);
    XtAddCallback(trash_deletePB,XmNactivateCallback,deleteTrash_CB,NULL);
    XtAddCallback(trash_repostPB,XmNactivateCallback,repostTrash_CB,NULL);

    return;
}

/********************************************************************************

  Load the scrolled lists for RejectedData table info.

*********************************************************************************/

void loadpeLI()
{
   char               buf[100];
   XmStringTable      xmStr;
   int                cnt=0, i=0;
   UniqueList         *uniquePtr;
   ShefPe            *shefPtr; 
   int                ac, empty=0;
   Arg                arg[10];
   char               pe_name[PE_NAME_LEN];
   
   if (UHead != NULL)
    {
      FreeUnique(UHead);
      UHead = (UniqueList *) NULL;
    }
   /**
    query the table, "shefpe", to get the kinds of pes and names of pes.
    We also get the unique pes from the ingestFilter table.
    Then, the program compares the full names of pes from the shefpe table with
    the pes from the ingestfilter table. 
    If they match, we put those data to the scroll list to display on the screen.
   **/

   UHead = (UniqueList *) LoadUnique("pe","ingestFilter","",&cnt);
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   uniquePtr = (UniqueList *) ListFirst(&UHead->list);
   SHead = (ShefPe *) GetShefPe(" ORDER BY pe " );

   for (i=0; i< cnt; i++)
    { 
      /* we need this function to take out unnecessary empty characters from pes.*/
       sched_strip(uniquePtr->uchar);

       /* many space found right after the element, so we need to take those space out */

      shefPtr = (ShefPe *) ListFirst(&SHead->list);

       while( shefPtr !=(ShefPe *) NULL)
	 {
	    if (strcmp(shefPtr->pe,uniquePtr->uchar) == 0)
	      {
		 empty = 1;
		 strcpy(pe_name,shefPtr->name); 
               }
  	    shefPtr = (ShefPe *) ListNext(&shefPtr->node);
          }

       memset(&buf,'\0',sizeof(buf));

       /* if the both pe and name are found, we display the both. 
	  otherwise, we only display pes. 
        */

       if (empty == 1)
           sprintf(buf,"%s  %s",uniquePtr->uchar,pe_name);
       else
	   sprintf(buf,"%s",uniquePtr->uchar);
       xmStr[i] = XmStringCreateSimple(buf);
       uniquePtr = (UniqueList *) ListNext(&uniquePtr->node);
       empty = 0;
     }
   ac=0;
   XtSetArg(arg[ac], XmNitemCount,cnt);ac++;
   XtSetArg(arg[ac], XmNitems, xmStr); ac++;
   XtSetValues(trash_peLS, arg, ac);

   for ( i=0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);

  return;
}

/*************************************************************************************

    Take out empty characters.

*************************************************************************************/

void sched_strip(char *str_ptr)
{
   int i;
   i = strlen(str_ptr) - 1;
   while ((i >= 0) && (*(str_ptr + i) == ' '))
       i--;
   *(str_ptr + i + 1) = '\0';
 return;
}

/****************************************************************************************
 
 Loading the main scroll list for the whole fields of RejectedData table.

 ***************************************************************************************/

void load_rejectdata_list(char *sql)
{
  XmStringTable            xmStr = NULL ;
  Arg                      arg[10];
  int                      ac,i;
  int                      cnt=0;
  char                     liststr[200];
  RejectedData             *RejectPtr;
  char                     where[MAX_PHRASE];
  char                     validtime_ansi[ANSI_TIME_LEN];
  char                     basistime_ansi[ANSI_TIME_LEN];
  char                     posttime_ansi[ANSI_TIME_LEN];
  char                     producttime_ansi[ANSI_TIME_LEN];
  int                      status=0;
  char                     *pc;
  char                     new_rej_type[5], new_intqual[MAXLEN_QCSYMBOL], rev_flag;
  int                      returnvalue;
  double                    dlat,dlon;
  char                     name[ALLOWED_NAME_LEN +1];

  /* we check the memory and free them if necessary. */
  if (RejDataHead != NULL)
    {
	FreeRejectedData(RejDataHead);
	RejDataHead = (RejectedData *) NULL;
    }

   /* now, start loading the list from the database */
   sprintf(where,sql);

   /* Calling the db function to get the data from the database */
   RejDataHead = GetRejectedData(where);  
   
   if (RejDataHead != NULL)
   {
      cnt = ListCount(&RejDataHead->list);
      xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
      RejectPtr = (RejectedData *) ListFirst(&RejDataHead->list);

      for (i = 0; i < cnt; i++)
      {
   	    status = yearsec_dt_to_ansi(RejectPtr->validtime, validtime_ansi);
	    if (status < 0)
	        strcpy(validtime_ansi,"    N/A    ");
            else
	    {
	        /* separate minute and second */
	        pc =  get_time_format(validtime_ansi);
	        strcpy(validtime_ansi, pc);
            }	     
             
	    status = yearsec_dt_to_ansi(RejectPtr->basistime, basistime_ansi);
	    if ((status < 0) || (RejectPtr->ts[0] == 'R') || 
                (RejectPtr->ts[0] == 'P'))
	       strcpy(basistime_ansi,"    N/A    ");
            else
	     {
	        /* separate minute and second */
	        pc =  get_time_format(basistime_ansi);
	        strcpy(basistime_ansi, pc);
              }	     
             
	    status = yearsec_dt_to_ansi(RejectPtr->postingtime, posttime_ansi);
	    if (status < 0)
	       strcpy(posttime_ansi,"    N/A    ");
            else
	     {
	       /* separate minute and second */
	       pc = get_time_format(posttime_ansi);
	       strcpy(posttime_ansi,pc);
             }

	    status = yearsec_dt_to_ansi(RejectPtr->producttime, 
                                        producttime_ansi);
	    if (status < 0)
	       strcpy(producttime_ansi,"    N/A    ");
            else
	     {
	       /* separate minute and second */
	       pc = get_time_format(producttime_ansi);
	       strcpy(producttime_ansi, pc);
             }

	     build_qc_symbol(RejectPtr->quality_code, new_intqual);

	     if (strcmp(RejectPtr->reject_type, "A") == 0)
	        strcpy(new_rej_type,"Auto"); 
             else
	        strcpy(new_rej_type,"Man ");
       
             if (RejectPtr->revision == 1)
	        rev_flag = 'T';
             else
	        rev_flag = 'F';      
            
	    /*get name for lid*/
	    
	    if (RejectPtr->lid != NULL)
	       returnvalue = get_loc_info(RejectPtr->lid,&dlat,&dlon,name);
	       
            sprintf(liststr,"%-8s %-20s %2s %4d %2s %1s %9.2f %11s %11s "
		            " %c  %1s  %s " 
                            "%-8s %4s %11s "
                            "%-10s %11s",
	    RejectPtr->lid, name, RejectPtr->pe, RejectPtr->dur, RejectPtr->ts,
	    RejectPtr->extremum, RejectPtr->value, validtime_ansi, 
                       basistime_ansi,
	    rev_flag, RejectPtr->shef_qual_code, new_intqual,
	    RejectPtr->userid, new_rej_type, posttime_ansi,
	    RejectPtr->product_id, producttime_ansi);

            xmStr[i] = XmStringCreateSimple(liststr);
         
            RejectPtr = (RejectedData *) ListNext(&RejectPtr->node);
      }

   }
   else
   {
      cnt = 0 ;
   }

   /* now, we load the list in the scrolled list */
   ac=0;
   XtSetArg(arg[ac], XmNitemCount,cnt);ac++;
   XtSetArg(arg[ac], XmNitems, xmStr); ac++;
   XtSetValues(trash_dataLS, arg, ac);

   /* need to free the memory */
   for (i=0; i < cnt; i++)
   {
      XmStringFree(xmStr[i]);
   }

   if ( xmStr != NULL ) XtFree ( ( char * ) xmStr ) ;
}

/******************************************************************************************

 To get the right format of validtime and posttime format. 

*******************************************************************************************/

char *get_time_format(char *strings)
{
   char *pc, *min;
   static char new_format[10];
   char month[3], date_hour[6];

   pc = strtok(strings,":");
   strcpy(strings,pc);
   min = strtok(NULL,":");
   strtok(strings,"-");
   strcpy(month,strtok(NULL,"-"));
   strcpy(date_hour,strtok(NULL,"-"));
   sprintf(new_format,"%s/%s:%s",month,date_hour,min);
 return new_format;
}

/*********************************************************************************************

  Making where clause for PE filter.

**********************************************************************************************/

char *createFilterPE()
{
  static   int *poslist = NULL;
  static char    pe_phrase[MAX_PHRASE];
  char          pos_pe[5];
  int           count;
  int           ctr;
  UniqueList        *listPtr=NULL;


  /* find out which PEs where selected from the scrolled list */
  XmListGetSelectedPos(trash_peLS,&poslist,&count);

  /* initialize the PE where clause filter */
  memset(&pe_phrase,'\0',sizeof(pe_phrase));
  strcpy(pe_phrase," pe IN (");

  if (count > 0)
  {
	for (ctr = 0; ctr<count; ctr++)
	{
	    listPtr = (UniqueList *) ListNth(&UHead->list,poslist[ctr]);
	    if (listPtr !=(UniqueList *) NULL)
	    { 
	        memset(&pos_pe,'\0',sizeof(pos_pe));
		if (ctr == count-1)
	           sprintf(pos_pe,"'%s'",listPtr->uchar);
                else
		   sprintf(pos_pe,"'%s',",listPtr->uchar);
             }
          strcat(pe_phrase,pos_pe);
         }
  }

  /* finish making PE where clause filter by adding final ")" */
  strcat(pe_phrase,")"); 

  return pe_phrase;

}

/**************************************************************************************
  
  Close button callback function.

***************************************************************************************/
void closeTrashBin_CB()
{
   if (XtIsManaged(trashDS))
     {
	XtDestroyWidget(trashDS);
	trashDS = NULL;
      }
   /* now, free any allocated memory */

   free_rejectdata();
}
/*********************************************************************************

  Free any memory allocated for the data.

**********************************************************************************/

void free_rejectdata()
{
    if (RejDataHead != NULL)
      {
	 FreeRejectedData(RejDataHead);
	 RejDataHead =(RejectedData *) NULL;
       }
    if (UHead != NULL)
      { 
	 FreeUnique(UHead);
	 UHead = (UniqueList *) NULL;
       }
     if (SHead != NULL)
       {
	  FreeShefPe(SHead);
	  SHead = (ShefPe *) NULL;
       }
     return;
}

/***********************************************************************************/

void trashBinFilterSort_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
    int		menu_pos;
    int         num_selected_items;
    char	filtersort_where[MAX_PHRASE], *buf;

    
    memset(&filtersort_where, '\0',sizeof(filtersort_where));

    /* calculate which reject type option is selected and use that filter */
    menu_pos = GetMenuPos(trash_rejtypeMPM);
    if (menu_pos == 1)
	strcpy(filtersort_where, " WHERE reject_type='A'" );
    else if (menu_pos == 2)
	strcpy(filtersort_where, " WHERE reject_type='M'");
    else
	strcpy(filtersort_where, " WHERE reject_type IS NOT NULL");
    
    /* if location filter button is pushed then add lid to where clause */
    if (XmToggleButtonGetState(trash_filter_locTB))
    {
       Sensitize(trash_locationTE);
	if ( ( buf = XmTextGetString(trash_locationTE) ) )
     	{
	   strcat(filtersort_where, " AND lid='");
	   strcat(filtersort_where, buf);
	   strcat(filtersort_where, "'");
	   XtFree(buf);
	}
    }
    else
       DeSensitize(trash_locationTE);

    /* if PE filter button is pushed then add PEs to where clause */
    if (XmToggleButtonGetState(trash_filter_peTB))
    {

	Sensitize(trash_peLS);

        /* Added a test to make sure that at least one PE is 
           selected from the PE list. This prevents the following
           construct: "where PE in ( )" - This is causing a psql 
           error. */
        XtVaGetValues ( trash_peLS, XmNselectedItemCount, 
                        & num_selected_items, NULL );

        if ( num_selected_items > 0 )
        {
	   strcat(filtersort_where, " AND ");
	   strcat(filtersort_where, createFilterPE());
        }
    }
    else
    {
       DeSensitize(trash_peLS);
    }

    /* calculate which sort option is selected and add that ORDER BY clause */
    menu_pos = GetMenuPos(trash_sortMPM);
    if (menu_pos == 0)
	strcat(filtersort_where, orderby_clause[0]);
    else
	strcat(filtersort_where, orderby_clause[1]);

    /* call function to load data in scrolled list using the WHERE clause created */
    load_rejectdata_list(filtersort_where);

}

/******************************************************************************/

void deleteTrash_CB(Widget w, XtPointer client_data, XtPointer call_data)
{

     Widget		  delete_questDS;
     Widget		  ok_button;
     Widget	  	  cancel_button;				
     char		  question[BUFSIZ];
     int		  count = 0;
     int		  *posList;
     
    
     XmListGetSelectedPos(trash_dataLS, &posList, &count);
     if (count == 1)
     {
	   sprintf(question,"Do you wish to delete this record?"); 
     }
     else if (count > 1)
     {
	   sprintf(question,"Do you wish to delete these %d records?", count);   	       
     }
     else
     	   return;
     
     if (posList)
	  free(posList);
     
     delete_questDS = QuestionDialog(trashDS, question);			
     SetTitle(delete_questDS,"Delete Confirmation");
     
     ok_button = XmMessageBoxGetChild(delete_questDS, XmDIALOG_OK_BUTTON);	
     XtAddCallback(ok_button, XmNactivateCallback, deleteTrash, delete_questDS);
     
     cancel_button = XmMessageBoxGetChild(delete_questDS, XmDIALOG_CANCEL_BUTTON);	
     XtAddCallback(cancel_button, XmNactivateCallback, rejectdata_question_cancel,
		   delete_questDS);
     
     return;

}
/*************************************************************************/

void	deleteTrash(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget	  questionDialog = (Widget) ptr;
     char	  where[BUFSIZ];
     char	  *message;
     
     int	  *poslist=NULL;
     int	  count;
     int	  pos;
     
     long	  result;
     
     RejectedData	*rejdataPtr;

     int		i;
     
     /*
     Find the selected position(s) in the list widget
     */
     if (poslist)
	  free(poslist);
     XmListGetSelectedPos(trash_dataLS, &poslist, &count);

     /*
     	  Delete all rows that have been selected
     */
     for (i = 0; i < count; i++)
     {
	  pos = poslist[i];
	  	  
	  if ( ( rejdataPtr = 
                 (RejectedData *) ListNth(&RejDataHead->list, pos) ) )
	  {
		/* create where clause and call delete function */
	       createDeleteWhere(where, rejdataPtr);
	       result = DeleteRejectedData(where);

	       if (result < 0)  /* error */
	       {
		    message = DbErrorString(result);
		   
		    ErrorDialog(trashDS, message);
		    if (message)
			 free(message);	    
		    
		    break;
	       } 
	  } /* rejdataPtr */
     } /* for count */
     
     /* call this function to read what to filter and sort by */
     trashBinFilterSort_CB(w, NULL, NULL);
     
     if (questionDialog)
	  XtDestroyWidget(questionDialog);
     
     return;
}


/*************************************************************************/

void repostTrash_CB(Widget w, XtPointer client_data, XtPointer call_data)
{

     char	where[BUFSIZ];
     char	*message;
     char	tablename[TABLE_NAME_LEN];
     
     int	*poslist=NULL;
     int	count;
     int	pos;
     int	ctr;
     int	success = 0 ;
     long	result=0;
     
     RejectedData	*rejdataPtr;
     Observation	obsRow;       
     Forecast		fcstRow;       
     

     /*
     Find the selected position(s) in the list widget
     */
     if (poslist)
	  free(poslist);
     XmListGetSelectedPos(trash_dataLS, &poslist, &count);

     /*
     Repost data to PE table and Delete all rows that have been selected
     */
     for (ctr = 0; ctr < count; ctr++)
     {
	  pos = poslist[ctr];
	  	  
	  if ( ( rejdataPtr = 
                 ( RejectedData * ) ListNth ( &RejDataHead->list , pos ) ) )
	  {
		/*  Find the table that corresponds to the pe code.  */
		getTableName(rejdataPtr->pe, rejdataPtr->ts, tablename);

		if (rejdataPtr->ts[0] == 'R' || rejdataPtr->ts[0] == 'P')
		{
		   /* create an Observation record from a RejectedData */
		   setRejectedData2Observation(rejdataPtr, &obsRow);

		   /* create where clause to possibly update observed data */
		   createUpdDelWhereObs(where, &obsRow);

		   if ((recordCount(tablename, where) == 0))
		   {
			result = PutObservation(&obsRow, tablename);
			if ( result != 0)  /* error */
			{
				message = DbErrorString(result);
				ErrorDialog(trashDS, message); 
				if (message)
				{
				    free(message);
				    message = NULL;
				}
			}   
			else /* successful add of a new observation record */
			{
				success = 1;
			}
		   }
		   else /* since one already exists, do an update */
		   {
			result = UpdateObservation(&obsRow, where, tablename);
			if ( result != 0)  /* error */
			{
				message = DbErrorString(result);
				ErrorDialog(trashDS, message); 
				if (message)
				{
				    free(message);
				    message = NULL;
				}
			}   
			else /* successful update of an observation record */
			{
				success = 1;
			}		   
		   }
		}
		else /* ts does NOT start with a 'R' or 'P' then assume forecast */
		{
		   /* create a Forecast record from a RejectedData */
		   setRejectedData2Forecast(rejdataPtr, &fcstRow);

		   /* create where clause to possibly update observed data */
		   createUpdDelWhereFcst(where, &fcstRow);
		   
		   if ((recordCount(tablename, where) == 0))
		   {
			result = PutForecast(&fcstRow, tablename);
			if ( result != 0)  /* error */
			{
				message = DbErrorString(result);
				ErrorDialog(trashDS, message); 
				if (message)
				{
				    free(message);
				    message = NULL;
				}
			}   
			else /* successful add of a new Forecast record */
			{
				success = 1;
			}
		   }
		   else /* since one already exists, do an update */
		   {
			result = UpdateForecast(&fcstRow, where, tablename);
			if ( result != 0)  /* error */
			{
				message = DbErrorString(result);
				ErrorDialog(trashDS, message); 
				if (message)
				{
				    free(message);
				    message = NULL;
				}
			}   
			else /* successful update of a Forecast record */
			{
				success = 1;
			}		   
		   }
		}
		
		/* if successful insert or update then delete RejectedData record */
		if (success)
		{
			/* create where clause and call delete function */
			createDeleteWhere(where, rejdataPtr);
			result = DeleteRejectedData(where);
			if (result < 0)  /* error */
			{
			    message = DbErrorString(result);
		   
			    ErrorDialog(trashDS, message);
			    if (message)
				 free(message);	    
		    
			    break;
			}
		}
		
	  } /* rejdataPtr */
     } /* for count */
     
     /* call this function to read what to filter and sort by */
     trashBinFilterSort_CB(w, NULL, NULL);
     
     return;

}

/******************************************************************************/

void emptyTrashBin_CB(Widget w, XtPointer client_data, XtPointer call_data)
{

     Widget		  empty_questDS;
     Widget		  ok_button;
     Widget	  	  cancel_button;				
     char		  question[BUFSIZ];
     
    
     if (recordCount("RejectedData", " ") > 0)
	sprintf(question,"Do you wish to delete ALL records in the Trash Bin?"); 
     else
	return; 

     empty_questDS = QuestionDialog(trashDS, question);			
     SetTitle(empty_questDS,"Empty Confirmation");
     
     ok_button = XmMessageBoxGetChild(empty_questDS, XmDIALOG_OK_BUTTON);	
     XtAddCallback(ok_button, XmNactivateCallback, emptyTrashBin, empty_questDS);
     
     cancel_button = XmMessageBoxGetChild(empty_questDS, XmDIALOG_CANCEL_BUTTON);	
     XtAddCallback(cancel_button, XmNactivateCallback, rejectdata_question_cancel,
		   empty_questDS);
     
     return;
}
/*************************************************************************/

void	emptyTrashBin(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget	  questionDialog = (Widget) ptr;
     char	  *message;
     long	  result;

          
     /*     Delete all rows     */
     result = DeleteRejectedData(" ");
     if (result < 0)  /* error */
     {
	   message = DbErrorString(result);
		   
	   ErrorDialog(trashDS, message);
	   if (message)
		 free(message);	    
     }
     
     trashBinFilterSort_CB(w, NULL, NULL);
     
     if (questionDialog)
	  XtDestroyWidget(questionDialog);
     
     return;
}

/*************************************************************************/

void	rejectdata_question_cancel(Widget w, XtPointer ptr, XtPointer cbs)
{
     Widget	questionDialog = (Widget) ptr;


     if (questionDialog)
	  XtDestroyWidget(questionDialog);
     
     return;
}

/*************************************************************************/

void	createDeleteWhere(char *delete_where, RejectedData *rejdataPtr)
{
     
     char validtimeString[ANSI_TIME_LEN];
     char basistimeString[ANSI_TIME_LEN];
     char postingtimeString[ANSI_TIME_LEN];
     
     yearsec_dt_to_ansi(rejdataPtr->validtime, validtimeString);
     yearsec_dt_to_ansi(rejdataPtr->basistime, basistimeString);
     yearsec_dt_to_ansi(rejdataPtr->postingtime, postingtimeString);
     
     /*     Used for deleting RejectedData.     */	
     sprintf(delete_where, " WHERE lid='%s' AND pe='%s' AND dur=%d AND ts='%s'"
		" AND extremum='%s' AND probability=%f"
		" AND validtime='%s' AND basistime='%s' AND postingtime='%s' ",
		rejdataPtr->lid, rejdataPtr->pe, rejdataPtr->dur,rejdataPtr->ts,
		rejdataPtr->extremum, rejdataPtr->probability,
		validtimeString, basistimeString, postingtimeString);
     
     return;  
}   

/*************************************************************************/
void	setRejectedData2Observation(RejectedData *trash, Observation *obs)
{
     time_t     currentTime = 0;
     dtime_t	postingTime;

     strcpy(obs->lid, trash->lid);

     strcpy(obs->pe, trash->pe);

     obs->dur = trash->dur;

     strcpy(obs->ts, trash->ts);

     strcpy(obs->extremum, trash->extremum);

     /* set obstime from validtime for observed data */
     obs->obstime = trash->validtime;

     obs->value = trash->value;

     obs->revision = trash->revision;

     strcpy(obs->shef_qual_code, trash->shef_qual_code);

     strcpy(obs->product_id, trash->product_id);

     obs->producttime = trash->producttime;

     /* set postingtime to current time */
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
     obs->postingtime = postingTime;

     obs->quality_code = trash->quality_code;


     return;

}

/*************************************************************************/
void	setRejectedData2Forecast(RejectedData *trash, Forecast *fcst)
{
     time_t     currentTime = 0;
     dtime_t	postingTime;

     strcpy(fcst->lid, trash->lid);

     strcpy(fcst->pe, trash->pe);

     fcst->dur = trash->dur;

     strcpy(fcst->ts, trash->ts);

     strcpy(fcst->extremum, trash->extremum);

     fcst->probability = trash->probability;

     fcst->validtime = trash->validtime;

     fcst->basistime = trash->basistime;

     fcst->value = trash->value;

     fcst->revision = trash->revision;

     strcpy(fcst->shef_qual_code, trash->shef_qual_code);

     strcpy(fcst->product_id, trash->product_id);

     fcst->producttime = trash->producttime;

     /* set postingtime to current time */
     time(&currentTime);
     timet_to_yearsec_dt(currentTime, &postingTime);
     fcst->postingtime = postingTime;

     fcst->quality_code = trash->quality_code;


     return;

}

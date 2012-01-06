/*******************************************************************
     File:		fstmt_show.c  (Hydrobase)
     Date:		March 1996
     Author:		Dale Shelton

     Purpose:		Provide support for the Impact Statement DS.	
********************************************************************/

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/DialogS.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>
#include <Xm/SelectioB.h>
#include <X11/Composite.h>
#include <X11/cursorfont.h>
#include "Filter.h"
#include "Xtools.h"

#include "fstmt_cbs.h"
#include "fstmt.h"
#include "Floodstmt.h"
#include "Location.h"
#include "DbmsDefs.h"
#include "ParamDefs.h"
#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "set_window_title.h"

/*******************************
 Declaration of global variables 
 *******************************/

static int	fstmt_insert_mode = 0;
static Widget   fstmt_dialog;
char		fstmt_lid[LOC_ID_LEN + 1];

/**************************
 The ShowFstmtDs() function 
 **************************/

void	ShowFstmtDs(Widget w, char *lid, Boolean editable)

{  /* Opening brace of ShowFstmtDs() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/
 
	if (!fstmtDS)
	{
            create_fstmtDS(GetTopShell(w));
	    fstmt_callbacks();
	}
	
	if (!XtIsManaged(fstmtDS))
	{
	    strcpy(fstmt_lid, lid);
	    fstmt_load_stglist(fstmt_lid);	
	    fstmt_load_nth(fstmt_lid, 1);
	    Sensitize(stgLB);
	    if ( ! editable )
	    {
                XtUnmanageChild(fsokPB);
                XtUnmanageChild(fsapplyPB);
                XtUnmanageChild(fsnewPB);
                XtUnmanageChild(fsdeletePB);
                XtUnmanageChild(fsprintPB);
                XtUnmanageChild(fssavePB);
	    }
            set_title(fstmtDS, "Impact Statement", lid);
	    XtManageChild(fstmtFM);
	    XtManageChild(fstmtDS);
	}
	

	return;

}  /* Closing brace of ShwFstmtDS() function */ 

/******************************
 The fstmt_callbacks() function 
 ******************************/

void	fstmt_callbacks(void)

{  /* Opening brace of fstmt_callbacks() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	Atom	wmAtom;
	
       /************************
	Window manager callbacks 
        ************************/

	wmAtom = XmInternAtom(XtDisplay(fstmtDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(fstmtDS, wmAtom, close_fstmt, NULL);

       /**************** 
	Widget callbacks 
        ****************/

	XtAddCallback(fsclsPB, 	  XmNactivateCallback,        close_fstmt,    NULL);
	XtAddCallback(fsapplyPB,  XmNactivateCallback,        apply_fstmt,    NULL);
	XtAddCallback(fsdeletePB, XmNactivateCallback,        fstmt_del_conf, NULL);
	XtAddCallback(fsnewPB,    XmNactivateCallback,        new_fstmt,      NULL);
	XtAddCallback(fsokPB,     XmNactivateCallback,        ok_fstmt,       NULL);	
	XtAddCallback(fsprintPB,  XmNactivateCallback,        print_fstmt,    NULL);	
	XtAddCallback(fssavePB,   XmNactivateCallback,        save_fstmt,     NULL);	
	XtAddCallback(stgLB,      XmNbrowseSelectionCallback, select_fstmt,   NULL);

	XtAddCallback(fdbeginTxt, XmNmodifyVerifyCallback, (XtCallbackProc) seasonal_filter,    fsbegOM);
	XtAddCallback(fdbeginTxt, XmNlosingFocusCallback,  (XtCallbackProc) seasonal_filter,    fsbegOM);
	XtAddCallback(fsbjanPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbfebPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbmarPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbaprPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbmayPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbjunPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbjulPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbaugPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbsepPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsboctPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbnovPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);
	XtAddCallback(fsbdecPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsbegOM);

	XtAddCallback(fdendTxt,   XmNmodifyVerifyCallback, (XtCallbackProc) seasonal_filter,    fsendOM);
	XtAddCallback(fdendTxt,   XmNlosingFocusCallback,  (XtCallbackProc) seasonal_filter,    fsendOM);
	XtAddCallback(fsejanPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsefebPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsemarPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fseaprPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsemayPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsejunPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsejulPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fseaugPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsesepPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fseoctPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsenovPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	XtAddCallback(fsedecPB,   XmNactivateCallback,     (XtCallbackProc) seasonal_pb_helper, fsendOM);
	
	XtAddCallback(fsstageTxt,XmNmodifyVerifyCallback, (XtCallbackProc) num_filter, (XtPointer) INTEGERS_AND_DECIMALS_AND_HYPHENS);
	XtAddCallback(imppeTxt,  XmNmodifyVerifyCallback, (XtCallbackProc) alpha_filter, (XtPointer) UPPERCASE);
	
	return;

}  /* Closing brace of fstmt_callbacks() function */ 

/**************************
 The close_fstmt() function 
 **************************/

void	close_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of close_fstmt() function */ 

   Sensitize(stgLB);
   Sensitize(fsdeletePB);	
   
   if (XtIsManaged(fstmtDS))
   {
      XtDestroyWidget(fstmtDS);
      fstmtDS = NULL;
   }
   
   return;

}  /* Closing brace of close_fstmst() function */ 

/***************************
 The select_fstmt() function 
 ***************************/

void	select_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of select_fstmt() function */ 

       /***********************************************
	Gets the selected item position from stgLB (w),
	and then loads the data corresponding to that
	position into the edit section of the dialog.
        ***********************************************/

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	int 	       *pos_list;
        int 	    	pos_cnt;
	
	fstmt_insert_mode = 0;
	Sensitize(fsdeletePB);

       /********************************************
	Get the position selected in the list
	and load that record into the lower section.
        ********************************************/

	XmListGetSelectedPos(w, &pos_list, &pos_cnt); 

	fstmt_load_nth(fstmt_lid, pos_list[0]);

	XtFree((char *) pos_list);

	return;

}  /* Closing brace of select_fstmt() function */ 

/**************************
 The apply_fstmt() function
 **************************/

void 	apply_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of apply_fstmt() function */ 

       /**********************************************************
	Saves data to the database.  If fstmt_insert_mode is true,
	the record is inserted; otherwise, the record is updated.
        **********************************************************/

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	Floodstmt	fs;
	char           key[MAX_WHERE_LEN] = "";
        char	       *buf = NULL ;
	int		pos;
	int             status = 0;
	bool            isUnique;
	
	buf = XmTextGetString(fsstageTxt);

	if (buf[0] == '\0')
	{
	    ErrorDialog(fstmtDS, "A valid flood stage must be entered.");  
	    return;
	}  

	XtFree(buf);
	
       /***************************************
	Make sure these widgets are sensitized.
        ***************************************/
	
	Sensitize(fsdeletePB);
	Sensitize(stgLB);
	
       /*****************************************
	Put data from widgets into the structure.
        *****************************************/

	memset(&fs, '\0', sizeof(fs));
	

	unload_fstmt(&fs);	
	
	if (fstmt_insert_mode)
	{
/*            PutFloodstmt(&fs); 	*/
           
	   status = InsertIfUniqueFloodstmt(&fs, &isUnique);
	   
	   if ( !isUnique )
	   {
	      printf( "\nIgnoring duplicate Record... ");
	   }
	   else if (status != 0)
	   {
	       printf("\nPostgreSQL error inserting to Floodstmt in apply_fstmt.");	       
	       return;
	   }	   	   
        }
	else
	{		  
	    fstmt_key(key,&pos);            	    
	    if (strlen(key))
	    {
	       UpdateFloodstmt(&fs,key);
	    }
	       
	}
	
	fstmt_insert_mode = 0;
	fstmt_load_stglist(fstmt_lid);
	
	return;

}  /* Closing brace of apply_fstmt() function */

/*****************************
 The defaults_fstmt() function
 *****************************/

void defaults_fstmt(void)

{  /* Opening brace of defaults_fstmt() function */ 

       /******************************************************
	Set defaults to Rising, start of 1/1, and end of 12/31
        ******************************************************/

	XtVaSetValues(fdriseTB, XmNset, TRUE, NULL);
	SetMenuPos(fsbegOM, 0);
	SetMenuPos(fsendOM, 11);
	XmTextSetString(fdbeginTxt, "01");
	XmTextSetString(fdendTxt, "31");

}  /*  Closing brace of defaults_fstmt() function */ 

/*****************************
 The fstmt_del_conf() function
 *****************************/

void 	fstmt_del_conf(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of fstmt_del_conf() function */ 

  /******************************************
   Declaration of automatic (local) variables 
   ******************************************/

   Widget	qstDS;
   Widget       okPB;
   char		buf[MAX_BUF_LEN];
   
   sprintf(buf, "Do you wish to delete this entry?");

   qstDS = QuestionDialog(fstmtDS, buf);

   SetTitle(qstDS, "Delete Confirmation");
   
  /***************************************************************
   Get the XmMessageBox ok button, and associate a callback to it.
   ***************************************************************/

   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);	

   XtAddCallback(okPB, XmNactivateCallback, fstmt_delete, NULL);		
   
   if(!XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;

}  /* Closing brace of defaults_fstmt() function */

/***************************
 The fstmt_delete() function
 ***************************/

void 	fstmt_delete(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of fstmt_delete() function */ 

	int 	       *pos_list;
        int 		pos_cnt;
        int 		pos;
	char            key[MAX_WHERE_LEN]="";

       /***************************************************
	Store the position of the currently selected record
        ***************************************************/

	XmListGetSelectedPos(stgLB, &pos_list, &pos_cnt); 
	
	SetCursor(fstmtFM, XC_watch);
	
	fstmt_key(key,&pos);

	if (strlen(key))
	{
            DeleteFloodstmt(key);	  
	}

	UnsetCursor(fstmtFM);
	
       /**********************************
	Load new current list into stglist
        **********************************/

	fstmt_load_stglist(fstmt_lid);
	
       /**********************************************************
	Select the record after the previous one.  It has the same
	number as the one that was deleted. It will then invoke
	the select_fstmt callback.
        **********************************************************/

	clearForm(fstmtFM);

	defaults_fstmt();

	XmListSelectPos(stgLB,pos_list[0],True);

	XtFree((char *) pos_list);
	
	return;

}  /* Closing brace of fstmt_delete() function */ 

/************************
 The new_fstmt() function 
 ************************/

void 	new_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of new_fstmt() */ 

       /******************************************
        Declaration of automatic (local) variables
        ******************************************/

	fstmt_insert_mode = 1;

       /************************
	Deselect all list items.
        ************************/

	XmListDeselectAllItems(stgLB);
	
       /***********************************************************
	When adding a record, another can't be selected or deleted.
        ***********************************************************/

	DeSensitize(fsdeletePB);
	DeSensitize(stgLB);
	
       /****************************************
	Clear the form and set the begin and end
	date fields to accept the entire year as
	the acceptable range.
        ****************************************/

	clearForm(fstmtFM);

	defaults_fstmt();

	Sensitize(fsokPB);

	Sensitize(fsapplyPB);

	return;	

}  /* Closing brace of new_fstmt() function */ 

/***************************
 The unload_fstmt() function 
 ***************************/

void    unload_fstmt(Floodstmt *fs)

{  /* Opening brace of unload_fstmt() function */ 

       /*************************************************************
        Takes the data out of the Text widgets and Toggle Buttons and
        puts it into the Floodstmt structure.
        *************************************************************/

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

	char           *buf;		
	char            begdate_str[SEASON_LEN + 1];
	char            enddate_str[SEASON_LEN + 1];
	long		temp_fldstage;
	int		pos;
	
       /*************************************************
	Set location id (lid)  and flood stage (fldstage)
        *************************************************/
	
	strcpy(fs->lid, (char *)fstmt_lid);

	/*************************************************************************
         Set fldstage and make sure that it takes only 2 points after the decimal.
         *************************************************************************/
        
	buf = XmTextGetString(fsstageTxt);
 
	fs -> impact_value = atof(buf) * 100.0;
	XtFree((char *) buf);
	
	temp_fldstage = (long) (fs -> impact_value);
	fs -> impact_value   = (double) (temp_fldstage) / 100.0;
      
       /*set impact_pe */
       
       if ( ( buf = XmTextGetString(imppeTxt) ) );
       {
            strcpy(fs -> impact_pe, buf);
	    XtFree((char *) buf);
       }
       	    
       /*************************
        Set fldstmt1 and fldstmt2 
        *************************/

	if ( (buf = XmTextGetString(fdstmtTxt)) )
	{
            strcpy(fs -> statement, buf);
            XtFree((char *) buf);
	}
	
        /************
         Set tendency 
         ************/

	if (XmToggleButtonGetState(fdriseTB))
            strcpy(fs -> rf, "R");

	else
            strcpy(fs -> rf, "F");
	
       /*********************************************************
	Extract the date strings from the text widgets and assign
        *********************************************************/

	pos = GetMenuPos(fsbegOM) + 1;
	buf = XmTextGetString(fdbeginTxt);
	sprintf(begdate_str, "%02d/%s", pos, buf);
	strcpy(fs -> datestart, begdate_str);
	XtFree((char *) buf);
	
	pos = GetMenuPos(fsendOM) + 1;
	buf = XmTextGetString(fdendTxt);
	sprintf(enddate_str, "%02d/%s", pos, buf);	
	strcpy(fs -> dateend, enddate_str);
	XtFree((char *) buf);

	return;

}  /* Closing brace of unload_fstmt() function */ 

/***********************
 The ok_fstmt() function 
 ***********************/

void 	ok_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of ok_fstmt() function */ 

	apply_fstmt(w, NULL, NULL);
	close_fstmt(w, NULL, NULL);

	return;

}  /* Closing brace of ok_fstmt() function */ 

/************************
 The fstmt_key() function 
 ************************/

void	fstmt_key(char *key, int *pos)

{  /* Opening brace of fstmt_key() function */ 

  /******************************************
   Declaration of automatic (local) variables 
   ******************************************/

   Floodstmt   *fstmtHead = NULL;
   Floodstmt   *fPtr;
   char		where[MAX_WHERE_LEN];
   char	        datestart_string[BUFSIZ];
   char	        dateend_string[BUFSIZ];
   int	       *poslist = NULL ;
   int   	cnt;
   
   XmListGetSelectedPos(stgLB, &poslist, &cnt);

   if ( poslist == NULL ) return ;

   sprintf(where, " WHERE lid = '%s' ORDER BY impact_value DESC ", fstmt_lid);

   if ((fstmtHead = GetFloodstmt(where)) != NULL)
   {
       fPtr = (Floodstmt *) ListNth(&fstmtHead -> list, poslist[0]);

       if (fPtr)
       {
	   memset(where, '\0', sizeof(where));				

	   if (strlen (fPtr -> datestart) == 0)
       	       sprintf(datestart_string, " AND datestart is NULL ");

	   else
	       sprintf(datestart_string, " AND datestart = '%s' ", fPtr -> datestart);
	 
	   if (strlen (fPtr -> dateend) == 0)
	       sprintf(dateend_string, " AND dateend is NULL ");

	   else
               sprintf(dateend_string, " AND dateend = '%s' ", fPtr -> dateend);
	 
	   sprintf(where, " WHERE lid = '%s' AND impact_value >= %f"
	           " AND impact_value <= %f AND rf = '%s' %s %s",
		   fPtr -> lid, fPtr -> impact_value - 0.001,
		   fPtr -> impact_value + 0.001 ,
		   fPtr -> rf, datestart_string, dateend_string);
	 
	   *pos = poslist[0];	  	   
	   strcpy(key,where);
	   XtFree((char *) poslist);
      }
      
      if (fstmtHead != NULL)
        FreeFloodstmt(fstmtHead);
   }
   
   return;

}  /* Closing brace of fstmt_key() function */ 

/*****************************
 The fstmt_load_nth() function 
 *****************************/

void    fstmt_load_nth(char *lid, int item_num)

{  /* Opening of fstmt_load_nth() function */ 

  /******************************************
   Declaration of automatic (local) variables
   ******************************************/

   Floodstmt   *fsHead = NULL ;
   Floodstmt   *fsPtr = NULL ;
   char		where[MAX_WHERE_LEN];
   char	        buf[9];
   char	       *str = NULL ; 
   char	       *tok = NULL ;
   
   clearForm(fstmtFM);
   defaults_fstmt();
   
   sprintf(where, " WHERE lid = '%s' ORDER BY impact_value DESC ", lid);

   if ((fsHead = GetFloodstmt(where)) != NULL)
   {
       fsPtr = (Floodstmt *) ListNth(&fsHead -> list,item_num);

       if (fsPtr != NULL)
       {
	   DataToString(&fsPtr -> impact_value, DOUBLE, buf, "%8.2f", "");
	   XmTextSetString(fsstageTxt, buf);
	   
	   XmTextSetString(imppeTxt, fsPtr -> impact_pe);
	 
	   str = fsPtr -> datestart;
	   tok = strtok(str, "/");

           if ( tok != NULL )
           {
	      SetMenuPos(fsbegOM, (atoi(tok) - 1));
	      str = NULL;
	      tok = strtok(str, "\n");
	      XmTextSetString(fdbeginTxt, tok);
           }
           else
           {
	      XmTextSetString(fdbeginTxt, "\0");
           }
	 
	   str = fsPtr -> dateend;
	   tok = strtok(str, "/");

           if ( tok != NULL )
           {
	      SetMenuPos(fsendOM, (atoi(tok) - 1));
	      str = NULL;
	      tok = strtok(str, "\n");
	      XmTextSetString(fdendTxt, tok);
           }
           else
           {
	      XmTextSetString(fdendTxt, "\0");
           }
	 
	   if (fsPtr -> rf[0] == 'R')
	       XmToggleButtonSetState(fdriseTB, True, True);

	   else
	       XmToggleButtonSetState(fdfallTB, True, True);
	 
	   XmTextSetString(fdstmtTxt, fsPtr -> statement);

      }
      
      if (fsHead != NULL)
        FreeFloodstmt(fsHead);
  }
   
   return;

}  /* Closing brace of fstmt_load_nth() function */ 

/*********************************
 The fstmt_load_stglist() function 
 *********************************/

void 	fstmt_load_stglist(char *lid)

{  /* Opening brace of fstmt_load_stglist() function */ 

  /******************************************
   Declaration of automatic (local) variables
   ******************************************/

   XmStringTable	xmStr;
   Arg			arg[MAX_ARGS];
   char			where[MAX_WHERE_LEN];
   char                 buf[MAX_BUF_LEN];
   char                 tendency_str[11];
   Floodstmt	       *fsHead;
   Floodstmt   	       *fsPtr;
   
   int			i;
   int                  ac;
   int                  cnt;
   
   sprintf(where, " WHERE lid = '%s' ORDER BY impact_value DESC ", lid);

   if ((fsHead = GetFloodstmt(where)) != NULL)
   {
       cnt = ListCount(&fsHead->list);
      
       if (cnt == 0)
       {
           DeSensitize(fsapplyPB);
	   DeSensitize(fsokPB);
       }

       else
       {
           Sensitize(fsapplyPB);
	   Sensitize(fsokPB);
       }
      
       xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
       fsPtr = (Floodstmt *) ListFirst(&fsHead->list);

       for (i=0; fsPtr; i++)
       {
	 if (strcmp(fsPtr -> rf, "F") == 0)
	    strcpy(tendency_str, "FALLING");
	 else
	    strcpy(tendency_str, "RISING");
	 
	 
	 sprintf(buf,"%8.2f %12s %24s %10s %29s",
		 fsPtr -> impact_value, fsPtr -> impact_pe, 
		 fsPtr -> datestart, fsPtr -> dateend, tendency_str);
	 
	 xmStr[i] = XmStringCreateSimple(buf);
	 fsPtr = (Floodstmt *) ListNext(&fsPtr -> node);
       }
      
       ac = 0;

       XtSetArg(arg[ac], XmNitemCount, cnt);
       ac++; 

       XtSetArg(arg[ac], XmNitems, xmStr); 
       ac++; 

       XtSetValues(stgLB, arg, ac);
       XmListSelectPos(stgLB, 1, True);
      
      /**************************************
       Free memory.  Sensitize delete button.
       **************************************/

       for (i = 0; i < cnt; i++)
            XmStringFree(xmStr[i]);

       XtFree((char *) xmStr);
      
       if (fsHead != NULL)
         FreeFloodstmt(fsHead);
       Sensitize(fsdeletePB);
   }

   else    /* no records */
   {
      XmListDeleteAllItems(stgLB);
      DeSensitize(fsdeletePB);
   }
   
   return;

}  /* Closing brace of fstmt_load_stglist() function */ 

/**************************
 The print_fstmt() function
 **************************/

void 	print_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of print_fstmt() function */ 

  /******************************************
   Declaration of automatic (local) variables
   ******************************************/

   char                 print_command_line_text[200]; 
   char                 remove_file_command_line_text[200]; 
   char                *directoryPath = "/tmp";
   int                  returnValue;
   char                 cmd_str[200];
   char                 lpr_print[128];
   int                  gad_token_len=0, gad_value_len=0;
   
     
  /**********************************************
   Get data from database table and populate file
   **********************************************/ 

   returnValue = populate_output_file(); 

   if (returnValue == SUCCESS)
   {
      /**********************************
       Spool output file to local printer
       **********************************/
       gad_token_len = strlen("whfs_printcommand"); /*token for the print command */
       get_apps_defaults("whfs_printcommand", &gad_token_len, lpr_print, &gad_value_len);
       
       
       if(strlen(lpr_print) > 0)
       {
	 strcpy(cmd_str, lpr_print);
       }
       else
       {
	 strcpy(cmd_str, "lp");
	}
       
       sprintf(print_command_line_text, "%s %s/tempfile.dat ",cmd_str,directoryPath); 

       system(print_command_line_text);
      
	
      /*****************************************
       Remove/Delete output file from filesystem 
       *****************************************/

       sprintf(remove_file_command_line_text, "rm %s/tempfile.dat > /dev/null 2>&1", 
               directoryPath); 

       system(remove_file_command_line_text); 
   }

   else if (returnValue == NO_DATA_AVAILABLE)
            ErrorDialog(w, "Impact Statement data not available for selected station."); 

   else if (returnValue == FILE_PROBLEM)
            ErrorDialog(w, "Error encountered when opening or closing output file."); 

   else ;

   return; 

}  /* Closing brace of print_fstmt() function */ 

/*************************
 The save_fstmt() function
 *************************/

void    save_fstmt(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of save_fstmt() function */

       /******************************************
        Declaration of automatic (local) variables
        ******************************************/

	Arg          args[10];
	int          ac;
        char        directory[128]; 
	XmString     str;
	int	gad_token_len=0, gad_value_len=0;

       /*******************************************
        Create and manage File Selection Dialog Box
        *******************************************/

        fstmt_dialog = XmCreateFileSelectionDialog(w, "fstmt_dialog", NULL, 0);

        XtAddCallback(fstmt_dialog, XmNcancelCallback, FSTMTclosefile_CB, NULL);

	XtAddCallback(fstmt_dialog, XmNokCallback, FSTMTselectfile_CB, NULL);

	XtUnmanageChild(XmSelectionBoxGetChild(fstmt_dialog, XmDIALOG_HELP_BUTTON));

	gad_token_len = strlen("whfs_report_dir");
	get_apps_defaults("whfs_report_dir", &gad_token_len, directory, &gad_value_len);

        strcat(directory, "/"); 

        str = XmStringCreateSimple(directory);

        ac = 0;

        XtSetArg(args[ac], XmNpattern, str); ac++;

        XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;

        XtSetValues(fstmt_dialog, args, ac);

        XtFree((char*) str);

        SetTitle(fstmt_dialog,"File Selection");

        if(! XtIsManaged(fstmt_dialog))
           XtManageChild(fstmt_dialog);

	return;

}  /* Closing brace of save_fstmt() function */ 

/***********************************
 The populate_output_file() function
 ***********************************/

int 	populate_output_file()

{  /* Opening brace of populate_output_file() function */ 

  /******************************************
   Declaration of automatic (local) variables
   ******************************************/

   char			FloodstmtTable_where_clause[MAX_WHERE_LEN];
   char                 LocationTable_where_clause[MAX_WHERE_LEN]; 
   char                *filenamePath;
   char                 filename[129];
   char                 timestr[17]; 
   time_t               tnow;
   size_t               returnValue; 
   struct tm           *tm_struct; 
   Floodstmt	       *fsHead;
   Floodstmt   	       *fsPtr;
   Location            *lcPtr; 
   FILE                *filePtr; 
   int                  i; 
   
  /******************************
   Ready query for data retrieval
   ******************************/

   sprintf(FloodstmtTable_where_clause, " WHERE lid = '%s' ORDER BY impact_value DESC ", fstmt_lid);

  /**************************************************
   Create/Open temporary output file for data storage
   **************************************************/

   filenamePath = "/tmp"; 

   strcpy(filename, filenamePath); 

   strcat(filename, "/tempfile.dat"); 

   filePtr = fopen(filename, "w"); 

   if (filePtr == NULL)
       return(FILE_PROBLEM); 

  /************************************************
   Retrieve data from Floodstmt table, if available
   ************************************************/

   if ((fsHead = GetFloodstmt(FloodstmtTable_where_clause)) != NULL)
   {
       fsPtr = (Floodstmt *) ListFirst(&fsHead -> list);

      /***************************************************************
       Format and write Impact Statement data to temporary output file
       ***************************************************************/

      /****************************
       Impact Statement Header Line
       ****************************/

       fprintf(filePtr, "%s\n", "FLOOD IMPACT STATEMENT LISTING FOR"); 

      /**********************************************************************
       Retrieve the station's name, county, and state from the Location table
       and output to file
       **********************************************************************/

       sprintf(LocationTable_where_clause, " WHERE lid = '%s' ", fstmt_lid); 

       if ((lcPtr = GetLocation(LocationTable_where_clause)) != NULL)
           fprintf(filePtr, "%s %s - %s COUNTY, %s\n", fstmt_lid, lcPtr -> name, 
                   lcPtr -> county, lcPtr -> state); 

       else 
           fprintf(filePtr, 
                   "The name, county, and state for station %s are not available.\n", fstmt_lid);

       fprintf(filePtr, "%s\n", "--------------------------"); 

       time(&tnow); 
    
       tm_struct = gmtime(&tnow); 

       returnValue = strftime(timestr, 17, "%m/%d/%Y %I:%M", tm_struct); 

       fprintf(filePtr, "GENERATED %s\n\n\n", timestr); 

       for (i = 0; fsPtr; i++)
       {
            fprintf(filePtr, "IMPACT PE: %s  ", fsPtr -> impact_pe);
	    
            if (strcmp(fsPtr -> impact_pe, "QR") == 0)
	       fprintf(filePtr, "IMPACT VALUE:  %9.2f CFS. ", fsPtr -> impact_value);
	    else   
               fprintf(filePtr, "IMPACT VALUE:  %9.2f FT. ", fsPtr -> impact_value); 
	    	    
	    if (strcmp(fsPtr -> rf, "F") == 0)
	        fprintf(filePtr, "(%s)     ", "FALLING");

	    else
	        fprintf(filePtr, "(%s)     ", "RISING");

            fprintf(filePtr, "APPLIES TO PERIOD:  %s - %s\n\n", 
                    fsPtr -> datestart, fsPtr -> dateend);  

            if (strcmp(fsPtr -> statement, "") == 0)
                fprintf(filePtr, "%s\n\n", "AN IMPACT STATEMENT FOR THIS STAGE WAS NOT AVAILABLE."); 

            else 
                text_wrapper(fsPtr -> statement, filePtr); 

           /************************
            Retrieve the next record
            ************************/

	    fsPtr = (Floodstmt *) ListNext(&fsPtr -> node);
       }
      
       if (fsHead != NULL)
         FreeFloodstmt(fsHead);

       if (fclose(filePtr) != 0)
           return(FILE_PROBLEM); 

       else
           return(SUCCESS);
   }
  
   else     
      return(NO_DATA_AVAILABLE);   
      
}  /* Closing brace of populate_output_file() function */ 

/******************************
 The FSTMTclosefile_CB function 
 ******************************/

void FSTMTclosefile_CB(Widget w, XtPointer ptr, XtPointer cbs)

{  /* Opening brace of FSTMTclosefile_CB() function */ 

    if (fstmt_dialog)
    {
        XtDestroyWidget(fstmt_dialog);
        fstmt_dialog = NULL; 
    }

    return;

}  /* Closing brace of FSTMTclosefile_CB() function */ 

/*******************************
 The FSTMTselectfile_CB function 
 *******************************/

void	FSTMTselectfile_CB(Widget w, XtPointer ptr, XtPointer cbs) 

{  /* Opening brace of FSTMTselectfile_CB() function */ 

       /******************************************
        Declaration of automatic (local) variables 
        ******************************************/

        XmStringCharSet  charset = XmSTRING_DEFAULT_CHARSET;
	char            *pathName;
        char            *directoryPath;
	char 		 buf[200];
        int              returnValue; 

       /********************************
        Enable File Selection Dialog Box 
        ********************************/

	XmFileSelectionBoxCallbackStruct *fscbs = (XmFileSelectionBoxCallbackStruct *) cbs;

	XmStringGetLtoR(fscbs -> value, charset, &pathName);

       /***********************************************
        Open, Populate, and close temporary output file
        ***********************************************/

        returnValue = populate_output_file(); 

        if (returnValue == NO_DATA_AVAILABLE)
        {
            ErrorDialog(fstmt_dialog, "Impact Statement data not available for selected station."); 
            XtFree(pathName); 
        }

        else if (returnValue == FILE_PROBLEM)
                 XtFree(pathName); 

        else  /* File was successfully opened, populated, and closed. */ 
        {
            directoryPath = "/tmp"; 

	    SetCursor(w, XC_watch);
            sprintf(buf, "mv %s/tempfile.dat %s", directoryPath, pathName); 
            system(buf); 

	    XtFree(pathName);
            XtDestroyWidget(fstmt_dialog);
            fstmt_dialog = NULL; 
	    UnsetCursor(w);
        }

	return;

}  /*  Closing brace of FSTMTselectfile_CB() function */ 

/***************************
 The text_wrapper() function 
 ***************************/

void text_wrapper(char *ImpactStatement, FILE *filePtr)

{  /* Opening brace of text_wrapper() function */ 

   char  LineBuffer[80 + 1];
   char *statement;
   char  wordBoundary[70 + 1]; 
   char *wordBoundaryPtr; 
   int   i; 

   /**********************
    Allocate memory blocks
    **********************/

    memset(LineBuffer, '\0', sizeof(LineBuffer)); 

    memset(wordBoundary, '\0', sizeof(wordBoundary)); 

    wordBoundaryPtr = wordBoundary; 

   /****************************************************************************
    The Impact Statement is defined in the Floodstmt table as character data
    of length 512.  Memory is allocated to an arbitrary 520 bytes to allow 
    for appendage of a blank space to the end.  Based on this wrapper algorithm,  
    the appendage is necessary to insure complete outputting of the Impact 
    Statement to the output file.  
    ****************************************************************************/

    statement = malloc(520); 

    strcpy(statement, ImpactStatement); 

    strcat(statement, " "); 

  /************************************************************
   Used pointer/offset notation to access individual characters
   within the impact statement variable.
   ************************************************************/

   for (i = 0; *(statement + i) != '\0'; i++)
   {
        if (*(statement + i) != ' ' && strlen(wordBoundary) < 70)
            *wordBoundaryPtr++ = *(statement + i); 

        else
        {
            *wordBoundaryPtr++ = *(statement + i);

            if ( (strlen(LineBuffer) + strlen(wordBoundary)) <= 80 )
                strcat(LineBuffer, wordBoundary); 

            else
            {
                fprintf(filePtr, "%s\n", LineBuffer); 
                memset(LineBuffer, '\0', sizeof(LineBuffer)); 
                strcpy(LineBuffer, wordBoundary); 
            }

            /*****************************************************************
             Reinitialize memory for the wordBoundary character array variable 
             *****************************************************************/

             memset(wordBoundary, '\0', sizeof(wordBoundary)); 

             wordBoundaryPtr = wordBoundary; 
        }
    }

   /********************************************************
    Output remaining line of Impact Statement to output file
    ********************************************************/

    fprintf(filePtr, "%s\n\n", LineBuffer); 

   /*********************************
    Free dynamically allocated memory
    *********************************/

    free(statement); 

} /* Closing brace of text_wrapper() function */ 




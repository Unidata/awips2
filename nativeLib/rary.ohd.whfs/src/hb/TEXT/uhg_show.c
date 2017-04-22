/*
	File:		uhg_show.c
	Date:		May 2001
	Author:		Russell Erb
	
	Purpose:	Provide support for the UnitGraph Curve DS.
*/


#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "fstmt_cbs.h"
#include "GeneralUtil.h"
#include "HwStages.h"
#include "UnitGraph.h"
#include "user_prefs.h"
#include "uhg_cbs.h"
#include "uhgds.h"
#include "hybase.h"
#include "List.h"


/*
	Globals.
*/
Widget		uhgfileSB = (Widget) NULL;

char		uhg_lid[LOC_ID_LEN + 1];

HwStages	*hw;

UnitGraph	*unitgraph = NULL ;



void	ShowUhgDs(Widget w, char *lid)
{
   char		where[MAX_WHERE_LEN];
   
   if (! uhgDS)
   {
      create_uhgDS(GetTopShell(w));
      uhg_callbacks();
   }
   
   
   if (! XtIsManaged(uhgDS))
   {
      strcpy(uhg_lid,lid);
      sprintf(where, " WHERE lid='%s' and dur=1001 ORDER BY ordinal ASC ", uhg_lid);
      unitgraph = GetUnitGraph(where);
      load_list_uhg(unitgraph);
      
      set_window_title(uhgDS, "Unit Hydrograph", uhg_lid);		
      XtManageChild(uhgFM);
      XtManageChild(uhgDS);
      uhg_sensitize();
   }
   
   return;
}


void	uhg_callbacks(void)
{
	Atom	wmAtom;
	
	wmAtom = XmInternAtom(XtDisplay(uhgDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(uhgDS, wmAtom, close_uhg, NULL);
	XtAddCallback(uhgLB,       XmNbrowseSelectionCallback, current_ordinal_uhg, NULL);
	XtAddCallback(uhgimpPB,    XmNactivateCallback, import_file_uhg, NULL);
	XtAddCallback(uhgaddPB,    XmNactivateCallback, uhg_add, NULL);
	XtAddCallback(uhgokPB,     XmNactivateCallback, uhg_ok, NULL);
	XtAddCallback(uhgapplyPB,  XmNactivateCallback, uhg_save, NULL);
	XtAddCallback(uhgclosePB,  XmNactivateCallback, close_uhg,  NULL);
	XtAddCallback(uhgremPB,    XmNactivateCallback, uhg_remove_conf, NULL);
	XtAddCallback(uhgdelPB,    XmNactivateCallback, uhg_del_conf, NULL);
	XtAddCallback(uhgmodifyPB, XmNactivateCallback, uhg_modify, NULL);
	
	XtAddCallback(uhgstgTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	XtAddCallback(uhgdurTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	XtAddCallback(uhgqTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);
	XtAddCallback(uhgareaTxt,XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE);


	return;
}


void	import_file_uhg(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	list;
   Arg		args[10];
   int		ac;
   XmString	str;
   char		import_dir[128];
   int		gad_token_len=0, gad_value_len=0;
   
   
   if(! uhgfileSB)
   {
      uhgfileSB = XmCreateFileSelectionDialog(GetTopShell(w), "uhgfileSB", NULL, 0);
      
      
      /* set XmNpattern & XmNdialogStyle */
      str = XmStringCreateSimple("*.uhg");
      ac = 0;
      XtSetArg(args[ac], XmNpattern, str); ac++;
      XtSetArg(args[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;

      gad_token_len = strlen("whfs_import_dir");
      get_apps_defaults("whfs_import_dir", &gad_token_len, import_dir, &gad_value_len);
      str = XmStringCreateSimple(import_dir);
      XtSetArg(args[ac], XmNdirectory, str); ac++;
      XtSetValues(uhgfileSB, args, ac);
      XmStringFree(str);
      
      
      XtAddCallback(uhgfileSB, XmNcancelCallback, close_file_uhg, NULL);
      XtAddCallback(uhgfileSB, XmNokCallback, select_file_uhg, NULL);
      
      list = XmFileSelectionBoxGetChild(uhgfileSB, XmDIALOG_LIST);
      XtAddCallback(list, XmNdefaultActionCallback, select_file_uhg, NULL);
   }   
   
   SetTitle(uhgfileSB,"Import File Selection");
   if(! XtIsManaged(uhgfileSB))
      XtManageChild(uhgfileSB);
   
   return;
}



void	select_file_uhg(Widget w, XtPointer ptr, XmFileSelectionBoxCallbackStruct *cbs)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        FILE		*fp = NULL ;
	char		*name = NULL ,
			*str = NULL ,
			*area_id = NULL ,
			*dur = NULL ,
			*ordinal = NULL ,
			*discharge = NULL ,
			buf [ MAX_BUF_LEN ] ,
                        error_message [ MAX_BUF_LEN ] ;
	int		i;
	UnitGraph	* uhg = NULL ;
	
	
	if (! XmStringGetLtoR(cbs->value, charset, &name))
		return;
		
	if (! *name)
	{
		XtFree(name);
		return;
	}

	if ((fp = fopen(name, "r")) == (FILE *) NULL)
	{
		return;
	}	
	
	
	/*
		Make sure it is free before malloc any more.
	*/
	if (unitgraph)
	{
	   FreeUnitGraph(unitgraph);
           unitgraph = NULL ;
	}	
	unitgraph = (UnitGraph *) malloc(sizeof(UnitGraph));
	ListInit(&unitgraph->list);

        /* For safety's sake, make sure that the "prev" and "next" pointers
           of the node member of the unitgraph are initially set to NULL. */
        unitgraph->node.next = NULL ;
        unitgraph->node.prev = NULL ;
	
	for (i = 0; fgets(buf, sizeof(buf), fp); i++)
	{
			str = buf;						
			area_id   = strtok(str, " ");
			dur       = strtok(NULL, " \n");
			ordinal   = strtok(NULL, " \n");
			discharge = strtok(NULL, " \n");

                        if ( area_id == NULL || dur == NULL ||
                             ordinal == NULL || discharge == NULL )
                        {
                           /* New logic added by Bryon Lawrence on
                              1/18/2001. If any of the fields read in from
                              the flat file are missing, then prompt the
                              user with an error message indicating that the
                              file is corrupt. */ 
                           sprintf ( error_message ,
                                     "File %s is corrupt.\n"
                                     "Import aborted.\n" , name ) ;
                           ErrorDialog ( w , error_message ) ;
         
                           /* Free any memory used by the unit graph linked
                              list. */ 
                           FreeUnitGraph ( unitgraph ) ;
                           unitgraph = NULL ;
                           return ;
                        }
			
			if ((uhg = (UnitGraph *) malloc(sizeof(UnitGraph))) == NULL)
			{
				return;
			}
			
			memset(uhg, '\0', sizeof(UnitGraph));

			strcpy(uhg->lid, (char *)CurrentLid());
			strcpy(uhg->area_id, area_id);
			uhg->dur       = atoi(dur);
			uhg->ordinal   = atoi(ordinal);
			uhg->discharge = atof(discharge);

			ListAdd(&unitgraph->list, &uhg->node);
	}
	
	load_list_uhg(unitgraph);

        /* Close the file dialog box. */
	if ( ( uhgfileSB != NULL ) && ( XtIsManaged (uhgfileSB ) ) )
	{
	   XtDestroyWidget(uhgfileSB);
	   uhgfileSB = NULL;
	}
	
	XtFree(name);	
	fclose(fp);
	
	return;
}


void	close_file_uhg(Widget w, XtPointer ptr, XtPointer cbs)
{
   if (XtIsManaged(uhgfileSB))
   {
      XtDestroyWidget(uhgfileSB);
      uhgfileSB = NULL;
   }
   
   return;
}


void	load_list_uhg(UnitGraph *unitgraph)
{
	XmStringTable	xmStr;
	Arg		arg[5];
	char		buf[MAX_BUF_LEN];
	int		cnt,
			i;
	UnitGraph	* uhgPtr = NULL ;
			
	XmTextSetString(uhgstgTxt, "");
	XmTextSetString(uhgdurTxt, "1001");
	XmTextSetString(uhgqTxt, "");
	XmTextSetString(uhgareaTxt, "");
 	
        /* If "unitgraph" is NULL, then don't do anything. */
        if ( unitgraph == NULL ) return ;
			
	if ((cnt = ListCount(&unitgraph->list)) >= 0)
	{
		xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
		uhgPtr = (UnitGraph *) ListFirst(&unitgraph->list);
		for (i = 0; uhgPtr; i++)
		{
			sprintf(buf, "%-8s    %4ld    %3ld    %10.1f",
				uhgPtr->area_id, 
                                uhgPtr->dur, 
                                uhgPtr->ordinal, 
                                uhgPtr->discharge);
			xmStr[i] = XmStringCreateSimple(buf);
			uhgPtr = (UnitGraph *) ListNext(&uhgPtr->node);			
		}
		
		
		XtSetArg(arg[0], XmNitemCount, cnt);
		XtSetArg(arg[1], XmNitems, xmStr);
		XtSetValues(uhgLB, arg, 2);

		for(i = 0; i < cnt; i++)
			XmStringFree(xmStr[i]);
		XtFree((char *) xmStr);	
	}
	
	XmListSelectPos(uhgLB,1,True);
	uhg_sensitize();
	
	return;
}


void 	uhg_modify(Widget w, XtPointer ptr, XtPointer cbs)
{

	UnitGraph	*ugPtr = NULL , 
                        *oldPtr = NULL , 
                        *newPtr = NULL ;
	char	c_ordinal[10],
		c_dur[10],
		c_flow[10],
		c_area[10];
	double	flow;
	int	ordinal, dur,
		success = 0,
		cnt,
		pos,
		*poslist = NULL ;
	/*
		Get strings from XmTexts.
		Check for empty strings.
	*/
	
	strcpy(c_ordinal,XmTextGetString(uhgstgTxt));
	strcpy(c_dur,XmTextGetString(uhgdurTxt));
	strcpy(c_flow,XmTextGetString(uhgqTxt));
	strcpy(c_area,XmTextGetString(uhgareaTxt));
	
	if ( (strlen(c_ordinal) == 0) || (strlen(c_flow) == 0) || (strlen(c_area) == 0) )
	{
		ErrorDialog(uhgDS,"Null fields may not be entered.");
		return;
	}


	/*
		Convert strings to numbers and check for negativity.
	*/
	
	ordinal = atoi(c_ordinal);
	dur = atoi(c_dur);
	flow  = atof(c_flow);
	
	if (flow < 0)
	{
		ErrorDialog(uhgDS,"Negative flow values may not be entered.");
		return;
	}

	/*
		malloc new node,check for success, assign entered values
	*/
	
	newPtr = (UnitGraph *) malloc(sizeof(UnitGraph));
	if (newPtr)
	{	
		strcpy(newPtr->lid,uhg_lid);
		newPtr->ordinal = ordinal;
		newPtr->dur = dur;
		newPtr->discharge = flow;
		strcpy(newPtr->area_id, c_area);
	}
	else
	{
		ErrorDialog(uhgDS,"Unable to allocate memory for new ordinal\n");
		return;
	}

	
	/*
		Find the selected record, check for errors, and then find the
		associated linked-list node.
	*/
	
	XmListGetSelectedPos(uhgLB,&poslist,&cnt);
	pos = poslist[0];
	XtFree( (char *) poslist);
	
	if (cnt == 1)
	{
                if ( unitgraph != NULL )
                {
	  	   oldPtr = (UnitGraph*)ListNth(&unitgraph->list,pos);
                }
	}
	else
	{
		ErrorDialog(uhgDS,"You must first select a record to modify.");
		free(newPtr);
		return;
	}
	
	
	/*
		Go through list, looking for place to insert new node.
	*/
	if ( unitgraph != NULL ) 
        {
           ugPtr = (UnitGraph *) ListFirst(&unitgraph->list);
        }

	while(ugPtr)
	{
		if (ordinal == ugPtr->ordinal) 
		{
			if (ugPtr == oldPtr)  /* replacing record with same 
                                                 ordinal */

			{
				ListInsert(&unitgraph->list,(Node *)oldPtr,(Node *)newPtr);
				ListDelete(&unitgraph->list,(Node *) oldPtr);
				success = 1;
				break;
			}
			else 
			{
				ErrorDialog(uhgDS,
					    "Duplicate ordinal values may not be entered.");
				free(newPtr);
				return;
				
			}
		}
		else if (ordinal < ugPtr->ordinal)
		{	
			ListInsert(&unitgraph->list,(Node *)ugPtr,(Node *)newPtr);
			ListDelete(&unitgraph->list,(Node *)oldPtr);
			success = 1;
			break;
		}
		ugPtr = (UnitGraph *) ListNext(&ugPtr->node);
	}
	
	
	/* 
	    if (ugPtr == NULL ), new node must be last in list. 
	    So can't use ListInsert, must use ListAdd instead
	*/
	
	if (ugPtr == NULL)
	{
                if ( unitgraph != NULL )
                {
		   ListAdd(&unitgraph->list, &newPtr->node);
		   ListDelete(&unitgraph->list,(Node *)oldPtr);
                }

		success = 1;
	}
	
	/*
		If have never put newnode in list, free the newPtr.
	*/
	
	if (! success )
	{
		free(newPtr);
		return;
	}
		
	
	if ( unitgraph != NULL ) load_list_uhg(unitgraph);
	if ( newPtr != NULL ) uhg_find(newPtr);
	
	uhg_sensitize();
}


void 	uhg_add(Widget w, XtPointer ptr, XtPointer cbs)
{
	UnitGraph	*ugPtr = NULL , *newPtr = NULL ;
	char	c_ordinal[10],
		c_dur[10],
		c_flow[10],
		c_area[10];
	double	flow;
	int	ordinal, dur,
		success = 0;
	
	
	/*
		Get strings from XmTexts.
		Check for empty strings.
	*/
	
	strcpy(c_ordinal,XmTextGetString(uhgstgTxt));
	strcpy(c_dur,XmTextGetString(uhgdurTxt));
	strcpy(c_flow,XmTextGetString(uhgqTxt));
	strcpy(c_area,XmTextGetString(uhgareaTxt));
	
	if ( (strlen(c_ordinal) == 0) || 
             (strlen(c_flow) == 0) || 
             (strlen(c_area) == 0) )
	{
		ErrorDialog(uhgDS,"Null fields may not be entered.");
		return;
	}


	/*
		Convert strings to numbers and check for flow negativity.
	*/
	
	ordinal = atoi(c_ordinal);
	dur = atoi(c_dur);
	flow  = atof(c_flow);
	if (flow < 0)
	{
		ErrorDialog(uhgDS,"Negative flow values may not be entered.");
		return;
	}

	
	/*
		malloc new node,check for success, assign entered values
	*/
	
	newPtr = (UnitGraph *) malloc(sizeof(UnitGraph));
	if (newPtr)
	{	
		strcpy(newPtr->lid, uhg_lid);
		newPtr->ordinal = ordinal;
		newPtr->dur = dur;
		newPtr->discharge = flow;
		strcpy(newPtr->area_id, c_area);
	}
	else
	{
		ErrorDialog(uhgDS,"Unable to allocate memory for new ordinal\n");
		return;
	}

	
	/*
		Go through list, looking for place to insert new node.
	*/
	
        if ( unitgraph != NULL )
        { 
	   ugPtr = (UnitGraph *) ListFirst(&unitgraph->list);
        }
        else
        {
	   unitgraph = (UnitGraph *) malloc(sizeof(UnitGraph));
	   ListInit(&unitgraph->list);
           /* For safety's sake, make sure that the "prev" and "next" pointers
            of the node member of the unitgraph are initially set to NULL. */
           unitgraph->node.next = NULL ;
           unitgraph->node.prev = NULL ;
        }

	while(ugPtr)
	{
		if (ordinal == ugPtr->ordinal)
		{
			ErrorDialog(uhgDS,
				    "Duplicate ordinal values may not be entered.");
			free(newPtr);
			return;
		}
		else if (ordinal < ugPtr->ordinal)
		{
			ListInsert(&unitgraph->list, &ugPtr->node, &newPtr->node);
			success = 1;
			break;
		}
		ugPtr = (UnitGraph *) ListNext(&ugPtr->node);
	}
	
	
	
	/* 
	    if (ugPtr == NULL ), new node must be last in list. 
	    So can't use ListInsert, must use ListAdd instead
	*/
	
	if (ugPtr == NULL)
	{	
		ListAdd(&unitgraph->list, &newPtr->node);
		success = 1;
	}
	
	
	/*
		If have never put newnode in list, free the newPtr.
	*/
	
	if ( ! success )
	{
                if ( newPtr != NULL ) free ( newPtr ) ;
		return ;
	}	
		
	if ( unitgraph != NULL ) load_list_uhg(unitgraph);
	if ( newPtr != NULL ) uhg_find(newPtr);
	
	uhg_sensitize();
}


void	current_ordinal_uhg(Widget w, XtPointer ptr, XtPointer cbs)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        XmString        *xmStr = NULL ;
        Arg             arg[2];
        char            *text = NULL ,
                        *ordinal = NULL ,
                        *flow = NULL ,
                        *area_id = NULL ,
                        *dur = NULL ;

        /*
                Get the currently selected list item.
        */
        XtSetArg(arg[0], XmNselectedItems, &xmStr);
        XtGetValues(uhgLB, arg, 1);

        if (XmStringGetLtoR(xmStr[0], charset, &text))
        {
                area_id = strtok(text, " ");
                dur     = strtok(NULL, " ");
                ordinal = strtok(NULL, " ");
                flow    = strtok(NULL, " ");

                XmTextSetString(uhgstgTxt, ordinal);
                XmTextSetString(uhgdurTxt, dur);
                XmTextSetString(uhgqTxt, flow);
                XmTextSetString(uhgareaTxt, area_id);
       }      	
	return;
}


void	uhg_remove_conf(Widget w, XtPointer ptr, XtPointer cbs) 
{
        Widget	qstDS,
		okPB;
        char	buf[MAX_BUF_LEN];


        sprintf(buf, "This will remove the highlighted record.");
        qstDS = QuestionDialog(uhgDS, buf);	                
        SetTitle(qstDS, "Remove Confirmation");


        /*
                Get the XmMessageBox ok button,
                and associate a callback to it.
        */       
	okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);              
        XtAddCallback(okPB, XmNactivateCallback, uhg_remove, NULL);              		
        uhg_sensitize();

        return;
}


void	uhg_remove(Widget w,  XtPointer ptr, XtPointer cbs)
{
   UnitGraph	* ugPtr = NULL ;
   
   int		pos,
      		* poslist = NULL ,
      		cnt = 0;
   
   char	where[MAX_WHERE_LEN];
   
   sprintf(where, " WHERE lid = '%s' ", uhg_lid);	
   
   XmListGetSelectedPos(uhgLB, &poslist, &cnt);
   pos = poslist[0];
   
   if (cnt > 0)
   {
      if ( unitgraph != NULL )
      {
       	 ugPtr = (UnitGraph *) ListNth(&unitgraph->list, pos);
         ListDelete(&unitgraph->list, &ugPtr->node);
      }
   }
   
   if ( unitgraph != NULL ) load_list_uhg(unitgraph) ;
   
   if (pos > ListRsrcGetCount(uhgLB))
   {
      pos = pos - 1;
   }
   XmListSetPos(uhgLB, pos);
   XmListSelectPos(uhgLB, pos, True);

   XtFree((char *) poslist); 
   uhg_sensitize();
   
   return;
}


void	clear_list_uhg(void)
{
	Arg	arg[2];
	int	cnt;
	
	XtSetArg(arg[0], XmNitemCount, &cnt);
	XtGetValues(uhgLB, arg, 1);	
	if (cnt) 
	{
		XmListDeleteAllItems(uhgLB);
		DeSensitize(uhgimpPB);
	}
	XmTextSetString(uhgstgTxt,"");
	XmTextSetString(uhgdurTxt,"");
	XmTextSetString(uhgqTxt,"");
	XmTextSetString(uhgareaTxt,"");
	
	if (unitgraph != NULL)
	{
	   FreeUnitGraph(unitgraph);
           unitgraph = NULL ;
	}
	uhg_sensitize();
	
	return;
}


void	uhg_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   uhg_save(NULL, NULL, NULL);
   close_uhg(NULL, NULL, NULL);
   
   return;
}


void	uhg_save(Widget w, XtPointer ptr, XtPointer cbs)
{
   UnitGraph		* uhgPtr = NULL ;
   char		where[MAX_WHERE_LEN];
   
   
   sprintf(where, " WHERE lid = '%s' ", uhg_lid);	
   
   DeleteUnitGraph(where);
   
   if ( unitgraph != NULL )
   {
      if (ListCount(&unitgraph->list) > 0 )
      {
         uhgPtr = (UnitGraph *) ListFirst(&unitgraph->list);

         while (uhgPtr)
         {
	    PutUnitGraph(uhgPtr);
	    uhgPtr = (UnitGraph *) ListNext(&uhgPtr->node);
         }
      }
   }
   
   
   return;
}


void	uhg_del_conf(Widget w, XtPointer ptr, XtPointer cbs) 
{
   Widget	qstDS,
		okPB;
   char		buf[MAX_BUF_LEN];
   
   
   sprintf(buf, "This will delete the unit hydrograph for %s.", uhg_lid);
   qstDS = QuestionDialog(uhgDS, buf);
   SetTitle(qstDS, "Delete Confirmation");
   
   
   /*
   	Get the XmMessageBox ok button,
   	and associate a callback to it.
   */
   okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
   XtAddCallback(okPB, XmNactivateCallback, uhg_delete, NULL);          
   
   
   if(! XtIsManaged(qstDS))
      XtManageChild(qstDS);
   
   return;
}


void	uhg_delete(Widget w, XtPointer ptr, XtPointer cbs)
{        
	char	where[MAX_WHERE_LEN];
	
	SetCursor(uhgFM, XC_watch);
	
	sprintf(where, " WHERE lid = '%s' ", uhg_lid);
	DeleteUnitGraph(where);
	
	if ( unitgraph != NULL )
	{
	   FreeUnitGraph(unitgraph);
           unitgraph = NULL ;
	}
	unitgraph = (UnitGraph *) malloc(sizeof(UnitGraph));
	ListInit(&unitgraph->list);

        /* Temporary fix by BAL on 1/11/01. */
        unitgraph->node.next = NULL ;
        unitgraph->node.prev = NULL ;
	
	clearForm(uhgFM);
	XmListDeleteAllItems(uhgLB);
        uhg_sensitize();
	
	
	UnsetCursor(uhgFM);
	
	
	return;
}


void	close_uhg(Widget w, XtPointer ptr, XtPointer cbs)
{

   clear_list_uhg();

   if(XtIsManaged(uhgDS))
   {
      XtDestroyWidget(uhgDS);
      uhgDS = NULL;
   }
 
   /*
   if ( hw != NULL )
   {
      FreeHwStages(hw);
   }
   */

   if ( unitgraph != NULL )
   {
      FreeUnitGraph(unitgraph);
      unitgraph = NULL;
   }
   
   return;
}


void	uhg_sensitize(void)
{
	int	count = 0 ;
	
        if ( unitgraph != NULL )
        {
	   count = ListCount(&unitgraph->list);
        }
	
	if (count > 0)
	{
		DeSensitize(uhgimpPB);
		Sensitize(uhgremPB);
		Sensitize(uhgmodifyPB);
	}	
	else
	{	
		Sensitize(uhgimpPB);
		DeSensitize(uhgremPB);
		DeSensitize(uhgmodifyPB);
	}
	Sensitize(uhgaddPB);
}



void	uhg_find(UnitGraph *uhgPtr)
{
   char			liststr[80];
   XmString		item;
   int			pos;

   
   sprintf(liststr, "%-8s    %4ld    %3ld    %10.1f",
	   uhgPtr->area_id, uhgPtr->dur, uhgPtr->ordinal, uhgPtr->discharge);

   item = XmStringCreateSimple(liststr);
   
   if (XmListItemExists(uhgLB, item))
   {
      pos = XmListItemPos(uhgLB, item);
      XmListSetPos(uhgLB, pos);
      XmListSelectPos(uhgLB, pos, True);
   }
   else
   {
      XmListSetPos(uhgLB, 1);
      XmListSelectPos(uhgLB, 1, True);
   }
   XmStringFree(item);
   
   return;
}





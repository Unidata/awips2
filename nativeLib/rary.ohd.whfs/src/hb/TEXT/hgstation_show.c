/*
	File:		hgstation_show.c
	Date:		March 2006
	Author:		Jingtao Deng
	
	Purpose:	Display, add, edit or delete data from HgStation table
	
        Revision:

	Cham Pham	09/05/07
			- Added the logic to handle HydroGen window if HgStation
			table is empty.
	
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "HgStation.h"
#include "hgstation.h"
#include "ShefTs.h"
#include "ShefPe.h"
#include "Location.h"
#include "LoadUnique.h"
#include "hgstation_show.h"
#include "hgstation_struct.h"
#include "ParamDefs.h"

ShefTs   *sheftsHead = NULL;
ShefTs   *sheffcsttsHead = NULL;
ShefPe   *shefpeHead = NULL;
hgstation_struct *hgs_struct;

/************************************************************************
   hgs_show()
   
   PURPOSE   
   Defines and contains the actions of the HgStation shell.
   
*********************************************************************/
void	hgs_show(Widget w)
{
	if (! hgstationDS)
	{	
	    create_hgstationDS(GetTopShell(w));
	    create_shefpe();
	    create_shefts();
	    create_sheffcstts();
	    hgs_callbacks();
        }

	
	if (! XtIsManaged(hgstationDS))
	{		
	    /*load the lid/pe/ts from HgStation table in the list*/
	    
	    load_hgs();
	    
	    /*display the window*/
	    
	    XtManageChild(hgstationFO);
	    XtManageChild(hgstationDS);	    	    
	}
	
	/*free space*/
		
	
	
	return;
}

/*****************************************************************
   hgs_callbacks()
   
   PURPOSE
   Defines and contains the callbacks for the HgStation window.
******************************************************************/      
void	hgs_callbacks()
{
	Atom	atom;
	
	/*Window manager callbacks.*/
	
	atom = XmInternAtom(XtDisplay(hgstationDS), "WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(hgstationDS, atom, hgs_close, NULL);
		
 	/*General widget callbacks.*/
	
        XtAddCallback(hgslistLI,   XmNdefaultActionCallback,   hgs_import,  NULL);
        XtAddCallback(hgslistLI,   XmNbrowseSelectionCallback, hgs_import,  NULL);
	XtAddCallback(hgs_applyPB, XmNactivateCallback,        hgs_apply,   NULL);
	XtAddCallback(hgs_closePB, XmNactivateCallback,        hgs_close,   NULL);
	XtAddCallback(hgs_deletePB,XmNactivateCallback,        hgs_del_conf,NULL);
		
	/*TextFilter callbacks.*/

       /* XtAddCallback(hgsitem_fcsttsTE,  XmNmodifyVerifyCallback, (XtCallbackProc)alpha_filter,
	              (XtPointer)UPPERCASE);*/
	XtAddCallback(hgsitem_locTE,     XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter,
	              (XtPointer)UPPERCASE);
	
	return;
}


/***************************************************************************
   hgs_close()
   
   PURPOSE
   Close the HgStation window.
   
***************************************************************************/   
void	hgs_close(Widget w, XtPointer ptr, XtPointer cbs)
{
	if(XtIsManaged(hgstationDS))
	{
	   XtDestroyWidget(hgstationDS);
	   hgstationDS = NULL;
	}

	if (hgs_struct != NULL)
	   free(hgs_struct);
	   
        if (sheftsHead != NULL)
	{
	   FreeShefTs(sheftsHead);
	   sheftsHead = NULL;
	}
	
	if (shefpeHead != NULL)
	{
	   FreeShefPe(shefpeHead);
	   shefpeHead = NULL;
	}
	      	   

	return;
}

/**********************************************************
  hgs_del_conf()
  
  PURPOSE
  Delete the selected lid/pe/ts data from the list,
  also deleted it from HgStation table.
************************************************************/	   
void	hgs_del_conf(Widget w, XtPointer ptr, XtPointer cbs)
{        
	Widget	qstDS;
	Widget	okPB;
	char         buf[MAX_BUF_LEN];

	sprintf(buf, "Do you wish to delete this entry?");
	qstDS = QuestionDialog(hgstationDS, buf);
	SetTitle(qstDS, "Delete Confirmation");


	/*Get the XmMessageBox ok button, and associate a callback to it.*/

	okPB = XmMessageBoxGetChild(qstDS, XmDIALOG_OK_BUTTON);
	XtAddCallback(okPB, XmNactivateCallback, hgs_delete, NULL);

	if(! XtIsManaged(qstDS))
	   XtManageChild(qstDS);

	return;
}

/**********************************************************
  hgs_delete()
  
  PURPOSE
  Delete the selected lid/pe/ts data from the list,
  also deleted it from HgStation table. Reload the list and
  select pos.
************************************************************/	
void	hgs_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
	char	key[MAX_WHERE_LEN];
	int	pos, status;
        char    msg[MAX_BUF_LEN] = "";

	/*Delete record from dbms,and reload list and select pos.*/
	
	SetCursor(hgslistFO, XC_watch);
	
	hgs_key(key, &pos);
	
	if (strlen(key))
	{
	   status =DeleteHgStation(key);
	   if (status != 0)
	   {
	      sprintf(msg, "Unable to delete record from table HgStation: %d\n", status);
              ErrorDialog(hgstationDS, msg);
	      return;
           }
	}
	   
	load_hgs();
	XmListSelectPos(hgslistLI, pos, TRUE);	
	
	UnsetCursor(hgslistFO);
	
	return;
}

/**********************************************************
  hgs_key()
  
  PURPOSE
  Return the primary key lid/pe/ts from HgStation table
  for the selected item in the list.
  
************************************************************/	
void	hgs_key(char *key, int *pos)
{	
	char		where[MAX_WHERE_LEN];
	int		*poslist;
	int		cnt = 0;
		
	XmListGetSelectedPos(hgslistLI, &poslist, &cnt);	
	if ( cnt == 0 )
	   return;
	
	sprintf(where, " WHERE lid = '%s' and pe = '%s' and ts = '%s'",
		        hgs_struct[*poslist - 1].lid, hgs_struct[*poslist - 1].pe,
			hgs_struct[*poslist - 1].ts);
	
	*pos = poslist[0];
	strcpy(key, where);
	XtFree((char *)poslist);		
		
	return;
}

/**********************************************************
  hgs_apply()
  
  PURPOSE
  If it is new lid/pe/ts data, insert it into HgStation table and
  update the display in in the list,
  If it exists, update the data in HgStation table and update the
  display in the list.
************************************************************/
void	hgs_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
	HgStation   hgs;
	char        *buf = NULL;
	char        key[MAX_WHERE_LEN] = "";
	char        msg[MAX_BUF_LEN] = "";
	int         error, pos;	
	int         *poslist, pe_nth;
	int         ts_menu_pos, ts_menu_nth, fcstts_menu_pos, fcstts_menu_nth;
	ShefTs      *ts_menu_shefts = NULL, *fcstts_menu_shefts = NULL;
	ShefPe      *menu_shefpe = NULL;
	int         cnt = 0;
	bool        isUnique;
	
	/*alloc space for hgs*/
	
	memset(&hgs, '\0', sizeof(hgs));		
	
	/*obtain lid from text box*/
	
	if ( (buf = XmTextGetString(hgsitem_locTE)) )
	{
	   strcpy(hgs.lid, buf);	   
	   XtFree(buf);
	   buf = NULL;
	}
	
	/*obtain type source from the option menu*/
	
	ts_menu_pos = GetMenuPos(hgsitem_tsOM);
	ts_menu_nth = ts_menu_pos + 1;
	ts_menu_shefts = (ShefTs *) ListNth(&sheftsHead->list, ts_menu_nth);
	if (ts_menu_shefts != NULL)
	   strcpy(hgs.ts, ts_menu_shefts->ts);
	else
	{
	    ErrorDialog(hgstationDS, "You must select a TypeSource. \n");
	    return;
	}
	
	/*obtain forecast type source from the option menu*/
	
	fcstts_menu_pos = GetMenuPos(hgsitem_fcsttsOM);
	fcstts_menu_nth = fcstts_menu_pos + 1;
	fcstts_menu_shefts = (ShefTs *) ListNth(&sheffcsttsHead->list, fcstts_menu_nth);
	if (fcstts_menu_shefts != NULL)
	   strcpy(hgs.fcstts, fcstts_menu_shefts->ts);
	else
	{
	    ErrorDialog(hgstationDS, "You must select a Forecast TypeSource. \n");
	    return;
	}    
	
	/*obtain pe from the list*/
	
	XmListGetSelectedPos(hgsitem_peLI, &poslist, &cnt);
	if (cnt > 0)
	{
	   pe_nth = poslist[0];
	   menu_shefpe = (ShefPe *) ListNth(&shefpeHead->list, pe_nth);
	   	   	   
	   if(menu_shefpe != NULL)
	      strcpy(hgs.pe, menu_shefpe->pe);
	   
	   /*clean up*/
	      
	   if (poslist != (int *) NULL)
	   {    
	       free(poslist);
	       poslist = NULL;
	   }              	      
	}
	else
	{
	    ErrorDialog(hgstationDS, "You must select a Physical Element. \n");
	    return;
	}
	
	
	/*Insert/Update the entered dbms record. If insert data failed (error!=0), 
	then update*/               
	       
	error = InsertIfUniqueHgStation(&hgs, &isUnique);
        if ( error != 0 )
	{
	   sprintf(msg,"Unable to insert record to table HgStation: %d\n",
		        error);
	   ErrorDialog(hgstationDS, msg);
	   return;
	}

	if ( !isUnique)	         
	{
	    hgs_key(key, &pos);
	    if (strlen(key))
	    {   
	       error = UpdateHgStation(&hgs, key);
	       if (error != 0)
	       {
	           sprintf(msg, "Unable to update record to table HgStation: %d\n", error);
		   ErrorDialog(hgstationDS, msg);
		   return;
	       }
	    }   	       
	}
	
	/*update the data in the list*/
	
	load_hgs();            	

	return;
}

/**************************************************************
   load_hgs()
   
   PURPOSE
   Load the lid/pe/ts information from HgStation table into
   the scrolled list.
   
**************************************************************/
void	load_hgs()
{
	XmStringTable	xmStr = NULL;
	HgStation	*hgsHead = NULL;
	HgStation       *hgshsaHead = NULL, *hgshsaPtr = NULL;
	UniqueList      *ulHead = NULL, * ulPtr = NULL;
	char		where[MAX_WHERE_LEN];
        char		buf[MAX_BUF_LEN];
	int		cnt = 0, hsa_cnt = 0, hgshsa_cnt = 0;
	int		i = 0, j = 0;
        char		hsa_name[HYD_SERV_LEN + 1];
	
	
        /*clean up*/ 

	clearForm(hgsitemFO);
	
	/* clear out the list each time*/
	
	XmListDeleteAllItems(hgslistLI);         
       
        /* blank out the contents in the text boxes */
   
        XmTextSetString(hgsitem_locTE, "");      			
        
	/*malloc space for the data list*/
	
	sprintf(where, " ");
	
	hgsHead = GetHgStation(where);
	
	if (hgsHead != NULL)
        {
            cnt = ListCount(&hgsHead->list);	   	    	    
            xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
	    malloc_hgstation_struct(cnt, &hgs_struct);	   
	}
	else
	{
            ErrorDialog(hgstationDS, "NO DATA in HgStation table.");
	    return;
	}
	
	/*initialize hgs_struct */
	
	for (i = 0; i< cnt; i++)
	{
	   strcpy(hgs_struct[i].lid, "");
	   strcpy(hgs_struct[i].pe, "");
	   strcpy(hgs_struct[i].ts, "");
	   strcpy(hgs_struct[i].fcstts, "");    
	}
	   
	/*load unique hsa from location table*/
	
	sprintf(where, " ORDER BY hsa");
	
	ulHead = LoadUnique("hsa", "Location", where, &hsa_cnt);
	
	if (ulHead != NULL)
	{
	    ulPtr = (UniqueList *) ListFirst(&ulHead->list);

	    /*load data from Hgstation table with lid as the one in the Location table*/

            while(ulPtr)
	    {
	        /* trim the blank padded uchar string */
      
                memset(hsa_name, 0, HYD_SERV_LEN + 1);
                strncpy(hsa_name, ulPtr->uchar, HYD_SERV_LEN);
		
	        sprintf(where, " WHERE lid IN (SELECT lid FROM location WHERE hsa = '%s') "
		               " ORDER BY lid, pe, ts", hsa_name); 

		hgshsaHead = GetHgStation(where);		                  
		
		if (hgshsaHead != NULL)
		{
		    hgshsa_cnt = ListCount(&hgshsaHead->list);		    
		    
		    hgshsaPtr = (HgStation *) ListFirst(&hgshsaHead->list);		    		    
                    
		    /*load the list*/
		    
		    for (i=0; hgshsaPtr; i++)
        	    {	   		   
	        	sprintf(buf, "%-10s%-6s%-6s%-6s%-6s", hgshsaPtr->lid, hsa_name, 
			          hgshsaPtr->pe, hgshsaPtr->ts, hgshsaPtr->fcstts);
                	xmStr[i+j] = XmStringCreateSimple(buf);
			
			strcpy(hgs_struct[i+j].lid, hgshsaPtr->lid);
			strcpy(hgs_struct[i+j].pe, hgshsaPtr->pe);
			strcpy(hgs_struct[i+j].ts, hgshsaPtr->ts);
			strcpy(hgs_struct[i+j].fcstts, hgshsaPtr->fcstts);

	        	hgshsaPtr = (HgStation *) ListNext(&hgshsaPtr->node);
        	    }               
		    
		    j = hgshsa_cnt + j;	   	 			    		                                
        	}
		
		ulPtr = (UniqueList *) ListNext(&ulPtr->node);	
	    }
	    
	    /* load the string in the list.*/

            XmListAddItems(hgslistLI, xmStr, cnt, 1);	
	    XmListSelectPos(hgslistLI, 1, TRUE);
	}	    
	else
	{
	   ErrorDialog(hgstationDS, "No HSA defined for location.");
	}   
	       
	/*cleanup.*/
		
        for (i = 0; i < cnt; i++)
	{
	    if (xmStr[i] != NULL)
               XmStringFree(xmStr[i]);
	}	
	
	if (xmStr != NULL)
	   XtFree((char *) xmStr);
            
	if (hgsHead != NULL)
	{
	   FreeHgStation(hgsHead);  
	   hgsHead = NULL;
	}     
		
	if (ulHead != NULL)
	{
	   FreeUnique(ulHead);
	   ulHead = NULL;
	}
	if (hgshsaHead != NULL)
	{
	   FreeHgStation(hgshsaHead);  
	   hgshsaHead = NULL;
	}                                          

        return;	
}

/********************************************************
   create_shefts()
   
   PURPOSE
   Creates the buttons needed for the hgsitem_tsOM, observed
   type source.
   
***********************************************************/
void    create_shefts()
{
	ShefTs     *sheftsPtr = NULL;
	int        i;
	char       wname[SHEF_TS_LEN+1 + SHEF_TS_NAME_LEN +1 +2];
	Widget     hgsitem_tsPB;
	char       msg[MAX_BUF_LEN] = "";

	/* load the list of candidates */

	sheftsHead =  GetShefTs(" WHERE ts LIKE 'P%' or ts LIKE 'R%' ORDER BY ts ");

	/* loop on the number of shefts in the ShefTs table*/

	if (sheftsHead != NULL)
	{
	   sheftsPtr = (ShefTs *) ListFirst(&sheftsHead->list);
	   for (i=0; sheftsPtr; i++)
	   {
	      strcpy(wname, "");
	      sprintf(wname, "%s (%s)", sheftsPtr->ts, sheftsPtr->name);
	      hgsitem_tsPB = XtVaCreateManagedWidget(wname,
						 xmPushButtonWidgetClass,
						 hgsitem_tsPDM,
						 NULL);
	      sheftsPtr = (ShefTs *) ListNext(&sheftsPtr->node);
	   } 
	}
	else
	{
	   sprintf(msg, "ERROR...No observed type source in ShefTs table");
	   ErrorDialog(hgstationDS, msg);      
	}      	   		

	return;
 }
/********************************************************
   create_sheffcstts()
   
   PURPOSE
   Creates the buttons needed for the hgsitem_fcsttsOM.
   forecast type source
   
***********************************************************/
void    create_sheffcstts()
{
	ShefTs     *sheffcsttsPtr = NULL;
	int        i;
	char       wname[SHEF_TS_LEN+1 + SHEF_TS_NAME_LEN +1 +2];
	Widget     hgsitem_fcsttsPB;
	char       msg[MAX_BUF_LEN] = "";

	/* load the list of candidates */
	
	sheffcsttsHead =  GetShefTs(" WHERE ts LIKE 'C%' or ts LIKE 'F%' ORDER BY ts ");

	/* loop on the number of shefts in the ShefTs table*/

	if (sheffcsttsHead != NULL)
	{
	   sheffcsttsPtr = (ShefTs *) ListFirst(&sheffcsttsHead->list);
	   for (i=0; sheffcsttsPtr; i++)
	   {
	      strcpy(wname, "");
	      sprintf(wname, "%s (%s)", sheffcsttsPtr->ts, sheffcsttsPtr->name);
	      hgsitem_fcsttsPB = XtVaCreateManagedWidget(wname,
						 xmPushButtonWidgetClass,
						 hgsitem_fcsttsPDM,
						 NULL);
	      sheffcsttsPtr = (ShefTs *) ListNext(&sheffcsttsPtr->node);
	   } 
	}
	else
	{
	   sprintf(msg, "ERROR...No forecast type source in ShefTs table");
	   ErrorDialog(hgstationDS, msg);      
	}      	   		

	return;
 }

/********************************************************
   create_shefpe()
   
   PURPOSE
   Creates the list to display pe/name data retrieved from ShefPe
   table.
***********************************************************/
void    create_shefpe()
{
	ShefPe        *shefpePtr = NULL;
	int           i; 
	XmStringTable xmStr = NULL;	
	char		 buf[MAX_BUF_LEN] = "";
	int		 cnt = 0;

	/* clear out the list each time*/

	XmListDeleteAllItems(hgsitem_peLI);    
	
	shefpeHead = GetShefPe(" WHERE pe LIKE 'H%' OR pe LIKE 'Q%' ORDER BY pe ");

	if (shefpeHead != NULL)
	{
	    cnt = ListCount(&shefpeHead->list);

	    /* malloc space for the list of data in the xmStr strings*/

	    xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));

	    shefpePtr = (ShefPe *) ListFirst(&shefpeHead->list);

	    /*load the list*/

	    for (i = 0; shefpePtr; i++)
	    {		   		   
		sprintf(buf, "%-2s %-20s", shefpePtr->pe, shefpePtr->name);
        	xmStr[i] = XmStringCreateSimple(buf);
		shefpePtr = (ShefPe *) ListNext(&shefpePtr->node);
	    }               

	    /* load the string in the list.*/

	    XmListAddItems(hgsitem_peLI, xmStr, cnt, 1);	 	
	    XmListSelectPos(hgsitem_peLI, 1, TRUE);		                                
	}      
	else
	{
	    sprintf(buf, "WARNING... No data in ShefPe table.");
	    xmStr[0] = XmStringCreateSimple(buf);
	}

	/*cleanup.*/

	for (i = 0; i < cnt; i++)
	{
	    XmStringFree(xmStr[i]);
	}	
	XtFree((char *) xmStr);

	return;	

}


/**************************************************************
   hgs_import()
   
   PURPOSE
   Load the selected item info in the list to the text box and option menu.
   
   ************************************************************/
void    hgs_import(Widget w, XtPointer ptr, XmListCallbackStruct *cbs)
{	   
        char        buf[MAX_BUF_LEN] = "";
	char        pename[SHEF_PE_NAME_LEN+1] = "";
        char        ts_tsname[SHEF_TS_LEN+1  + SHEF_TS_NAME_LEN+1 +2]; 
	char        fcstts_tsname[SHEF_TS_LEN+1 + SHEF_TS_NAME_LEN+1 +2];
	char        pe_pename[SHEF_PE_LEN+1  + SHEF_PE_NAME_LEN+1 +2]; 
        XmString    item;
	ShefTs      *sheftsPtr = NULL, *sheffcsttsPtr = NULL;
	ShefPe      *shefpePtr = NULL;	       
	int         select_pos = 0;
	
        /* blank out the contents of the text boxes */
          
	XmTextSetString(hgsitem_locTE, "");         
   
        select_pos = cbs->item_position - 1;
			       	           		        
	if (hgs_struct[select_pos].lid != NULL)	   
	   strcpy(buf, hgs_struct[select_pos].lid);	      
	XmTextSetString(hgsitem_locTE, buf);

	/*load the selected type source+name into the observed type source optional menu*/

	if (sheftsHead != NULL)
        {
	    sheftsPtr = (ShefTs *) ListFirst(&sheftsHead->list);
	    while (sheftsPtr)
	    {
	        if (strcmp(sheftsPtr->ts, hgs_struct[select_pos].ts) == 0)
	        {
	            sprintf(ts_tsname, "%s (%s)", sheftsPtr->ts, sheftsPtr->name); 
	        }	    
	        sheftsPtr = (ShefTs *) ListNext(&sheftsPtr->node);
	    }           
        }

	SetMenuHistory(hgsitem_tsOM, ts_tsname);
	
	/*load the selected type source+name into the forecast type source optional menu*/

	if (sheffcsttsHead != NULL)
        {
	    sheffcsttsPtr = (ShefTs *) ListFirst(&sheffcsttsHead->list);
	    while (sheffcsttsPtr)
	    {
	        if (strcmp(sheffcsttsPtr->ts, hgs_struct[select_pos].fcstts) == 0)
	        {
	            sprintf(fcstts_tsname, "%s (%s)", sheffcsttsPtr->ts, sheffcsttsPtr->name); 
	        }	    
	        sheffcsttsPtr = (ShefTs *) ListNext(&sheffcsttsPtr->node);
	    }           
        }

	SetMenuHistory(hgsitem_fcsttsOM, fcstts_tsname);

	/*load the selected pe into the physical element's list*/

	if (shefpeHead != NULL)
	{
	    shefpePtr = (ShefPe *) ListFirst(&shefpeHead->list);

	    while (shefpePtr)
	    {
		if (strcmp(shefpePtr->pe, hgs_struct[select_pos].pe) == 0)
		{		    
		   strcpy(pename, shefpePtr->name);
		}
		shefpePtr = (ShefPe *) ListNext(&shefpePtr->node);
	    }	   			   
	}
                        
	if (hgs_struct[select_pos].pe != NULL)
	   strcpy(pe_pename, hgs_struct[select_pos].pe);
	strcat(pe_pename, " ");	
	strcat(pe_pename, pename);
	while (strlen(pe_pename) < SHEF_PE_LEN + SHEF_PE_NAME_LEN +1)
	   strcat(pe_pename, " ");               

	item = XmStringCreateSimple(pe_pename);
	XmListSetItem(hgsitem_peLI, item);
	XmListSelectItem(hgsitem_peLI, item, TRUE);

	XmStringFree(item);				        
				
        return;
}

/*********************************************************************
  malloc_hgstation_struct()
  
  PURPOSE
  Allocate the memory for the hgsation information for the product 
  being generated.    
  
********************************************************************/
void    malloc_hgstation_struct(int		cnt,
		     hgstation_struct	**hgs_struct)
{
    	*hgs_struct= (hgstation_struct *)malloc(sizeof(hgstation_struct) * cnt);
	if (*hgs_struct == NULL)
	   printf("Failed to malloc hgs_struct in hgstation_show");

	return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}      

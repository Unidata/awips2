/*
	File:		ugc_show.c
	Date:		12/20/96
	Author:		Dale Shelton, Paul Taylor
	
	Purpose:	Provides support for UGC DS.
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/ScrollBar.h>
#include <Xm/Text.h>
#include "Xtools.h"
#include "List.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Eligzon.h"
#include "Countynum.h"
#include "Counties.h"
#include "State.h"
#include "CountyInfo.h"
#include "ZoneInfo.h"
#include "Zonenum.h"
#include "hybase.h"
#include "user_prefs.h"

#include "ugc.h"
#include "ugc_show.h"


#define	COUNTIES	0
#define	ZONES		1


/*
	Structures.
*/
typedef struct ugc_info_st
{
   Counties*	cnty_avail;
   CountyInfo*	cnty_sel;
   
   Eligzon*	zone_avail;
   ZoneInfo*	zone_sel;

} ugc_info_type;


/*
	Globals.
*/
static char	ugc_lid[BUFSIZ];
ugc_info_type	*ugc;



void	ShowUgcDs(Widget w, char *lid)
{
	Atom		wmAtom;

	
	if (! ugcDS)
	{
	   
	   create_ugcDS(GetTopShell(w));
	   
	   ugc = (ugc_info_type *) malloc(sizeof(ugc_info_type));
	   memset(ugc, '\0', sizeof(ugc_info_type));

	   wmAtom = XmInternAtom(XtDisplay(ugcDS), "WM_DELETE_WINDOW", False);
	   XmAddWMProtocolCallback(ugcDS, wmAtom, ugc_close, NULL);

	   
	   /*
		Add callbacks for Lists.
	   */
	   XtAddCallback(ugccntyPB, XmNactivateCallback, ugc_checkPB, NULL);
	   XtAddCallback(ugczonePB, XmNactivateCallback, ugc_checkPB, NULL);
	   
	   XtAddCallback(ugcaddPB,   XmNactivateCallback, ugc_add,    ugcOM);
	   XtAddCallback(ugcdelPB,   XmNactivateCallback, ugc_delete, ugcOM);
	   XtAddCallback(ugcclearPB, XmNactivateCallback, ugc_clear,  ugcOM);
	   
	   XtAddCallback(ugcokPB,    XmNactivateCallback, ugc_ok,    ugcOM);
	   XtAddCallback(ugcapplyPB, XmNactivateCallback, ugc_apply, ugcOM);
	   XtAddCallback(ugccancelPB,XmNactivateCallback, ugc_close, NULL);
	   XtAddCallback(ugchelpPB,  XmNactivateCallback, ugc_help,  NULL);
/*	   
	   XtAddCallback(ugcaL, XmNdefaultActionCallback, ugc_defact, NULL);
	   XtAddCallback(ugcsL, XmNdefaultActionCallback, ugc_defact, NULL);
*/ 
	   XtAddCallback(ugcaL, XmNextendedSelectionCallback, ugc_extsel, NULL);
	   XtAddCallback(ugcsL, XmNextendedSelectionCallback, ugc_extsel, NULL);

	}

	
	if (! XtIsManaged(ugcDS))
	{
	   strcpy(ugc_lid, lid);
	   set_window_title(ugcDS, "County/Zone UGC", (char *) lid);
	   
	   SetMenuPos(ugcOM, COUNTIES);
	   ugc_checkPB(ugccntyPB, NULL, NULL);  /* default is cnty */
	   
	   XtManageChild(ugcFM);
	   XtManageChild(ugcDS);
	   XtUnmanageChild(ugchelpPB);
	}

	return;
}


void	ugc_add(Widget w, XtPointer ptr, XtPointer cbs)
{
	Widget		om = (Widget) ptr;

	int			*pos_list = NULL;

	XmStringTable	items;
	XmString		insertItem;

	int		pos_cnt = 0 ,
      		i,
      		insert_pos = 0 ,
			nth,
			nth_item;

	void	*nthPtr = NULL ,
      		*firstPtr = NULL ;


	XmListGetSelectedPos(ugcaL, &pos_list, &pos_cnt);

	if (pos_cnt > 0)
	{
		/*
    		Get all items from Available--
    		Adding only the selected items to Selected.
    		(Ignore requests to add duplicate entries.)
		*/
		XtVaGetValues(ugcaL, XmNitems, &items, NULL);
		XtUnmapWidget(ugcsL);

		for(i=0; i<pos_cnt; i++)
		{
			nth = pos_list[i];
			nth_item = nth - 1;

			if(! XmListItemExists(ugcsL, items[nth_item]))
			{
				/*
					Logic for COUNTIES/ZONES:

					Get the nth Ptr from the available list.
					Get the first Ptr from the selected list.
					Find where to insert into selected list (insert_pos).
					Add the DATA from available list to selected list.

					Add the XmString item from available list to selected list.
				*/

				if (GetMenuPos(om) == COUNTIES)
				{
					nthPtr = (void *) ListNth(&ugc->cnty_avail->list, nth);

					/* Need to test to make sure that there are any 
					  counties in the selected list. */
					if ( ugc->cnty_sel != NULL )
					{
						firstPtr = (void *) ListFirst(&ugc->cnty_sel->list);
						insert_pos = ugc_countyfindpos((Counties *)   nthPtr,
								  (CountyInfo *) firstPtr);
					}
					else
					{
						insert_pos = 1 ;
					}

					ugc_countyadd(nth, insert_pos);
				}
				else if (GetMenuPos(om) == ZONES)
				{
					nthPtr = (void *) ListNth(&ugc->zone_avail->list, nth);

					/* Need to test to make sure that there are any 
					  counties in the selected list. */
					if ( ugc->zone_sel != NULL )
					{
						firstPtr = (void *) ListFirst(&ugc->zone_sel->list);
						insert_pos = ugc_zonefindpos((Eligzon *)  nthPtr,
								(ZoneInfo *) firstPtr);
					}
					else
					{
						insert_pos = 1 ;
					}

					ugc_zoneadd(nth, insert_pos);
				}

				insertItem = XmStringCopy(items[nth_item]);
				XmListAddItem(ugcsL, insertItem, insert_pos);
				XmStringFree(insertItem);
			}
		}      

		XtMapWidget(ugcsL);

		/**
		 * Commented out by guoxian zhou 08-2004
		 * This cleanup code caused memory fault.
		 */

		/*
    		Cleanup.
		*/
/*
		for (i = 0; i < pos_cnt; i++)
		{
			XmStringFree ( items[i] ) ;
		}
*/
		if (pos_cnt && pos_list)
		{
			free(pos_list);
			pos_list = NULL;
		}
	}

	return;
}


int	ugc_countyfindpos(Counties *nthPtr, CountyInfo *firstPtr)
{
   int	compare1 = -1,
      	compare2 = -1,
	pos = 1;
   
   
   if (nthPtr == NULL)
      return(-1); /* failure */

   
   while(firstPtr)
   {
      compare1 = strcmp(nthPtr->state,  firstPtr->state);
      compare2 = strcmp(nthPtr->county, firstPtr->county);
      
      if (compare1 < 0)
	 break;
      if ((compare1 == 0) && (compare2 < 0))
	 break;
      
      firstPtr = (void *) ListNext(&firstPtr->node);
      pos = pos + 1;
   }
   
   
   if(firstPtr == NULL)
      return(0); /* item goes at end of list */
   else
      return(pos);
}


int	ugc_zonefindpos(Eligzon *nthPtr, ZoneInfo *firstPtr)
{
   int	compare1 = -1,
      	compare2 = -1,
	pos = 1;
   
   
   if (nthPtr == NULL)
      return(-1); /* failure */

   
   while(firstPtr)
   {
      compare1 = strcmp(nthPtr->state,   firstPtr->state);
      compare2 = strcmp(nthPtr->zonenum, firstPtr->zonenum);
      
      if (compare1 < 0)
	 break;
      if ((compare1 == 0) && (compare2 < 0))
	 break;
      
      firstPtr = (void *) ListNext(&firstPtr->node);
      pos = pos + 1;
   }
   
   
   if(firstPtr == NULL)
      return(0); /* item goes at end of list */
   else
      return(pos);
}


/*
ugc_countyadd()

nth        == nth item from the available list
insert_pos == the computed position where to insert into selected list.

*/
void	ugc_countyadd(int nth, int insert_pos)
{
   Counties		*acntyPtr;
   CountyInfo		*scntyPtr,
      			*insertPtr;
   
   /*
   	Selected List is empty.
   */
   if (! ugc->cnty_sel)
   {
      ugc->cnty_sel = (CountyInfo *) malloc(sizeof(CountyInfo));
      ListInit(&ugc->cnty_sel->list);
      
      
      acntyPtr = (Counties *) ListNth(&ugc->cnty_avail->list, nth);
      scntyPtr = (CountyInfo *) ugc->cnty_sel;
      
      
      if (acntyPtr && scntyPtr)
      {
	 storeCountyInfo(scntyPtr, acntyPtr);
	 
	 ListAdd(&ugc->cnty_sel->list, &scntyPtr->node);
      }
   }
   
   
   /* 
   	Selected List is NOT empty.
   */
   else
   {
      acntyPtr = (Counties *) ListNth(&ugc->cnty_avail->list, nth);
      scntyPtr = (CountyInfo *) malloc(sizeof(CountyInfo));
      
      
      if (acntyPtr && scntyPtr)
      {
	 storeCountyInfo(scntyPtr, acntyPtr);
	 
	 
	 if (insert_pos == 0) /* means insert at end of list */
	 {
	    ListAdd(&ugc->cnty_sel->list, &scntyPtr->node);
	 }
	 else
	 {
	    insertPtr = (CountyInfo *) ListNth(&ugc->cnty_sel->list,
					       insert_pos);	 
	    ListInsert(&ugc->cnty_sel->list,
		       &insertPtr->node, &scntyPtr->node);
	 }
      }
   }
}


/*
ugc_zoneadd()

nth        == nth item from the available list
insert_pos == the computed position where to insert into selected list.

*/
void	ugc_zoneadd(int nth, int insert_pos)
{
   Eligzon		*azonePtr;
   ZoneInfo		*szonePtr,
      			*insertPtr;

   /*
   	Selected List is empty.
   */
   if (! ugc->zone_sel)
   {
      ugc->zone_sel = (ZoneInfo *) malloc(sizeof(ZoneInfo));
      ListInit(&ugc->zone_sel->list);
      
      
      azonePtr = (Eligzon *) ListNth(&ugc->zone_avail->list, nth);
      szonePtr = (ZoneInfo *) ugc->zone_sel;
      
      
      if (azonePtr && szonePtr)
      {   
	 storeZoneInfo(szonePtr, azonePtr);
	 
	 ListAdd(&ugc->zone_sel->list, &szonePtr->node);
      }
   }
   
   
   /* 
   	Selected List is NOT empty.
   */
   else
   {
      azonePtr = (Eligzon *) ListNth(&ugc->zone_avail->list, nth);
      szonePtr = (ZoneInfo *) malloc(sizeof(ZoneInfo));
      
      
      if (azonePtr && szonePtr)
      {
	 storeZoneInfo(szonePtr, azonePtr);
	 
	 
	 if (insert_pos == 0) /* means insert at end of list */
	 {
	    ListAdd(&ugc->zone_sel->list, &szonePtr->node);
	 }
	 else
	 {
	    insertPtr = (ZoneInfo *) ListNth(&ugc->zone_sel->list, insert_pos);	 
	    ListInsert(&ugc->zone_sel->list,
		       &insertPtr->node, &szonePtr->node);
	 }
      }
   }
}


void	storeCountyInfo(CountyInfo *dest, Counties *source)
{
   Node tempNode;
   List tempList; 
   
   tempNode = dest->node;
   tempList = dest->list;
   
   strcpy(dest->lid, ugc_lid);
   strcpy(dest->state, source->state);
   strcpy(dest->county, source->county);
/*
printf("ugc_lid <%s>, source->state <%s>, source->county <%s>\n",
       ugc_lid, source->state, source->county);
*/
   dest->node = tempNode;
   dest->list = tempList; 
   
   return;     
}


void	storeZoneInfo(ZoneInfo *dest, Eligzon *source)
{
     Node tempNode;
     List tempList; 
     
     tempNode = dest->node;
     tempList = dest->list;
     
     strcpy(dest->lid, ugc_lid);
     strcpy(dest->state, source->state);
     strcpy(dest->zonenum, source->zonenum);
/*
printf("ugc_lid <%s>, source->state <%s>, source->zonenum <%s>\n",
       ugc_lid, source->state, source->zonenum);
*/
     dest->node = tempNode;
     dest->list = tempList; 
     
     return;     
}


void	ugc_delete(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	om = (Widget) ptr;
   
   int		*pos_list = NULL;
   int		pos_cnt = 0;
   int		i;
   
   CountyInfo	*scntyPtr;
   ZoneInfo	*szonePtr;
   
   Node		*nodePtr;
   
   
   /*
   	Delete only the selected ones from Selected.
   */
   XtUnmapWidget(ugcsL);
   
   XmListGetSelectedPos(ugcsL, &pos_list, &pos_cnt);
   
   
   for (i=0; i<pos_cnt; i++)
   {
      /*
      		Logic for COUNTIES/ZONES:
      
      		Grab the Ptr to the data to be deleted from selected list.
      		If Ptr is not NULL,
      		Delete the specified node (data).
      		Free the associated memory.
      
      		(Below), Remove the XmList entry(ies).
      */
      if (GetMenuPos(om) == COUNTIES)
      {
	 scntyPtr = (CountyInfo *) ListNth(&ugc->cnty_sel->list,
					   pos_list[i] - i);
	 
/*
printf("DELETE: the (%i th) item in selected list...",
       pos_list[i] - i);
*/
	 if (scntyPtr)
	 {
	    ListDelete(&ugc->cnty_sel->list, &scntyPtr->node);
	    
	    
	    if (scntyPtr == ugc->cnty_sel) /* if deleting first node */
	    {
	       nodePtr = scntyPtr->node.next;
	       if (nodePtr) /* see if there is a next node */
	       {
		  /*
		  	"Promote" this next node to owner of the list.
		  */
		  ((CountyInfo *) nodePtr)->list = scntyPtr->list;
		  
		  ugc->cnty_sel = (CountyInfo *) nodePtr;
	       }
	       else
	       {
		  /*
		  	Cannot "promote" (set to NULL and free memory below).
		  */
		  ugc->cnty_sel = (CountyInfo *) NULL;
	       }
	    }
	    
	    
/*
printf("done.\n		Freeing associated data...");
*/	    
	    free(scntyPtr);
	    scntyPtr = NULL;
/*
printf("done.\n");
*/
	 }
      }
      else if (GetMenuPos(om) == ZONES)
      {
	 szonePtr = (ZoneInfo *) ListNth(&ugc->zone_sel->list,
					 pos_list[i] - i);
	 
/*
printf("DELETE: the (%i th) item in selected list...",
       pos_list[i] - i);
*/
	 if (szonePtr)
	 {
	    ListDelete(&ugc->zone_sel->list, &szonePtr->node);
	    
	    
	    if (szonePtr == ugc->zone_sel) /* if deleting first node */
	    {
	       nodePtr = szonePtr->node.next;
	       if (nodePtr) /* see if there is a next node */
	       {
		  /*
		  	"Promote" this next node to owner of the list.
		  */
		  ((ZoneInfo *) nodePtr)->list = szonePtr->list;
		  
		  ugc->zone_sel = (ZoneInfo *) nodePtr;
	       }
	       else
	       {
		  /*
		  	Cannot "promote" (set to NULL and free memory below).
		  */
		  ugc->zone_sel = (ZoneInfo *) NULL;
	       }
	    }
	    
/*
printf("done.\n		Freeing associated data...");
*/
	    free(szonePtr);
	    szonePtr = NULL;
/*	    
printf("done.\n");
*/
	 }
      }
   }   
   
   
   XmListDeletePositions(ugcsL, pos_list, pos_cnt);
   if (pos_cnt && pos_list)
   {
      free(pos_list);
      pos_list = NULL;
   }
   
   
   XtMapWidget(ugcsL);
   
   
   return;
}


void	ugc_clear(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	om = (Widget) ptr;
   int		pos;
   
   
   XmListDeleteAllItems(ugcsL);
   
   pos = GetMenuPos(om);
   
   if (pos == COUNTIES)
   {
      if (ugc->cnty_sel)
      {
	 FreeCountyInfo(ugc->cnty_sel);
	 ugc->cnty_sel = NULL;
      }
   }
   else if (pos == ZONES)
   {
      if (ugc->zone_sel)
      {
	 FreeZoneInfo(ugc->zone_sel);
	 ugc->zone_sel = NULL;
      }
   }
   
   return;
}


void	ugc_ok(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	om = (Widget) ptr;
   
   
   ugc_apply(w, (XtPointer) om, cbs);
   ugc_close(w, NULL, NULL);
   
   return;
}


void	ugc_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   Widget	om = (Widget) ptr;
   
   CountyInfo	*cntyPtr = NULL ;
   Countynum	cnty;
   
   ZoneInfo	*zonePtr = NULL ;
   Zonenum	zone;
   
   char		where[MAX_WHERE_LEN];
   
   int		status = 0 ;

   
   /*
   	Switch between zones and counties, based on 
   	the currently selected option.
   */
   sprintf(where, " WHERE lid = '%s' ", ugc_lid);
   if (GetMenuPos(om) == COUNTIES)
   {
      status = DeleteCountynum(where);
      
      if (status >= 0)
      {
         /* Check to make sure that cnty_sel is not NULL. */
         if ( ugc->cnty_sel != NULL )
         {
	    cntyPtr = (CountyInfo *) ListFirst(&ugc->cnty_sel->list);
         }

	 while(cntyPtr)
	 {
	    memset(&cnty, '\0', sizeof(cnty));
	    
	    strcpy(cnty.lid, ugc_lid);
	    strcpy(cnty.state, cntyPtr->state);
	    strcpy(cnty.county, cntyPtr->county);
/*
printf("ugc_lid <%s>, cntyPtr->state <%s>, cntyPtr->county <%s>\n",
ugc_lid, cntyPtr->state, cntyPtr->county);
*/	 
	    status = PutCountynum(&cnty);
	    if (status < 0)
	       break;
	    
	    cntyPtr = (CountyInfo *) ListNext(&cntyPtr->node);
	 }
      }
   }
   else if (GetMenuPos(om) == ZONES)
   {
      status = DeleteZonenum(where);
      
      if (status >= 0)
      {
         /* Check to make sure that zone_sel is not NULL. */
         if ( ugc->zone_sel != NULL )
         {
	    zonePtr = (ZoneInfo *) ListFirst(&ugc->zone_sel->list);
         }

	 while(zonePtr)
	 {
	    memset(&zone, '\0', sizeof(zone));
	    
	    strcpy(zone.lid, ugc_lid);
	    strcpy(zone.state, zonePtr->state);
	    strcpy(zone.zonenum, zonePtr->zonenum);
/*
printf("ugc_lid <%s>, zonePtr->state <%s>, zonePtr->zonenum <%s>\n",
ugc_lid, zonePtr->state, zonePtr->zonenum);
*/
	    status = PutZonenum(&zone);
	    if (status < 0)
	       break;
	    
	    zonePtr = (ZoneInfo *) ListNext(&zonePtr->node);
	 }
      }
   }
   

   if (status < 0)
   {
      ErrorDialog(ugcDS, DbErrorString(status));
   }
   
   
   return;
}


void	ugc_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(ugc->cnty_avail)
   {
      FreeCounties(ugc->cnty_avail);
      ugc->cnty_avail = NULL;
   }
   if(ugc->cnty_sel)
   {
      FreeCountyInfo(ugc->cnty_sel);
      ugc->cnty_sel = NULL;
   }
   if(ugc->zone_avail)
   {
      FreeEligzon(ugc->zone_avail);
      ugc->zone_avail = NULL;
   }
   if(ugc->zone_sel)
   {
      FreeZoneInfo(ugc->zone_sel);
      ugc->zone_sel = NULL;
   }
   
   if (XtIsManaged(ugcDS))
   {
      XtDestroyWidget(ugcDS);
      ugcDS = NULL;
   }
   
   return;
}


void	ugc_help(Widget w, XtPointer ptr, XtPointer cbs)
{
	return;
}


void	ugc_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
   /*
   	Clear out Available & Selected lists.
   */
   XmListDeleteAllItems(ugcaL);
   XmListDeleteAllItems(ugcsL);
   
   
   /*
   	Load the lists with data.
   */
   if(w == ugccntyPB)
   {
      ugc_load("counties", ugc_lid);
   }
   else if (w == ugczonePB)
   {
      ugc_load("eligzon", ugc_lid);
   }
   
   
   return;
}



void	ugc_load(const char *tablename, const char *lid)
{
   
   /*
   	Load info based on tablename.
   */
   if (strcmp(tablename,"counties") == 0)
   {
      /*
		Free zone_avail info.
      		Get cnty_avail info.
      */
      if (ugc->zone_avail)
      {
	 FreeEligzon(ugc->zone_avail);
	 ugc->zone_avail = NULL;
      }
      ugc->cnty_avail = ugc_getCountyAvail();	/* must be freed */
     
      
      /*
		Free zone_sel info.
      		Get cnty_sel info.
      */
      if (ugc->zone_sel)
      {
	 FreeZoneInfo(ugc->zone_sel);
	 ugc->zone_sel = NULL;
      }
      ugc->cnty_sel = ugc_getCountySel(lid);	/* must be freed */
   }
   
   
   else if(strcmp(tablename,"eligzon") == 0)
   {
      /*
      		Free cnty_avail info.
		Get zone_avail info.
      */
      if (ugc->cnty_avail)
      {
	 FreeCounties(ugc->cnty_avail);
	 ugc->cnty_avail = NULL;
      }
      ugc->zone_avail = ugc_getZoneAvail();	/* must be freed */

      
      /*
      		Free cnty_sel info.
		Get zone_sel info.
      */
      if (ugc->cnty_sel)
      {
	 FreeCountyInfo(ugc->cnty_sel);
	 ugc->cnty_sel = NULL;
      }
      ugc->zone_sel = ugc_getZoneSel(lid);	/* must be freed */
   }
}



Counties*	ugc_getCountyAvail(void)
{
   Counties		*cnty,
      			*cntyPtr;
   char			where[MAX_WHERE_LEN],
      			buf[MAX_BUF_LEN];
   XmStringTable	xmStr;
   int			cnt,
      			i;
   
   
   /*
   	Select the available counties for this HSA.
   */
   sprintf(where," where state != 'XX' ORDER BY state, county ");
   if ((cnty = GetCounties(where)) != (Counties *) NULL)
   {
      cnt   = ListCount(&cnty->list);
      xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
      
      
      /*
      		Iterate through the counties list
		and build the string table.
      */
      cntyPtr = (Counties *) ListFirst(&cnty->list);
      for (i = 0; cntyPtr; i++)
      {
	 sprintf(buf, "%3s   %-20s   %2s", 
		 cntyPtr->countynum, cntyPtr->county, cntyPtr->state);
	 
	 xmStr[i] = XmStringCreateSimple(buf);
	 cntyPtr  = (Counties *) ListNext(&cntyPtr->node);
      }
      
      /*
      		Load the XmList.
      */
      XmListAddItems(ugcaL, xmStr, cnt, 1);
      
      
      /*
      		Free allocated memory.
      */
      for (i = 0; i < cnt; i++)
	 XmStringFree(xmStr[i]);
      XtFree((char *) xmStr);
   }
   
   return(cnty);
}   


CountyInfo*	ugc_getCountySel(const char *lid)
{
   CountyInfo		*cnty,
      			*cntyPtr;
   char			where[MAX_WHERE_LEN],
      			buf[MAX_BUF_LEN];
   XmStringTable	xmStr;
   int			cnt,
      			i;
   
   /*
   	Construct the where clause, and load the
   	local pointer.
   */
   sprintf(where, " WHERE lid = '%s' ORDER BY state, county ", lid);
   if ((cnty = GetCountyInfo(where)) != NULL)
   {
      /*
      		Allocate local storage.
      */
      cnt   = ListCount(&cnty->list);
      xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
      
      
      /*
      		Iterate through the list.
      */
      cntyPtr = (CountyInfo *) ListFirst(&cnty->list);
      for (i = 0; cntyPtr; i++)
      {
	 sprintf(buf, "%3s   %-20s   %2s",
		 cntyPtr->countynum, cntyPtr->county, cntyPtr->state);
	 
	 xmStr[i] = XmStringCreateSimple(buf);
	 cntyPtr = (CountyInfo *) ListNext(&cntyPtr->node);
      }
      
      
      /*
      		Add elements to XmList.
      */
      XmListAddItems(ugcsL, xmStr, cnt, 1);
      
      
      /*
      		Free any allocated memory.
      */
      for (i = 0; i < cnt; i++)
	 XmStringFree(xmStr[i]);
      XtFree((char *) xmStr);
   }
   
   return(cnty);
}  


Eligzon*	ugc_getZoneAvail(void)
{
   Eligzon		*elig,
      			*eligPtr;
   char			where[MAX_WHERE_LEN],
      			buf[MAX_BUF_LEN];
   XmStringTable	xmStr;
   int			cnt,
      			i;
   
   
   /*
   	Select all eligible zone numbers for this HSA,
   	and iterate through the list.
   */
   cnt = 0;
   sprintf(where," where state != 'XX' ORDER BY state, zonenum ");
   if ((elig = GetEligzon(where)) != NULL)
   {
      /*
      		Allocate necessary space.
      */
      cnt = ListCount(&elig->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      
      /*
      		Iterate through the list of eligible
      		zones, and build the string table.
      */
      eligPtr = (Eligzon *) ListFirst(&elig->list);
      for (i = 0; eligPtr; i++)
      {
	 sprintf(buf, "%3s   %-20s   %2s", 
		 eligPtr->zonenum, eligPtr->descr, eligPtr->state);
	 
	 xmStr[i] = XmStringCreateSimple(buf);
	 eligPtr = (Eligzon *) ListNext(&eligPtr->node);
      }
      
      
      
      /*
      		Load the XmList.
      */
      XmListAddItems(ugcaL, xmStr, cnt, 1);
      
      
      /*
      		Free allocated memory.
      */
      for (i = 0; i < cnt; i++)
	 XmStringFree(xmStr[i]);
      XtFree((char *) xmStr);
   }
   
   return(elig);
}


ZoneInfo*	ugc_getZoneSel(const char *lid)
{
   ZoneInfo		*zone,
      			*zonePtr;
   char			where[MAX_WHERE_LEN],
      			buf[MAX_BUF_LEN];
   XmStringTable	xmStr;
   int			cnt,
      			i;
   
   
   /*
   	Construct the where clause, and load the 
   	local pointer.
   */
   sprintf(where, " WHERE lid = '%s' ORDER BY state, zonenum ", lid);
   if ((zone = GetZoneInfo(where)) != NULL)
   {
      /*
      		Allocate local storage.
      */
      cnt = ListCount(&zone->list);
      xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
      
      
      /*
      		Iterate through the list.
      */
      zonePtr = (ZoneInfo *) ListFirst(&zone->list);
      for (i = 0; zonePtr; i++)
      {
	 sprintf(buf, "%3s   %-20s   %2s",
		 zonePtr->zonenum, zonePtr->descr, zonePtr->state);
	 
	 xmStr[i] = XmStringCreateSimple(buf);
	 zonePtr = (ZoneInfo *) ListNext(&zonePtr->node);
      }
      
      
      /*
      		Add elements to XmList.
      */
      XmListAddItems(ugcsL, xmStr, cnt, 1);
      
      
      /*
      		Free any allocated memory.	
      */
      for (i = 0; i < cnt; i++)
	 XmStringFree(xmStr[i]);
      XtFree((char *) xmStr);
   }
   
   return(zone);
}
   

void	ugc_defact(Widget w, XtPointer ptr, XtPointer cbs)
{
	/*
		When double-clicking on an item, either:
		perform an Add operation (if in ugcaL list), or
		perform a Delete operation (if in ugcsL list).
	*/
	if (w == ugcaL)
	   ugc_add(w, ugcOM, NULL);
	
	if (w == ugcsL)
	   ugc_delete(w, ugcOM, NULL);
	
	
	return;
}


void	ugc_extsel(Widget w, XtPointer ptr, XtPointer cbs)
{
   XmListCallbackStruct *listcbs = (XmListCallbackStruct *) cbs;
   
   
   /*
   	Deselect other list's items
   	upon "entering" a new list.
   */
   if (listcbs->selection_type == XmINITIAL)
   {
      if (w == ugcaL)
	 XmListDeselectAllItems(ugcsL);
      
      if (w == ugcsL)
	 XmListDeselectAllItems(ugcaL);
   }
   
   
   return;
}



/****************************************************************************

show_serv_bkup()

Modifed by Jingtao Deng   02/25/2003
Note: Only filtered by wfo, remove filtered by primary and secondary backup
offices. Use table LocClass instead of Counties to get unique WFOs in function
load_serv_bkup_lists.

Modified by Bryon Lawrence 02/20/2004
Note: Changed the service backup filtering so that it is based on the hsa
column in the LocClass view as opposed to the wfo column.  This represents
a fundamental shift in how service backup support is done. It is now done
on HSA

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

#include "pointservice_backup.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_show.h"
#include "pointcontrol_options.h"
#include "LoadUnique.h"

/* variables global to this file */
static char responsible_hsa[MAXLEN_AREALIST]  = "";
static UniqueList * ulhsaHead       = NULL;

static Widget topShell = NULL;

/*****************************************************************************/

void show_serv_bkup(Widget w)
{
            
   /* create the dialog */
   
   if ( ! hbsbfilterDS)
   {
   	  topShell = GetTopShell(w);
      create_hbsbfilterDS(topShell);
      
      /* set up the callbacks */
                  
      XtAddCallback(hbsbfapplyPB,  XmNactivateCallback, serv_bkup_apply, NULL);
      XtAddCallback(hbsbfcancelPB, XmNactivateCallback, serv_bkup_close, NULL);
      
   } 
      
   /* load in the data for the three lists */
   load_serv_bkup_lists();            

   if (! XtIsManaged(hbsbfilterDS))
   {
      /*  Manage dialog shell and form  */
      XtManageChild(hbsbfilterFO);
      XtManageChild(hbsbfilterDS);
   }
   
   XRaiseWindow(XtDisplay(hbsbfilterDS), XtWindow(hbsbfilterDS));
   
   return;
}

/*****************************************************************************/

void load_serv_bkup_lists()
{
   Arg			arg[10];
   char *               pChar = NULL ;
   char                 hsa_id [ HYD_SERV_LEN + 1 ] ;
   int			count;
   int			ctr, ac;
   int *                pSelectFlags = NULL ;
   int                  hsa_list_length ;
   pc_options_struct * pc_options = get_pc_options();
   
   UniqueList		*ulPtr=NULL;
   XmStringTable	xmStr;
   
   /* Deselect all items in the WFO list. */
   XmListDeselectAllItems ( hbsbfhsaLI ) ;

   /* load in the data needed for the three lists */
   
   if ( ulhsaHead == NULL )
   {
      ulhsaHead = (UniqueList *) LoadUnique ( "hsa", "LocClass", " ", &count) ;
   }
   
   /* load in the list of WFOs */
   if (ulhsaHead != NULL)
   {
      hsa_list_length = strlen ( pc_options->hsa_list ) ;

      ulPtr = (UniqueList *) ListFirst(&ulhsaHead->list);
      count = ListCount(&ulhsaHead->list);

      pSelectFlags = ( int * ) malloc ( sizeof ( int ) * count ) ;

      if ( pSelectFlags == NULL )
      {
         fprintf  ( stderr , "\nIn routine 'load_serv_bkup_lists':\n"
                             "Cannot allocate %d bytes for array of %d\n"
                             "integers.\n" , count * sizeof ( int ) , 
                             count ) ;
         return ;
      }
      
      memset ( pSelectFlags , 0 , sizeof ( int ) * count ) ;
      xmStr = (XmStringTable) XtMalloc(count * sizeof(XmString *));
      
      for ( ctr = 0 ; ctr < count ; ++ ctr )
      {
         memset ( hsa_id , '\0' , HYD_SERV_LEN + 1 ) ;
         strncpy ( hsa_id , ulPtr->uchar , HYD_SERV_LEN ) ;

         if ( hsa_list_length != 0 )
         {
            pChar = strstr ( pc_options->hsa_list , hsa_id ) ;

            if ( pChar != NULL )
            {
               /* A match has been found. */
               pSelectFlags [ ctr ] = 1 ;
            }
               
         }

	     xmStr[ctr] = XmStringCreateSimple ( hsa_id ) ;
	     ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      }
      
      /* now, we load the list in the scrolled lists */
      
      ac=0;
      XtSetArg(arg[ac], XmNitemCount, count);ac++;
      XtSetArg(arg[ac], XmNitems, xmStr); ac++;
      XtSetValues(hbsbfhsaLI, arg, ac);

      /* Highlight any hsa ids that were in the pc_options hsa list. */
      for ( ctr = 0 ; ctr < count ; ++ ctr )
      {
          if ( pSelectFlags [ ctr ] == 1 )
          {
             XmListSelectPos ( hbsbfhsaLI , ctr + 1 , False ) ;
          }
      }

      if ( pSelectFlags != NULL )
      {
         free ( pSelectFlags ) ;
         pSelectFlags = NULL ;
      }

      /* need to free the memory */
      
      for (ctr=0; ctr < count; ctr++)
      {
	      XmStringFree(xmStr[ctr]);
      }
      XtFree((char *)xmStr);     
   }    
   
   return;   
}


/*****************************************************************************/

void serv_bkup_apply(Widget w, XtPointer ptr, XtPointer cbs)
{
   serv_bkup_save();
  
  // XtUnmanageChild(hbsbfilterDS);
      
   pc_drawMapIfNotDisabled(topShell, False);
       
   return;
}


/*****************************************************************************/

void serv_bkup_close(Widget w, XtPointer ptr, XtPointer cbs)
{  
   XtUnmanageChild(hbsbfilterDS);
     
   return;
}

/*****************************************************************************/

void serv_bkup_save()
{
   
   int		*poslist=NULL;
   int		count=0, ctr=0;
   UniqueList	*ulPtr=NULL;
   char		temp_dude[4] = "";
   char		one_hsa[8] = "";
   
   
   /* initialize */
   
   memset(temp_dude,        '\0', sizeof(temp_dude));
   memset(one_hsa,          '\0', sizeof(one_hsa));
   memset(responsible_hsa,  '\0', MAXLEN_AREALIST);   
   
   
   /* get the values for each sub-filter.  if sub-filter not applied,
      then leave the string blank */
   
   /* find out which WFOs where selected from the WFO scrolled list */
   XmListGetSelectedPos(hbsbfhsaLI, &poslist, &count);

   if (count > 0)
   {
      for (ctr = 0; ctr < count; ctr++)
      {
	      ulPtr = (UniqueList *) ListNth(&ulhsaHead->list, poslist[ctr]);
	      strncpy(temp_dude, ulPtr->uchar, HYD_SERV_LEN);

	      sprintf(one_hsa, "%s ",   temp_dude);

	      strcat(responsible_hsa, one_hsa);
      }
   }
   else
   {
      strcpy(responsible_hsa, ALL_AREAS);
   }
      
   set_serv_bkup_info ( responsible_hsa ) ;
   
   
   return;
}

void free_service_backup_list ( )
{
   UniqueList * pNode = NULL ;
   UniqueList * pTemp = NULL ;

   if ( ulhsaHead != NULL )
   {
      pNode = ( UniqueList * ) ListFirst ( & ulhsaHead->list ) ;

      while ( pNode != NULL )
      {
         pTemp = ( UniqueList * ) ListNext ( & pNode->node ) ;
         free ( pNode ) ;
         pNode = pTemp ;
      } 

      ulhsaHead = NULL ;
   }
}

#include "hv_mainCallbacks.h"
#include "map_library.h"
#include "map_resource.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_report.h"
#include "PointDisplayControl.h"
#include "select_show.h"

#define ACTUAL_ID_LEN 5

/* A pointer to the global linked list of ReportList structures owned 
   by the HvAreal library file PointDisplayControl.c */
extern ReportList * reportlistHead ;

static int numStationStrings = 0 ;
static RussText * stationString = NULL ;

int  is_select_shown()
{
   int status;
   
   
   status = False;
   if (XtIsManaged(selectDS))
      status = True;
   
   return (status);   
}   


/****************************************************************************/

void loadStationList ( ReportList * pReportListHead )
{
   int          reportListCount ;
   ReportList   * pReportListNode = NULL ;
   
   if ( pReportListHead == NULL )
   {
      /* There are no point data stations displayed on the Hydroview/MPE
         viewing area. */
      return ;
   }

   /* load the list of stations, but only list those
      that are currently displayed on the map */

   /* As of March 29, 2003, the functionality of this routine has
      been changed to directly use the ReportList linked list returned 
      from the Point Data Control utility. */
   reportListCount = ListCount ( & pReportListHead->list ) ; 

   if ( reportListCount == 0 )
   {
      return ;
   }

   if ( stationString != NULL )
   {
      free ( stationString ) ;
      stationString = NULL ;
   }

   numStationStrings = 0 ;

   stationString = ( RussText * ) malloc ( sizeof ( RussText ) * 
                                           reportListCount ) ;

   if ( stationString == NULL )
   {
      fprintf ( stderr , "\nIn routine 'loadStationList':\n"
                         "Could not allocate memory for the array\n"
                         "of station names to be displayed in the\n"
                         "station selection gui.\n" ) ;
      return ;
   }

   
   pReportListNode = ( ReportList * ) ListFirst ( & pReportListHead->list ) ;

   while ( pReportListNode != NULL )
   {

      if ( pReportListNode->use == 1 )
      {
         if ( pReportListNode->longitude <= -100.)
         {
	    sprintf ( stationString [ numStationStrings ] , 
                      "%s %s  [%5.1f %4.1f]" ,
		      pReportListNode->lid , pReportListNode->name ,
		      -1. * pReportListNode->longitude , 
                      pReportListNode->latitude ) ;
         }
	 else
         {
	    sprintf ( stationString [ numStationStrings ] , 
                      "%s %s  [%4.1f %4.1f]" ,
		      pReportListNode->lid , pReportListNode->name ,
		      -1. * pReportListNode->longitude , 
                      pReportListNode->latitude ) ;
         }

         numStationStrings ++;
      }

      pReportListNode = ( ReportList * ) ListNext ( & pReportListNode->node ) ;
   }
      
   XmListDeleteAllItems(selectLB);
   loadXmList100(selectLB, stationString, numStationStrings);
   
   /* Don't free stationString in this routine anymore. 
      Free it when the station selection gui is closed. */ 
   return;
}


/****************************************************************************/

void close_select(Widget w, XtPointer ptr, XtPointer cbs)
{
   XtUnmanageChild(selectDS);
   XtDestroyWidget(selectDS);
   selectDS = NULL;

   /* Free the stationString array if required. */
   if ( stationString != NULL )
   {
      free ( stationString ) ;
      stationString = NULL ;
   }

   numStationStrings = 0 ;
   
   return;
}


/****************************************************************************/


int get_list_position_binary(char *lid)
{
   /* get_list_position_binary() does a binary search on the 
      XmStrings  to find the correct list position which
      contains the matching lid. */
   
   int 	ac, i, rv;
   int	item_count;
   int	u_bound, l_bound, res;
   
   char 		station_lid[LOC_ID_LEN + 1];
   XmStringTable	xmStr;
   Arg             	arg[10]; 
      
   
   /* get the list items. Don't free them, these are the
      actual items, not copies. */
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, &item_count); ac++;
   XtSetArg(arg[ac], XmNitems, &xmStr); ac++;
   XtGetValues(selectLB, arg, ac);
   
      
   /* do a binary search on the list and select the right
      pos in the list */
   
   u_bound = item_count - 1;
   l_bound = 0;
   
   rv = 0;
   
   while (u_bound >= l_bound)
   {
      i = l_bound + (u_bound - l_bound)/2;
      
      GetStationLid(xmStr[i], station_lid);
      res = strcmp(lid, station_lid);
      
      if (res < 0) 		  /* too high */
	 u_bound = i-1;
      
      else if (res > 0) 	  /* too low */
	 l_bound = i+1;  
      
      else  /* found it */
      {
	 rv = i + 1;  /* list positions are numbered 1..n */	   
	 break;   
      }
   }   
   
   return (rv);
}


/****************************************************************************/

int get_list_position_linear(char *lid)
{
   /* get_list_position_linear() does a linear search on the 
      XmStrings  to find the correct list position which
      contains the matching lid. */
   
   int 	ac, i, rv;
   int	item_count, res, len;
   
   char 		station_lid[LOC_ID_LEN + 1];
   XmStringTable	xmStr;
   Arg             	arg[10]; 
   
   
   /* get the list items. Don't free them, these are the
      actual items, not copies. */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, &item_count); ac++;
   XtSetArg(arg[ac], XmNitems, &xmStr); ac++;
   XtGetValues(selectLB, arg, ac);
   
   
   rv = 0;	

   for (i=0; i < item_count; i++)
   {
      GetStationLid(xmStr[i], station_lid);
      
      len = strlen(lid);		   
      res = strncmp(lid, station_lid, len);
      
      if (res < 0)
      {
	 rv = 0;  
	 break;
      }   
      
      else if (res == 0)
      {
	 rv = i + 1;  /* list positions are numbered 1..n */	 
	 break;   
      }
   }  
   
   return (rv);
}


/****************************************************************************/
/* select_list_position gets the list position where lid is located. 
   then selects and sets it.  */

void select_list_position(char *lid)
{     
   int	pos;

   pos = get_list_position_binary(lid);	

   if ( pos )
   {
      /* highlight position pos */
      XmListSelectPos(selectLB, pos, False);
      
      /* set position count to top of the list */
      XmListSetPos(selectLB, pos);	    
   }
   
   return;
}


/****************************************************************************/
/* this function gets the location id (lid) from an XmString, by returning
   the first character string in the list item */

void GetStationLid (XmString xmStr, char * lid)
{   
   
   XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
   char	  *text, *s;
   
   
   /* parse the selected items to get the station.  If successful,
      show the Precip dialog, else show the error dialog. */
   
   s = NULL;
   memset(lid, '\0', LOC_ID_LEN + 1);
   
   
   /* parse the location id element from the
      passed XmString. */
   
   if (XmStringGetLtoR(xmStr, charset, &text))
   {
      s = strtok(text, " \t");
      strncpy(lid, s, LOC_ID_LEN);
      free ((char *) text);	
   } 
   
   return;	
}	


/****************************************************************************/

void clear_searchTxt(Widget w, XtPointer ptr, XtPointer cbs)
{
   XmTextSetString(select_searchTxt, ""); 
   
   return;
}   


/****************************************************************************/
/* lookup_lid finds, selects, and sets the list to the 
   position in which a matching lid is found.
   This was slightly modified from code in Hydrobase. */

void lookup_lid(Widget w, XtPointer ptr, XtPointer cbs)
{       
   char		buf[9];
   char	        *lid;    
   int		pos, i;
   XmTextVerifyCallbackStruct *cbs2 = (XmTextVerifyCallbackStruct *) cbs;
   
   
   /* null string entered. */
   
   if (cbs2->text->length == 0)
      return;
   
   
   for (i = 0; i < cbs2->text->length; i++)
   {
      /* verify input text is alphanumeric,
	 and convert to uppercase, if necessary. */
      
      
      if (! isAlphaNum(cbs2->text->ptr[i]))
      {
	 cbs2->doit = False;
	 break;
      }
      
      if (islower(cbs2->text->ptr[i]))
	 cbs2->text->ptr[i] = toupper(cbs2->text->ptr[i]);
   }
   
   
   /* build search pattern. */
   
   if ( ( lid = XmTextGetString(w) ) )
   {
      strcpy(buf, lid);
      XtFree((char *) lid);
      strcat(buf, cbs2->text->ptr);
   }
   
   
   /* search through the list and find the list position that
      matches the specified input string. */
   
   if ( (pos = get_list_position_linear(buf)) <= 0)
      cbs2->doit = False;
   
   else if (strlen(buf) >= ACTUAL_ID_LEN)
   {
      memset(buf, '\0', sizeof(buf));
      XmTextSetString(select_searchTxt, buf);
      cbs2->doit = False; /* don't add it to displayed search box */
   }   
   
   
   /* reset the currently selected position
   in the location list. */
   
   if (pos > 0)
   {   
      XmListSelectPos(selectLB, pos, True);
      XmListSetPos(selectLB, pos);
   }
   
   
   return;
}


/****************************************************************************/

void selectStationCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   char * station_id = NULL ;
   int * posList = NULL ;
   int numSelected ;
   int pos = -1 ;
   RussText station_copy ;
   Station * station = NULL ;

   /* Retrieve the selected position. */
   XmListGetSelectedPos ( selectLB , & posList , & numSelected ) ;

   /* Compare this to the array of station information used
      to create the items in the station selection list box. */
   if ( posList )
   {
      pos = posList [ 0 ] ;
      free ( posList ) ;
   }

   if ( pos != -1 )
   {
      /* Retrieve the name of the station which can then be searched
         for in the in the ReportList linked list. */
      strcpy ( station_copy , stationString [ pos - 1 ] ) ;
      station_id = ( char * ) strtok ( station_copy , " " ) ;
      station  = findDisplayedStation ( station_id ) ;
   }
   
   if ( station )
   {
      /* Select the station in the station selection box if it is open.
         Also, set the CurStation variable to indicate which station is
         currently selected. Indicate to the drawing routines that a 
         red rectangle needs to be drawn around  */ 
      selectStation ( station , False ) ;

      mUpdateMap ( 0 ) ;
   }
   
   return;   
}


/****************************************************************************/

void selectTimeSeriesCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
   Station * pCurrentStation = NULL ;
   TSGEN_INFO 	tsgen;
   
   pCurrentStation = get_current_station ( ) ;
      
   tsgen.standalone     = NON_STANDALONE;
   tsgen.pedtse_defined = 0;
   tsgen.nitems         = 1;

   if ( pCurrentStation != NULL )
   {
      strcpy ( tsgen.lid , pCurrentStation->lid ) ;
   }
   else
   {
      tsgen.group_check    = 0;
      strcpy ( tsgen.lid , " " ) ;
   }
   
   show_TSControlDS ( _get_top_level ( ) , tsgen ) ;
   
   return ;
}

/****************************************************************************/

void selectStation ( Station * station , Boolean selectListPos )
{
   /* If the selectDS is displayed, select the correct
      position, but don't activate callback, since it would
      cause an infinite loop. */
   if  ( ( selectDS ) && ( XtIsManaged ( selectDS ) ) && 
         ( selectListPos ) )
   {
      select_list_position ( station->lid ) ;
   }
  
   set_highlight_flag ( ) ;

   return ;
}

/****************************************************************************/

int findStationXmListPos ( Station * station )
{
   char * station_id = NULL ;
   int  pos = -1 ;
   int status ;
   long i = 0 ;
  
   /* NOTE: pos is from an XmList which is numbered 1..N
      but the stations array is numbered 0..M-1
      Since count is incremented BEFORE the equality check,
      it works out correctly. */
   station_id = station->lid ;

   for ( i = 0 ; i < numStationStrings ; ++ i )
   {
      status = strcmp ( stationString [ i ] , station_id ) ;

      if ( status == 0 )
      { 
         pos = i + 1 ;
         break;
      }
      else if ( status > 0 )
      {
         break ;
      }
   }

   return ( pos ) ;
}

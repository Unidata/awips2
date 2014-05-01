/*
	File:		ListRsrc.c
	Author:		Mark Glaudemans/Chip Gobs
	Date:		03/28/95, 1/5/98
	
	Purpose:	Implements convenience functions to get
			the values associated with a Motif XmList.
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include "Xtools.h"


/*
	Check the status of a list and return an array of ints that
	indicates for EACH item, whether the item is selected in the list.  
	In keeping with Xm usage, the list begins at count 1, not 0.
	The select_state array allocates memory that should be freed
	by the calling function when it is no longer needed.
	This function expects a list widget and a count; it returns
	a allocated array of items sized to count.    
*/

void CheckListStatus(Widget	widget, 
		     int 	cnt,
		     int 	**select_state)
{
	int *list;
	int listcnt;
	int i, numbytes;
	int actualcnt;
	
	if (!XtIsSubclass(widget, xmListWidgetClass))
	{
		printf("Invalid widget passed to CheckListStatus.\n");
		return;
	}
	
	actualcnt = ListRsrcGetCount(widget);	
	if (actualcnt != cnt) 
	{
		printf("Expected count != actual count in CheckListStatus.");
		return;
	}
	
	numbytes = (cnt + 1) * sizeof(int);  
	*select_state = (int *)malloc(numbytes);
	if (*select_state == NULL)
	{ 
		printf("Malloc in CheckListStatus failed...\n");
		return;
	}
	memset(*select_state, 0, numbytes);
	
	
	/* 
		loop on the number of selected items; set the items
		that are indeed selected
	*/
	
	XmListGetSelectedPos(widget, &list, &listcnt);
	if (listcnt > actualcnt)
	{
		printf("Number of selected items > actual number of items.");
		return;
	}
		       
	for (i = 0; i < listcnt; i++)
		(*select_state)[list[i]] = 1;     
	
	XtFree ( ( char * ) list ) ; 
	return;
}

/*
	Get the number of items in the list widget.
*/
int ListRsrcGetCount(Widget w)
{
   int ac;
   Arg	arg[1];
   int cnt;
   
   cnt = 0;
   
   if (XtIsSubclass(w, xmListWidgetClass))
   {
      ac = 0;
      XtSetArg(arg[ac], XmNitemCount, &cnt); ac++;
      XtGetValues(w, arg, ac);
   }
   else
   {
      fprintf(stderr, "Invalid widget passed to ListRsrcGetCount...\n");
   }
   
   
   return(cnt);
}


/*
	Returns a count of the seleted positions given any List Widget.
*/
int	ListRsrcGetSelectedCount(Widget w)
{
   int	*poslist;
   int	cnt;
   
   if (XtIsSubclass(w, xmListWidgetClass))
   {
      XmListGetSelectedPos(w, &poslist, &cnt);
      
      if (cnt > 0)
      {
	 XtFree ( ( char * ) poslist);
	 return(cnt);	/* num pos selected */
      }
   }
   else
   {
      fprintf(stderr, "Invalid widget passed to ListRsrcGetSelectedCount...\n");
   }
   
   
   return ( 0 ) ;	/* no positions are currently selected */
}

/**************************************************************************/

void	loadXmList(Widget xmList, LongText *items, long numItems)
{
     XmStringTable	xmStr;
     char		buf[BUFSIZ];
     int		i = 0;
     
     
     /*
     Clear form.
     */
     XmListDeleteAllItems(xmList);
     
     
     /*
     Create XmStringTable
     */
     xmStr = (XmStringTable) XtMalloc(numItems * sizeof(XmString *));		
     
     
     for (i = 0; i < numItems; i++)
     {
	  memset(buf, '\0', 100);
	  strcpy(buf, items[i]);
	  
	  xmStr[i] = XmStringCreateSimple(buf); 
     }
     
     
     /*
     Add all items to the XmList
     */ 
     XmListAddItems(xmList, xmStr, numItems, 1);
     
     
     /*
     Free the XmStringTable
     */
     for (i = 0; i < numItems; i++)
	  XmStringFree(xmStr[i]);
     XtFree ( ( char * ) xmStr ) ;
     
     return;
}


/**************************************************************************/

int ListRsrcGetFirstSelectedPos(Widget list)
{
     int *posList;
     int count;
     int pos = -1;
          
     XmListGetSelectedPos(list, &posList, &count);
     
     if (count > 0)
     {
          pos = posList[0];
          if (posList)
          {
             XtFree ( ( char * ) posList ) ;
          }
     }

     return pos;
}

/**************************************************************************/

void	loadXmList100(Widget xmList, RussText *items, long numItems)
{
     XmStringTable	xmStr;
     char		buf[100];
     int		i = 0;
     
     
     /*
     Clear form.
     */
     XmListDeleteAllItems(xmList);
     
     
     /*
     Create XmStringTable
     */
     xmStr = (XmStringTable) XtMalloc(numItems * sizeof(XmString *));		
     
     
     for (i = 0; i < numItems; i++)
     {
	  memset(buf, '\0', 100);
	  strcpy(buf, items[i]);
	  
	  xmStr[i] = XmStringCreateSimple(buf); 
     }
     
     
     /*
     Add all items to the XmList
     */ 
     XmListAddItems(xmList, xmStr, numItems, 1);
     
     
     /*
     Free the XmStringTable
     */
     for (i = 0; i < numItems; i++)
	  XmStringFree(xmStr[i]);
     XtFree ( ( char * ) xmStr ) ;
     
     return;
}

/**************************************************************************/

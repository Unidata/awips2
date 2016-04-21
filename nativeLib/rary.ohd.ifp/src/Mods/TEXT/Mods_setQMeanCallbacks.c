/***************************************************************/
/*								*/
/*	FILE:		Mods_setQMeanCallbacks.c		*/
/*								*/
/*	Callback functions for the setQMean mod window  	*/
/*	        						*/
/*								*/
/*	Coded by:	D. Page 				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		10/21/95                                */
/*	Modified by:    D. Page - 21 Nov. 95 - pass in     	*/
/*                      startDate to getLastListDate            */
/*                                                              */
/****************************************************************/

#include "Mods_globalDefs.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "ifp_atoms.h"

extern void getModStartDate(Mods_everythingStruct *, date *);
extern void getLastListDate(Mods_everythingStruct *, int, date *);
extern void createSelectionCB(Widget, Mods_everythingStruct *,XmPushButtonCallbackStruct *);
extern int Mod_text_parse(char *, int*, float*);
extern void change_the_hour(int, date*);
void setQMeanTextCB(Widget, Mods_everythingStruct *, XmAnyCallbackStruct *);

void setQMeanNextSelectionCB(Widget w, Mods_everythingStruct *data, 
                             XmPushButtonCallbackStruct *call_data)
{
   setQMeanTextCB(data->setQMeanWidgets->setQMeanText, data,
                  (XmAnyCallbackStruct *) call_data);
}


void setQMeanDoneSelectionCB(Widget w, Mods_everythingStruct *data, 
                             XmPushButtonCallbackStruct *call_data)
{
   int  num_items;
      
   /* If the values list widget is not empty, create the mod 
    * (no need to check both)
    */
   XtVaGetValues(data->setQMeanWidgets->setQMeanValuesList, 
                 XmNitemCount, &num_items, NULL);
   if(num_items > 0)
      createSelectionCB(w, data, NULL);
   
   XmToggleButtonSetState(data->widgetData->dataEntryToggle, FALSE, FALSE);
   XtUnmanageChild(data->setQMeanWidgets->setQMeanMsgBox);
   		
}


void setQMeanCancelSelectionCB(Widget w, Mods_everythingStruct *data, 
                               XmPushButtonCallbackStruct *call_data)
{
   XmListDeleteAllItems(data->setQMeanWidgets->setQMeanDatesList);
   XmListDeleteAllItems(data->setQMeanWidgets->setQMeanValuesList);
   
   XmToggleButtonSetState(data->widgetData->dataEntryToggle, FALSE, FALSE);
   XtUnmanageChild(data->setQMeanWidgets->setQMeanMsgBox);       
}


void setQMeanHelpSelectionCB(Widget w, Mods_everythingStruct *data, 
                             XmPushButtonCallbackStruct *call_data)
{
    printf("Sorry, no help available at this time\n");

}

void setQMeanTextCB(Widget textWidget, Mods_everythingStruct *data, 
                         XmAnyCallbackStruct *call_data)
{
 /*
  * Have gotten here because <cr> was entered in the 
  * SetQMean Mod text widget.
  *
  * Extract the text field from the textWidget and pass it
  *  to Mods_parse_text.
  */
 
 #include <Xm/Text.h>
 
 char  * text;
 int   status;
 int   num_vals, tot_num_vals;
 float values[200];
 int   i;
 
 text = XmTextGetString(textWidget);
 
 num_vals = 0;
 tot_num_vals =0;
 
 status = Mod_text_parse(text,
                         &num_vals,
                         values);
                         
/* Mod_text_parse output debug printout 
 * ====================================           
 * printf("in setQMeanTextCB, text parsing, status = %d, num_vals = %d\n",
 *               status, num_vals);
  
 
 if(num_vals > 0)
   {
    if(num_vals > 200)
      {
       printf("number of values being printed truncated to 200\n");
       num_vals = 200;
      }
    printf("values =");
    for (i = 0; i < num_vals; i++)
      {
       if (i > 0 && (i/10)*10 == i)
         printf("\n");
       printf(" %f", values[i]);
      }
    printf("\n");
   }
 * ======================================== 
 * End Mod_text_parse output debug printout */

 {
  char * make_date_string(date *);
  
  date nextDate;
  date *startDate;

  char        dateString[40];
  char        valueString[40];
  XmString  * xmDateString;
  XmString  * xmValueString;
  int         deltaT;
  int         num_items;

  xmDateString  = (XmString *)XtMalloc(sizeof(XmString));
  xmValueString = (XmString *)XtMalloc(sizeof(XmString));
  startDate = (date *)malloc(sizeof(date));
  
 /*
  *  Need to:
  *   5. could possibly replace calls to XmListAddItem for each list entry with
  *       a single call to XmListAddItems to add all items in each list at one
  *       time - probably not worth the effort.
  *   6. think about a prettier format that %f to display the flows - maybe
  *       tie number of decimal places to range of values (i.e., no decimal
  *       places for flows > 1000, say, and then some decimal places as
  *       flows decrease - again, maybe should just set to no decimal places
  *       and say "good enough" - the values will be stored with the precision
  *       entered, just not displayed that way
  */
  
   deltaT = *data->ModSettings->time_series.delta_t;
   
   XtVaGetValues(data->setQMeanWidgets->setQMeanValuesList, 
                 XmNitemCount, &num_items, NULL);
   if(num_items == 0)       
      getModStartDate(data, startDate);
   else
   {
      getLastListDate(data, num_items, startDate);
      change_the_hour(deltaT, startDate);
   }
   
    nextDate = *(startDate);
  
  
   for (i = 0; i < num_vals; i++)
   {
      strcpy(dateString, make_date_string(&nextDate));
      sprintf(valueString, "%.1f", values[i]);
  
      *xmDateString = XmStringCreate(dateString, XmSTRING_DEFAULT_CHARSET);
      *xmValueString = XmStringCreate(valueString, XmSTRING_DEFAULT_CHARSET);
     
      XmListAddItem(data->setQMeanWidgets->setQMeanDatesList, *xmDateString, (int) 0);
      XmListAddItem(data->setQMeanWidgets->setQMeanValuesList, *xmValueString, (int) 0);
     
      change_the_hour(deltaT, &nextDate);
   }
   
   /* Reset the visible item count for a list widget to the new number
    * of values.
    */
   tot_num_vals = num_items + num_vals;
   XtVaSetValues(data->setQMeanWidgets->setQMeanDatesList, 
                 XmNvisibleItemCount, tot_num_vals, NULL);

   XtFree ((char *)xmDateString);
   XtFree ((char *)xmValueString);
   free (startDate);
 }
 
   /* Reset the text string to blank */
   strcpy(text, "");
   XmTextSetString(textWidget, text); 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mods_setQMeanCallbacks.c,v $";
 static char rcs_id2[] = "$Id: Mods_setQMeanCallbacks.c,v 1.4 2006/04/18 15:28:23 aivo Exp $";}
/*  ===================================================  */

}

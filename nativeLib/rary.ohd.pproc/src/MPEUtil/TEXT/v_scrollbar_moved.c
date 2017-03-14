/*=========================================================================*/
/*                    FILE PATH/NAME:  STAGE3_SOURCE/v_scrollbar_moved.c   */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   v_scrollbar_moved()                */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "menus.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/v_scrollbar_moved.c                      */
/*  FUNCTION NAME:   v_scrollbar_moved()                                   */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   change_rc

Local variables:
   w - Widget structure;
   cb_data - deref cb_struct structure;
   call_data - deref XmScrollBarCallbackStruct structure;
   j - integer;
   n - integer; index (incrementor) for wargs array
   wargs - stack deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 10

******************************************** BEGIN v_scrollbar_moved *******/

void v_scrollbar_moved(w, cb_data, call_data)
   Widget                       w;
   cb_struct                   *cb_data;
   XmScrollBarCallbackStruct   *call_data;
{
   int                          n;
   Arg                          wargs[5];

 /*--------------------------------------------------------------*/
 /*     If a toTop or toBottom scrollbar event, move the         */
 /*     scrollbar to the top or bottom of the scroll area        */
 /*--------------------------------------------------------------*/

 if(call_data->reason == 7 || call_data->reason == 8)
    {
    n = 0;
    XtSetArg(wargs[n], XmNvalue, call_data->value); n++;
    XtSetValues(w, wargs, n);
    }

 cb_data->first_row_visible = call_data->value;

 /*--------------------------------------------------------------*/
 /*     Move data into 1st through maximum rows and cols of      */
 /*     rc widget                                                */
 /*--------------------------------------------------------------*/

   change_rc(cb_data);



}

/********************************************* END v_scrollbar_moved *******/

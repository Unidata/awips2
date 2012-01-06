/*=========================================================================*/
/*                    FILE PATH/NAME:  STAGE3_SOURCE/edit_gage.c           */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   edit_gage_value                    */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "menus.h"
#include "stage3.h"

/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/edit_gage.c                              */
/*  FUNCTION NAME:   edit_gage_value                                       */
/*       FUNCTION:   modify the gage value based on user input to the gage */
/*                    table edit column                                    */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) gage_table

Functions called:
   none

******************************************** BEGIN edit_gage_value *********/

void edit_gage_value(widget,cb_data, call_data)
   Widget               widget;
   cb_struct           *cb_data;
   XmAnyCallbackStruct *call_data;
{
   char                *new_value;
   int                  i, row = 0, num;

 /*--------------------------------------------------------------*/
 /*     find character input                                     */
 /*--------------------------------------------------------------*/

 new_value = (char *) XmTextGetString(widget);

 /*--------------------------------------------------------------*/
 /*     determine which gage is being modified                   */
 /*--------------------------------------------------------------*/

 for (i=0;i<cb_data->visible_rows;i++)
    if (widget == cb_data->text_w[cb_data->visible_columns-1][i])
       row = i;

 num = row - 1 + cb_data->first_row_visible;

 /*--------------------------------------------------------------*/
 /*     modify gage edit value                                   */
 /*--------------------------------------------------------------*/

 strcpy(gage[num].edit,new_value);



}

/********************************************* END edit_gage_value *********/

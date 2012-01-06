/*=========================================================================*/
/*                    FILE PATH/NAME:  STAGE3_SOURCE/change_rc.c           */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   change_rc()                        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "menus.h"
extern char *table_data();

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/change_rc.c                              */
/*  FUNCTION NAME:   change_rc()                                           */
/*       FUNCTION:   modify date displayed in gage table                   */
/***************************************************************************

Function type:
   void

Called by function:
   gage_table

Functions called:
   table_data

Local variables:
   cb_struct - structure cb_struct; structure of client data,
      passed by reference
   i - integer; visible column incrementor
   j - integer; visible row incrementor
 UNUSED col - integer
 UNUSED row - integer
   n - integer; index (incrementor) for wargs array
   text - dereferenced character; 14 characters of text; value
      assigned in function table_data(), which draws text from
      structure cb_struct based on row and column position
   wargs - stack-derefenced (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 15

******************************************** BEGIN change_rc ***************/

void change_rc(cb_data)
   cb_struct   *cb_data;
{
   int          i, j, n;
   char        *text;
   Arg          wargs[5];

 text = (char *)malloc(14*sizeof(char));

 /*-------------------------------------------------------------------------*/
 /*     get values for all columns except edit column                       */
 /*-------------------------------------------------------------------------*/

 for(i=0; i<= cb_data->visible_columns-1; i++)
    {
    if (i != cb_data->visible_columns-1)
       {
       for (j=0; j<cb_data->visible_rows-1; j++)
	  {
	  n = 0;
	  text=table_data(i,j+cb_data->first_row_visible);
	  XtSetArg(wargs[n], XmNlabelString,
	     XmStringCreate(text,XmSTRING_DEFAULT_CHARSET)); n++;
	  XtSetValues(cb_data->labels[i][j+1], wargs, n);
	  }
       }

 /*-------------------------------------------------------------------------*/
 /*     set data for edit column                                            */
 /*-------------------------------------------------------------------------*/

    else if (i == cb_data->visible_columns-1)
       {
       for (j=0; j<cb_data->visible_rows-1; j++)
	  {
	  n = 0;
	  text=table_data(i,j+cb_data->first_row_visible);
	  XmTextSetString(cb_data->text_w[i][j+1],text);
	  }
       }
    }



}

/********************************************* END change_rc ***************/

/*=========================================================================*/
/*               FILE PATH/NAME:   st3_src/post_table_data.c               */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   table_data()                       */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>

/*#include "postX.h"*/

#include "xs_create_menu_buttons.h"
#include "set_fields.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include <stdlib.h>
#include "post_stage3_interface.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/* FILE PATH/NAME:   st3_src/post_table_data.c                             */
/*  FUNCTION NAME:   table_data()                                          */
/*       FUNCTION:   transforms numbers to characters for display in       */
/*                     gage table                                          */
/*                   updates manedit flag in gage structure                */
/***************************************************************************

Function type:
   pointer to char

Called by function:
   create_gage_table

******************************************** BEGIN table_data **************/

char *table_data(i, j)
   int  i, j;
{
   char *value;
   float gv;

  value = (char *)malloc(10*sizeof(char));
  if (i==0)  strcpy(value, gage[j].id);
  /*else if (i==1) sprintf(value, "%d",   gage[j].qc);*/ //NEW
  else if (i==1) 
  {
   if(gage[j].gval == -9999.)
   {
     strcpy(value,"M");
   }
   else
   {
     gv = gage[j].gval/25.4;
     sprintf(value, "%.2f", gv);
   }
  }

  else if (i==2) 
  {
     gv = gage[j].ms/25.4;
     if(gage[j].ms == -9999.) gv = -9999.;
     sprintf(value, "%.2f", gv);
  }

  else if (i==3) 
  {
   gv = gage[j].mslow/25.4;
   if(gage[j].mslow == -9999.) gv = -9999.;
   sprintf(value, "%.2f", gv);
  }
  else if (i==4) 
  {
   gv = gage[j].mshi/25.4;
   if(gage[j].mshi == -9999.) gv = -9999.;
   sprintf(value, "%.2f", gv);
  }
  else if (i==5)
  {
    strcpy(value, gage[j].edit);
  }
  return value;


}

/********************************************* END table_data **************/

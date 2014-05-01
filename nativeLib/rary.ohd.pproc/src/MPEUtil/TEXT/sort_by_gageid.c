#include "stage3.h"
#include "stage3_interface.h"
#include "stage3_globals.h"
#include "menus.h"
#include "drawa.h"

/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/gage_table.c                             */
/*  FUNCTION NAME:   sort_by_gageid                                        */
/*       FUNCTION:   reorder the gage table alphabetically by gageid       */
/***************************************************************************

Function type:
   integer

Called by function:
   sort

******************************************** BEGIN sort_by_gageid **********/

int MPEUtil_sort_by_gageid(gage1, gage2)
   gage_struct *gage1, *gage2;
{
 return(strncmp(gage1->id,gage2->id,strlen(gage1->id)));

}

/********************************************* END sort_by_gageid **********/

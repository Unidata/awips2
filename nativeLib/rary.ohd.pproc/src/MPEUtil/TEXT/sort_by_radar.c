#include "stage3.h"
#include "stage3_interface.h"
#include "stage3_globals.h"
#include "menus.h"
#include "drawa.h"

/***************************************************************************/
/*  FUNCTION NAME:   sort_by_radar                                         */
/*       FUNCTION:   reorder the gage table alphabetically by radar        */
/***************************************************************************

Function type:
   integer

Called by function:
   sort

Functions called:
   none

******************************************** BEGIN sort_by_radar ***********/

int sort_by_radar(gage1,gage2)
   gage_struct *gage1,*gage2;
{
  
  if  ( (strcmp(gage1->rid , "M")) == 0  )
      return 1;
      
   else if ( (strcmp(gage2->rid , "M")) == 0 )
      return -1;
	      
  return(strncmp(gage1->rid,gage2->rid,strlen(gage1->rid)) );
}

/********************************************* END sort_by_radar ***********/


#include "drawa.h"
#include "menus.h"
#include "post_functions.h"
#include "stage3.h"
#include "stage3_interface.h"
#include "stage3_globals.h"




/********************************************************************************/                        
/*  FUNCTION NAME:   sort_by_gageval                                            */
/*       FUNCTION:   reorder the gage table with highest gage values at the top */
/********************************************************************************

Function type:
   integer

Called by function:
   sort

Functions called:
   none

******************************************** BEGIN sort_by_gageval *********/

int MPEUtil_sort_by_gageval(gage1, gage2)
   gage_struct *gage1, *gage2;
{
 if (gage1->gval > gage2->gval) return(-1);
 else if (gage1->gval < gage2->gval) return (1);
 return (MPEUtil_sort_by_gageid(gage1, gage2));
}

/********************************************* END sort_by_gageval *********/

/*===================================================================*/
/*                    FILE PATH/NAME:  st3_src/get_vip_levels.c      */
/*                                                                   */
/*  FUNCTIONS CONTAINED IN THIS FILE:   get_vip_levels()             */
/*===================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include "stage3.h"
#include "drawa.h"

/***************************************************************************/
/* FILE PATH/NAME:  st3_src/get_vip_levels.c                               */
/*  FUNCTION NAME:   get_vip_level()                                       */
/*       FUNCTION:   associate color level to pixel value                  */
/***************************************************************************

Function type:
   integer

Called by function:
   fill_pixmap

Functions called:
   none

Local variables:
   num_levels -  number of data levels
   levels -  value of each data level
   value -  value to determine which level
   i -  incrementor

******************************************** BEGIN get_vip_level ***********/

int get_vip_level(num_levels, levels, value)
   int  num_levels;
   int *levels;
   int  value;
{
   int  i;

 if(value < 0) return(0);
 if(value == 0) return(1);

 for(i=0; i<num_levels - 2; i++)
 {
    if(value < levels[i]) return(i+1);
 }

return(num_levels-1);



}

/********************************************* END get_vip_level ***********/

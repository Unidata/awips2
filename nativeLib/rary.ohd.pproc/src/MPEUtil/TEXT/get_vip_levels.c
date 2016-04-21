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

int get_vip_level(int num_levels, int *levels, int value)
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



int get_vip_level_allow_negative(int num_levels, int *levels, int value)
{
    int  i;


    for(i=1; i < num_levels - 1; i++)
    {
        if(value < levels[i])
        {
            return(i);
        }
    }
    //must belong in the final category, > final value
    return(num_levels-1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEUtil/RCS/get_vip_levels.c,v $";
 static char rcs_id2[] = "$Id: get_vip_levels.c,v 1.2 2012/05/10 15:35:55 cgobs Exp $";}
/*  ===================================================  */

}

/********************************************* END get_vip_level ***********/

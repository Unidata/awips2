/*******************************************************************************
* FILENAME:              SPE.h
* DESCRIPTION:           Provides the prototype of the tadj routine.
*                        It also contains definitions of variables used
*                        by the ffconv routine. Ffconv currently uses
*                        the upper_thres, mmfactor, and miss_value 
*                        constants.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         May 20, 2003
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   5/20/2003    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef SPE_H
#define SPE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*--------------------------------*/
/*  definition of     variables   */
/*--------------------------------*/

/*   variables used in function ffconv  */

#define lower_thres  201
#define upper_thres  253
#define mmfactor .33
#define miss_value -999
#define fill_value -998

/*  fill value for unsigned char    */

#define fill_value_uchar 0   

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

#define TADJ tadj_

void TADJ ( int * iyr , int * imo , int * ida , int * im , int * ihr , 
            int * is , int * tdiff , int * tunit ) ;

#endif /* #ifndef SPE_H */

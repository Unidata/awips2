/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getstartstr( int *sstr,float *strength)
/* return initial classification flag and value from AODT library
   Inputs : none
   Outputs: initial classification (start) flag and value
   Return : 0 : o.k.
*/
{

  *sstr=ostartstr_v64;      /* user defined initial classification flag */
  *strength=osstr_v64;      /* starting initial classification value */

  return 0;
}

int aodtv64_setstartstr(int sstr,float strength)
/* set initial classification flag and value from AODT library 
   Inputs : initial classification (start) flag and value
   Outputs: none 
   Return : 0 : o.k.
*/
{

  ostartstr_v64=sstr;       /* user defined initial classification flag */
  osstr_v64=strength;       /* starting initial classification value */

  return 0;
}

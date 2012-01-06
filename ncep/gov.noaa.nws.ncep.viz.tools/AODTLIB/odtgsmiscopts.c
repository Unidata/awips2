/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getmiscoptions(int *land,int *search)
/* return various AODT library variables from AODT library
   Inputs : none
   Outputs: various AODT library variables
   Return : 0 : o.k.
*/
{

  *land=oland_v64;          /* allow AODT operation over land */
  *search=osearch_v64;      /* search for maximum curved band position */

  return 0;
}

int aodtv64_setmiscoptions(int land,int search)
/* set various AODT library variables within AODT library memory
   Inputs : various AODT library variables
   Outputs: none 
   Return : 0 : o.k.
*/
{

  oland_v64=land;           /* allow AODT operation over land */
  osearch_v64=search;       /* search for maximum curved band position */

  return 0;
}

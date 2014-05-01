/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getversion( char *version  )  
/* returns string variable containing the current AODT Version number 
   Inputs : none
   Outputs: AODT version number string
   Return : 0 : o.k.
*/
{
  strcpy(version,"AODT-Version 6.4.3");  
  version[strlen(version)]='\0';  
  return 0;
}

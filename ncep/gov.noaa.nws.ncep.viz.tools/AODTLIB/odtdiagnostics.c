/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_qdiagnostics(char *message) 
/* Subroutine to gather up running diagnostic messages during AODT runtime
   for output within API
   Inputs : none
   Outputs: all non-fatal diagnostic messages 
   Return : 0 : o.k.
*/
{
  strcpy(message,diagnostics_v64); 
  message[strlen(diagnostics_v64)]='\0';

  return 0;
}

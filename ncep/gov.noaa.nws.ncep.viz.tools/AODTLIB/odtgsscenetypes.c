/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getscenetypes(int *neyescene,int *ncloudscene,int *oeyescene, int *ocloudscene )
/* Routine to obtain cloud and eye region scene types and return values to API.
   Inputs : none
   Outputs: current (new) and original (if overridden) eye and cloud scenes
   Return : 0 : o.k.
*/
{

  *neyescene=odtcurrent_v64->IR.eyescene;            /* current/new eye scene */
  *ncloudscene=odtcurrent_v64->IR.cloudscene;        /* current/new cloud scene */
  *oeyescene=odtcurrent_v64->IR.eyesceneold;         /* original (if override) eye scene */
  *ocloudscene=odtcurrent_v64->IR.cloudsceneold;     /* original (if override) cloud scene */
  return 0;
}

int aodtv64_setscenetypes(int neyescene,int ncloudscene,int oeyescene, int ocloudscene )
/* Routine to reset cloud and eye scene types and store within AODT library.
   Inputs : current (new) and original (if overridden) eye and cloud scenes
   Outputs: none
   Return : 0 : o.k.
*/
{

  odtcurrent_v64->IR.eyescene=neyescene;            /* current/new eye scene */
  odtcurrent_v64->IR.cloudscene=ncloudscene;        /* current/new cloud scene */
  odtcurrent_v64->IR.eyesceneold=oeyescene;         /* original (if override) eye scene */
  odtcurrent_v64->IR.cloudsceneold=ocloudscene;     /* original (if override) cloud scene */
  return 0;
}

/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

void aodtv64_freememory(void)
/* free any stored areas of memory within AODT library
   Inputs : none
   Outputs: none
*/
{
  struct odtdata *ptr;			/* added by CDB */
/*---------------------------------------------------------------*/

  if(odthistoryfirst_v64 != (struct odtdata *)NULL) {
    do {
      ptr=odthistoryfirst_v64->nextrec;	/* added by CDB */
      free(odthistoryfirst_v64);
      odthistoryfirst_v64=ptr;		/* added by CDB */
    } while (odthistoryfirst_v64 != (struct odtdata *)NULL);	/* added by CDB */
  }

  /* if(odtcurrent_v64 != (struct odtdata *)NULL) { */
  if(odtcurrent_v64 != NULL) {
    free(odtcurrent_v64);
    odtcurrent_v64=NULL;		/* added by CDB */
  }

  if(tcircfirst_v64 != (struct ringdata *)NULL) {
      free(tcircfirst_v64);
      tcircfirst_v64=NULL;
  }

  if(areadata_v64 != (struct datagrid *)NULL) {
    free(areadata_v64);
    areadata_v64=NULL;			/* added by CDB */
  }

  if(diagnostics_v64 != (char *)NULL) {
    free(diagnostics_v64);
    diagnostics_v64=NULL;
  }

  if(hfile_v64 != (char *)NULL) {
    free(hfile_v64);
    hfile_v64=NULL;
  }

  if(fixfile_v64 != (char *)NULL) {
    free(fixfile_v64);
    fixfile_v64=NULL;
  }

  if(atcftype_v64 != (char *)NULL) {
    free(atcftype_v64);
    atcftype_v64=NULL;
  }

  return;

}

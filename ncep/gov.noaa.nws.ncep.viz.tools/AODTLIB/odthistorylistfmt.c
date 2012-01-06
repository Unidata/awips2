/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int aodtv64_historylistfmt(struct odtdata *historyrec,int itype,char *srcID,char *listing) 
/* Subroutine to load AODT history file record listing into a character string
   for output within API.
   Passing in value of zero for historyrec will return listing column 
   header definitions.
   Inputs : pointer to history structure entry 
            listing format type (original or ATCF format)
   Outputs: intensity estimate listing for history structure entry
   Return : 0 : o.k.
*/
{
  char *retstrng; 

  retstrng=(char *)calloc((size_t)50000,sizeof(char));
  aodtv64_listhistory(historyrec,itype,srcID,retstrng);
printf("NATIVE--aodtv64_historylistfmt....retstrng = %s\n",retstrng);
  strcpy(listing,retstrng); 
  listing[strlen(retstrng)]='\0';

  free(retstrng);
  return 0;
}

/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_readhistoryfile(void);

char*  aodtv64_gethistoryfile2()
/* return history file name to application from AODT library
   Inputs : none
   Outputs: AODT library history file name
   Return : -1 : error reading history file
             0 : o.k.
*/
{
  //int iret;
  /* check for valid history file name */
  if(strlen(hfile_v64)>0) {
    /* set history file name to return variable */
    /* memcpy(outchar,hfile_v64,strlen(hfile_v64));  */
    //strcpy(outchar,hfile_v64);
    //outchar[strlen(hfile_v64)]='\0';  /* added by CDB */
    //iret=0;

    return hfile_v64;
  } else {
    //iret=-1;
    return '\0';
  }

  //return iret;
}

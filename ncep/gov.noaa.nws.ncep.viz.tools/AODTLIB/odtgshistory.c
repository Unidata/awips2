/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_readhistoryfile(void);

int aodtv64_gethistoryfile( char *outchar )
/* return history file name to application from AODT library
   Inputs : none
   Outputs: AODT library history file name
   Return : -1 : error reading history file
             0 : o.k.
*/
{
  int iret;
  /* check for valid history file name */
  if(strlen(hfile_v64)>0) {
    /* set history file name to return variable */
    /* memcpy(outchar,hfile_v64,strlen(hfile_v64));  */
    strcpy(outchar,hfile_v64);
    outchar[strlen(hfile_v64)]='\0';  /* added by CDB */
    iret=0;
  } else {
    iret=-1;
  }

  return iret;
}

int aodtv64_sethistoryfile( char *inchar ) 
/* set history file name in AODT library and read history file into AODT library memory
   Inputs : history file name
   Outputs: none
   Return : -1 : error reading history file
	     11: successful read of history file
*/
{
  int iok,iret;

  /* assign history file name to AODT library variable */
//printf("NATIVE...000000000000000\n");
  strcpy(hfile_v64,inchar); 
//printf("NATIVE1111.aodtv64_sethistoryfile...hfile_v64=%s\n", hfile_v64);

  hfile_v64[strlen(inchar)]='\0';  /* added by CDB */
//printf("NATIVE222222222...aodtv64_sethistoryfile...hfile_v64=%s\n", hfile_v64);
  
  /* read history file into AODT memory 
     function will return negative for error, or
     positive value for number of successful records read */
  iok=aodtv64_readhistoryfile();

  /* set return error code */
  iret=iok;
  if(iok>=0) iret=11;

  return iret;
}

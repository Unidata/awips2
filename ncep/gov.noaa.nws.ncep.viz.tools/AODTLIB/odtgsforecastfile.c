/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_getforecastfile( char *outchar,int *type,char *atcf ) 
/* return AODT library automode forecast file name
   Inputs : none
   Outputs: AODT library forecast file name
            file type : 0-ATCF
	                1-NHC forecast discussion
			2-JTWC forecast
			3-generic
            ATCF file type
   Return : -51 : error access forecast file 
             14 : o.k.
*/
{
  int iret;

  /* check for valid forecast file name */
  if(strlen(fixfile_v64)>0) {
    /* set forecast file name to return variable */
    strcpy(outchar,fixfile_v64); 
    outchar[strlen(fixfile_v64)]='\0';
    iret=14;
  } else {
    iret=-51;
  }
  *type=ifixtype_v64;
  if(*type==0) {
    strcpy(atcf,atcftype_v64); 
    atcf[strlen(atcftype_v64)]='\0'; 
  }

  return iret;
}

int aodtv64_setforecastfile( char *inchar,int type,char *atcf ) 
/* set forecast file name and type within AODT library memory
   Inputs : AODT library forecast file name
            file type : 0-ATCF
	                1-NHC forecast discussion
			2-JTWC forecast
			3-generic
            ATCF file type
   Outputs: none
   Return : 0 : o.k.
*/
{
  /* assign forecast file name to AODT library variable */
  strcpy(fixfile_v64,inchar); 
  fixfile_v64[strlen(inchar)]='\0'; 
  ifixtype_v64=type;
  if(ifixtype_v64==0) {
    strcpy(atcftype_v64,atcf); 
    atcftype_v64[strlen(atcf)]='\0';  
  }
  return 0;
}

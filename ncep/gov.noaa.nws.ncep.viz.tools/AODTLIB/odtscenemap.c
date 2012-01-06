/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv64_scenemap( int eyescene,int cloudscene, char *retstring ) 
/* returns string for requested scene type 
   Inputs : eye and cloud scene integer values.
            if one scene is entered as -1, the absolute value for the 
	    other scene type will be output, otherwise the scene type
	    for the combined scene type set will be returned.
   Outputs: scene type string
   Return : 0 : o.k.
*/
{
  if((eyescene==-1)&&(cloudscene==-1)) {
    retstring=NULL;
    return 0;
  }
  if(eyescene==-1) {
    strcpy(retstring,cloudtype_v64[cloudscene]);  
    retstring[strlen(cloudtype_v64[cloudscene])]='\0';
  } else if(cloudscene==-1) {
    strcpy(retstring,eyetype_v64[eyescene]); 
    retstring[strlen(eyetype_v64[eyescene])]='\0';
  } else {
    if((eyescene<6)&&(cloudscene<2)) {
      /* eye scene */
      strcpy(retstring,eyetype_v64[eyescene]);  
      retstring[strlen(eyetype_v64[eyescene])]='\0';
    } else {
      /* cloud scene */
      strcpy(retstring,cloudtype_v64[cloudscene]);  
      retstring[strlen(cloudtype_v64[cloudscene])]='\0';
    }
  }
    
  return 0;
}

#include "geminc.h"
#include "gemprm.h"

void gdc_gcyc ( char *gdfile, char  *cycles, int *iret )
/************************************************************************
C* Input parameters:                                                    *
C*      GDFILE          CHAR*           Grid file name                  *
C*                                                                      *
C* Output parameters:                                                   *
C*      CYCLES          CHAR*           List of GEMPAK times            *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
 ************************************************************************/
{
   int nn,ret=0;
   char tmp[1024];
/*----------------------------------------------------------------------*/
   *iret = 0;
   gd_gcyc(gdfile, ";", &nn, tmp, &ret,strlen(gdfile),1,sizeof(tmp));
   if ( ret == 0 ) {
       strcpy ( cycles, tmp );
   }
   else {
       *iret = ret;
   }
   return;
}

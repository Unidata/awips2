#include "geminc.h"
#include "gemprm.h"

void gdc_open ( char *gdfile, int *wrtflg, int *mxanl, int *mxnav,
                int  *iacss,  float *anl, float *rnav, 
                int  *msxgrd, int *iret )
/************************************************************************
C* Input parameters:                                                    *
C*      GDFILE          CHAR*           Grid file name                  *
C*      WRTFLG          LOGICAL         Write access flag               *
C*      MXANL           INTEGER         Number of analysis block return *
C*      MXNAV           INTEGER         NUmber of nav block return      *
C*                                                                      *
C* Output parameters:                                                   *
C*      IACSS           INTEGER         Access number to do I/O on file *
C*      BKANL (MXANL)   REAL            Analysis block data array       *
C*      BKNAV (MXNAV)   REAL            Navigation block data array     *       
C*      MXGRD           INTEGER         Maximum number of grids         *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -14 = file name is blank        *
C*                                      -17 = file number limit reached *
 ************************************************************************/
{
   int wrtflg2;
/*----------------------------------------------------------------------*/
    if ( *wrtflg == 0 ) {
       wrtflg2 = G_TRUE;
    }
    else {
       wrtflg2 = G_FALSE;
    }
    wrtflg2 = G_FALSE;
    gd_open ( gdfile, &wrtflg2, mxanl, mxnav, iacss, anl, rnav, msxgrd,
              iret, strlen(gdfile) );
    return;
}

#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_rdtr ( char *queryType, char *source, char *part, char *dattim,  
               char *stid, char *dataUri, int *icnt, float *rdata, int *nword, 
               int *iret )
/************************************************************************
 *									*
 * db_dataquery               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
/*---------------------------------------------------------------------*/
/*
    int      jj;
    if ( clbkPtr != NULL && dataUri != NULL ) {
       clbkPtr(dataUri);
       if ( rdataBackSize > 0 && rdataBack != NULL ) {
          for( jj=0; jj < rdataBackSize; jj++ ) {
             rdata[jj] = rdataBack[jj];
          }
          *iret = 0;
          *nword = rdataBackSize;
          G_FREE ( rdataBack, float );
       }
       else {
          *iret = -1;
          *nword = 0;
       }
    }
    else {
       *iret = -1;
       *nword = 0;
    }
*/
    return;
}

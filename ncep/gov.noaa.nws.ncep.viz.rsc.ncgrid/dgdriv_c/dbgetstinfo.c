#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_getstinfo ( char *queryType, char *gemArea,
                  char *stInfo, int *lStInfo, int *iret )
/************************************************************************
 *									*
 * db_getstinfo               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      bufferSize = 200000;
    char     xsltFile[LLSCRN] = "response.xlt";
    char     xsltDir[LLSCRN]  = "$NAWIPS/gempak/tables/xslt";
    int      ier, nbytes, ier1;
    long     flen;
    char     queryText[320], queryResult[bufferSize+1], xsltfile[FILE_FULLSZ];
    unsigned char*   bigStr;
/*---------------------------------------------------------------------*/

   /*
    * Initialization
    */
    *iret = 0;
    stInfo[0] = '\0';
    queryText[0] = '\0';
    queryResult[0] = '\0';
    *lStInfo = 0;

   /*
    * Populate the query strings 
    */

   if (strcmp ( queryType, "stidqry" ) == 0 || 
        strcmp ( queryType, "stnmqry" ) == 0 ) {
          sprintf (ePlugIn,   "%s", "obs");
          sprintf (eLibClass, "%s", "GempakAreaRequest");
          strcpy (gArea, gemArea);
   }
   else {
      ier = -8;
      er_wmsg ( "DB", &ier, queryType, &ier1, 2, strlen(queryType) );
      *iret = -1;
      return;
  }

   /*
    * Get the query text
    */
    db_getQueryText ( queryType, queryText, &ier);
    if ( ier !=0 ) {
       ier = -3;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }

   /*
    * Connect to database and get the query result
    */
    db_runQuery ( queryText, queryResult, &ier);
    if ( ier !=0 ) {
       ier = -4;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }

   /*
    * Transform XML string to a string containing list of header parameters
    */
    cfl_inqr(xsltFile, xsltDir, &flen, xsltfile, &ier);
    if ( ier !=0 ) {
      /* 
       * XSLT file not found -> returning
       */
       ier = -5;
       er_wmsg ( "DB", &ier, xsltFile, &ier1, 2, strlen(xsltFile) );
       *iret = -1;
       return;
    }
   
    nbytes=xml_transform( queryResult, strlen(queryResult), xsltfile, &bigStr, &ier );
    if ( ier !=0 || nbytes==0) {
      /* 
       * XML Transform run unsuccessfully -> returning
       */
       ier = -6;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       G_FREE( bigStr, unsigned char );
       *iret = -1;
       return;
    }   
    
    sprintf (stInfo, "%s", bigStr);
    *lStInfo = strlen(stInfo);

    G_FREE( bigStr, unsigned char );
    return;

}

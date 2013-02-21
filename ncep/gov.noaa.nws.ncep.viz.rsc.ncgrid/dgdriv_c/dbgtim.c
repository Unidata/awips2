#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_gtim ( char *queryType, char *source, char *qparms, 
               char *times, int *ltimes, int *iret )
/************************************************************************
 *									*
 * db_gtim               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      bufferSize = 200000;
    char     xsltFile[LLSCRN] = "response.xlt";
    char     xsltDir[LLSCRN]  = "$NAWIPS/gempak/tables/xslt";
    //char     queryType[11]    = "dbaseTime";
    int      ier, nbytes, ier1, ier2, iparm = 129, ierEvt;
    long     flen;
    char     queryText[320], queryResult[bufferSize+1], evtName[iparm-1];
    char     errStr[100], xsltfile[FILE_FULLSZ], parm[iparm-1];
    unsigned char*   bigStr;
    struct timeb t_start, t_query, t_run, t_xml, t_current;
/*---------------------------------------------------------------------*/
    ftime(&t_start);

   /*
    * Initialization
    */
    *iret = 0;
    times[0] = '\0';
    queryText[0] = '\0';
    queryResult[0] = '\0';

    db_getparm ( parm, &ier2,  iparm);
    if ( ier2 != 0 ) {
      *iret = -1;
      return;
    }
    db_getevtname ( evtName, &ierEvt,  iparm);

   /*
    * Populate the query strings 
    */
    if (strcmp ( queryType, "dbTime" ) == 0 ) {
       strcpy (eSrc, source);
       if ( strcmp ( source, "GRID") == 0 ) {
         sprintf (ePlugIn,   "%s", parm);
         sprintf (eGrid,   "%s", qparms);
         sprintf (eLibClass, "%s", "GempakCatalogTimeQuery");
         if ( ierEvt == 0 ) {
            sprintf (gEventName, "%s", evtName);
         }
       }
       else if ( strcmp ( source, "METAR") == 0 ) {
          sprintf (ePlugIn,   "%s", "obs");
          sprintf (eLibClass, "%s", "NomTimeQuery");
       }
/*
       else if ( strcmp ( source, "BUFRUA") == 0 ) {
          sprintf (ePlugIn,   "%s", "bufrua");
          sprintf (eLibClass, "%s", "GempakCatalogTimeQuery");
       }
       else if ( strcmp ( source, "SYNOP") == 0 ) {
          sprintf (ePlugIn,   "%s", "sfcobs");
          sprintf (eLibClass, "%s", "GempakCatalogTimeQuery");
       }
*/
       else {
         ier = -9;
         sprintf (errStr, "%s+", queryType);
         strcat  (errStr, source);
         er_wmsg ( "DB", &ier, errStr, &ier1, 2, strlen(errStr) );
         *iret = -1;
         return;
       }
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
    ftime(&t_query);
    db_getQueryText ( queryType, queryText, &ier);
    ftime(&t_current);
   // printf("\t\t\t time spent in db_getQueryText: %d\n", (int) (1000.0 * (t_current.time - t_query.time) + (t_current.millitm - t_query.millitm)));

    if ( ier !=0 ) {
      /*
       * Query text not set -> returning
       */
       ier = -3;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }

   /*
    * Connect to database and get the query result
    */
    ftime(&t_run);
    db_runQuery ( queryText, queryResult, &ier);
    ftime(&t_current);
   // printf("\t\t\t time spent in db_runQuery: %d\n", (int) (1000.0 * (t_current.time - t_run.time) + (t_current.millitm - t_run.millitm)));

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

    ftime(&t_xml);
    nbytes=xml_transform( queryResult, strlen(queryResult), xsltfile, &bigStr, &ier );
    ftime(&t_current);
   // printf("\t\t\t time spent in xml_transform: %d\n", (int) (1000.0 * (t_current.time - t_xml.time) + (t_current.millitm - t_xml.millitm)));

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

    sprintf (times, "%s", bigStr);

    *ltimes = strlen(times);

    G_FREE( bigStr, unsigned char );
    ftime(&t_current);
   // printf("\t\t time spent in db_gtim: %d\n", (int) (1000.0 * (t_current.time - t_start.time) + (t_current.millitm - t_start.millitm)));
    return;

}

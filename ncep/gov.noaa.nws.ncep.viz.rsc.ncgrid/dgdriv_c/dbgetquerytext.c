#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void db_getQueryText ( char *queryType, char *queryText, int *iret ) 
/************************************************************************
 *									*
 * db_getQueryText            						*
 *									*
 * This function generates a string containing microEngine user script  * 
 * based on the query type and strings defined in the dncmn.h header.	*
 *									*
 * Currently supported query types:					*
 *									*
 *	flName								* 
 *									*
 * void db_getQueryText ( char *queryType, char *queryText, int *iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *queryType	char		type of the query		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *queryText	char		text of the query		*
 *	*iret		int		return code:			*
 *                                      -1: query type is incorrect     *
 *					 0: normal                      *
 **                                                                     *
 * Log:                                                                 *
 *									*
 * m.gamazaychikov/CWS	01/10	Created                                 *
 * m.gamazaychikov/CWS  11/10   Removed code intented for NMAP bridge   *
 ************************************************************************/
{
    char columnName[25];
    int  ier;
/*---------------------------------------------------------------------*/

    *iret = 0;
 
   /* 
    * Construct the import statement
    */
    
    //sprintf (queryText, "%s", "import ");
    sprintf (queryText, "%s", "import time; t1 = time.time();");
    strcat  (queryText, "import ");
    strcat  (queryText,  eLibClass);
    strcat  (queryText, ";");
    strcat  (queryText, "query =");
    strcat  (queryText,  eLibClass);
    strcat  (queryText, ".");
    strcat  (queryText,  eLibClass);
    strcat  (queryText, "(");
    strcat  (queryText, "\"");
    strcat  (queryText,  ePlugIn);
    strcat  (queryText, "\"");
    strcat  (queryText, ");");

   /* 
    * Construct the body of the query based on the query type
    *
    * 1. file name query (flName):
    * eLibClass = GempakGridCycleQuery
    */
    if ( strcmp ( queryType, "flName" ) == 0 ) {
       strcat  (queryText, "query.setParameters(\"");
       strcat  (queryText, eParameters);
       strcat  (queryText, "\");");
    }
   /* 
    * 2. grid navigation query (gridNav):
    * eLibClass = GempakGridNavigationRequest
    */
    else if ( ( strcmp ( queryType, "gridNav" ) == 0 ) && 
         ( strcmp ( eSrc, "GRID" ) == 0 ) ) {
       strcat  (queryText, "query.setGridIdParms(\"");
       strcat  (queryText, eGrid);
       if ( gNavDattim[0] != '\0' ) {
           strcat  (queryText, "\",");
           strcat (queryText,"\"");
           strcat (queryText, gNavDattim);
       }
       if ( gEventName[0] != '\0' ) {
           strcat  (queryText, "\",");
           strcat (queryText,"\"");
           strcat (queryText, gEventName);
       }
       strcat  (queryText, "\");");
    }
   /* 
    * 3. db time query (dbTime):
    * eLibClass = GempakCatalogTimeQuery
    */
    else if ( ( strcmp ( queryType, "dbTime" ) == 0 ) && 
         ( strcmp ( eSrc, "GRID" ) == 0 ) ) {
       strcat  (queryText, "query.setParameters(\"");
       strcat  (queryText, eGrid);
       if ( gEventName[0] != '\0' ) {
           strcat (queryText,"|");
           strcat (queryText, gEventName);
       }
       strcat  (queryText, "\");");
    }
   /* 
    * 4. data URI query (dataURI):
    * eLibClass = GempakDataURIRequest
    */
    else if ( ( strcmp ( queryType, "dataURI" ) == 0 ) &&
         ( strcmp ( eSrc, "GRID" ) == 0 ) ) {
       strcat  (queryText, "query.setDataParms(\"");
       cst_uclc (queryType, columnName, &ier);
       strcat  (queryText, columnName);
       strcat  (queryText, ":");
       strcat  (queryText, eMdl);
       strcat  (queryText, "|");
       strcat  (queryText, gDattim);
       strcat  (queryText, "|");
       strcat  (queryText, gCord);
       strcat  (queryText, "|");
       strcat  (queryText, gLevel);
       strcat  (queryText, "|");
       strcat  (queryText, gParm);
       if ( gEventName[0] != '\0' ) {
           strcat  (queryText, "|");
           strcat (queryText, gEventName);
       }
       strcat  (queryText, "\");");
    }
   /* 
    * 5. grid data query (gridDat):
    * eLibClass = GempakGridLinkRequest
    */
    else if ( ( strcmp ( queryType, "gridDat" ) == 0 ) && 
         ( strcmp ( eSrc, "GRID" ) == 0 ) ) {
       strcat  (queryText, "query.setDataUri(\"");
       strcat  (queryText, eDistnctField);
       strcat  (queryText, "\");");
    }
   /* 
    * 6. (ensMember):
    * eLibClass = GempakEnsMemberRequest
    */
    else if ( ( strcmp ( queryType, "ensMember" ) == 0 ) &&
         ( strcmp ( eSrc, "GRID" ) == 0 ) ) {
       strcat  (queryText, "query.setDataParms(\"");
       strcat  (queryText, eMdl);
       strcat  (queryText, "|");
       strcat (queryText, gEventName);
       strcat  (queryText, "|");
       strcat  (queryText, gDattim);
       strcat  (queryText, "\");");
    }
    else if ( ( strcmp ( queryType, "scanDb" ) == 0 ) &&
         ( strcmp ( eSrc, "GRID" ) == 0 ) ) {
       strcat  (queryText, "query.setParameters(\"");
       strcat  (queryText, eParameters);
       strcat  (queryText, "\");");
    }
   /*
    *  TODO: add queries for the following query types
    *        in 'old notation' and change to 'new noation'
    *
    *  	old notation	new notation
    *
    *  	timeqry         dbTime
    *   stidqry
    *   stnmqry
    *   obrvqry
    *
    else if ( ( strcmp ( queryType, "timeqry" ) == 0 ) && 
         ( strcmp ( eSrc, "METAR" ) == 0 ) ) {
       strcat  (queryText, "query.setSource(\"");
       strcat  (queryText, eSrc);
       strcat  (queryText, "\");");
    }
    else if ( ( strcmp ( queryType, "timeqry" ) == 0 ) &&
         ( strcmp ( eSrc, "BUFRUA" ) == 0 ) ) {
    }
    else if ( ( strcmp ( queryType, "timeqry" ) == 0 ) &&
         ( strcmp ( eSrc, "SYNOP" ) == 0 ) ) {
    }
    else if ( ( strcmp ( queryType, "stidqry" ) == 0 ) ) {
       strcat  (queryText, "query.setGempakArea(\"");
       strcat  (queryText, gArea);
       strcat  (queryText, "\");");
    }
    else if ( ( strcmp ( queryType, "stnmqry" ) == 0 ) ) {
       strcat  (queryText, "query.setGempakArea(\"");
       strcat  (queryText, gArea);
       strcat  (queryText, "\");");
       strcat  (queryText, "query.returnStationNumber();");
    }
    else if (  ( strcmp ( queryType, "obrvqry" ) == 0 ) ) {
       if ( strcmp ( eSrc, "METAR" ) == 0 )  {
          strcat  (queryText, "query.setCount(");
          sprintf (dum, "%d", eCount);
          strcat  (queryText, dum);
          strcat  (queryText, ");");
          strcat  (queryText, "query.setType(\"");
          strcat  (queryText, eSrc);
          strcat  (queryText, "\");");
          strcat  (queryText, "query.setArea(\"");
          strcat  (queryText, gArea);
          strcat  (queryText, "\");");
          strcat  (queryText, "query.setTime(\"");
          strcat  (queryText, gDattim);
          strcat  (queryText, "\");");
       }
       else if ( strcmp ( eSrc, "BUFRUA" ) == 0 )  {
          strcat  (queryText, "query.setArea(\"");
          strcat  (queryText, gArea);
          strcat  (queryText, "\");");
          strcat  (queryText, "query.setTime(\"");
          strcat  (queryText, gDattim);
          strcat  (queryText, "\");");
          strcat  (queryText, "query.setPart(\"");
          strcat  (queryText, ePrt);
          strcat  (queryText, "\");");
        }
        else if ( strcmp ( eSrc, "SYNOP" ) == 0 )  {
          strcat  (queryText, "query.setArea(\"");
          strcat  (queryText, gArea);
          strcat  (queryText, "\");");
          strcat  (queryText, "query.setTime(\"");
          strcat  (queryText, gDattim);
          strcat  (queryText, "\");");
          strcat  (queryText, "query.setRepType(\"");
          strcat  (queryText, ePrt);
          strcat  (queryText, "\");");
        }
    }
    *
    */
    else {
       *iret = -1;
       return;
    }
    //strcat  (queryText, "return query.execute();");
    strcat  (queryText, "qResults = query.execute(); print \"");
    strcat  (queryText, eLibClass);
    strcat  (queryText, " took %0.3f ms\" % ((time.time()-t1)*1000.0); return qResults;");
    return;
}

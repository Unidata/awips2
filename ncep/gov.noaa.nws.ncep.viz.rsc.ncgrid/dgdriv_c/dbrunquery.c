#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_runQuery ( char *queryText, char *queryResult, int *iret )
/************************************************************************
 *									*
 * db_runQuery               						*
 *                                                                      *
 * This function sends a query, specified in queryText to the AWISP II 	* 
 * end point and returns the result of the query in the queryResult	*
 * string.								*
 *                                                                      *
 * void db_runQuery ( char *queryText, char *queryResult, int *iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *queryText      char            Text of the query               *
 *                                                                      *
 * Output parameters:                                                   *
 *      *queryResult	char            Query result			*
 *      *iret           int             return code:                    *
 *                                      -1: query run unsuccessfully	*
 *                                       0: normal                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * m.gamazaychikov/CWS	01/10	Created                                 *
 ************************************************************************/
{
char headcmd[128], popncmd[512], uengine[50], sep[5], *errorReturn = NULL;
int  queryResultSize = 20000;
int  chars_read=0 , ier, ier1;
FILE *read_fp;

struct timeb t_start, t_open, t_read, t_current;
/*---------------------------------------------------------------------*/

    ftime(&t_start);
    *iret = 0;

   /*
      echo 'import GempakCatalogTimeQuery;query =GempakCatalogTimeQuery.GempakCatalogTimeQuery("mosaic");query.setParameters("BREF|2.0");return query.execute();' | uengine -r python

   */
   /*
    * Create the string containing the curl command and flags:
    * -s	Silent mode.  Makes Curl mute - does not show progress
    *		meter or error messages.
    * -d	Sends the specified data in a POST request to the HTTP
    *		server.
    *
    sprintf (headcmd, "%s", "curl -s -d");
    */

   /*
    * command to direct string to standard input
    */
    sprintf (headcmd, "%s", "echo");

   /*
    * Create a string that invokes the AWIPS II uEngine CLI untility
    */
    sprintf (sep, "%s", "|");
    sprintf (uengine, "%s", "uengine -r python -m");
   /*
    * Create the string for the open pipe
    */
    sprintf (popncmd, "%s \'%s\' %s %s", headcmd, queryText, sep, uengine);

   /*
    * Initialize the queryResult string, open the pipe, 
    * and read data from the pipe
    */
    memset (queryResult, '\0', sizeof(queryResult));
    ftime(&t_open);
    read_fp = popen(popncmd, "r");
    ftime(&t_current);
    printf("\t\t\t time spent in db_runquery in popen: %d\n", (int) (1000.0 * (t_current.time - t_open.time) + (t_current.millitm - t_open.millitm)));
    
    if ( read_fp != NULL ) { 
      /* comment out for now - remove later
       */
       ftime(&t_read);
       chars_read = fread( queryResult, sizeof( char ), 
                           queryResultSize, read_fp );
       queryResult[chars_read] = '\0';
       ftime(&t_current);
       printf("\t\t\t time spent in db_runquery in read: %d\n", (int) (1000.0 * (t_current.time - t_read.time) + (t_current.millitm - t_read.millitm)));
      /* comment out for now - remove later
       while (chars_read > 0) {
          queryResult[chars_read - 1] = '\0';
          chars_read = fread(queryResult, sizeof(char), 
                                   queryResultSize, read_fp);
       } 
       */
       pclose(read_fp);
       if ( chars_read == 0 ) {
          ier = -16;
          er_wmsg ( "DB", &ier, uengine, &ier1, 2, strlen(uengine) );
          *iret = -1;
          return;
       }
    }
    else {
       ier = -11;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }

    
    errorReturn = strstr (queryResult, "error");

    if ( errorReturn != NULL ) {
       ier = -12;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }
    ftime(&t_current);
    printf("\t\t\t time spent in db_runquery: %d\n", (int) (1000.0 * (t_current.time - t_start.time) + (t_current.millitm - t_start.millitm)));
    return;
}

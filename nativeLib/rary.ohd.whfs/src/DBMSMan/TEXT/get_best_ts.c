#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "IngestFilter.h"
#include "DbmsDefs.h"

#include "get_best_ts.h"
   
/****************************************************************
   
   For a given location and pe code and type-source prefix, this function
   returns the type-source code with the lowest rank in IngestFilter.
   Alternatively, if a specific ordinal number is passed, then the 
   Nth ranking ts is returned.  If no (<= 0) ordinal number (i.e. 1st, 2nd)
   is requested, then the highest rank (1st) is returned.
   The type-source prefix is normally given as a one-character string,
   R for observed data and F for forecast data.
   
   The function argument returns a status variable indicating 
   whether the request was satisfied.
   
   **************************************************************/

int get_best_ts(char 	*lid,
		char	*pe,
		char 	*ts_prefix,
		int	ordinal,
		char	*ts_found)
{
   
   char			where[130];
   int 			cnt;
   IngestFilter		*ingestHead, *ingestPtr;
   int			status;
   
   
   /* initialize */
   
   memset(ts_found, 0, SHEF_TS_LEN + 1);
   status = -1;
   
   
   /* get the ingest filter entries for this location. note that the
      retrieval is ordered so that if multiple best ranks exist, there
      is some predicatibility for the identified best one. also note that
      this approach ignores the duration, extremum, and probabilty code. */
   
   sprintf(where, " WHERE lid = '%s' AND pe = '%s' AND "
           " ts LIKE '%s%%' AND ingest = 'T' "
	   " ORDER BY ts_rank, ts",
           lid, pe, ts_prefix);
   ingestHead = GetIngestFilter(where);
   
   if (ingestHead != NULL)
   {
      /* if no specific ordinal number was requested, return with
	 the highest rank. */
      
      if (ordinal <= 0)
      {
	 strcpy(ts_found, ingestHead->ts);
	 status = 1;
      }
      
      
      /* if a specific ordinal number was requested. */
      
      else 
      {	 
	 /* get a count of the number of matching ts entries.
	    if the requested ordinal number is greater than 
	    the number available then return with a not found status. */
	 
	 cnt = ListCount(&ingestHead->list);
	 if (ordinal <= cnt)
	 {
	    ingestPtr = (IngestFilter *) ListNth(&ingestHead->list, ordinal);
	    strcpy(ts_found, ingestPtr->ts);
	    status = 1;
	 }
      }     
      
      
      /* free memory */
      
      FreeIngestFilter(ingestHead);
   }
   
   
   return(status);
}



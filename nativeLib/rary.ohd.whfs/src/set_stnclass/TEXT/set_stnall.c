#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "Location.h"
#include "StnClass.h"

#include "set_stnall.h"
#include "set_stnclass.h"

#include "DbmsUtils.h"
#include "DbmsDefs.h"


/************************************************************************
   
   Control the processing for determining the characteristics
   for all stations.
   
   ************************************************************************/

void set_stnclass_all()
{
   
   Location	*locHead, *locPtr;
   int 		cnt = 0;
   int          status;
   char	  	where[120];  
   
   
   /* get the list of locations */   
   
   sprintf(where, " ORDER BY lid ");   
   if ( (locHead = GetLocation(where)) )
   {
     
      status = DeleteStnClass("");
      if (status < 0)
      {
	 fprintf(stderr, "Error %d deleting data from StnClass table\n",
		 status);
      }
      else
         printf("Deleting existing entries in the StnClass table.\n");
         
         
      printf("Creating new entries in the StnClass table.\n");
      
      
      /* process the stations */
      
      locPtr = (Location *) ListFirst(&locHead->list); 
      
      while(locPtr)
      {
	 cnt++;
	 /* printf("Processing station %d %s\n", i, locPtr->lid); */
	 
	 /* process the current station */
	 
	 set_stnclass(locPtr->lid);
	 
	 
	 /* get the next station */
	 
	 locPtr = (Location *) ListNext(&locPtr->node);
      }
      
      FreeLocation(locHead);
   }
   
   else
   {
      fprintf(stderr, "No locations defined.\n");      
   }
   
   printf("Processed %d stations.\n", cnt);
   
   return;
}



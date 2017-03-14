#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "Telem.h"
#include "Dcp.h"
#include "Observer.h"
#include "IngestFilter.h"
#include "RpfFcstPoint.h"
#include "StnClass.h"

#include "set_stnclass.h"

#include "DbmsUtils.h"
#include "DbmsDefs.h"


/************************************************************************
   
   Determine the characteristics for a given station
   and store it in memory.
   
   ************************************************************************/

void set_stnclass(char *lid)
{
   
   IngestFilter	*ingestHead = NULL;
   StnClass	class;
   Telem	*telHead = NULL;
   char	  	where[BUFSIZ];
   int 		numpe;
   int		status;
   int		Hnum, Pnum, Snum, Tnum, Qnum, PAnum;
   int		dcp_found, obs_found, tel_found, fp_found, res_found;
   int		is_offriv, is_res, is_riv;
   int		is_precip, is_snow, is_temp, is_other, is_undef;
   
   
   /* set the SQL where clause */
   
   sprintf(where, " where lid= '%s' ", lid);
   
   
   /* determine the station's data sources  */
 
 
   if ( ( telHead = GetTelem(where) ) )
      tel_found = 1;
   else
      tel_found = 0;   
  
   if (recordCount("Dcp", where) > 0)
      dcp_found = 1;	
   else
      dcp_found = 0;
   
   if (recordCount("Observer", where) > 0)
      obs_found = 1;
   else
      obs_found = 0;
   
   if (recordCount("RpfFcstPoint", where) > 0)
      fp_found = 1;
   else
      fp_found = 0;
       
   if (recordCount("Reservoir", where) > 0) 
      res_found = 1;
   else
      res_found = 0;
         
         
   /* determine the station class */
   
   /* initialize */
   
   Hnum = Pnum = Snum = Tnum = Qnum = PAnum = 0;   
   is_offriv = is_res = is_riv = 0;
   is_precip = is_snow = is_temp = is_other = is_undef = 0;
   
   
   /* an official forecast point is a station that is 
      defined in the RpfFcstPoint table.  a reservoir
      include points that have an entry in the reservoir table */
   
   if (fp_found)
      is_offriv = 1;
      
   if (res_found)
      is_res = 1;
   
   
   /* get data elements defined for station */
   
   sprintf(where, " where lid= '%s' and ingest= 'T' ", lid);
   if ( ( ingestHead = GetIngestFilter(where) ) )    
   {     

      /* get the count of params for later use */
      
      Hnum = check_pe_match(ingestHead,  "H");
      Qnum = check_pe_match(ingestHead,  "Q");
      
      Snum = check_pe_match(ingestHead,  "S");    
      Tnum = check_pe_match(ingestHead,  "T");
      
      Pnum = check_pe_match(ingestHead,  "P");
      PAnum = check_pe_match(ingestHead, "PA");
      Pnum = Pnum - PAnum;
      
      numpe = ListCount(&ingestHead->list);
      
      
      /* also, a station is a reservoir if it has a 
	 param type of HP or HT or LS */
	 
            
      if  (  
             check_pe_match(ingestHead,  "HP") > 0 || 
	     check_pe_match(ingestHead,  "HT") > 0 ||
	     check_pe_match(ingestHead,  "LS") > 0
	  ) 
      {
	 is_res = 1;
      }
      
      
      /* a station is a river data point if it has an H* or Q*
	 parameter and is not considered an official forecast
	 point or reservoir station */
      
      if (!is_offriv && !is_res)
      {
	 if (Hnum > 0 || Qnum > 0)
	    is_riv = 1;  
      }
      
      
      /* check if the station is a precipitation station, snow,
	 or temperature station */
      
      if (Pnum  > 0)
	 is_precip = 1; 
      
      if (Snum  > 0)
	 is_snow   = 1;
      
      if (Tnum > 0)
	 is_temp   = 1;
            
      
      /* free info */  
      
      FreeIngestFilter(ingestHead);
   }
   
   /* no ingest filter entries found for this station */
   
   else
      numpe = 0;
   
   
   /* now check the special station classes */
    
   if ((numpe - (Hnum + Qnum + Pnum + Snum + Tnum)) > 0) 
      is_other = 1;
   
   if (!is_offriv && !is_riv  && !is_res  &&
       !is_precip && !is_snow && !is_temp &&
       !is_other)
      is_undef = 1;
   
   
   /* now with all the information in hand, load the information 
      into the StnClass table.  */
   
   strcpy(class.lid, lid);
   
   memset(class.disp_class, 0, TYPE_LEN + 1);
   if (is_offriv) strcat(class.disp_class, "F");
   if (is_res)    strcat(class.disp_class, "D"); 
   if (is_riv)    strcat(class.disp_class, "R");
   if (is_precip) strcat(class.disp_class, "P");
   if (is_snow)   strcat(class.disp_class, "S");
   if (is_temp)   strcat(class.disp_class, "T"); 
   if (is_other)  strcat(class.disp_class, "O");
   if (is_undef)  strcat(class.disp_class, "U");
   
   if (dcp_found) 
      strcpy(class.dcp, "T");
   else
      strcpy(class.dcp, "F");
   
   if (tel_found) 
      strcpy(class.telem_type, telHead->type);
   else
      strcpy(class.telem_type, "");
   
   if (obs_found) 
      strcpy(class.observer, "T");
   else
      strcpy(class.observer, "F");
   
   
   /* load the data by deleting the existing entry and loading
      the new entry. */
     
   sprintf(where, " where lid= '%s' ", lid);
   status = DeleteStnClass(where);
   if (status < 0)
   {
      fprintf(stderr, "Error %d deleting from StnClass table for %s\n",
	      status, lid);
   }
   
   status = PutStnClass(&class);
   if (status < 0)
   {  
      fprintf(stderr, "Error %d putting into StnClass table for %s\n",
	      status, lid);
   }
   
 
   if (telHead != NULL)
      FreeTelem(telHead);
   
   
   return;
}


/************************************************************************
   
   check if there is a pe match
   
   **********************************************************************/

int check_pe_match(IngestFilter *ingestHead,
		   char  	*pe)
{
   IngestFilter 	*ingestPtr;
   int 			i;
   int 			cnt;
   int 			numchars;
   
   
   /* initialize; allow search parameters to be either
      one or two chars*/
   
   numchars = strlen(pe);   
   cnt = 0;
   
   
   ingestPtr = (IngestFilter *) ListFirst(&ingestHead->list);   
   for (i = 0; ingestPtr; i++)
   {
      if (strncmp(ingestPtr->pe, pe, numchars) == 0)
      {
	 cnt++;
      }
      ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
   }
   
   
   return(cnt);
}



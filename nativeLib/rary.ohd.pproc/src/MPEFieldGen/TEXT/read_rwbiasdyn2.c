#include <string.h>
#include <stdio.h>

#include "mpe_fieldgen.h"
#include "RWBiasDyn.h"
#include "time_convert.h"

void MPEFieldGen_read_rwbiasdyn2(const char *rad, 
                     const char  * site_id,
		     const char * str,
		     const int lag_cut,
		     double *num_pairs,
		     double *sumgag,
		     double *sumrad,
		     double *bias, 
	    	     int *lag,
	  	     char sstr1[20], /* Why the magic number? */
	  	     long int *irc)

/*
   this function reads records from the RWBiasDyn table for previous hours
   these records contain the state variables used in the bias calculations
   each previous hour up to lag_cut hours is searched to find records in the table 
   if no records is found, then state variables are reinitialized

   calling function: mfb_subr
*/

{
   int j, irec, ctr;
   char rrad[4], strp[22], dude[22];
   RWBiasDyn	*bdHead = NULL, *bdPtr;
   char	        where[BUFSIZ];	
   char		obstime_ANSI[ANSI_TIME_LEN+1];  
   time_t     firstTime = 0;

   memset(rrad,'\0',4);
   strncpy(rrad,rad,3);
   memset(strp,'\0',22);
   memset(sstr1,'\0',20);


   memset(dude,'\0',22);
   strncpy(dude,str,19);

   yearsec_ansi_to_timet(dude, &firstTime);
   
   /*------------------------------------------------*/
   /*   subtract 1 hour and attempt to select record */
   /*------------------------------------------------*/

   *irc = -1;
   for(j=0; j<lag_cut; j++)
   {
        firstTime -= SECONDS_PER_HOUR;
	timet_to_yearsec_ansi(firstTime, strp);
	strncpy(sstr1,strp,19);

	irec = 0;
	*lag = j+1;

	/*------------------------------------------*/
	/*   select records for all mem_span values */
	/*------------------------------------------*/
	
	timet_to_yearsec_ansi(firstTime, obstime_ANSI);
        strncpy(sstr1,obstime_ANSI,19);
        sstr1[19] = '\0';
	sprintf(where, " WHERE radid='%s' AND obstime='%s' AND "
                       " office_id = '%s'", rrad, obstime_ANSI,
                        site_id);
      
        sprintf ( message, "Select * FROM RWBiasDyn %s\n", where );
        printMessage ( message, logFile );         			
			

	bdHead = GetRWBiasDyn(where);
	if (bdHead)
	{
		bdPtr = (RWBiasDyn *) ListFirst(&bdHead->list);	
		ctr = 0;
		while (bdPtr) 
		{
			num_pairs[ctr] = (double)bdPtr->numpairs;
			sumgag[ctr] = (double)bdPtr->sumgag;
			sumrad[ctr] = (double)bdPtr->sumrad;
			bias[ctr] = (double)bdPtr->bias;
			
			ctr++;
			bdPtr = (RWBiasDyn *) ListNext(&bdPtr->node);  
		}

                if ( bdHead != NULL )
                {
                   FreeRWBiasDyn(bdHead);
                   bdHead = NULL ;
                }

		if (ctr > 0)
		{
		   *irc = 0;
                   break;
		}
	}
	else
        {
		*irc = 100;
        }

   }   /*  end for (j=0  ...  */

   return ;

} /* end MPEFieldGen_read_rwbiasdyn2 */ 

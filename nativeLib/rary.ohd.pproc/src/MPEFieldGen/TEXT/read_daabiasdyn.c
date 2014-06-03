#include <string.h>
#include <stdio.h>

#include "mpe_fieldgen.h"
#include "DAABiasDyn.h"
#include "time_convert.h"

void MPEFieldGen_read_daabiasdyn(const char *rad,
             const char  * site_id,
		     const char * str,
		     const int lag_cut,
		     double *num_pairs,
		     double *sumgag,
		     double *sumrad,
		     double *bias, 
	    	     int *lag,
	  	     char sstr1[20],
	  	     long int *irc)

/*
   this function reads records from the DAABiasDyn table for previous hours
   these records contain the state variables used in the bias calculations
   each previous hour up to lag_cut hours is searched to find records in the table 
   if no records is found, then state variables are reinitialized

   calling function: calculateMeanBiasDP
*/

{
   int j, irec, ctr;
   char rrad[4], dude[22];
   DAABiasDyn	*bdHead = NULL, *bdPtr;
   char	        where[BUFSIZ];	
   char		obstime_ANSI[ANSI_TIME_LEN+1];  
   time_t     firstTime = 0;

   memset(rrad,'\0',4);
   strncpy(rrad,rad,3);
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
      
        sprintf ( message, "Select * FROM DAABiasDyn %s\n", where );
        printMessage ( message, logFile );         			
			

	bdHead = GetDAABiasDyn(where);
	if (bdHead)
	{
		bdPtr = (DAABiasDyn *) ListFirst(&bdHead->list);	
		ctr = 0;
		while (bdPtr) 
		{
			num_pairs[ctr] = (double)bdPtr->numpairs;
			sumgag[ctr] = (double)bdPtr->sumgag;
			sumrad[ctr] = (double)bdPtr->sumrad;
			bias[ctr] = (double)bdPtr->bias;
			
			ctr++;
			bdPtr = (DAABiasDyn *) ListNext(&bdPtr->node);  
		}

                if ( bdHead != NULL )
                {
                   FreeDAABiasDyn(bdHead);
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


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/read_daabiasdyn.c,v $";
 static char rcs_id2[] = "$Id: read_daabiasdyn.c,v 1.2 2012/07/06 14:34:32 pst Exp $";}
/*  ===================================================  */

}  /*  end read_daabiasdyn function  */

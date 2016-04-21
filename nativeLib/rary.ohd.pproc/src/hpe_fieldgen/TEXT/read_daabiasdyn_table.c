#include <stdio.h>
#include <string.h>
#include "empe_fieldgen.h"
#include "time_convert.h"
#include "DAABiasDyn.h"
#include "RadarLoc.h"
		     
void read_daabiasdyn(const char *radar_id, 
                     const char *office_id,
                     const char * str,
                     const int lag_cut,
                     double *num_pairs,
                     double *sumgag,
                     double *sumrad,
                     double *bias, 
                     int *lag,
                     char sstr1[19],
                     long int *irc)
/*
   this function reads records from the DAABiasDyn table for previous hours
   these records contain the state variables used in the bias calculations
   each previous hour up to lag_cut hours is searched to find records in the  
   table if no records is found, then state variables are reinitialized

   calling function: mfb_subr
*/

{
   int j, irec, ctr;
   DAABiasDyn *bdHead = NULL;
   DAABiasDyn *bdPtr = NULL;
   char rrad[4] = {'\0'};
   char strp[22] = {'\0'};
   char dude[22] = {'\0'};
   char where[BUFSIZ] = {'\0'};
   char obstime_ANSI[ANSI_TIME_LEN+1] = {'\0'};  
   time_t firstTime = 0;

   strncpy(rrad, radar_id, 3);
   strncpy(dude, str, 19);

   yearsec_ansi_to_timet(dude, &firstTime);
   
   /*------------------------------------------------*/
   /*   subtract 1 hour and attempt to select record */
   /*------------------------------------------------*/

   *irc = -1;
   for(j = 0; j < lag_cut; j ++)
   {
        firstTime -= SECONDS_PER_HOUR;
        timet_to_yearsec_ansi(firstTime, strp);
        strncpy(sstr1, strp, 19);

        irec = 0;
        *lag = j+1;

        /*------------------------------------------*/
        /*   select records for all mem_span values */
        /*------------------------------------------*/
        
        timet_to_yearsec_ansi(firstTime, obstime_ANSI);
        sprintf(where, " WHERE radid='%s' AND obstime='%s' AND "
                       " office_id = '%s'",
                       rrad, obstime_ANSI, office_id);

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_daabiasdyn_table.c,v $";
 static char rcs_id2[] = "$Id: read_daabiasdyn_table.c,v 1.1 2012/09/12 18:14:14 deng Exp $";}
/*  ===================================================  */

}  




/**********************************************************
   This subroutine read lightning data from lightning table.
   The lightning data is used by Gauge QC task.

   Developed by Feng Ding (Sept. 2003)

   calling subroutine: read_gage_data(rfcfieldgen)
   
**********************************************************/

#include <stdio.h>
#include <string.h>

#include "Lightning.h"
#include "sqlca.h"

void read_lightning(char dt[19], int *ihrap, int *jhrap,
                  int *num_strike, long int *irc)
{

   char datetime1[22] = {'\0'};
   char where [ 100 ] = {'\0'} ;
   int i;
   int j;
   int this_db_xhrap;
   int this_db_yhrap;
   Lightning * pLightningHead = NULL;

   strncpy(datetime1,dt,19);

   *num_strike = 0;
      
   for(i=0; i<3; i++)
   {
      for(j=0; j<3; j++)
      {
         this_db_xhrap = *ihrap + i;
         this_db_yhrap = *jhrap + j;
                 
         /* Create the where clause. */
         sprintf ( where, "WHERE x_hgrid='%d' AND y_hgrid='%d' and "
                          "obstime='%s'", this_db_xhrap, this_db_yhrap, 
                          datetime1 ) ;

         pLightningHead = GetLightning ( where );

         if( SQLCODE < 0 )
         {
            *irc=SQLCODE;
            return;
         }
                
         if ( pLightningHead != NULL ) 
         {
            * num_strike = pLightningHead->no_of_strike;
            FreeLightning ( pLightningHead );
            pLightningHead = NULL;

            return ;
         }
      }
   }
}

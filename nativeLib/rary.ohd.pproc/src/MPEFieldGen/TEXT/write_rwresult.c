#include <string.h>

#include "DbmsDefs.h"
#include "RWResult.h"
#include "time_convert.h"
#include "current_GMT_dt.h"

void MPEFieldGen_writeRWResult ( const char *rfc, 
					const char * dt, 
					const int ngag,
					const int isat,
					const int nrad,
					const char *field,
					int * overwrt,
					long int *irc )
{
/* this subroutine writes information about each mpe_fieldgen execution
   to the RWResult table

   if no record exists, then record is inserted (overwrt set to 0)              
   if a record already exists, then the auto_save flag is checked

      if auto_save = T, then 
         record is updated (overwrt set to 0)
      else 
         update last_exec_time only 
         (field has been saved through mpe_gui) (overwrt set to 1)

   calling subroutine: writeQPE
*/

   int ret;
   char rrfc[RFS_RFC_LEN+1];
   char asat[BOOL_LEN+1];
   char asave[BOOL_LEN+1];
   char drawp[BOOL_LEN+1];
   char fieldtype[MOSAIC_TYPE_LEN+1];
      
   RWResult fppp; 
   RWResult *pRWResult = NULL; 

   dtime_t GMTtime_dt, dttm;
   
   char where[100];
   char datetime[ANSI_YEARSEC_TIME_LEN + 1];

   *overwrt = 0;
   *irc = 0;

   rrfc[RFS_RFC_LEN]='\0';
   fieldtype[MOSAIC_TYPE_LEN]='\0';
   asat[BOOL_LEN]='\0';
   asat[0]='n';
   asave[BOOL_LEN]='\0';
   asave[0]='T';
   drawp[BOOL_LEN]='\0';
   drawp[0]='F';

   if(isat == 1) asat[0]='y';

   strncpy(rrfc,rfc,RFS_RFC_LEN);
   strncpy(fieldtype,field,MOSAIC_TYPE_LEN);

   /*-----------------------------------------*/
   /*   manipulate datetime variable          */
   /*-----------------------------------------*/
   memset(datetime, '\0', ANSI_YEARSEC_TIME_LEN + 1);
   strncpy(datetime,dt,ANSI_YEARSEC_TIME_LEN);
   yearsec_ansi_to_dt(datetime,&dttm);

   /*-----------------------------------------*/
   /*   get current time in GMT               */
   /*-----------------------------------------*/
   *irc = current_GMT_dt(&GMTtime_dt);

   if(*irc != 0) return;

   /*-----------------------------------------*/
   /*   set up variables in RWResult          */ 
   /*   structure                             */
   /*-----------------------------------------*/

   strcpy(fppp.rfc,rrfc);
   fppp.obstime = dttm;
   fppp.num_gag_avail = ngag;
   fppp.num_rad_avail = nrad;
   fppp.num_pseudo_gages = 0;
   strcpy(fppp.sat_avail,asat);
   strcpy(fppp.mapx_field_type, fieldtype);
   strcpy(fppp.draw_precip,drawp);
   strcpy(fppp.auto_save,asave);
   fppp.last_exec_time = GMTtime_dt;
   fppp.last_save_time = GMTtime_dt;

   /*-----------------------------------------*/
   /*   attempt to select record              */
   /*-----------------------------------------*/
      
   /* Create the where clause. */
   sprintf ( where, "WHERE rfc='%s' AND obstime='%s'",
             rrfc, datetime ) ;
   pRWResult = GetRWResult ( where ) ;

   if ( pRWResult != NULL )
   {
      strncpy(asave, pRWResult->auto_save, BOOL_LEN);

      /* Perform an Update. Must check the auto save flag. If the 
         auto save flag is 'F', then only update the Last Exec Time. */
      if(strcmp(asave,"F") == 0)
      {
         *overwrt = 1;
         fppp.num_gag_avail = pRWResult->num_gag_avail;
         fppp.num_rad_avail = pRWResult->num_rad_avail;
         fppp.num_pseudo_gages = pRWResult->num_pseudo_gages;
         strcpy(fppp.sat_avail,pRWResult->sat_avail);
         strcpy(fppp.mapx_field_type, pRWResult->mapx_field_type);
         strcpy(fppp.draw_precip,pRWResult->draw_precip);
         strcpy(fppp.auto_save,asave);
         fppp.last_save_time = pRWResult->last_save_time;
      }
      else
      {

         /* Make sure that the value of the satellite availability
            flag is not overwritten if the current state of the
            satellite availability is not known. */
         if ( fppp.sat_avail[0] == 'u' )
         {
            memset ( fppp.sat_avail, '\0', BOOL_LEN + 1 );
            strncpy ( fppp.sat_avail, pRWResult->sat_avail, BOOL_LEN );
         }
      }

      ret = UpdateRWResult ( &fppp, where ) ;

      *irc = ret ;

      FreeRWResult( pRWResult );
      pRWResult = NULL ;
   }
   else /* ( pRWResult == NULL ) */
   {
      /* The record does not exist. Perform an Insert. */
      ret = PutRWResult(&fppp);
      * irc = ret ;
   }

   return;
} /* end MPEFieldGen_writeRWResult */

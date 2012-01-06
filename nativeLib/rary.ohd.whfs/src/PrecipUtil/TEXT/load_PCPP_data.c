/*******************************************************************************
* FILENAME:            load_PCPP_data.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <string.h>
#include <time.h>

#include "CurPC.h"
#include "CurPP.h"
#include "DbmsDefs.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "load_PCPP_data.h"
#include "QualityCode.h"
#include "RawPC.h"
#include "RawPP.h"
#include "time_convert.h"

/* Contains the where clause used by the most recently called
   data loading function. A call to get_where_clause will return 
   the where clause.  This is useful for the purposes of logging. */
static char where [ BUFSIZ ] = { '\0' } ;

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
const char * get_pcpp_query ( )
{
   return where ; 
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

static char * build_ts_clause ( const char ** ts ,  int num_ts )
{
   char * pChar = NULL ;
   const static char * ts_single = "ts = '$$'" ;
   const static char * ts_not_single = "ts != '$$'" ;
   char * pTsString = NULL ;
   const static char * ts_multi = "ts in ( '$$' )" ; 
   const static char * ts_not_multi = "ts not in ( '$$' )" ; 
   const static char * ts_element = ", '$$'" ; 
   int i ;
   int len ;
   int not = 0 ;

   /* Check for '!'.  If a '!' is found, then it means that the list
      of typesources is for exclusion. */
   pChar = strchr ( ts [ 0 ] , '!' ) ;

   if ( pChar != NULL )
   {
      not = 1 ;
   }

   if ( num_ts == 1 )
   {
      if ( not == 1 )
      {
         len = strlen ( ts_not_single ) + 1 ;
      }
      else
      {
         len = strlen ( ts_single ) + 1 ;
      }

      pTsString = ( char * ) malloc ( sizeof ( char ) * len ) ;  

      if ( pTsString != NULL )
      {
         if ( not == 1 )
         {
            strcpy ( pTsString , ts_not_single ) ; 
         }
         else
         {
            strcpy ( pTsString , ts_single ) ;
         }

         pChar = strchr  ( pTsString , '$' ) ; 

	 if ( not == 1 )
         {
            strncpy ( pChar , & ts [ 0 ] [ 1 ] , SHEF_TS_LEN ) ;  
         }
         else
         {
            strncpy ( pChar , ts [ 0 ] , SHEF_TS_LEN ) ;  
         }
      }
   }
   else
   {
      if ( not == 1 )
      {
         len = strlen ( ts_not_multi ) + ( num_ts - 1 ) * 
               strlen ( ts_element ) + 1 ; 
      }
      else
      {
         len = strlen ( ts_multi ) + ( num_ts - 1 ) * 
               strlen ( ts_element ) + 1 ; 
      }

      pTsString = ( char * ) malloc ( sizeof ( char ) * len ) ;

      if ( pTsString != NULL )
      {
         if ( not == 1 )
         {
            strcpy ( pTsString , ts_not_multi ) ;
         }
         else
         {
            strcpy ( pTsString , ts_multi ) ;
         }

         pChar = strchr ( pTsString , '$' ) ;
    
         if ( not == 1 )
         {
            strncpy ( pChar , & ts [ 0 ] [ 1 ] , SHEF_TS_LEN ) ;
         }
         else
         {
            strncpy ( pChar , ts [ 0 ] , SHEF_TS_LEN ) ;
         }

         pChar += SHEF_TS_LEN + 1 ;

         for ( i = 1 ; i < num_ts ; ++ i )
         {
            strcpy ( pChar , ts_element ) ;
            pChar = strchr ( pChar , '$' ) ;
            strncpy ( pChar , ts [ i ] , SHEF_TS_LEN ) ;
            pChar += SHEF_TS_LEN + 1 ;
         }

         strcat ( pChar , " )" ) ; 
        
      }
   }

   return pTsString ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

HourlyPC * load_PC_hourly ( time_t   query_begin_time ,
                            time_t   query_end_time ,
                            const char * lid ,
                            const char ** ts ,
                            int num_ts ,
                            int * pc_rec_cnt)
{
   char beginstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char endstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char * ts_clause = NULL ;
   int length_lid = 0 ;
   int status ;
   HourlyPC * pHourlyPC = NULL ;
   struct tm * pTm = NULL ;

   /* Need special logic to account for accumulation intervals which 
      start at 00Z.  This is because the 00Z PC value is actually placed 
      in the 24 hour slot of the previous day. */
   pTm = gmtime ( & query_begin_time ) ;

   if ( pTm->tm_hour == 0 )
   {
      query_begin_time -= SECONDS_PER_HOUR ;
   }

   /* Need to convert the query begin and end times into dates. */
   status = timet_to_yearday_ansi ( query_begin_time , beginstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   status = timet_to_yearday_ansi ( query_end_time ,   endstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   /* consider according to whether type-source specified. */
   /* load data which is not missing value (-9999.0) */
   if ( lid != NULL )
   { 
      length_lid = strlen ( lid ) ;
   }

   if ( num_ts > 0 )
   {
      ts_clause = build_ts_clause ( ts , num_ts ) ;
      
      if ( ts_clause == NULL ) return NULL ;
   }

   if ( ( length_lid > 0 ) && ( num_ts > 0 ) )
   {
      sprintf ( where, " WHERE lid = '%s' AND %s "
                       " AND obsdate >= '%s' AND "
	               " obsdate <= '%s' "
                       " ORDER BY ts ASC, obsdate ASC",
	               lid, ts_clause, beginstr, endstr);
   }
   else if ( num_ts > 0 )
   {
      sprintf ( where , " WHERE %s "
                        " AND obsdate >= '%s' AND "
	                " obsdate <= '%s' "
                        " ORDER BY lid ASC, ts ASC, obsdate ASC",
	                ts_clause , beginstr , endstr ) ;
   }
   else if ( length_lid > 0 )
   {
      sprintf ( where, " WHERE lid = '%s' "
                       " AND obsdate >= '%s' AND "
                       " obsdate <= '%s' "
                       " ORDER BY ts ASC, obsdate ASC" ,
                       lid , beginstr , endstr ) ;
   }
   else
   {
      sprintf ( where , " WHERE obsdate >= '%s' AND "
                        " obsdate <= '%s' "
                       " ORDER BY lid ASC, ts ASC, obsdate ASC" ,
                       beginstr , endstr ) ;
   
   }

   if ( ts_clause != NULL )
   {
      free ( ts_clause ) ;
      ts_clause = NULL ;
   }

   /* get the data */
   pHourlyPC = GetHourlyPC ( where ) ;

   if ( pHourlyPC != NULL )
   {
      * pc_rec_cnt = ListCount( & pHourlyPC->list);
   }
   else
   {
      * pc_rec_cnt = 0;
   }

   return pHourlyPC ;
}
                       
/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
HourlyPP * load_PP_hourly ( time_t   query_begin_time ,
                            time_t   query_end_time ,
                            const char * lid ,
                            const char ** ts ,
                            int num_ts ,
                            int * pp_rec_cnt )
{
   char beginstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char endstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char * ts_clause = NULL ;
   int length_lid = 0 ;
   int status ;
   HourlyPP * pHourlyPP = NULL ;
   struct tm * pTm = NULL ;

   /* Need special logic to account for accumulation intervals which 
      start at 00Z.  This is because the 00Z PC value is actually placed 
      in the 24 hour slot of the previous day. */
   pTm = gmtime ( & query_begin_time ) ;

   if ( pTm->tm_hour == 0 )
   {
      query_begin_time -= SECONDS_PER_HOUR ;
   }

   /* Need to convert the query begin and end times into dates. */
   status = timet_to_yearday_ansi ( query_begin_time , beginstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   status = timet_to_yearday_ansi ( query_end_time ,   endstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   /* consider according to whether type-source specified. */
   /* load data which is not missing value (-9999.0) */
   if ( lid != NULL )
   { 
      length_lid = strlen ( lid ) ;
   }

   if ( num_ts > 0 )
   {
      ts_clause = build_ts_clause ( ts , num_ts ) ;

      if ( ts_clause == NULL ) return NULL ;
   }

   if ( ( length_lid > 0 ) && ( num_ts > 0 ) )
   {
      sprintf ( where, " WHERE lid = '%s' AND %s "
                       " AND obsdate >= '%s' AND "
	               " obsdate <= '%s' "
                       " ORDER BY ts ASC, obsdate ASC",
	               lid, ts_clause, beginstr, endstr);
   }
   else if ( num_ts > 0 )
   {
      sprintf ( where , " WHERE %s "
                        " AND obsdate >= '%s' AND "
	                " obsdate <= '%s' "
                        " ORDER BY lid ASC, ts ASC, obsdate ASC",
	                ts_clause , beginstr , endstr ) ;
   }
   else if ( length_lid > 0 )
   {
      sprintf ( where, " WHERE lid = '%s' "
                       " AND obsdate >= '%s' AND "
                       " obsdate <= '%s' "
                       " ORDER BY ts ASC, obsdate ASC" ,
                       lid , beginstr , endstr ) ;
   }
   else
   {
      sprintf ( where , " WHERE obsdate >= '%s' AND "
                        " obsdate <= '%s' "
                        " ORDER BY lid ASC, ts ASC, obsdate ASC" ,
                        beginstr , endstr ) ;
   }

   if ( ts_clause != NULL )
   {
      free ( ts_clause ) ;
      ts_clause = NULL ;
   }

   /* get the data */
   pHourlyPP = GetHourlyPP ( where ) ;

   if ( pHourlyPP != NULL )
   {
      * pp_rec_cnt = ListCount( & pHourlyPP->list);
   }
   else
   {
      * pp_rec_cnt = 0;
   }

   return pHourlyPP ;
}


/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
RawPC * load_PC_raw ( time_t query_begin_time ,
                      time_t query_end_time ,
                      const char * lid ,
                      const char ** ts ,
                      int num_ts ,
		      RawPrecipTable table ,
                      int * pc_rec_cnt )
{
   char beginstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char endstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char * ts_clause = NULL ;
   int length_lid = 0 ;
   int status ;
   RawPC * pRawPC = NULL ;

   /* Convert the query begin and end times to INFORMIX ANSI year
      to seconds strings (YYYY-MM-DD hh:mm:ss). */
   status = timet_to_yearsec_ansi ( query_begin_time , beginstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   status = timet_to_yearsec_ansi ( query_end_time ,   endstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   /* consider according to whether type-source specified. */
   /* load data which is not missing value (-9999.0) */
   if ( lid != NULL )
   { 
      length_lid = strlen ( lid ) ;
   }

   if ( num_ts > 0 )
   {
      ts_clause = build_ts_clause ( ts , num_ts ) ;

      if ( ts_clause == NULL ) return NULL ;
   }

   if ( ( length_lid > 0 ) && ( num_ts > 0 ) )
   {
      sprintf ( where, " WHERE lid = '%s' AND %s "
                       " AND value != '-9999.0' AND obstime >= '%s' AND "
	               " obstime <= '%s' ORDER BY obstime DESC ",
	               lid, ts_clause , beginstr, endstr);
   }
   else if ( num_ts > 0 )
   {
      sprintf ( where , " WHERE %s "
                        " AND value != '-9999.0' AND obstime >= '%s' AND "
	                " obstime <= '%s' "
                        " ORDER BY lid ASC, obstime DESC",
	                ts_clause , beginstr , endstr ) ;
   }
   else if ( length_lid > 0 )
   {
      sprintf ( where, " WHERE lid = '%s' "
                       " AND value != '-9999.0' AND obstime >= '%s' AND "
                       " obstime <= '%s' "
                       " ORDER BY ts ASC, obstime DESC" ,
                       lid , beginstr , endstr ) ;
   }
   else
   {
      sprintf ( where , " WHERE value != '-9999.0' AND obstime >= '%s' AND "
                        " obstime <= '%s' "
                       " ORDER BY lid ASC, ts ASC, obstime DESC" ,
                       beginstr , endstr ) ;
   
   }

   if ( ts_clause != NULL )
   {
      free ( ts_clause ) ;
      ts_clause = NULL ;
   }

   /* get the data */
   if ( table == CurRawPrecip )
   {
      pRawPC =  ( RawPC * ) GetCurPC ( where  ) ;
   }
   else
   {
      pRawPC = GetRawPC ( where ) ;
   }


   if ( pRawPC != NULL )
   {
      * pc_rec_cnt = ListCount( & pRawPC->list);
   }
   else
   {
      * pc_rec_cnt = 0;
   }

   return pRawPC ;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

RawPP * load_PP_raw ( time_t query_begin_time ,
                      time_t query_end_time ,
                      const char * lid ,
                      const char ** ts ,
                      int num_ts ,
                      RawPrecipTable table ,
                      int * pp_rec_cnt )
{
   char beginstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char endstr [ ANSI_YEARSEC_TIME_LEN + 1 ] ;
   char qcwhere [ BUFSIZ ] ;
   char * ts_clause = NULL ;

   int length_lid = 0 ;
   int status ;

   RawPP * pRawPP = NULL ;

   status = timet_to_yearsec_ansi ( query_begin_time , beginstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   status = timet_to_yearsec_ansi ( query_end_time , endstr ) ;

   if ( status != 0 )
   {
      return NULL ;
   }

   /* Only retrieve valid PP data. */
   build_qc_where ( QC_NOT_FAILED , qcwhere ) ;

   /* consider according to whether type-source specified. */
   /* load data which is not missing value (-9999.0) */
   if ( lid != NULL )
   { 
      length_lid = strlen ( lid ) ;
   }

   if ( num_ts > 0 )
   {
      ts_clause = build_ts_clause ( ts , num_ts ) ;

      if ( ts_clause == NULL ) return NULL ;
   }

   if ( ( length_lid > 0 ) && ( num_ts > 0 ) )
   {

      sprintf ( where, " WHERE lid = '%s' AND %s "
                       " AND value != '-9999.0' AND obstime >= '%s' AND "
	               " obstime <= '%s' AND %s ORDER BY dur DESC,"
                       " obstime DESC",
	               lid, ts_clause, beginstr, endstr, qcwhere ) ;
   } 
   else if ( num_ts > 0 )
   {
      sprintf(where, " WHERE %s "
                     " AND value != '-9999.0' AND obstime >= '%s' AND "
	             " obstime <= '%s' AND %s ORDER BY lid ASC,"
                     " dur DESC, obstime DESC",
	             ts_clause , beginstr , endstr , qcwhere ) ;
   }
   else if ( length_lid > 0 )
   {
      sprintf(where, " WHERE lid = '%s' "
                     " AND value != '-9999.0' AND obstime >= '%s' AND "
                     " obstime <= '%s' AND %s "
                     " ORDER BY ts ASC, DUR DESC, obstime DESC" ,
                     lid , beginstr , endstr , qcwhere ) ;
   }
   else
   {
      sprintf(where, " WHERE value != '-9999.0' AND obstime >= '%s' AND "
                     " obstime <= '%s' AND %s "
                     " ORDER BY lid ASC, ts ASC, dur DESC, obstime DESC" ,
                     beginstr , endstr , qcwhere ) ;
   
   }

   if ( ts_clause != NULL )
   {
      free ( ts_clause ) ;
      ts_clause = NULL ;
   }

   /* get the data */
   if ( table == CurRawPrecip )
   {
      pRawPP = ( RawPP * ) GetCurPP ( where ) ;
   }
   else
   {
      pRawPP = GetRawPP ( where ) ;
   }

   if ( pRawPP != NULL )
   {
      * pp_rec_cnt = ListCount ( & pRawPP->list ) ;
   }
   else
   {
      * pp_rec_cnt = 0;
   }

   return pRawPP ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

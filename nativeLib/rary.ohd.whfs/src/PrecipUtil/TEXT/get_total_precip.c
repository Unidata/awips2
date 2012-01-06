/*****************************************************************************
* FILENAME:            get_total_precip.c
* NUMBER OF MODULES:   9
* GENERAL INFORMATION:
*   MODULE 1:          get_ingest_info
* DESCRIPTION:         Retrieves IngestFilter information for a given
*                      station.  This information is order by TS ascending.
*                      It can be used for TS ranking.
*
*   MODULE 2:          get_ts_count_hourly
* DESCRIPTION:         For a single lid, returns an integer array containing 
*                      a count of the number of data records for a given TS.
*
*   MODULE 3:          compare_tsrank
* DESCRIPTION:         Compares the ranks of two typesources to determine
*                      which is ranked the highest.  The TS rank information
*                      is contained in the IngestFilter table.
*
*   MODULE 4:          get_ts_count_raw
* DESCRIPTION:         For a single lid, returns an integer array containing
*                      a count of the number of data records for a give TS.
*
*   MODULE 5:          get_highest_ranking_hourly_ts
* DESCRIPTION:         For a given lid, finds the highest ranking TS.
*                      This routine is for use with the HourlyPC and HourlyPC
*                      tables.
*
*   MODULE 6:          get_highest_ranking_raw_ts
* DESCRIPTION:         For a given lid, finds the highest ranking TS.
*                      This routine is for use with the RawPC, RawPP, CurPC,
*                      and CurPP tables.
*
*   MODULE 7:          advance_list_pointer
* DESCRIPTION:         Advances a pointer to a linked list by a given 
*                      number of elements.
*
*   MODULE 8:          get_total_hourly_precip
* DESCRIPTION:         Computes total precipitation based on  PP/PC data
*                      peprocessed by the gage precipitation processor and
*                      stored in the HourlyPC, HourlyPP, and PPother tables.
*
*   MODULE 9:          get_total_raw_precip
* DESCRIPTION:         Computes total precipitation based on PC/PP data not
*                      preprocessed by the gage precipitation processor.
* 		       This data is read from the RawPC and RawPP tables.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       July 9, 2004
* ORGANIZATION:        OHD / HSEB
* MACHINE:             i686, Redhat Linux version 2.4.9-31 enterprise
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        July 9, 2004 Bryon Lawrence    Original Coding 
*          2        July 2004    Bryon Lawrence    Original Coding
*          3        July 2004    Bryon Lawrence    Original Coding
*          4        July 2004    Bryon Lawrence    Original Coding
*          5        July 2004    Bryon Lawrence    Original Coding
*          6        August 2004  Bryon Lawrence    Original Coding
*          7        August 2004  Bryon Lawrence    Original Coding
*          6        September 2005 Bryon Lawrence  Modified to use PC instead 
*                                                  of PP when PP QC is 'L'
*                                                  or 'C' and the PC value
*                                                  is different
*          4        March 2006   Bryon Lawrence    Modified the
*                                                  get_hourly_ts_rank routine
*                                                  to call compare_tsrank
*                                                  with the correct
*                                                  duration for PP (1001)
*                                                  and PC(0).
*                                                  Also added an additional
*                                                  condition where 
*                                                  if an HourlyPP value
*                                                  has failed the SCC or MSC
*                                                  it will use the PC value
*                                                  unless the PC value
*                                                  is missing.  
*          8        April 2006   Bryon Lawrence    Modified the
*                                                  get_total_hourly_precip
*                                                  routine 
*                                                  to correctly use the
*                                                  report_miss_min_percent
*                                                  flag.  This flag was
*                                                  not working before.
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "BinarySearch.h"
#include "DbmsDefs.h"
#include "get_precip_settings.h"
#include "get_total_PCPP.h"
#include "get_total_precip.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "RawPC.h"
#include "RawPP.h"
#include "IngestFilter.h"
#include "List.h"
#include "time_defs.h"

/* User defined types.  These types are only needed within this  
   this file. */
typedef struct HourlyTS 
{
   Node node ;
   char lid [ LOC_ID_LEN + 1 ] ;
   char ts [ SHEF_TS_LEN + 1 ] ;
} HourlyTS ;

typedef struct RawTS
{
   Node node ;
   char lid [ LOC_ID_LEN + 1 ] ;
   char pe  [ SHEF_PE_LEN + 1 ] ;
   short int dur ;
   char ts [ SHEF_TS_LEN + 1 ] ;
   char extremum [ SHEF_EX_LEN + 1 ] ;
} RawTS ;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   get_ingest_info
* PURPOSE:       Retrieves information from the IngestFilter table for a given
*                lid.  This information is order by TS ascending.  It is 
*                intended for use in TS ranking.  Only rows which have
*                a Master Ingest flag set to 'T' are retrieved.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  char *      lid                  The identifier of the station to
*                                           retrieve the IngestFilter
*                                           information for.
*
* RETURNS:
*   DATA TYPE          DESCRIPTION
*   pIngestFilter *    A linked list of all of the IngestFilter information
*                      pertaining to the station order by ascending TS.
* APIs UTILIZED:
*   NAME               HEADER FILE      DESCRIPTION
*   GetIngestFilter    IngestFilter.h   Contains the DbGen routines for 
*                                       manipulating data in the IngestFilter
*                                       table.
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE      NAME          DESCRIPTION
*   char [ ]       where_clause  Contains the SQL which controls the 
*                                data returned from the IngestFilter. 
*   IngestFilter * pIngestHead   Points to the IngestFilter information 
*                                retrieved from the IHFS database.
*
* DATA FILES AND/OR DATABASE:
* Reads the IngestFilter table in the IHFS database.  A connection to 
* the IHFS database must be established prior to calling this routine.
*
* ERROR HANDLING:
*   None.
*
********************************************************************************
*/

static IngestFilter * get_ingest_info ( const char * lid )
{
   char where_clause [ 200 ] ;
   IngestFilter * pIngestHead = NULL ;

   /* Construct the where clause used to retrieve rows from the
      IngestFilter table. */
   sprintf ( where_clause , "WHERE lid = '%s' "
                            "AND ingest = 'T' "
                            "ORDER by ts ASC" , lid  )  ;

   pIngestHead = GetIngestFilter ( where_clause ) ;

   return pIngestHead ;
}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    get_ts_count_hourly ( file scope)
* PURPOSE:        For a linked list of HourlyPP or HourlyPC data for a given
*                 station, retrieves each TS.  For each TS, it counts the
*                 number of data records there are. 
*
*                 pRecords is dynamically allocated.  The calling routine
*                 is reponsible for freeing this memory.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME      DESCRIPTION/UNITS
*   Input  void *      pHourly   A pointer to the linked list of HourlyPC
*                                or HourlyPP data.
*   Output int **      pRecords  A dynamically created array of integers.
*                                Each element of the array corresponds to
*                                a TS.  It contains the number of data 
*                                records for that TS.
*
* RETURNS:
*   DATA TYPE   DESCRIPTION
*   int         An error return. 
*
* APIs UTILIZED:
*    NAME         HEADER FILE     DESCRIPTION
*    ListNext     List.h          Retrieves the next node in a linked list...
*                                 returns NULL if there are no more nodes. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME              DESCRIPTION
*   char *     lid               The station identifier
*   char *     ts                The station's typesource
*   int        i                 A loop indexing variable
*   int        max_num_ts_groups The maximum number of TS per lid before
*                                the pRecords array needs to be resized.
*   int *      pRecordsTemp      When the pRecords needs to be resized
*                                (there are more TS than max_num_ts_groups)
*                                then pRecordsTemp points to the TS data
*                                until the new pRecords array is created. 
*   int        record_count      The number of data records for a given LID
*                                TS.
*   int        status            Contains return values for error checking.
*   int        ts_group_count    The number of elements in the pRecords array.
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*   0 Means that this routine ran successfully.
*   -1 Means that this routine failed because it could not allocate
*   enough memory for pRecords.
*   
********************************************************************************
*/
static int get_ts_count_hourly ( const void * pHourly ,
                                 int ** pRecords )
{
   const char * lid = NULL ;
   const char * ts = NULL ;

   int i ;
   int max_num_ts_groups = 20 ;
   int * pRecordsTemp = NULL ;
   int record_count = 0 ;
   int status ;
   int ts_group_count = 0 ;

   const struct HourlyTS * pHourlyTS = NULL ; 
                      
   if ( pHourly != NULL )
   {
      pHourlyTS = ( struct HourlyTS * ) pHourly ;

      * pRecords = ( int * ) malloc ( max_num_ts_groups * sizeof ( int ) ) ; 

      if ( * pRecords == NULL )
      {
         return -1 ;
      }

      ++ record_count ;
      ++ ts_group_count ;

      lid = pHourlyTS->lid ;
      ts = pHourlyTS->ts ;

      pHourlyTS = ( HourlyTS * ) ListNext ( & pHourlyTS->node ) ; 

      while ( pHourlyTS != NULL )
      {
         status = strcmp ( lid , pHourlyTS->lid ) ; 

         if ( status == 0 )
         {
            status = strcmp ( ts , pHourlyTS->ts ) ;

            if ( status != 0 )
            {
               if ( ts_group_count + 1 > max_num_ts_groups )
               {
                  pRecordsTemp = * pRecords ;
                  max_num_ts_groups += max_num_ts_groups ;

                  * pRecords = ( int * ) malloc ( max_num_ts_groups *
                                                sizeof ( int ) );

                  if ( * pRecords == NULL )
                  {
                     free ( pRecordsTemp ) ;
                     pRecordsTemp = NULL ;
                     return -1 ;
                  }

                  for ( i = 0 ; i < ts_group_count ; ++ i )
                  {
                     ( * pRecords ) [ i ] = pRecordsTemp [ i ] ;
                  }

		  free ( pRecordsTemp ) ;
                  pRecordsTemp = NULL ; 
               }
               
               ( * pRecords ) [ ts_group_count - 1 ] = record_count ;
               record_count = 1 ;
               ts_group_count ++ ;
               ts = pHourlyTS->ts ;
            }
            else
            {
               ++ record_count ;
            }

            pHourlyTS = ( HourlyTS * ) ListNext ( & pHourlyTS->node ) ; 
         }
         else
         {
            break ;
         }
      }

      /* Add the last group being processed to the pRecords array. */ 
      ( * pRecords ) [ ts_group_count - 1 ] = record_count ;
   }

   return ts_group_count ;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   compare_tsrank   ( file scope )
* PURPOSE:       Given a linked list of IngestFilter records for a single lid,
*                this routine compares the ranks of two TS for a given PE.
*                The return value of this routine indicates which TS has
*                the lower rank value.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME         DESCRIPTION/UNITS
*   Input  char *         ts1          The first of the two TS to be compared.
*   Input  char *         ts2          The second of the two TS to be compared.
*   Input  IngestFilter * pIngestHead  The linked list of IngestFilter records
*                                      for a given station.
*   Input  char *         pe           The PE to compare the two TS for.
*   Input  int            dur          The dur to compate the two TS for.
*   Input  char           extremum     The extremum to compare the two TS for.
*
* RETURNS:
*   DATA TYPE      DESCRIPTION
*   int            -1     :   TS1 has a rank number of lower value than 
*                             TS2 (hence it has a higher rank). 
*                   0     :   The ranks of the two TS are equal.
*                   1     :   TS1 has a higher rank number than that of TS2
*                             (hence it has a lower ranking).
*
* APIs UTILIZED:
*   NAME       HEADER FILE    DESCRIPTION
*   ListNext   List.h         Retrieves the next node in the linked list.
*                             Returns NULL if there are no more nodes.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE      NAME       DESCRIPTION
*   IngestFilter * ingestPtr  Used to walk through the linked list of
*                             IngestFilter records. 
*   int            rank1      The retrieved rank value of ts1.
*   int            rank2      The retrieved rank value of ts2.
*   int            result     The result of the comparison of ts1 and ts2.
*   int            status     Contains the return status of strcmp.  A value
*                             0 means that the two strings are identical.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
static int compare_tsrank ( const char * ts1  , const char * ts2 , 
	       	            const IngestFilter * pIngestHead ,
		            const char * pe , int dur , char extremum )
{
   const IngestFilter * ingestPtr = NULL ;
   int          rank1 , rank2 ;
   int          result ;
   int          status ;

   /* initialize */
   rank1 = rank2 = 0;
   
   ingestPtr = pIngestHead ;

   while ( ingestPtr != NULL )
   {
      status = strcmp ( ingestPtr->pe , pe ) ; 

      if ( ( status == 0 )  && 
           ( dur == ingestPtr->dur ) &&
           ( extremum == ingestPtr->extremum [ 0 ] ) )
      {
         if (strcmp(ingestPtr->ts, ts1) == 0)
            rank1 = ingestPtr->ts_rank;
         if (strcmp(ingestPtr->ts, ts2) == 0)
            rank2 = ingestPtr->ts_rank;

         if ( ( rank1 != 0 ) && ( rank2 != 0 ) ) break ;
      }

      ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
   }

   if (rank1 > rank2)
      result = 1;
   else if (rank1 < rank2)
      result = -1;
   else
      result = 0;

   return(result);
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   get_ts_count_raw  (file scope)
* PURPOSE:       Retrieves the number of TS for one LID and PE.  For
*                each TS it gets a count of the number of data records.
*                The results of this routine are returned to the calling
*                routine in an integer array.  Each element of the array
*                corresponds to a unique TS for which there is data.  
*                Each array element contains the number of records of data 
*                that exist for a particular lid,pe,ts combination.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME        DESCRIPTION/UNITS
*   Input  void *      pRaw        Points to a linked list of
*                                  data.  This can be from any IHFS 
*                                  table which has the same
*                                  schema as the Height table.  In the case
*                                  of this routine, the data will be
*                                  from CurPC, CurPP, RawPC, or RawPP.
*   Output int **      pRecords    Points to a dynamically allocated array
*                                  of integers created in this routine.
*                                  This array contains the number of records
*                                  per unique TS.  
*
* RETURNS:
*   DATA TYPE            DESCRIPTION
*   int                  Return status of this function. See error 
*                        handling function below.
* APIs UTILIZED:
*   NAME          HEADER FILE      DESCRIPTION
*   ListNext      List.h           Retrieves the next node in a linked list.
*                                  If there are no more nodes, then this
*                                  routine returns a NULL value.
* LOCAL DATA ELEMENTS (OPTIONAL):
*  DATA TYPE NAME               DESCRIPTION
*  char *    lid                The id of the station TS groups are being 
*                               counted for.
*  char *    ts                 The name of the current TS being processed.
*  int       i                  A loop indexing variable.
*  int       max_num_ts_groups  The size of pRecords is adjusted, as necessary, 
*                               by multiples of this value.
*  int *     pRecordsTemp       When the current capacity of pRecords is 
*                               exceeded, this pointer temporarity points to
*                               the TS data until pRecords is reallocated 
*                               and the data can be copied into it.
*  int       record_count       The number of data records for a given TS.
*  int       status             Contains error return codes.
*  int       ts_group_count     The number of processed TSs.
*
*  const struct RawTS * pRawTS  The structure pRaw is casted to (since it is
*                               a void *.
*
* DATA FILES AND/OR DATABASE:
* None.
*
* ERROR HANDLING:
*   This routine returns -1 on a malloc failure.  It returns 0 upon 
*   success.
********************************************************************************
*/
static int get_ts_count_raw ( const void * pRaw ,
                              int ** pRecords )
{
   const char * lid = NULL ;
   const char * ts = NULL ;

   int i ;
   int max_num_ts_groups = 20 ;
   int * pRecordsTemp = NULL ;
   int record_count = 0 ;
   int status ;
   int ts_group_count = 0 ;

   const struct RawTS * pRawTS = NULL ; 
                      
   if ( pRaw != NULL )
   {
      pRawTS = ( struct RawTS * ) pRaw ;

      * pRecords = ( int * ) malloc ( max_num_ts_groups * sizeof ( int ) ) ; 

      if ( * pRecords == NULL )
      {
         return -1 ;
      }

      ++ record_count ;
      ++ ts_group_count ;

      lid = pRawTS->lid ;
      ts = pRawTS->ts ;

      pRawTS = ( RawTS * ) ListNext ( & pRawTS->node ) ; 

      while ( pRawTS != NULL )
      {
         status = strcmp ( lid , pRawTS->lid ) ; 

         if ( status == 0 )
         {
            status = strcmp ( ts , pRawTS->ts ) ;

            if ( status != 0 )
            {
               if ( ts_group_count + 1 > max_num_ts_groups )
               {
                  pRecordsTemp = * pRecords ;
                  max_num_ts_groups += max_num_ts_groups ;

                  * pRecords = ( int * ) malloc ( max_num_ts_groups *
                                                sizeof ( int ) );

                  if ( * pRecords == NULL )
                  {
                     free ( pRecordsTemp ) ;
                     pRecordsTemp = NULL ;
                     return -1 ;
                  }

                  for ( i = 0 ; i < ts_group_count ; ++ i )
                  {
                     ( * pRecords ) [ i ] = pRecordsTemp [ i ] ;
                  }

		  free ( pRecordsTemp ) ;
                  pRecordsTemp = NULL ;
               }

               ( * pRecords ) [ ts_group_count - 1 ] = record_count ;
               record_count = 1 ;
               ts_group_count ++ ;
               ts = pRawTS->ts ;
            }
            else
            {
               ++ record_count ;
            }

            pRawTS = ( RawTS * ) ListNext ( & pRawTS->node ) ; 
         }
         else
         {
            break ;
         }
      }

      /* Add the last group being processed to the pRecords array. */ 
      ( * pRecords ) [ ts_group_count - 1 ] = record_count ;
   }

   return ts_group_count ;
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   get_highest_ranking_hourly_ts
* PURPOSE:       For a given LID, PE, this routine determines the highest
*                ranked TS.  This routine is for the data in the HoulryPC
*                and HourlyPP tables.
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
static void * get_highest_ranking_hourly_ts ( const void * pHourlyData ,
                                              const char * pe ,
                                              int num_ts ,
                                              int * pTsIndex ,
                                              int * pTs ,
                                              const IngestFilter * 
					            pIngestHead )
{
   char * pPreviousTS = NULL ;
   int i ;
   int j ;
   int num_rows ;
   int status ;
   struct HourlyTS * pBestHourlyData = NULL ;
   struct HourlyTS * pHourlyTS = NULL ;

   if ( pHourlyData == NULL )
   {
      return NULL ;
   }

   pHourlyTS = ( struct HourlyTS * ) pHourlyData ;

   for ( i = 0 ; i < num_ts ; ++ i )
   {
      if ( i == 0 )
      {
         pPreviousTS = pHourlyTS->ts ;
         pBestHourlyData = pHourlyTS ;
         * pTsIndex = i ;
      }
      else
      {
         pPreviousTS = pHourlyTS->ts ;

         status = strcmp ( pe, "PP" ) ;

         if ( status == 0 )
         {
            status = compare_tsrank ( pHourlyTS->ts , pBestHourlyData->ts ,
                                      pIngestHead , pe , 1001 , 'Z'  ) ;
         }
         else
         {
            status = compare_tsrank ( pHourlyTS->ts , pBestHourlyData->ts ,
                                      pIngestHead , pe , 0 , 'Z'  ) ;
         }

         if ( status < 0 )
         {
            pBestHourlyData = pHourlyTS ;
            * pTsIndex = i ;
         }

      }

      num_rows = pTs [ i ] ;

      for ( j = 0 ; j < num_rows ; ++ j )
      {
         pHourlyTS = ( HourlyTS * ) ListNext ( & pHourlyTS->node ) ;
      }

   }

   return pBestHourlyData ;
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   get_highest_ranking_raw_ts
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
static void * get_highest_ranking_raw_ts ( const void * pRawData ,
                                           const char * pe ,
                                           int num_ts ,
                                           int * pTsIndex ,
                                           int * pTs ,
                                           const IngestFilter * pIngestHead )
{
   char * pPreviousTS = NULL ;
   int i ;
   int j ;
   int num_rows ;
   int status ;
   struct RawTS * pBestRawData = NULL ;
   struct RawTS * pRawTS = NULL ;

   if ( pRawData == NULL )
   {
      return NULL ;
   }

   pRawTS = ( struct RawTS * ) pRawData ;

   for ( i = 0 ; i < num_ts ; ++ i )
   {
      if ( i == 0 )
      {
         pPreviousTS = pRawTS->ts ;
         pBestRawData = pRawTS ;
         * pTsIndex = i ;
      }
      else
      {
         pPreviousTS = pRawTS->ts ;
         status = compare_tsrank ( pRawTS->ts , pBestRawData->ts ,
                                   pIngestHead , pe , pRawTS->dur ,
                                   pRawTS->extremum [ 0 ] ) ;

         if ( status < 0 )
         {
            pBestRawData = pRawTS ;
            * pTsIndex = i ;
         }

      }

      num_rows = pTs [ i ] ;

      for ( j = 0 ; j < num_rows ; ++ j ) ;
      {
         pRawTS = ( struct RawTS * ) ListNext ( & pRawTS->node ) ;
      }
   }

   return pBestRawData ;
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

static Node * advance_list_pointer ( Node * pNode , int count ) 
{
   int i ;
                        
   if ( pNode != NULL )
   {
      for ( i = 0 ; ( i < count ) && ( pNode != NULL ) ; ++ i )
      {
         pNode = ListNext ( pNode ) ;
      } 
   }
   
   return pNode;
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   get_total_hourly_precip
* PURPOSE:
*
*     If there are no flags set, then precipitation will be totaled as follows:
*     derive a total for the current lid, pe, ts combination only.  If
*     precipitation is being totaled for PP and PC, then pick the best
*     precipitation estimate of the two.
*
*     If the PRECIP_TS_BEST flag is set then:
*     derive a precip total for the current lid, picking the TS which produces
*     the best preciptation estimate.  That is, all TSs for the give lid are
*     processed and a best total is chosen from them.
*     Else if the PRECIP_TS_RANK flag is set then:
*     derive a precip total for the current lid, picking the highest ranked TS
*     and producing a total for it.  Use the Ingest Filter table to get the
*     TS ranking.
*
*     If the PRECIP_PE_BEST flag is set, then produce a precip total which 
*     represents the best estimate from the PC and PP amounts.
*     Else if the PRECIP_PE_RANK flag is set, then use PE ranking to determine
*     whether to use PP or PC data.
*
*     If the PRECIP_EXACT_MATCH flag is set, then look for a PP report that 
*     matches the interval withing the exact_match_window fudge factor.
*
*     If the REPORT_MISSING_BELOW_MIN_PERCENT flag is set, then indicate that 
*     a precipitation total value of missing should be reported if 
*     the minimum precipitation coverage requirement is not met.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME           DESCRIPTION/UNITS
*   Input  HourlyPC **   pHourlyPCptr
*   Input  HourlyPP **   pHourlyPPptr
*   Input  time_t        ending_time
*   Input  short int     num_hours
*   Input  float         min_percent
*   Input  unsigned char settings
*   Input  short int     advance
*   Output int *         pc_records
*   Output int *         pp_records
*
* RETURNS:
*   DATA TYPE              DESCRIPTION
*   struct total_precip    Contains the total precipitation value 
*                          and associated PE, TS, coverage, and error
*                          information.
*
* APIs UTILIZED:
*   NAME                           HEADER FILE   DESCRIPTION
*   advance_list_pointer 
*   get_highest_ranking_hourly_ts 
*   get_ingest_info 
*   get_total_hourly_PC 
*   get_total_hourly_PP 
*   get_ts_count_hourly 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME              DESCRIPTION
*  char best_pc_qc               Used to contain the quality code of the
*                                pc which provides the best coverage
*                                of the accumulation interval.
*  char best_pp_qc               Used to contain the quality code of the 
*                                pp ts combination which provides the best
*                                coverage of the accumulation interval.
*  char best_pc_ts [ ]           Contains the ts which provides the best
*                                coverage of the accumulation interval.
*  char best_pp_ts [ ]           Contains the ts which provides the best
*                                coverage of the accumulation interval.
*  const char * lid              The station for which the accumulation
*                                is being computed for.
*  char previous_settings        Keeps track of the settings passed into 
*                                this routine to detect when they have
*                                changed and need to be reprocessed.
*  char temp_pc_qc               Temp variable to contain the quality code
*                                of the pc currently being processed. 
*  char temp_pp_qc               Temp variable to contain the quality code
*                                of the pp currently being processed.
*  float best_pc_amount          The best pc based rainfall amount.
*  float best_pp_amount          The best pp based rainfall amount.
*  float pc_precip_amount        The current pc precip amount being
*                                examined.
*  float pp_precip_amount        The current pp precip amount being 
*                                examined. 
*  HourlyPC * pBestHourlyPC      Points to the group of highest ranked TS
*                                PC reports for a station.  The ranking
*                                information is taken from the 
*                                IngestFilter table in the IHFS database.
*  HourlyPC * pHourlyPC          Used to select nodes in the linked
*                                list of HourlyPC data. 
*  HourlyPP * pBestHourlyPP      Points to the group of highest ranked TS
*                                PP reports for a station.  The ranking
*                                information is read from the IngestFilter
*                                table in the IHFS database.
*  HourlyPP * pHourlyPP          Used to select nodes in the linked list of
*                                HourlyPP data.
*  IngestFilter * pIngestNode    Points to a linked list of IngestFilter
*                                records for a given station. 
*  int best_covered              Keeps track of the best coverage found for 
*                                the accumulation interval.
*  int best_pc_coverage          The best coverage found from PC reports.
*  int best_pp_coverage          The best coverage found from PP reports.
*  int best_reported_missing     The reported missing flag from the best
*                                PP report.
*  int i                         A loop index variable.
*  int k                         A loop index variable.
*  int num_pc_records            The number of pc records to process.
*  int num_pp_records            The number of pp records to process.
*  int num_ts_pc                 The number of unique TS for the given
*                                PC LID combination.
*  int num_ts_pp                 The number of unique TS for the given
*                                PP LID combination. 
*  int pc_seconds_covered        The number of seconds of the accumulation
*                                interval covered by pc data.
*  int pp_seconds_covered        The number of seconds of the accumulation
*                                interval covered by pp data.
*  int * pTsPc                   Points to an array of unique TSs for the LID
*                                and PC PE.  Each element of the array contains
*                                a count of the number of HourlyPC records
*                                for a specific TS.
*  int * pTsPp                   Points to an array of unique TSs for the LID
*                                and PP PE.  Each element of the array contains
*                                a count of the number of HourlyPP records
*                                for a specific TS.
*  int reported_missing          Contains the reported_missing flag for
*                                a PP report for a particular TS.
*  int status                    Used for testing error return codes.
*  int ts_index                  An index into the pTsPp and pTsPc arrays
*                                indicating which TS is the highest ranked.
*  enum PrecipPEmode pe_mode     Indicates how the caller of this routine
*                                wants precipitation totals from PP and PC
*                                sensors handled for a given station.
*  enum PrecipTSmode ts_mode     Indicates how the call of this routine
*                                wants precipitation totals from different
*                                TS for an given LID and PE.
*  short int report_miss_min_percent  The minimum accumulation interval
*                                     coverage below which a precipitaion
*                                     total is reported as missing. 
*  struct total_precip total_precip ; The total precipitation.
*
* DATA FILES AND/OR DATABASE:
*
* Data read from the HourlyPP and HourlyPC tables are supplied to this
* routine.
*
* ERROR HANDLING:
*    This routine will return a total precipitation of missing when a valid
*    precipitation total cannot be computed. The err_struct member of the
*    total_precip structure contains three fields for indicating errors:
*
*    negval:  If not 0, indicates a negative precipitation total was
*             computed.
*    negdiff:  Indicates that a negative difference was encountered.
*    largediff:  Indicates that an excessively large difference.
********************************************************************************
*/

struct total_precip get_total_hourly_precip ( HourlyPC ** pHourlyPCptr ,
                                              HourlyPP ** pHourlyPPptr ,
                                              time_t ending_time ,
		                              short int num_hours ,
                                              float min_percent ,
                                              unsigned char settings ,
	                                      short int advance ,
                                              int * pc_records ,
                                              int * pp_records )
{
   char best_pc_qc ;
   char best_pp_qc ;
   char best_pc_ts [ SHEF_TS_LEN + 1 ] ;
   char best_pp_ts [ SHEF_TS_LEN + 1 ] ;
   const char * lid = NULL ;
   static char previous_settings = ( char ) 0 ;
   char temp_pc_qc = 'Z' ;
   char temp_pp_qc = 'Z' ;
   float best_pc_amount = MISSING_PRECIP ;
   float best_pp_amount = MISSING_PRECIP ;
   float pc_precip_amount ;
   float pp_precip_amount ;
   HourlyPC * pBestHourlyPC = NULL ;
   HourlyPC * pHourlyPC = NULL ;
   HourlyPP * pBestHourlyPP = NULL ;
   HourlyPP * pHourlyPP = NULL ;
   IngestFilter * pIngestNode = NULL ;
   int best_covered = MISSING_PRECIP ;
   int best_pc_coverage = MISSING_PRECIP ;
   int best_pp_coverage = MISSING_PRECIP ;
   int best_reported_missing = 0 ;
   int i ;
   int k ;
   int num_pc_records = 0 ;
   int num_pp_records = 0 ;
   int num_ts_pc = 0 ;
   int num_ts_pp = 0 ;
   int pc_seconds_covered ;
   int pp_seconds_covered ;
   int * pTsPc = NULL ;
   int * pTsPp = NULL ;
   int reported_missing = 0 ;
   int status ;
   int ts_index ;
   static enum PrecipPEmode pe_mode = PrecipPEbest ;
   static enum PrecipTSmode ts_mode = PrecipTSsingle ;
   static short int report_miss_min_percent = 0 ;
   
   struct total_precip total_precip ;

   * pc_records = 0 ;
   * pp_records = 0 ;

   /* Make local copies of the HourlyPC, HourlyPP, and RawPPother pointers.
      The originals will only be modified if the user has specified the
      advance pointer option. */
   if ( pHourlyPCptr != NULL ) pHourlyPC = * pHourlyPCptr ;
   if ( pHourlyPPptr != NULL ) pHourlyPP = * pHourlyPPptr ;

   /* Initialize the total_precip structure. */
   memset ( total_precip.lid, '\0' , LOC_ID_LEN + 1 ) ;
   memset ( total_precip.PE , '\0' , SHEF_PE_LEN + 1 ) ;
   memset ( total_precip.TS , '\0' , SHEF_TS_LEN + 1 ) ;
   memset ( best_pc_ts , '\0' , SHEF_TS_LEN + 1 ) ;
   memset ( best_pp_ts , '\0' , SHEF_TS_LEN + 1 ) ;
   total_precip.value = MISSING_PRECIP ;
   total_precip.summed_flag = 0 ;
   total_precip.hours_covered = 0.0 ;
   total_precip.percent_filled = 0.0 ;
   total_precip.value_indicator = MISSING_CHAR ;
   total_precip.qc = 'Z' ;
   total_precip.err.negval = 0 ;
   total_precip.err.negdiff = 0 ;
   total_precip.err.largediff = 0 ;
   total_precip.reported_missing = 0 ;

   /* Check the PC, PP, and PPother pointers for availability of data. */
   if ( ( pHourlyPC == NULL ) && ( pHourlyPP == NULL ) )
   {
      return total_precip ;  /* Total precip stuct initialized to missing. */
   }

   /* Check the current settings against the settings used when this routine
      was last called.  If they are the same, then do not process the
      settings. */ 
   if ( settings != previous_settings )
   {
      previous_settings = settings ;

      /* Check the settings flag to determine how to process these
       * precipitation reports. */
      report_miss_min_percent = settings & REPORT_MISSING_BELOW_MIN_PERCENT ;

      if ( ( settings & PRECIP_PE_BEST ) == PRECIP_PE_BEST )
      {
         pe_mode = PrecipPEbest ;
      }
      else if ( ( settings & PRECIP_PP ) == PRECIP_PP )
      {
         pe_mode = PrecipPEPP ;
      }
      else if ( ( settings & PRECIP_PC ) == PRECIP_PC )
      {
         pe_mode = PrecipPEPC ;
      }

      if ( ( settings & PRECIP_TS_BEST ) == PRECIP_TS_BEST )
      {
         ts_mode = PrecipTSbest ;
      }
      else if ( ( settings & PRECIP_TS_RANK ) == PRECIP_TS_RANK )
      {
         ts_mode = PrecipTSrank ;
      }
      else
      {
         ts_mode = PrecipTSsingle ;
      }
   }

   /* Check the station id being pointed to by the HourlyPP
      and the HourlyPC pointers.  If these stations are not
      the same, then only process the station which is lexically
      smaller. */
   if ( ( pHourlyPC != NULL ) && ( pHourlyPP != NULL ) )
   {
      status = strcmp ( pHourlyPC->lid , pHourlyPP->lid ) ;

      if ( status == 0 )
      {
         /* The stations are the same. */
         num_ts_pc = get_ts_count_hourly ( pHourlyPC , & pTsPc ) ;
         num_ts_pp = get_ts_count_hourly ( pHourlyPP , & pTsPp ) ;
         strcpy ( total_precip.lid , pHourlyPC->lid ) ; 
      }
      else if ( status < 0 )
      {
         /* The PC lid is the smallest. Use it.  Save the PP lid for later
            since there may be a matching PC lid further down the list. */
         num_ts_pc = get_ts_count_hourly ( pHourlyPC , & pTsPc ) ;
         strcpy ( total_precip.lid , pHourlyPC->lid ) ;
      }
      else
      {
         /* The PP lid is the smallest.  Use it.  Save the PC lid for later
            since there may be a matching PP lid further down the list. */
         num_ts_pp = get_ts_count_hourly ( pHourlyPP , & pTsPp ) ;
         strcpy ( total_precip.lid , pHourlyPP->lid ) ;
      }
   }
   else if ( pHourlyPC == NULL )
   {
      /* There are no PC data. */
      num_ts_pp = get_ts_count_hourly ( pHourlyPP , & pTsPp ) ;
         strcpy ( total_precip.lid , pHourlyPP->lid ) ;
   }
   else
   {
      /* There are no PP data. */
      num_ts_pc = get_ts_count_hourly ( pHourlyPC , & pTsPc ) ;
      strcpy ( total_precip.lid , pHourlyPC->lid ) ;
   }

   /* Retrieve the precip totals. If necessary process multiple TSs, choosing
      either the TS which produces the best precip coverage or the TS which
      Has the highest rank. */

   switch ( ts_mode )
   {
      /* This is the case where the user wants to retrieve the precip total
       * for a single typesource. */
      case PrecipTSsingle :

         if ( num_ts_pc > 0 )
         {
            num_pc_records = pTsPc [ 0 ] ;
            strncpy ( best_pc_ts , pHourlyPC->ts , SHEF_TS_LEN );
            best_pc_amount = get_total_hourly_PC ( pHourlyPC ,
                                                   ending_time ,
			                           num_hours ,
			                           num_pc_records ,
                                                   & best_pc_coverage ,
			                           & best_pc_qc ) ;

         }

	 if ( num_ts_pp > 0 )
         {
            num_pp_records = pTsPp [ 0 ] ;
            strncpy ( best_pp_ts , pHourlyPP->ts , SHEF_TS_LEN ) ;
            best_pp_amount = get_total_hourly_PP ( pHourlyPP ,
                                                   ending_time ,
						   num_hours ,
                                                   num_pp_records ,
                                                   & best_pp_coverage ,
                                                   & best_pp_qc ,
                                                   & best_reported_missing ) ;
         }

         break ;

      case PrecipTSbest :
      

         /* This is the case where the user wants to retrieve 
            the precipitation total for the typesource
            which results in best precipitation total (most coverage)
            for the user-specified interval. */ 
         for ( i = 0 ; i < num_ts_pc ; ++ i )
         {
	        num_pc_records = pTsPc [ i ] ;
            pc_precip_amount = get_total_hourly_PC ( pHourlyPC ,
                                                     ending_time ,
						                             num_hours ,
                                                     num_pc_records ,
                                                     & pc_seconds_covered ,
						                             & temp_pc_qc ) ;
						     

            if ( pc_seconds_covered > best_pc_coverage )
            {
                best_pc_coverage = pc_seconds_covered ;
                best_pc_amount = pc_precip_amount ;
                strncpy ( best_pc_ts , pHourlyPC->ts , SHEF_TS_LEN ) ;
                best_pc_qc = temp_pc_qc ; 
            }
            
            pHourlyPC = ( HourlyPC * ) advance_list_pointer ( 
                                        & pHourlyPC->node, num_pc_records ) ;
     }

	 /* When totaling the precipitation amount from the HourlyPP and
	    the RawPPother tables, make sure that the TSs match. */

    for ( i = 0 ; i < num_ts_pp ; ++ i )
    {
	    num_pp_records = pTsPp [ i ] ;
            pp_precip_amount = get_total_hourly_PP ( pHourlyPP ,
                                                     ending_time ,
						                             num_hours ,
                                                     num_pp_records ,
                                                     & pp_seconds_covered ,
                                                     & temp_pp_qc ,
                                                     & reported_missing ) ;

            if ( pp_seconds_covered > best_pp_coverage )
            {
                best_pp_coverage = pp_seconds_covered ;
                best_pp_amount = pp_precip_amount ;
                strncpy ( best_pp_ts , pHourlyPP->ts , SHEF_TS_LEN ) ;
                best_pp_qc = temp_pp_qc ;
                best_reported_missing = reported_missing ;
            }

            pHourlyPP = ( HourlyPP * ) advance_list_pointer ( 
                                       & pHourlyPP->node , num_pp_records ) ;

         }

	 break ;

      case PrecipTSrank :

         /* This case retrieves the precipitation total for the 
            highest ranking typesource. */
         if ( num_ts_pc > 0 )
         {
            lid = pHourlyPC->lid ;
         }
         else
         {
            lid = pHourlyPP->lid ;
         }

         /* Only perform TS ranking if for the given PE there are multiple
            typesources. */
         pBestHourlyPC =  pHourlyPC ;
         ts_index = 0 ;

         if ( num_ts_pc > 1 )
         {
             pIngestNode = get_ingest_info ( lid ) ;

             if ( pIngestNode != NULL )
             {
                pBestHourlyPC = ( HourlyPC * ) get_highest_ranking_hourly_ts ( 
                                                             pHourlyPC ,
                                                             "PC" ,
                                                             num_ts_pc ,
                                                             & ts_index ,
                                                             pTsPc ,
                                                             pIngestNode ) ;
             }
         }

         /* Get the total precipitation for the highest ranked ts. */
         if ( num_ts_pc > 0 )
         {
            best_pc_amount = get_total_hourly_PC ( pBestHourlyPC ,
                                                   ending_time ,
						   num_hours ,
                                                   pTsPc [ ts_index ] ,
                                                   & pc_seconds_covered ,
                                                   & temp_pc_qc ) ;
            best_pc_coverage = pc_seconds_covered ;
            strcpy ( best_pc_ts , pBestHourlyPC->ts ) ;
            best_pc_qc = temp_pc_qc ;
         }

         pBestHourlyPP = pHourlyPP ;         
         ts_index = 0 ;

         if ( num_ts_pp > 1 )
         {

            if ( pIngestNode == NULL )
            {
               pIngestNode = get_ingest_info ( lid ) ;
            }

            if ( pIngestNode != NULL ) 
            {
               pBestHourlyPP = ( HourlyPP * ) get_highest_ranking_hourly_ts ( 
                                                                pHourlyPP ,
                                                                "PP" ,
                                                                num_ts_pp ,
                                                                & ts_index ,
                                                                pTsPp ,
                                                                pIngestNode ) ;
            }
         }

         if ( num_ts_pp > 0 )
         {
            best_pp_amount = get_total_hourly_PP ( pBestHourlyPP ,
                                                   ending_time ,
						   num_hours ,
                                                   pTsPp [ ts_index ] ,
                                                   & pp_seconds_covered ,
                                                   & temp_pp_qc ,
                                                   & reported_missing ) ;
                
            best_pp_coverage = pc_seconds_covered ;
            strcpy ( best_pp_ts , pBestHourlyPP->ts ) ;
            best_pp_qc = temp_pp_qc ;
            best_reported_missing = reported_missing ;
         }

         if ( pIngestNode != NULL )
         {
            FreeIngestFilter ( pIngestNode ) ;
            pIngestNode = NULL ;
         }
         
         break ;

      default :

         /* This case should not be reached. We should do something here
	    anyway. */
         break ;
   }

   if ( ( num_ts_pc > 0 ) && ( num_ts_pp > 0 ) )
   {
      switch ( pe_mode )
      {
         case PrecipPEbest :

            if ( ( best_pc_amount != MISSING_PRECIP ) ||
                 ( best_pp_amount != MISSING_PRECIP ) )
            {

               /* Select the PE or PC estimate which provides the best
                  precipitation amount estimate. */
               if ( best_pc_coverage > best_pp_coverage )
               {
                  total_precip.value = best_pc_amount ;
                  strcpy ( total_precip.PE , "PC" ) ;
                  strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
                  best_covered = best_pc_coverage ;
                  total_precip.qc = best_pc_qc ;
                  total_precip.summed_flag = 0 ;
               }
               else
               {
                  total_precip.value = best_pp_amount ;
                  strcpy ( total_precip.PE , "PP" ) ;
                  strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
                  best_covered = best_pp_coverage ;
                  total_precip.qc = best_pp_qc ;

                  if ( num_hours > 1 )
                  {
                     total_precip.summed_flag = 1 ;
                  }
                  else
                  {
                     total_precip.summed_flag = 0 ;
                  }

                  total_precip.reported_missing = best_reported_missing ;
               }
            }
 
            break ;

         case PrecipPEPP :

            /* If there are PC and PP values, then use the PP value. */
            if ( ( num_ts_pp > 0 ) && 
                 ( ( best_pp_amount != MISSING_PRECIP ) || 
		   ( best_pp_qc == 'M' ) ||  
                   ( ( best_reported_missing == 1 ) && 
                     ( best_pc_amount == MISSING_PRECIP ) ) ) &&
                 ( ( ( best_pp_qc != 'L' ) && ( best_pp_qc != 'C' ) ) ||
                   ( best_pp_amount == best_pc_amount ) ||
                   ( best_pc_amount == MISSING_PRECIP ) ) )
      
            {
               total_precip.value = best_pp_amount ;
               strcpy ( total_precip.PE , "PP" ) ;
               strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
               best_covered = best_pp_coverage ;
               total_precip.qc = best_pp_qc ;

               if ( num_hours > 1 )
               {
                  total_precip.summed_flag = 1 ;
               }
               else
               {
                  total_precip.summed_flag = 0 ;
               }

               total_precip.reported_missing = best_reported_missing ;
            }
            else
            {
               total_precip.value = best_pc_amount ;
               strcpy ( total_precip.PE , "PC" ) ;
               strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
               best_covered = best_pc_coverage ;
               total_precip.qc = best_pc_qc ;
               total_precip.summed_flag = 0 ;
            }

            break ;

         case PrecipPEPC :

            /* If there are totals from PC and PP, then use the PC value. */
            if ( ( num_ts_pc > 0 ) && ( ( best_pc_amount != MISSING_PRECIP ) ||
	   			     ( best_pc_qc == 'M' ) ) )
            {
               total_precip.value = best_pc_amount ;
               strcpy ( total_precip.PE , "PC" ) ;
               strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
               best_covered = best_pc_coverage ;
               total_precip.qc = best_pc_qc ;
               total_precip.summed_flag = 0 ;
            }
            else
            {
               total_precip.value = best_pp_amount ;
               strcpy ( total_precip.PE , "PP" ) ;
               strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
               best_covered = best_pp_coverage ;
               total_precip.qc = best_pp_qc ;

               if ( num_hours > 1 )
               {
                  total_precip.summed_flag = 1 ;
               }
               else
               {
                  total_precip.summed_flag = 0 ;
               }

               total_precip.reported_missing = best_reported_missing ;
            }

            break ;

         default :
   
            /* This case should not be reached. */
            break ;
      }
   }
   else if ( num_ts_pc > 0 )
   {
      total_precip.value = best_pc_amount ;
      strcpy ( total_precip.PE , "PC" ) ;
      strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
      best_covered = best_pc_coverage ;
      total_precip.qc = best_pc_qc ;
      total_precip.summed_flag = 0 ;
   }
   else
   {
      total_precip.value = best_pp_amount ;
      strcpy ( total_precip.PE , "PP" ) ;
      strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
      best_covered = best_pp_coverage ;
      total_precip.qc = best_pp_qc ;

      if ( num_hours > 1 )
      {
         total_precip.summed_flag = 1 ;
      }
      else
      {
         total_precip.summed_flag = 0 ;
      }

      total_precip.reported_missing = best_reported_missing ;
   }

   if ( total_precip.value != MISSING_PRECIP )
   {
      total_precip.hours_covered = ( float ) best_covered / 
	                           ( float ) SECONDS_PER_HOUR ; 
      total_precip.percent_filled = ( float ) best_covered /
                                    ( float ) ( num_hours * 
                                                SECONDS_PER_HOUR ) ;

      /* Do not allow a percentage filled of greater than 100%. */
      if ( total_precip.percent_filled > 1.0 )
      {
         total_precip.percent_filled = 1.0 ;
      }

      total_precip.value_indicator = OK_CHAR ; 

      /* Set the QC and error flags. */
      if ( report_miss_min_percent > 0)
      {
         if ( total_precip.percent_filled < min_percent )
         {
            total_precip.value = MISSING_PRECIP ;
            total_precip.value_indicator = REJECTED_CHAR ;
         }
      }    

      if ( ( total_precip.value < 0 ) && 
           ( total_precip.value != MISSING_PRECIP ) ) 
      {
         total_precip.value = MISSING_PRECIP ;
         total_precip.err.negdiff = 1 ;
         total_precip.value_indicator = MISSING_CHAR ;
      }

   }
   else
   {
      total_precip.err.negval = 1 ;
   }

   /* If the user has requested it, advance the pointer. This enables
    * colooping, a feature which can save alot of CPU time. */
   if ( advance == 1 )
   {
      switch ( ts_mode )
      {
         case PrecipTSsingle :

            if ( num_ts_pc > 0 )
            {
               * pHourlyPCptr = ( HourlyPC * ) advance_list_pointer ( 
                                 & (* pHourlyPCptr)->node , pTsPc [ 0 ] ) ;
               * pc_records = pTsPc [ 0 ] ;            
            }

            if ( num_ts_pp > 0 )
            {
               * pHourlyPPptr = ( HourlyPP * ) advance_list_pointer ( & (* pHourlyPPptr)->node , pTsPp [ 0 ] ) ;
               * pp_records = pTsPp [ 0 ] ;
            }

	    break ;

         case PrecipTSbest :
         case PrecipTSrank :

            for ( k = 0 ; k < num_ts_pc ; ++ k )
            {
               * pHourlyPCptr = ( HourlyPC * ) advance_list_pointer ( & ( * pHourlyPCptr)->node , pTsPc [ k ] ) ;
               * pc_records += pTsPc [ k ] ; 
            }

            for ( k = 0 ; k < num_ts_pp ; ++ k )
            {
               * pHourlyPPptr =  ( HourlyPP * ) advance_list_pointer ( & (* pHourlyPPptr )->node , pTsPp [ k ] ) ;
               * pp_records += pTsPp [ k ] ;
            }

            break ;

         default :

            /* Should never reach this point. */
	   break ;
      }
   }

   if ( pTsPc != NULL )
   {
      free ( pTsPc ) ;
      pTsPc = NULL ;
   }
  
   if ( pTsPp != NULL )
   {
      free ( pTsPp ) ;
      pTsPp = NULL ;
   }

   return total_precip ;
}
        
/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   get_total_raw_precip
* PURPOSE:       Produces a precipitation total based on the Raw precipitation
* tables.  These include the RawPP, RawPC, CurPP, and CurPC tables.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME               DESCRIPTION/UNITS
*   I/O    RawPC **      pRawPCptr
*   I/O    RawPP **      pRawPPptr
*   Input  time_t        starting_time
*   Input  time_t        ending_time
*   Input  short int     exact_match_window
*   Input  float         min_percent
*   Input  unsigned char settings
*   Input  short int     advance
*   Output int *         pc_records
*   Output int *         pp_records 
*
* RETURNS:
*   DATA TYPE              DESCRIPTION
*   struct total_precip    Contains the total precipitation value as well
*                          as the PE and TS it was derived from.  Also 
*                          includes error information.
*                           
* APIs UTILIZED:
*   NAME                       HEADER FILE    DESCRIPTION
*   advance_list_pointer 
*   get_highest_ranking_raw_ts 
*   get_ingest_info 
*   get_total_raw_PC 
*   get_total_raw_PP 
*   get_ts_count_raw 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME               DESCRIPTION
*   char best_pc_ts [ ]           The typesource associated with the best
*                                 pc precipitation estimate.
*   char best_pp_ts [ ]           The typesource associated with the 
*                                 best pp precipitation estimate.
*   const char * lid              The id of the station the precipitation
*                                 total is being computed for.
*   char previous_settings        Used to track changes in the precipitation
*                                 totaling settings between successive calls
*                                 to this function.
*   float best_pc_amount          The best pc based precipitation total.
*   float best_pp_amount          The best pp based precipitation total.
*   float pc_precip_amount        The current pc based precipitation amount
*                                 being processed.
*   float pp_precip_amount        The current pp based precipitation amount
*                                 being processed.
*   IngestFilter * pIngestNode    A pointer to the retrieved rows in the 
*                                 IngestFilter table for the station 
*                                 precipitation totals are being processed for.
*   int best_covered              The best coverage of the precipitation
*                                 accumulation interval.
*   int best_pc_coverage          The best pc based coverage of the
*                                 precipitation accumulation interval.
*   int best_pp_coverage          The best pp based coverage of the 
*                                 precipitation accumulation interval.
*   int i                         A loop index variable.
*   int k                         A loop index variable.
*   int num_pc_records            The number of pc records for a given
*                                 TS.
*   int num_pp_records            The number of pp records for a given
*                                 TS. 
*   int num_ts_pc                 The number of unique PC TS for the lid.
*   int num_ts_pp                 The number of unique PP TS for the lid. 
*   int pc_seconds_covered        The number of seconds in the accumulation
*                                 interval covered by pc data.
*   int pp_seconds_covered        The number of seconds in the accumulation
*                                 interval covered by pp data.
*   int * pTsPc                   Each element in this array corresponds to 
*                                 a unique typesource for the lid for PC. Each
*                                 element contains the number of records with 
*                                 that TS.
*   int * pTsPp                   Each element in this array corresponds to
*                                 a unique typesource for the lid for PP.  Each
*                                 element contains the number of records with
*                                 that TS.
*   int status                    Used to test return codes.
*   int summed_flag               Indicates if a PP based accumulation is the
*                                 result of summing several PP reports together
*                                 or if a single PP report was used to obtain
*                                 the total. 
*   int ts_index                  Index into the pTsPc or pTsPp array.  It 
*                                 indicates the TS with the highest rank
*                                 as defined in the IngestFilter table.
*   enum PrecipPEmode pe_mode     How the user wants to handle the case
*                                 where a single station has both a PC and
*                                 a PP based rainfall total.
*   enum PrecipTSmode ts_mode     How the user wants to handle the case where 
*                                 a PE (PC or PP) has multiple typesources
*                                 yielding multiple precipitation accumulations.
*   RawPC * pBestRawPC            Points to the group of PC reports which have
*                                 the highest ranking typesource.
*   RawPP * pBestRawPP            Points to the group of PP reports which have
*                                 the highest ranking typesource.
*   RawPP * pRawPP                A temporary pointer to the linked list
*                                 PP reports. 
*   short int exact_match         Indicates that PP based precipitation 
*                                 totals should not sum PP reports.
*   short int report_miss_min_percent Indicates the minimum coverage of 
*                                     an accumulation interval below
*                                     which a precipitation total is 
*                                     reported as missing.
*
* DATA FILES AND/OR DATABASE:
* Requires the IngestFilter table in the IHFS database.  The database
* must be opened prior to calling this routine.
*
* ERROR HANDLING:
*    This routine will return a total precipitation of missing when a valid
*    precipitation total cannot be computed. The err_struct member of the
*    total_precip structure contains three fields for indicating errors:
*
*    negval:  If not 0, indicates a negative precipitation total was
*             computed.
*    negdiff:  If not 0, indicates that a negative difference was encountered.
*    largediff:  If not 0, indicates that an excessively large difference.
********************************************************************************
*/

struct total_precip get_total_raw_precip ( RawPC ** pRawPCptr ,
                                           RawPP ** pRawPPptr ,
                                           time_t starting_time ,
                                           time_t ending_time ,
                                           short int ending_time_match ,
                                           float min_percent ,
                                           unsigned char settings ,
                                           short int advance ,
                                           int * pc_records ,
                                           int * pp_records )
{
   char best_pc_ts [ SHEF_TS_LEN + 1 ] ;
   char best_pp_ts [ SHEF_TS_LEN + 1 ] ;
   const char * lid = NULL ;
   static char previous_settings = ( char ) 0 ;
   float best_pc_amount = MISSING_PRECIP ;
   float best_pp_amount = MISSING_PRECIP ;
   float pc_precip_amount ;
   float pp_precip_amount ;
   IngestFilter * pIngestNode = NULL ;
   int best_covered = MISSING_PRECIP ;
   int best_pc_coverage = MISSING_PRECIP ;
   int best_pp_coverage = MISSING_PRECIP ;
   int i ;
   int k ;
   int num_pc_records = 0 ;
   int num_pp_records = 0  ;
   int num_ts_pc = 0 ;
   int num_ts_pp = 0 ;
   int pc_seconds_covered ;
   int pp_seconds_covered ;
   int * pTsPc = NULL ;
   int * pTsPp = NULL ;
   int status ;
   int summed_flag = 0 ;
   static int sum_pc_reports = DEFAULT_SUM_PC_REPORTS_VALUE;
   int ts_index ;
   static enum PrecipPEmode pe_mode = PrecipPEbest ;
   static enum PrecipTSmode ts_mode = PrecipTSsingle ;
   RawPC * pBestRawPC = NULL ;
   RawPC * pRawPC = NULL ;
   RawPP * pBestRawPP = NULL ;
   RawPP * pRawPP = NULL ;
   static int first = 1;
   static short int no_accum_flag = 0 ;
   static short int report_miss_min_percent = 0 ;
   time_t best_match_time = 0 ;
   time_t match_time ; 

   /* Make local copies of the HourlyPC, HourlyPP, and RawPPother pointers.
      The originals will only be modified if the user has specified the
      pointer advance option. */
   if ( pRawPCptr != NULL ) pRawPC = * pRawPCptr ;
   if ( pRawPPptr != NULL ) pRawPP = * pRawPPptr ;
 
   struct total_precip total_precip ;

   * pc_records = 0 ;
   * pp_records = 0 ;

   /* Initialize the total_precip structure. */
   memset ( total_precip.PE , '\0' , SHEF_PE_LEN + 1 ) ;
   memset ( total_precip.TS , '\0' , SHEF_TS_LEN + 1 ) ;
   total_precip.value = MISSING_PRECIP ;
   total_precip.summed_flag = 1 ;
   total_precip.hours_covered = 0.0 ;
   total_precip.percent_filled = 0.0 ;
   total_precip.value_indicator = MISSING_CHAR ;
   total_precip.qc = 'Z' ;  /* This doesn't really apply to raw 
                               precip totals. */
   total_precip.err.negval = 0 ;
   total_precip.err.negdiff = 0 ;
   total_precip.err.largediff = 0 ;

   total_precip.match_time = 0 ;

   /* Check the PC, PP, and PPother pointers for availability of data. */
   if ( ( pRawPC == NULL ) && ( pRawPP == NULL ) ) 
   {
      return total_precip ;
   }

   /* Get the algorithm to use for totaling PC precipitation amounts. */
   if ( first == 1 )
   {
      first = 0;
      sum_pc_reports = check_sum_pc_reports ( );
   }
   
   /* Check the current settings against the settings used when this routine
    * was last called.  If they are the same, then do not process the settings.
    * just use the previous settings.  Otherwise, this is either the first
    * time this routne has been called or one of the settings has changed. */
   if ( settings != previous_settings )
   {
      previous_settings = settings ;

      /* Check the settings flag to determine how to process these
       * precipitation reports. */
      no_accum_flag = settings & PRECIP_NO_ACCUM ;
      report_miss_min_percent = settings & REPORT_MISSING_BELOW_MIN_PERCENT ;

      if ( ( settings & PRECIP_PE_BEST ) == PRECIP_PE_BEST )
      {
         pe_mode = PrecipPEbest ;
      }
      else if ( ( settings & PRECIP_PP ) == PRECIP_PP )
      {
         pe_mode = PrecipPEPP ;
      }
      else if ( ( settings & PRECIP_PC ) == PRECIP_PC )
      {
         pe_mode = PrecipPEPC ;
      }

      if ( ( settings & PRECIP_TS_BEST ) == PRECIP_TS_BEST )
      {
         ts_mode = PrecipTSbest ;
      }
      else if ( ( settings & PRECIP_TS_RANK ) == PRECIP_TS_RANK )
      {
         ts_mode = PrecipTSrank ;
      }
      else
      {
         ts_mode = PrecipTSsingle ;
      }
   }

   /* Check the station id being pointed to by the pRawPC and pRawPP
      pointers.  If these stations are not the same, then only process
      the station which is lexically smaller. */
   if ( ( pRawPC != NULL ) && ( pRawPP != NULL ) )
   {
      status = strcmp ( pRawPC->lid , pRawPP->lid ) ;
     
      if ( status == 0 )
      {
         /* The stations are the same. */
         num_ts_pc = get_ts_count_raw ( pRawPC , & pTsPc ) ;
         num_ts_pp = get_ts_count_raw ( pRawPP , & pTsPp ) ;
         strcpy ( total_precip.lid , pRawPC->lid ) ;
      }
      else if ( status < 0 )
      {
         /* The PC lid is the smallest.  Use it.  Save the PP lid for later
            since there may be a matching PC lid further down the list. */
         num_ts_pc = get_ts_count_raw ( pRawPC , & pTsPc ) ;
         strcpy ( total_precip.lid , pRawPC->lid ) ;
      }
      else
      {
         /* The PP lid is the smallest.  Use it.  Save the PC lid for later
            since there may be a matching PP lid further down the list. */
         num_ts_pp = get_ts_count_raw ( pRawPP , &pTsPp ) ;
         strcpy ( total_precip.lid , pRawPP->lid ) ; 
      }
   }
   else if ( pRawPC == NULL )
   {
      /* There are no PC data. */
         num_ts_pp = get_ts_count_raw ( pRawPP , &pTsPp ) ;
         strcpy ( total_precip.lid , pRawPP->lid ) ; 
   }
   else
   {
      /* There are no PP data. */
         num_ts_pc = get_ts_count_raw ( pRawPC , & pTsPc ) ;
         strcpy ( total_precip.lid , pRawPC->lid ) ;
   }

   /* Retrieve the precip totals. If necessary process multiple TSs, choosing
      either the TS which produces the best precip coverage or the TS which
      has the highest rank. */

   switch ( ts_mode )
   {
      case PrecipTSsingle :

         if ( num_ts_pc > 0 )
         {
            num_pc_records = pTsPc [ 0 ] ;

            best_pc_amount = get_total_raw_PC (   pRawPC ,
                                                  starting_time ,
                                                  ending_time ,
         			                  num_pc_records ,
						  sum_pc_reports ,
                                                  & best_pc_coverage ) ;

            strncpy ( best_pc_ts , pRawPC->ts , SHEF_TS_LEN );
         }

	 if ( num_ts_pp > 0 )
         {
            best_match_time = 0 ;
            num_pp_records = pTsPp [ 0 ] ;
            best_pp_amount = get_total_raw_PP ( pRawPP ,
                                                starting_time ,
                                                ending_time ,
                                                num_pp_records ,
	    	   		                no_accum_flag ,
		                                ending_time_match ,
						& best_match_time ,
                                                & best_pp_coverage ,
			                        & summed_flag ) ;
	                

            strncpy ( best_pp_ts , pRawPP->ts , SHEF_TS_LEN ) ;
         }

         break ;

      case PrecipTSbest :

         for ( i = 0 ; i < num_ts_pc ; ++ i )
         {
	    num_pc_records = pTsPc [ i ] ;
            pc_precip_amount = get_total_raw_PC ( pRawPC ,
                                                  starting_time ,
                                                  ending_time ,
                                                  num_pc_records ,
						  sum_pc_reports , 
                                                  & pc_seconds_covered ) ;

            if ( pc_seconds_covered > best_pc_coverage )
            {
                best_pc_coverage = pc_seconds_covered ;
                best_pc_amount = pc_precip_amount ;
                strncpy ( best_pc_ts , pRawPC->ts , SHEF_TS_LEN ) ;
            }

            pRawPC = ( RawPC * ) advance_list_pointer ( & pRawPC->node , 
                                                        num_pc_records ) ;

         }
	 
         for ( i = 0 ; i < num_ts_pp ; ++ i )
         {
	    num_pp_records = pTsPp [ i ] ;

            pp_precip_amount = get_total_raw_PP ( pRawPP ,
                                                  starting_time ,
                                                  ending_time ,
                                                  num_pp_records ,
                                                  no_accum_flag ,
                                                  ending_time_match ,
						  & match_time ,
                                                  & pp_seconds_covered ,
			                          & summed_flag ) ;

            if ( pp_seconds_covered > best_pp_coverage )
            {
                best_pp_coverage = pp_seconds_covered ;
                best_pp_amount = pp_precip_amount ;
                best_match_time = match_time ;
                strncpy ( best_pp_ts , pRawPP->ts , SHEF_TS_LEN ) ;
            }

            pRawPP = ( RawPP * ) advance_list_pointer ( & pRawPP->node , 
                                                          num_pp_records ) ;
            
            if ( summed_flag == 0 )
            {
               break ; /* If an exact match has been found, then 
                          stop searching for the best PP rain amount. */ 
            }
         }

         break ;

      case PrecipTSrank :

         /* Get the TS rank info from the IngestFilter table for this
            station. */
         if ( num_ts_pc > 0 )
         {
            lid = pRawPC->lid ;
         }
         else
         {
            lid = pRawPP->lid ;
         }

	 /* Only perform TS ranking if there there are precipitation
            data from multiple PC/TS and PP/TS combinations. */

         /* Retrieve the highest ranking PC typesource. */
         pBestRawPC = pRawPC ;
         ts_index = 0 ;

         if ( num_ts_pc > 1 )
         {
            pIngestNode = get_ingest_info ( lid ) ;

            if ( pIngestNode != NULL )
            {
               pBestRawPC = ( RawPC * ) get_highest_ranking_raw_ts ( 
                                                                pRawPC ,
                                                                "PC" ,
                                                                num_ts_pc ,
                                                                & ts_index ,
                                                                pTsPc ,
                                                                pIngestNode ) ;

            }

         }

         /* Get the total precipitation for the highest ranked ts. */
         if ( num_ts_pc > 0 )
         {
            best_pc_amount = get_total_raw_PC ( pBestRawPC ,
                                                starting_time ,
                                                ending_time ,
                                                pTsPc [ ts_index ] ,
                                                sum_pc_reports ,
                                                & pc_seconds_covered ) ;
            best_pc_coverage = pc_seconds_covered ;
            strcpy ( best_pc_ts , pBestRawPC->ts ) ;
         }

         pBestRawPP = pRawPP ; 
         ts_index = 0 ;

         if ( num_ts_pp > 1 )
         {

            if ( pIngestNode == NULL )
            {
               pIngestNode = get_ingest_info ( lid ) ;
            }

            if ( pIngestNode != NULL )
            {
                  
               /* Retrieve the highest ranking PP typesource. */
               pBestRawPP = ( RawPP * ) get_highest_ranking_raw_ts ( 
                                                                pRawPP ,
                                                                "PP" ,
                                                                num_ts_pp ,
                                                                & ts_index ,
                                                                pTsPp ,
                                                                pIngestNode ) ;
            }
         }

         if ( num_ts_pp > 0 )
         {
             best_pp_amount = get_total_raw_PP ( pBestRawPP ,
                                                 starting_time ,
                                                 ending_time ,
                                                 pTsPp [ ts_index ]  ,
                                                 no_accum_flag ,
                                                 ending_time_match ,
                                                 & best_match_time ,
                                                 & pp_seconds_covered ,
			                         & summed_flag ) ;
                
         best_pp_coverage = pc_seconds_covered ;
	     strcpy ( best_pp_ts , pBestRawPP->ts ) ;
         }

         if ( pIngestNode != NULL )
         {
            FreeIngestFilter ( pIngestNode ) ;
            pIngestNode = NULL ;
         }
         
         break ;

      default :

         /* This case should not be reached. We should do something here
	    anyway. */
         break ;
   }

   if ( ( num_ts_pp > 0 ) && ( num_ts_pc > 0 ) )
   {
      switch ( pe_mode )
      {
         case PrecipPEbest :

            if ( ( best_pc_amount != MISSING_PRECIP ) ||
                 ( best_pp_amount != MISSING_PRECIP ) )
            {
               /* Select the PE or PC estimate which provides the best
                  precipitation amount estimate. */
               if ( best_pc_coverage > best_pp_coverage )
               {
                  total_precip.value = best_pc_amount ;
                  strcpy ( total_precip.PE , "PC" ) ;
                  strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
                  best_covered = best_pc_coverage ;

                  /* The summed flag is not applicable to 
                     PC data. */
                  total_precip.summed_flag = 0 ;
               }
               else
               {
                  total_precip.value = best_pp_amount ;
                  strcpy ( total_precip.PE , "PP" ) ;
                  strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
                  best_covered = best_pp_coverage ;
                  total_precip.summed_flag = summed_flag ;
                  total_precip.match_time = best_match_time ;
               }
            }
 
            break ;

         case PrecipPEPP :

            /* If there are PC and PP values, then use the PP value. */
            if ( ( num_ts_pp > 0 ) && ( best_pp_amount != MISSING_PRECIP ) )
            {
               total_precip.value = best_pp_amount ;
               strcpy ( total_precip.PE , "PP" ) ;
               strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
               best_covered = best_pp_coverage ;
               total_precip.summed_flag = summed_flag ;
               total_precip.match_time = best_match_time ;
              
            }
            else
            {
               total_precip.value = best_pc_amount ;
               strcpy ( total_precip.PE , "PC" ) ;
               strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
               best_covered = best_pc_coverage ;
               total_precip.summed_flag = 0 ;
            }

            break ;

         case PrecipPEPC :

            /* If there are totals from PC and PP, then use the PC value. */
            if ( ( num_ts_pc ) > 0 && ( best_pc_amount != MISSING_PRECIP ) )
            {
               total_precip.value = best_pc_amount ;
               strcpy ( total_precip.PE , "PC" ) ;
               strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
               best_covered = best_pc_coverage ;

               /* The summed flag is not applicable to PC data. */
               total_precip.summed_flag = 0 ;
            }
            else
            {
               total_precip.value = best_pp_amount ;
               strcpy ( total_precip.PE , "PP" ) ;
               strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
               best_covered = best_pp_coverage ;
               total_precip.summed_flag = summed_flag ;
               total_precip.match_time = best_match_time ;
            }

            break ;

         default :
   
            /* This case should not be reached. */
            break ;
      }

   }
   else if (  num_ts_pp > 0 )
   {
      total_precip.value = best_pp_amount ;
      strcpy ( total_precip.PE , "PP" ) ;
      strncpy ( total_precip.TS , best_pp_ts , SHEF_PE_LEN ) ;
      best_covered = best_pp_coverage ;
      total_precip.summed_flag = summed_flag ;
      total_precip.match_time = best_match_time ;
   }
   else
   {
      total_precip.value = best_pc_amount ;
      strcpy ( total_precip.PE , "PC" ) ;
      strncpy ( total_precip.TS , best_pc_ts , SHEF_TS_LEN ) ;
      best_covered = best_pc_coverage ;
      total_precip.summed_flag = 0 ;
   }

   if ( total_precip.value != MISSING_PRECIP )
   {
      total_precip.hours_covered = ( float ) best_covered / 
	                           ( float ) SECONDS_PER_HOUR ; 
      total_precip.percent_filled = ( float ) best_covered /
                                    ( float ) ( ending_time - starting_time) ; 

      /* Do no allow for a percent filled of greater than 100%. */
      if ( total_precip.percent_filled > 1.0 )
      {
         total_precip.percent_filled = 1.0 ;
      }

      total_precip.value_indicator = OK_CHAR ; 

      /* Set the QC and error flags. */
      if ( report_miss_min_percent > 0 )
      {
         if ( total_precip.percent_filled < min_percent )
         {
            total_precip.value = MISSING_PRECIP ;
            total_precip.value_indicator = REJECTED_CHAR ;
         }
      }    

      if ( ( total_precip.value < 0 ) && 
           ( total_precip.value != MISSING_PRECIP ) )
      {
         total_precip.value = MISSING_PRECIP ;
         total_precip.err.negdiff = 1 ;
         total_precip.value_indicator = MISSING_CHAR ;
      }

   }
   else
   {
      total_precip.err.negval = 1 ;
   }

   /* If the user has requested it, advance the pointer. This enables
    * colooping, a feature which can save alot of CPU time. */
   if ( advance == 1 )
   {
      switch ( ts_mode )
      {
         case PrecipTSsingle :

            if ( num_ts_pc > 0 )
            {
               *pRawPCptr = ( RawPC * ) advance_list_pointer ( 
                                        & (*pRawPCptr)->node , pTsPc [ 0 ] ) ;
               * pc_records = pTsPc [ 0 ] ;
            }

            if ( num_ts_pp > 0 )
            {
               *pRawPPptr = ( RawPP * ) advance_list_pointer ( & (*pRawPPptr)->node , pTsPp [ 0 ] ) ;
               * pp_records = pTsPp [ 0 ] ;
            }

	    break ;

         case PrecipTSbest :
         case PrecipTSrank :

            for ( k = 0 ; k < num_ts_pc ; ++ k )
            {
               *pRawPCptr = ( RawPC * ) advance_list_pointer ( 
                                       & ( *pRawPCptr)->node , pTsPc [ k ] ) ;
               * pc_records += pTsPc [ k ] ;
            }

            for ( k = 0 ; k < num_ts_pp ; ++ k )
            {
               *pRawPPptr = ( RawPP * ) advance_list_pointer ( 
                                       & (*pRawPPptr)->node , pTsPp [ k ] ) ;
               * pp_records += pTsPp [ k ] ;
            }

            break ;

         default :

            /* Should never reach this point. */
            break ;
      }  
   }


   if ( pTsPc != NULL )
   {
      free ( pTsPc ) ;
      pTsPc = NULL ;
   }
  
   if ( pTsPp != NULL )
   {
      free ( pTsPp ) ;
      pTsPp = NULL ;
   }

   return total_precip ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


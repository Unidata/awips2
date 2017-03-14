#ifndef POINTCONTROL_REPORT_H
#define POINTCONTROL_REPORT_H


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "DbmsDefs.h"
#include "List.h"

/* structure for linked list of data Reports */

#define REPORTLIST_DISPLAYCLASS_NAME_LEN 11
#define THREAT_MISSING_DATA        'Z'
#define THREAT_MISSING_STAGE       'M'
#define THREAT_NONE                'G'
#define THREAT_ACTION              'Y'
#define THREAT_FLOOD               'R'
#define REPORTLIST_STATIONNAME_LEN 20

struct PointPixel
{
   long x ;
   long y ;
} ;

typedef struct
{
   Node node;
   
   char    lid  [ LOC_ID_LEN + 1 ] ;
   char    name [ REPORTLIST_STATIONNAME_LEN + 1 ] ; 
   double  elevation;  
   
   char    pe [ SHEF_PE_LEN + 1 ] ;
   long    dur;
   char    ts [ SHEF_TS_LEN + 1 ] ;
   char    extremum [ SHEF_EX_LEN + 1 ] ;
   float   probability;
   
   char    shef_qual_code[SHEF_QC_LEN + 1];
   long    quality_code;
   
   double  value;
   double  value2;
   time_t  validtime;
   time_t  basistime;
   
   char	   use;  /* indicates whether to show this value or filter it */	   

   char threat_index ; /* This is the riverstatus threat index. It may
                          have the following values:
                          "R" - MOFO stage/flow is at or above flood 
                                stage/flow. Red.
                          "Y" - MOFO stage/flow is at or above action 
                                stage/flow. Yellow.
                          "G" - MOFO stage/flow is below action stage/flow.
                                Green.
                          "M" - MOFO stage/flow is available, but action or
                                flood stage is missing.
                          "Z" - Threat is no available. Missing. */
   double latitude ;  /* The latitude of the station point. Added
                         on March 26, 2003 - BAL. */
   double longitude ; /* The longitude of the station point.
                         Added on March 26, 2003 - BAL. */

   char disp_class [ REPORTLIST_DISPLAYCLASS_NAME_LEN + 1 ] ; 
                      /* The display class of this station. Added
                         on March 26, 2003 - BAL. */
   struct PointPixel  pos ; /* This user may store the pixel values
                               corresponding to the latitude/longitude
                               of this station here.*/
   
   /*
    *  8/2/2006 Chip Gobs
    *  Used for implementing the ability to shift the apparent location of a station for
    *  decluttering purposes.
    * */
   int x_shift;
   int y_shift;
   
   List	   list;
   
   

} ReportList;



#endif


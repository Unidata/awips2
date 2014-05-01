/*******************************************************************
   cat_and_product_defs.h
   
   PURPOSE
   To serve as include file for parameters related to the 
   category information.

********************************************************************/

#ifndef CAT_AND_PRODUCT_DEFS_H
#define CAT_AND_PRODUCT_DEFS_H


/* define constants used for determining forecast trend phrases */

#define RECTYPE_LEN  3
#define MAXNUM_OF_TRENDPHRASE 50 
#define BE_FLAT_THRESHOLD 0.1
#define NUM_OF_BASEPOINTS 2

#define RISE_STEADY  10
#define RISE_ABOVEWS 11
#define RISE_ABOVEFS 12
#define RISE_CONT    13
#define RISE_CREST   14

#define FLUCTUATE_NEAR          20
#define FALL_STEADY             30
#define FALL_BELOWFS            31
#define FALL_BELOWWS            32
#define TRENDPHRASE_BASE_INDEX  50
#define TWOPOINTS_CASE          60
#define THREEPOINTS_CASE        61
#define FOURPOINTS_CASE         62


/* constants for forecast trend index at weir points */

#define WEIR_NO_OVERFLOW       40
#define WEIR_SLIGHT_OVERFLOW   41
#define WEIR_BEGIN_OVERFLOW    42
#define WEIR_OVERFLOW_INC      43
#define WEIR_PRESENT_OVERFLOW  44
#define WEIR_REMAIN_OVERFLOW   45
#define WEIR_OVERFLOW_DEC      46
#define WEIR_END_OVERFLOW      47
#define WEIR_STOP_OVERFLOW     48
#define WEIR_RENEW_OVERFLOW    49

/* constants for normal forecast point, weir and fremont */

#define NORMAL 0
#define WEIR   1
#define FREMONT 2
#define TIDAL 3

#define TIDAL_THRESHOLD 7


/* define the flood categories; their consecutive order is important */

#define NULLCAT   -1 /* also apply on cat for MON threshold */
#define MAX_CAT    5

#define NONFLOOD   0 /* also means below monitor stage for MON threshold */
#define MINOR      1 /* also means above monitor stage for MON threshold */
#define MODERATE   2
#define MAJOR      3
#define RECORD     4


/* define the stage tendencies */

#define RISE      +1
#define UNCHANGED  0
#define FALL      -1

/* define the product ids; their consecutive order is important */

#define OTHER_PROD    0
#define RVS           1                
#define FLS           2                
#define FLW           3 
               

/* define the different reasons for selecting the products;
   the unique values themselves are arbitrary and for internal use only */

#define RVS_NO_DATA		 10
#define RVS_NO_FLOODING          11

#define FLS_CONTINUED_FLOODING   20
#define FLS_EXPIRED_FLOODING     21
#define FLS_ENDED_FLOODING       22
#define FLS_GROUP_IN_FLS         23

#define FLW_NEW_FLOODING         30
#define FLW_INCREASED_FLOODING   31
#define FLW_GROUP_IN_FLW         32



/* product section index. Note: should not change the values because
   they are used as index in array */

#define HEADER           0    
#define BASIS            1
#define SUMMARY          2
#define TABULAR          3
#define POINT_SPECIFIC   4
#define DATA_ROUNDUP          41
#define IMPACT_STATEMENT      42
#define HISTORICAL_COMPARISON 43
#define CALL_TO_ACTION   5
#define HEADLINE         6


#endif

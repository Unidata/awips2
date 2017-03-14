#ifndef AREAL_PRODUCT_SETTINGS_H
#define AREAL_PRODUCT_SETTINGS_H


#define SOURCE_ID_LEN 32

#include "DbmsDefs.h"
#include <time.h>

#define MAXHRS_FFG_LOOKBACK  168
#define NUM_FFG_DURS 5

#define NUM_PTPRECIP_DURS 7

#include "ArealDataAttr.h"    /* for enumerated variables ---- */


/* structures for product descriptors and specifiers --------------- */

typedef struct ArealProductTypeDescriptor
{
   ArealDisplayMode	mode;   
   PrecipType		precipType;   
   ResolutionLevel	resolutionLevel;
   
}  ArealProductTypeDescriptor;


typedef struct ArealProductSpecifier
{
   /* time range of the data, duration is in seconds.
      for ffg data, the endtime is the latest allowable
      valid time */
   
   time_t		duration;     
   time_t		endTime;  
   
   /* the source is usually a radar id */
   
   char 		sourceId[SOURCE_ID_LEN + 1]; 
   
   
   /* have an indicator of the data status; this is
      helpful in knowing which grid (stage1, stage2)
      data is either all zeroes for the entire grid,
      or is non-zero */
   
   ArealDataStatus	dataStatus;
      
} ArealProductSpecifier;




/* structure for product preset definition ------------------------- */

typedef struct ArealProductControl
{
     char		hourMode;
     int		hour;
     int		duration;
     int		maxHoursBack;  /* used for absolute hrs only */
} ArealProductControl;



/* structure for areal product data, includes precip and ffg data ---- */

typedef struct ArealData
{
   char         lid[LOC_ID_LEN + 1];
   float        value;
   time_t       validtime;
   int		isOk;
} ArealData;


typedef struct ArealProduct
{
   ArealProductTypeDescriptor	descr;
   ArealProductSpecifier 	spec;
   
   /* precip */
   
   int 		precip_status;
   float 	*precip_grid;
   ArealData 	*precip_data;
   long 	precip_cnt;
   
   
   /* ffg */ 
   
   int 		ffg_status;
   float 	*ffg_grid;
   ArealData 	*ffg_data;
   long 	ffg_cnt;  
} ArealProduct;



#endif

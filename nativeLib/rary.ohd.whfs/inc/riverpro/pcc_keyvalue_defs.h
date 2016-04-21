/*******************************************************************
   pcc_keyvalue_defs.h
   
   PURPOSE
   Serves as include file for variable names supported in the 
   product content control file. 
   
   NOTES
   These values identify the options; the values themselves are somewhat arbitrary.
     
   ********************************************************************/

#ifndef PCC_KEYVALUE_DEFS_H
#define PCC_KEYVALUE_DEFS_H
		


#define ORDER_DEFAULT        300    /* forecast point order options */
#define ORDER_GROUP_DEFAULT  301
#define ORDER_GROUP_FP       302 
#define ORDER_BY_ACTION      303


#define CUR_OBSSTAGE         400    /* stage search options */   
#define MAX_FCSTSTAGE        401      
#define MAX_STAGE            402

#define RECENT_IN_WINDOWS     600   /* impact and crest search type options */
#define CLOSEST_IN_WINDOWS    601
#define RECENT_IN_STGWINDOW   602
#define CLOSEST_IN_STGWINDOW  603
#define HIGHEST_IN_STGWINDOW  604
#define BELOW_UPPER_STGWINDOW 605

#define CASE_FORCEUPPER      700    /* product text upper-lower case */
#define CASE_MIXED           701

#define QCFILTER_NONE	     750    /* filter by qc options */
#define QCFILTER_RANGE       751

#define FORECAST_TS_NOQPF    780    /* forecast time-series typesource usage */
#define FORECAST_TS_QPF      781
#define FORECAST_TS_BYRANK   782

#define NWR_ALERT_BOTH       240    /* the NWR header attribute settings */
#define NWR_ALERT_SAME_ONLY  241
#define NWR_ALERT_NEITHER    242

#define SEGMENT_MODE_NONE    800    /* possible modes of segmentation */
#define SEGMENT_MODE_POINT   801
#define SEGMENT_MODE_GROUP   802
#define SEGMENT_MODE_COUNTY  803   

#define VTEC_CAT_OPERATIONAL  900  /* for VTEC event modes */
#define VTEC_CAT_EXPERIMENTAL 901
#define VTEC_CAT_TEST         902
#define VTEC_CAT_EXPOPER      903

#define UGC_BY_COUNTY         501 /*for product UGC code*/
#define UGC_BY_ZONE           502


#endif

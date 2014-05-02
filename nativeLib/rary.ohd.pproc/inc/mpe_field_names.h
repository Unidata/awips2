/*******************************************************************************
* FILENAME:            mpe_field_names.h
* DESCRIPTION:         Contains the ColorUse enumeration which
*                      lists all of the MPE products being generated.
*                      Also contains the prototype of the get_mpe_field_names
*                      routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 15, 2004
* ORGANIZATION:        HSEB/OHD
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/15/2004   Bryon Lawrence    Original Coding
*                    08/2008     Jingtao Deng      Q2 data
********************************************************************************
*/

#ifndef MPE_FIELD_NAMES_H
#define MPE_FIELD_NAMES_H

/* Note that there should be a one to one correspondence between the
   ColorUse enumeration and the DisplayFieldData enumeration.  The
   DisplayFieldData enumeration will contain one extra element,
   display_subValue, which must appear before the display_missing element
   at the end of the enumeration. */

typedef enum ColorUse
{  RADAR_MOSAIC_USE,
   AVG_RADAR_MOSAIC_USE,
   MAX_RADAR_MOSAIC_USE,


   FIELD_BIAS_MOSAIC_USE,
   LOCAL_BIAS_MOSAIC_USE,

   GAGE_ONLY_ANALYSIS,

   SATELLITE_PRECIP,
   LOCAL_BIAS_SATELLITE_PRECIP,

   MULTI_SENSOR_MOSAIC,
   LOCAL_BIAS_MULTI_MOSAIC,

   P3_LOCAL_BIAS_MOSAIC,

   BEST_ESTIMATE_QPE,
   MULTI_HOUR_QPE,
   LOCAL_SPAN,
   LOCAL_BIAS,
   LOCAL_SPAN_DP,
   LOCAL_BIAS_DP,
   HEIGHT_FIELD,
   RADAR_COVERAGE_FIELD,
   PRISM,
   MAX_TEMP_PRISM,
   MIN_TEMP_PRISM,
   RFC_QPE_MOSAIC,
   SATELLITE_GAGE_MOSAIC,
   SATELLITE_RADAR_MOSAIC,
   SATELLITE_RADAR_GAGE_MOSAIC,
   RFC_BIAS_MOSAIC,
   RFC_MULTISENSOR_MOSAIC,
   Q2_MOSAIC,
   Q2_LOCAL_BIAS_MOSAIC,
   MULTI_SENSOR_Q2_MOSAIC,

   DUAL_POL_RADAR_MOSAIC_USE,
   DUAL_POL_AVG_RADAR_MOSAIC_USE,
   DUAL_POL_MAX_RADAR_MOSAIC_USE,
   DUAL_POL_FIELD_BIAS_MOSAIC_USE,
   DUAL_POL_LOCAL_BIAS_MOSAIC_USE,

   DUAL_POL_MULTISENSOR_FIELD_BIAS_MOSAIC_USE,
   DUAL_POL_MULTISENSOR_LOCAL_BIAS_MOSAIC_USE,

   DUAL_POL_SATELLITE_RADAR_MOSAIC_USE,
   DUAL_POL_SATELLITE_RADAR_GAGE_MOSAIC_USE,

   LOCAL_FIELD1,
   LOCAL_FIELD2,
   LOCAL_FIELD3,
   NUM_COLORUSE_ITEMS
} ColorUse ;

typedef enum DisplayFieldData { display_rMosaic,
                        display_avgrMosaic,
                        display_maxrMosaic,

                        display_bMosaic,
                        display_lMosaic,

                        display_gageOnly,
                        display_satPrecip,
                        display_lsatPrecip,

                        display_mMosaic,
                        display_mlMosaic,

                        display_p3Mosaic,

                        display_Xmrg,
                        display_multiHour,

                        display_Locspan,
                        display_Locbias,

                        display_Locspan_dp,
                        display_Locbias_dp,

                        display_Height,
                        display_Index,

                        display_Prism,
                        display_maxtempPrism,
                        display_mintempPrism,

                        display_rfcMosaic,
                        display_sgMosaic,
                        display_srMosaic,
                        display_srgMosaic,
                        display_rfcbMosaic,
                        display_rfcmMosaic,
                        display_qMosaic,
                        display_lqMosaic,
                        display_mlqMosaic,

			//dual pol fields
			display_rdMosaic,
			display_avgrdMosaic,
			display_maxrdMosaic,
			display_bdMosaic,
			display_ldMosaic,
			display_mdMosaic,
			display_mldMosaic,

			display_srdMosaic,
			display_srdgMosaic,

			display_localField1,
			display_localField2,
			display_localField3,

			//new comparison fields
			display_diffField,
			display_ratioField,

                        display_subValue,
            display_missing
} DisplayFieldData;

typedef enum MPEFieldSubtype
{
   hourly_precip_accum_subtype,
   multihour_precip_accum_subtype,

   precip_diff_subtype,
   precip_ratio_subtype,

   height_subtype,
   index_subtype,
   prism_subtype,

   locbias_subtype,
   locspan_subtype,

   num_subtypes
} MPEFieldSubtype;



typedef struct MPEField
{
    DisplayFieldData fieldType;
    char * color_value_db_use;
    char * file_name_use;
    char * dir_token_name;
    MPEFieldSubtype subtype;
   // ColorUse color_use // do we really both a string version and an enum version?
   // char * legend_display_name; //from drawMpeLegend
  
    char * long_name;
} MPEField;


extern MPEField mpe_field_table[];
extern DisplayFieldData comparisonFieldData1;
extern DisplayFieldData comparisonFieldData2;

const char ** get_mpe_field_names ( ) ;

MPEField getMPEField(DisplayFieldData fieldType);
DisplayFieldData getDisplayFieldDataByName(const char * fieldName);

#endif /*ifndef MPE_FIELD_NAMES_H */

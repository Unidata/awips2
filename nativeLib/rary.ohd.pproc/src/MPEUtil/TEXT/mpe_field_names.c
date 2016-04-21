/*******************************************************************************
* FILENAME:            mpe_field_names.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          get_mpe_field_names
* DESCRIPTION:         Retrieves an array containing the names of the
*                      available MPE products.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 15, 2004
* ORGANIZATION:        HSEB/OHD
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/15/2004   Bryon Lawrence    Original Coding
********************************************************************************
*/
#include "mpe_field_names.h"

/* These must be upper case. */
static const char * mpe_field_names [ NUM_COLORUSE_ITEMS ] =
                                    { "RMOSAIC", "AVGRMOSAIC", "MAXRMOSAIC",
                                      "BMOSAIC", "LMOSAIC", "GAGEONLY",
                                      "SATPRE", "LSATPRE", "MMOSAIC",
                                      "MLMOSAIC", "P3LMOSAIC", "XMRG",
                                      "MULTIHOUR", "LOCSPAN", "LOCBIAS",
                                      "LOCSPANDP", "LOCBIASDP",
                                      "HEIGHT", "INDEX", "PRISM",
                                      "MAX_TEMP_PRISM", "MIN_TEMP_PRISM",
                                      "RFCMOSAIC", "SGMOSAIC", "SRMOSAIC",
                                      "SRGMOSAIC", "RFCBMOSAIC", "RFCMMOSAIC",
				      "QMOSAIC", "LQMOSAIC", "MLQMOSAIC",   

				      "RDMOSAIC", "AVGRDMOSAIC", "MAXRDMOSAIC",
				      "BDMOSAIC", "LDMOSAIC",
				      "MDMOSAIC", "MLDMOSAIC",
				      "SRDMOSAIC",  "SRDGMOSAIC",
				      "LOCALFIELD1", "LOCALFIELD2", "LOCALFIELD3" } ;

// see mpe_field_names.h, MPEField and MPEFieldSubtype in comments here are just for reference

/*

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
    char * long_name;

 // ColorUse color_use // do we really both a string version and an enum version?
   // char * legend_display_name; //from drawMpeLegend
  
} MPEField;


*/

// define variables and set default DisplayFieldData values

DisplayFieldData comparisonFieldData1 = display_rdMosaic;
DisplayFieldData comparisonFieldData2 = display_rMosaic;


// mpe_field_table[] was added during the Spring 2012 Dual Pol updates for MPE.

MPEField mpe_field_table[] =
{
        //single pol fields
        { display_rMosaic,  "RMOSAIC",   "RMOSAIC",  "rfcwide_rmosaic_dir", hourly_precip_accum_subtype, "Radar Mosaic"      },
        { display_avgrMosaic, "AVGRMOSAIC", "AVGRMOSAIC", "rfcwide_avg_rmosaic_dir",hourly_precip_accum_subtype,"Average Radar Mosaic"     },
        { display_maxrMosaic, "MAXRMOSAIC", "MAXRMOSAIC", "rfcwide_max_rmosaic_dir",hourly_precip_accum_subtype, "Max Radar Mosaic"     },
        { display_bMosaic, "BMOSAIC", "BMOSAIC", "rfcwide_bmosaic_dir",hourly_precip_accum_subtype, "Field Bias Radar Mosaic"         },
        { display_lMosaic, "LMOSAIC",  "LMOSAIC",  "rfcwide_lmosaic_dir",hourly_precip_accum_subtype , "Local Bias Radar Mosaic"       },
        { display_gageOnly, "GAGEONLY",  "GAGEONLY", "rfcwide_gageonly_dir", hourly_precip_accum_subtype , "Gage Only Analysis"         },
        { display_satPrecip,"SATPRE",   "SATPRE",  "rfcwide_satpre_dir", hourly_precip_accum_subtype, "Satellite Precip"   },
        { display_lsatPrecip, "LSATPRE",  "LSATPRE", "rfcwide_lsatpre_dir", hourly_precip_accum_subtype , "Local Bias Satellite Precip"         },
        { display_mMosaic, "MMOSAIC", "MMOSAIC",  "rfcwide_mmosaic_dir", hourly_precip_accum_subtype, "Multisensor Mosaic"          },
        { display_mlMosaic, "MLMOSAIC",  "MLMOSAIC","rfcwide_mlmosaic_dir", hourly_precip_accum_subtype,"Local Bias Multisensor Mosaic"            },
        { display_p3Mosaic, "P3LMOSAIC", "P3LMOSAIC", "rfcwide_p3lmosaic_dir", hourly_precip_accum_subtype, "Triangulated Local Bias Mosaic"          },


        { display_maxtempPrism, "MAX_TEMP_PRISM", "MAX_TEMP_PRISM", "rfcwide_prism_dir",prism_subtype          },
        { display_mintempPrism,"MIN_TEMP_PRISM", "MIN_TEMP_PRISM",  "rfcwide_prism_dir",prism_subtype          },
        { display_rfcMosaic, "RFCMOSAIC", "RFCMOSAIC","mpe_rfcmosaic_dir", hourly_precip_accum_subtype , "RFC QPE Mosaic"   },
        { display_sgMosaic, "SGMOSAIC", "SGMOSAIC", "mpe_sgmosaic_dir", hourly_precip_accum_subtype, "Satellite Gage Mosaic"   },
        { display_srMosaic,"SRMOSAIC", "SRMOSAIC",  "mpe_srmosaic_dir", hourly_precip_accum_subtype, "Satellite Radar Mosaic"  },
        { display_srgMosaic, "SRGMOSAIC","SRGMOSAIC",   "mpe_srgmosaic_dir", hourly_precip_accum_subtype,  "Satellite Radar Gage Mosaic"          },
        { display_rfcbMosaic,"RFCBMOSAIC", "RFCBMOSAIC",  "mpe_rfcbmosaic_dir", hourly_precip_accum_subtype, "RFC Field Bias Mosaic"   },
        { display_rfcmMosaic, "RFCMMOSAIC",  "RFCMMOSAIC", "mpe_rfcmmosaic_dir", hourly_precip_accum_subtype, "RFC Multisensor Mosaic"   },
        { display_qMosaic,"QMOSAIC", "QMOSAIC",    "mpe_qmosaic_dir", hourly_precip_accum_subtype, "Raw Q2 Mosaic"  },
        { display_lqMosaic, "LQMOSAIC", "LQMOSAIC", "mpe_lqmosaic_dir", hourly_precip_accum_subtype, "Local Bias Q2 Mosaic"    },
        { display_mlqMosaic, "MLQMOSAIC", "MLQMOSAIC", "mpe_mlqmosaic_dir", hourly_precip_accum_subtype, "Local Bias Multisensor Q2 Mosaic"  },

        { display_Locspan, "LOCSPAN",  "LOCSPAN", "rfcwide_locspan_dir", locspan_subtype   },
        { display_Locbias, "LOCBIAS", "LOCBIAS", "rfcwide_locbias_dir", locbias_subtype  },

        //dual pol fields
        { display_rdMosaic,   "RDMOSAIC", "RDMOSAIC",  "mpe_rdmosaic_dir", hourly_precip_accum_subtype, "DP Radar Mosaic"       },
        { display_avgrdMosaic, "AVGRDMOSAIC", "AVGRDMOSAIC", "mpe_avgrdmosaic_dir", hourly_precip_accum_subtype, "DP Average Radar Mosaic" },
        { display_maxrdMosaic, "MAXRDMOSAIC","MAXRDMOSAIC", "mpe_maxrdmosaic_dir", hourly_precip_accum_subtype, "DP Max Radar Mosaic"   },
        { display_bdMosaic,  "BDMOSAIC",  "BDMOSAIC",  "mpe_bdmosaic_dir", hourly_precip_accum_subtype, "DP Field Bias Radar Mosaic"  },
        { display_ldMosaic, "LDMOSAIC", "LDMOSAIC",   "mpe_ldmosaic_dir", hourly_precip_accum_subtype, "DP Local Bias Radar Mosiac" },
        { display_mdMosaic,  "MDMOSAIC",   "MDMOSAIC",  "mpe_mdmosaic_dir", hourly_precip_accum_subtype,   "DP Multisensor Field Bias Mosaic"   },
        { display_mldMosaic, "MLDMOSAIC", "MLDMOSAIC",     "mpe_mldmosaic_dir", hourly_precip_accum_subtype, "DP Multisensor Local Bias Mosaic"   },
        { display_srdMosaic,  "SRDMOSAIC",   "SRDMOSAIC",  "mpe_srdmosaic_dir", hourly_precip_accum_subtype,  "DP Satellite Radar Mosaic"  },
        { display_srdgMosaic,  "SRDGMOSAIC",   "SRDGMOSAIC",  "mpe_srdgmosaic_dir", hourly_precip_accum_subtype, "DP Satellite Radar Gage Mosaic"  },

        { display_Locspan_dp, "LOCSPANDP",  "LOCSPANDP", "mpe_locspandp_dir", locspan_subtype   },
        { display_Locbias_dp, "LOCBIASDP", "LOCBIASDP", "mpe_locbiasdp_dir", locbias_subtype  },

        //local fields
        { display_localField1, "LOCALFIELD1", "LOCALFIELD1",  "mpe_localfield1_dir", hourly_precip_accum_subtype, "Local Field 1"   },
        { display_localField2, "LOCALFIELD2", "LOCALFIELD2",  "mpe_localfield2_dir", hourly_precip_accum_subtype, "Local Field 2"   },
        { display_localField3, "LOCALFIELD3", "LOCALFIELD3", "mpe_localfield3_dir", hourly_precip_accum_subtype, "Local Field 3"    },

        //best estimate
        { display_Xmrg,  "XMRG",  "xmrg",    "rfcwide_xmrg_dir", hourly_precip_accum_subtype, "Best Estimate QPE"   },

        //comparison fields
        { display_diffField,  "PRECIP_DIFF",  "N/A",  "N/A", precip_diff_subtype      },
        { display_ratioField, "PRECIP_RATIO", "N/A",   "N/A", precip_ratio_subtype    },

        //misc fields
        { display_subValue, "N/A", "N/A",  "N/A",           },
        { display_missing,  "N/A", "N/A", "N/A",           },
        { display_Height, "HEIGHT",  "HEIGHT",  "rfcwide_height_dir", height_subtype    },
        { display_Index,  "INDEX", "INDEX", "rfcwide_indexdir", index_subtype    },
        { display_Prism, "PRISM",  "PRISM",   "rfcwide_prism_dir",prism_subtype        },
        { display_multiHour, "MULTIHOUR", "MULTIHOUR",  "N/A",multihour_precip_accum_subtype         }


}; //end mpe_field_table[]

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    get_mpe_field_names
* PURPOSE:        Returns an array containing the names of all of the available
*                 MPE products.  The elements in this array correspond to the
*                 ColorUse enumeration.
*
* ARGUMENTS:
*    None
*
* RETURNS:
*   DATA TYPE       DESCRIPTION
*   const char **   A pointer to the mpe_field_names array.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

const char ** get_mpe_field_names ( )
{
   return mpe_field_names ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
MPEField getMPEField(DisplayFieldData fieldType)
{
/*
    This function returns the matching MPEField from mpe_field_table, given a fieldType

*/
    MPEField field;
    field.fieldType = display_missing;

    int i = 0;
    for (i = 0; i <= display_missing; i++)
    {
        if (mpe_field_table[i].fieldType == fieldType)
        {
            field = mpe_field_table[i];
            break;
        }

    }

    return field;

} //end getMPEField()

DisplayFieldData getDisplayFieldDataByName(const char * fieldName)
{
/*
     This function returns the matching DisplayFieldData from mpe_field_table, given the short file use name.

*/
    printf("getDisplayFieldDataByName(): searching for fieldName = :%s:\n", fieldName);
    DisplayFieldData fieldType = display_missing;
  
    int i = 0;
    for (i = 0; i <= display_missing; i++)
    {
        if (strcmp(mpe_field_table[i].file_name_use, fieldName) == 0)
        {
            fieldType = mpe_field_table[i].fieldType;
            printf("getDisplayFieldDataByName(): found fieldName = :%s:, i = %d fieldType = %d\n", mpe_field_table[i].file_name_use, i, fieldType);
            break;
        }

    }

    return fieldType;

} //end getDisplayFieldDataByName()


DisplayFieldData getDisplayFieldDataByLongName(const char * longFieldName)
{
/*
    This function returns the matching DisplayFieldData from mpe_field_table, given the long field name

*/
    printf("getDisplayFieldDataByLongName(): searching for longFieldName = :%s:\n", longFieldName);
    DisplayFieldData fieldType = display_missing;
  
    int i = 0;
    for (i = 0; i <= display_missing; i++)
    {
        if (mpe_field_table[i].subtype == hourly_precip_accum_subtype)
        {
            if (strcmp(mpe_field_table[i].long_name, longFieldName) == 0)
            {
                fieldType = mpe_field_table[i].fieldType;
                printf("getDisplayFieldDataByLongName(): found longFieldName = :%s:, i = %d fieldType = %d\n",
                        mpe_field_table[i].long_name, i, fieldType);
                break;
            }
        }

    }

    return fieldType;

} //end getDisplayFieldDataByLongName()

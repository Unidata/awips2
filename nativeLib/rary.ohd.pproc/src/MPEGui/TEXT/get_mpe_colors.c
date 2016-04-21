
/*******************************************************************************
* FILENAME:            get_mpe_colors.c
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

#include <stdio.h>

#include "mpe_colors.h"
#include "mpe_log_utils.h"
#include "NamedColorSetGroup.h"
#include "NamedColorUseSet.h"
#include "write_colorsets.h"

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

static NamedColorSetGroup * pColorSetGroup = NULL;

static NamedColorSetGroup * build_mpe_colors ( )
{
   NamedColorUseSet * pColorUseSet = NULL;

   /* Create a color use group for each of the MPE products. */

   /* Radar Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "RMOSAIC", "Radar Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );


   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Average Radar Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "AVGRMOSAIC",
                                               "Average Radar Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Max Radar Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "MAXRMOSAIC",
                                               "Maximum Radar Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Field Bias Radar Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "BMOSAIC",
                                               "Field Bias Radar Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Local Bias Radar Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "LMOSAIC",
                                               "Local Bias Radar Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Gage Only Analysis */
   pColorUseSet = initializeNamedColorUseSet ( "GAGEONLY",
                                               "Gage Only Analysis",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Satellite Precip */
   pColorUseSet = initializeNamedColorUseSet ( "SATPRE", "Satellite Precip",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Local Bias Satellite Precip */
   pColorUseSet = initializeNamedColorUseSet ( "LSATPRE",
                                               "Local Bias Satellite Precip",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* Multi-sensor Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "MMOSAIC",
                                               "Multisensor Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );


   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* Local Bias Multi-sesnor Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "MLMOSAIC",
                                               "Local Bias Multi-sensor Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* P3 Local Bias Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "P3LMOSAIC",
                                               "P3 Local Bias Mosaic",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* Best Estimate QPE */
   pColorUseSet = initializeNamedColorUseSet ( "XMRG", "Best Estimate",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Multi-hour QPE */
   pColorUseSet = initializeNamedColorUseSet ( "MULTIHOUR", "Multi-Hour QPE",
                                               num_precip_colors,
                                               precip_levels, precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Local Span Field */
   pColorUseSet = initializeNamedColorUseSet ( "LOCSPAN", "Local Span",
                                               num_locspan_colors,
                                               locspan_levels,
                                               locspan_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Local Bias Field */
   pColorUseSet = initializeNamedColorUseSet ( "LOCBIAS", "Local Bias",
                                               num_locbias_colors,
                                               locbias_levels,
                                               locbias_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Radar Height Field */
   pColorUseSet = initializeNamedColorUseSet ( "HEIGHT", "Height",
                                               num_height_colors,
                                               height_levels, height_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Radar Coverage Field */
   pColorUseSet = initializeNamedColorUseSet ( "INDEX", "Radar Coverage",
                                               num_index_colors,
                                               index_levels, index_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* PRISM Field */
   pColorUseSet = initializeNamedColorUseSet ( "PRISM", "Prism",
                                               num_prism_colors,
                                               prism_levels, prism_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Max Temp PRISM Field */
   pColorUseSet = initializeNamedColorUseSet ( "MAX_TEMP_PRISM",
                                               "Max Temp PRISM",
                                               num_max_temp_prism_colors,
                                               max_temp_prism_levels,
                                               max_temp_prism_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* Min Temp PRISM Field */
   pColorUseSet = initializeNamedColorUseSet ( "MIN_TEMP_PRISM",
                                               "Min Temp PRISM",
                                               num_min_temp_prism_colors,
                                               min_temp_prism_levels,
                                               min_temp_prism_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /* RFC Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "RFCMOSAIC",
                                               "RFC Mosaic",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* RFC Field Bias Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "RFCBMOSAIC",
                                               "RFC Field Bias Mosaic",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* RFC Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "RFCMMOSAIC",
                                               "RFC Multisensor Mosaic",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }


   /*  Satellite Gage Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "SGMOSAIC",
                                               "Satellite Gage Mosaic",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* Satellite Radar Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "SRMOSAIC",
                                               "Satellite Radar Mosaic",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* Satellite Radar Gage Mosaic */
   pColorUseSet = initializeNamedColorUseSet ( "SRGMOSAIC",
                                               "Satellite Radar Gage",
                                               num_precip_colors,
                                               precip_levels,
                                               precip_colors,
                                               "GRAY30", "BLACK", 3600 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }





   /* Radclim */
   pColorUseSet = initializeNamedColorUseSet ( "RADCLIM",
                                               "Radclim",
                                               num_radclim_colors,
                                               radclim_levels,
                                               radclim_colors,
                                               "GRAY30", "BLACK", 0 );

   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );

   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* Add color sets for the DailyQC portion of MPE Editor. */

   /* Gridded 6hr precipitation colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "6hGRID_PRECIP",
                                               "6hr Gridded Precip",
                                               num_6hr_gridded_precip_colors,
                                               gridded_precip_levels_6hr,
                                               gridded_precip_colors_6hr,
                                               "GRAY", "BLACK", 21600 );  
                                      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );      
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   /* Gridded 24hr precipitation colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "24hGRID_PRECIP",
                                               "24hr Gridded Precip",
                                               num_24hr_gridded_precip_colors,
                                               gridded_precip_levels_24hr,
                                               gridded_precip_colors_24hr,
                                               "GRAY", "BLACK", 86400 );  
                                      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );      
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   
   /* Gridded 6hr temperature colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "sixhGRID_TEMP",
                                              "6hr Gridded Temperature",
                                              num_6hr_gridded_temperature_colors,
                                              gridded_temperature_levels_6hr,
                                              gridded_temperature_colors_6hr,
                                              "GRAY", "BLACK", 21600 );  
					      
				      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   /* Gridded max/min daily temperature colormaps */
   
  pColorUseSet = initializeNamedColorUseSet ( "maxGRID_TEMP",
                                              "Max Gridded Temperature",
                                              num_max_gridded_temperature_colors,
                                              gridded_temperature_levels_max,
                                              gridded_temperature_colors_max,
                                              "GRAY", "BLACK", 86400 );  
					      
				      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

  pColorUseSet = initializeNamedColorUseSet ( "minGRID_TEMP",
                                              "Min Gridded Temperature",
                                              num_min_gridded_temperature_colors,
                                              gridded_temperature_levels_min,
                                              gridded_temperature_colors_min,
                                              "GRAY", "BLACK", 86400 );  
					      
				      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   
   
   /* Gridded 6hr freezing level colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "6hGRID_FREEZL",
                                               "6hr Gridded Freezing Level",
                                               num_6hr_gridded_freezing_colors,
                                               gridded_freezing_levels_6hr,
                                               gridded_freezing_colors_6hr,
                                               "GRAY", "BLACK", 21600 );  
                                      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }

   /* 6hr MAP precipitation colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "6hMAREA_PRECIP",
                                               "6hr Mean Area Precip",
                                               num_6hr_mean_precip_colors,
                                               mean_precip_levels_6hr,
                                               mean_precip_colors_6hr,
                                               "GRAY", "BLACK", 21600 );  
                                      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );      
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   /* 24hr MAP precipitation colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "24hMAREA_PRECIP",
                                               "24hr Mean Area Precip",
                                               num_24hr_mean_precip_colors,
                                               mean_precip_levels_24hr,
                                               mean_precip_colors_24hr,
                                               "GRAY", "BLACK", 86400 );  
                                      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );      
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   /* 6hr MAT temperature colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "sixhMAREA_TEMP",
                                              "6hr Mean Area Temperature",
                                              num_6hr_mean_temperature_colors,
                                              mean_temperature_levels_6hr,
                                              mean_temperature_colors_6hr,
                                              "GRAY", "BLACK", 21600 );  
					      
				      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   
   /* 6hr MAZ freezing level colormap */
   
   pColorUseSet = initializeNamedColorUseSet ( "6hMAREA_FREEZL",
                                               "6hr Mean Area Freezing Level",
                                               num_6hr_mean_freezing_colors,
                                               mean_freezing_levels_6hr,
                                               mean_freezing_colors_6hr,
                                               "GRAY", "BLACK", 21600 );  
                                      
   pColorSetGroup = addNamedColorUseSet ( pColorSetGroup, pColorUseSet );
   
   if ( pColorSetGroup == NULL )
   {
      return NULL;
   }
   
   
   return pColorSetGroup;
}


void writeMpeEditorDefaultColorDataFile(const char * outputFileName)
{
   FILE *outputFilePtr = NULL;

   outputFilePtr = fopen( outputFileName, "w" );

   if ( outputFilePtr == NULL )
   {
      logMessage( "Unable to open the output file %s for writing!", outputFileName );
       exit( -1 );
   }
   

   writeColorDataSetToOutputFile ( outputFilePtr, 
                                   "Radar Mosaic",
                                   "RMOSAIC", 
                                   precip_colors,
                                   precip_levels, 
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Average Radar Mosaic",
                                   "AVGRMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Maximum Radar Mosaic",
                                   "MAXRMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Field Bias Radar Mosaic",
                                   "BMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                    "Local Bias Radar Mosaic",
                                    "LMOSAIC",
                                    precip_colors,
                                    precip_levels,
                                    num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Gage Only Analysis",
                                   "GAGEONLY",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Satellite Precip",
                                   "SATPRE",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Local Bias Satellite Precip",
                                   "LSATPRE",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Multisensor Mosaic",
                                   "MMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Local Bias Multi-sensor Mosaic",
                                   "MLMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "P3 Local Bias Mosaic",
                                   "P3LMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Best Estimate",
                                   "XMRG",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Multi-Hour QPE",
                                   "MULTIHOUR",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Local Span",
                                   "LOCSPAN",
                                   locspan_colors,
                                   locspan_levels,
                                   num_locspan_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Local Bias",
                                   "LOCBIAS",
                                   locbias_colors,
                                   locbias_levels,
                                   num_locbias_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Height",
                                   "HEIGHT",
                                   height_colors,
                                   height_levels,
                                   num_height_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Radar Coverage",
                                   "INDEX",
                                   index_colors,
                                   index_levels,
                                   num_index_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Prism",
                                   "PRISM",
                                   prism_colors,
                                   prism_levels,
                                   num_prism_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Max Temp PRISM",
                                   "MAX_TEMP_PRISM",
                                   max_temp_prism_colors,
                                   max_temp_prism_levels,
                                   num_max_temp_prism_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Min Temp PRISM",
                                   "MIN_TEMP_PRISM",
                                   min_temp_prism_colors,
                                   min_temp_prism_levels,
                                   num_min_temp_prism_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "RFC Mosaic",
                                   "RFCMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "RFC Field Bias Mosaic",
                                   "RFCBMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "RFC Multisensor Mosaic",
                                   "RFCMMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Satellite Gage Mosaic",
                                   "SGMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Satellite Radar Mosaic",
                                   "SRMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Satellite Radar Gage",
                                   "SRGMOSAIC",
                                   precip_colors,
                                   precip_levels,
                                   num_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Radclim",
                                   "RADCLIM",
                                   radclim_colors,
                                   radclim_levels,
                                   num_radclim_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "6hr Gridded Precip",
                                   "6hGRID_PRECIP",
                                   gridded_precip_colors_6hr,
                                   gridded_precip_levels_6hr,
                                   num_6hr_gridded_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "24hr Gridded Precip",
                                   "24hGRID_PRECIP",
                                   gridded_precip_colors_24hr,
                                   gridded_precip_levels_24hr,
                                   num_24hr_gridded_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                  "6hr Gridded Temperature",
                                   "sixhGRID_TEMP",
                                  gridded_temperature_colors_6hr,
                                  gridded_temperature_levels_6hr,
                                  num_6hr_gridded_temperature_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Max Gridded Temperature",
                                   "maxGRID_TEMP",
                                   gridded_temperature_colors_max,
                                   gridded_temperature_levels_max,
                                   num_max_gridded_temperature_colors );

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "Min Gridded Temperature",
                                   "minGRID_TEMP",
                                   gridded_temperature_colors_min,
                                   gridded_temperature_levels_min,
                                   num_min_gridded_temperature_colors );


   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "6hr Gridded Freezing Level",
                                   "6hGRID_FREEZL",
                                   gridded_freezing_colors_6hr,
                                   gridded_freezing_levels_6hr,
                                   num_6hr_gridded_freezing_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "6hr Mean Area Precip",
                                   "6hMAREA_PRECIP",
                                   mean_precip_colors_6hr,
                                   mean_precip_levels_6hr,
                                   num_6hr_mean_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                   "24hr Mean Area Precip",
                                   "24hMAREA_PRECIP",
                                   mean_precip_colors_24hr,
                                   mean_precip_levels_24hr,
                                   num_24hr_mean_precip_colors);

   writeColorDataSetToOutputFile ( outputFilePtr,
                                  "6hr Mean Area Temperature",
                                  "sixhMAREA_TEMP",
                                  mean_temperature_colors_6hr,
                                  mean_temperature_levels_6hr,
                                  num_6hr_mean_temperature_colors);

   writeColorDataSetToOutputFile ( outputFilePtr, 
                                   "6hr Mean Area Freezing Level",
                                   "6hMAREA_FREEZL",
                                   mean_freezing_colors_6hr,
                                   mean_freezing_levels_6hr,
                                   num_6hr_mean_freezing_colors);

   fclose ( outputFilePtr );
   outputFilePtr = NULL;
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
NamedColorSetGroup * get_mpe_default_colors ( )
{
   static int first = 1;

   if ( first == 1 )
   {
      /* Build the colors for the MPE products. */
      pColorSetGroup = build_mpe_colors ( );

      if ( pColorSetGroup == NULL )
      {
        logMessage ( "Could not create color groups for MPE.\n" );
         return NULL;
      }

      first = 0;
   }

   return pColorSetGroup;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/get_mpe_colors.c,v $";
 static char rcs_id2[] = "$Id: get_mpe_colors.c,v 1.5 2007/10/18 18:07:20 lawrence Exp $";}
/*  ===================================================  */

}

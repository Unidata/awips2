/*******************************************************************************
* FILENAME:            NamedColorUseSet.c
* NUMBER OF MODULES:   2
* GENERAL INFORMATION:
*   MODULE 1:          loadColorThresholdSet
* DESCRIPTION:         Creates and loads a color threshold array.
*   MODULE 2:          initializeNamedColorUseSet         
* DESCRIPTION:         Creates and initializes a named color use set.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       September 7, 2006
* ORGANIZATION:        OHD-11 HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*   1,2             9/7/2006     B. Lawrence       Original Coding
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>

#include "ColorThreshold.h"
#include "NamedColorUseSet.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    loadColorThresholdSet
* PURPOSE:        This routine loads the set of color names and their 
*                 thresholds into the color threshold member of the
*                 named color use set.
*
* ARGUMENTS:
*   TYPE   DATA TYPE             NAME                 DESCRIPTION/UNITS
*   Input  ColorThresholdArray * pThresholdArray      The color threshold array
*                                                     to be filled.
*   Input  char *                missing_color_name   The name of the color to
*                                                     be used for missing
*                                                     values.
*   Input  char *                default_color_name   The name of the default
*                                                     color.
*   Input  double                threshold_values []  The color threshold
*                                                     values.
*   Input  char *                color_names [ ]      The names of the colors
*                                                     corresponding to the
*                                                     color threshold values
*   Input  int                   num_color_names      The number of color
*                                                     names.
*
* RETURNS:
*   DATA TYPE             NAME                   DESCRIPTION
*   ColorThresholdArray * pThresholdArray        The filled array of 
*                                                color threshold values.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            Loop index counter.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE and DESCRIPTION                     
*    Returns a NULL value if memory could not be allocated for the color
*    level colors and thresholds array.
*
********************************************************************************
*/

static ColorThresholdArray * loadColorThresholdSet ( 
                                    ColorThresholdArray * pThresholdArray,
                                    const char * missing_color_name,
                                    const char * default_color_name,
                                    const double threshold_values [],
                                    const char * color_names [ ],
                                    int num_color_names )
{
   int i;

   pThresholdArray->thresholds = ( ColorThreshold * )
                                 malloc ( num_color_names * 
                                 sizeof ( ColorThreshold ) );

   if ( pThresholdArray->thresholds == NULL )
   {
      return NULL;
   }

   for ( i = 0; i < num_color_names; ++i )
   {
       pThresholdArray->thresholds[i].value = threshold_values[i];
       memset ( pThresholdArray->thresholds[i].colorName, '\0', 
                COLOR_NAME_LEN + 1 );
       strncpy ( pThresholdArray->thresholds[i].colorName, color_names[i],
                COLOR_NAME_LEN );
   }

   memset ( pThresholdArray->missingColorName, '\0', COLOR_NAME_LEN + 1 );
   memset ( pThresholdArray->defaultColorName, '\0', COLOR_NAME_LEN + 1 );

   strncpy ( pThresholdArray->missingColorName, missing_color_name,
             COLOR_NAME_LEN );
   strncpy ( pThresholdArray->defaultColorName, default_color_name,
             COLOR_NAME_LEN );

   pThresholdArray->length = num_color_names;

   return pThresholdArray;

} 

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   initializeNamedColorUseSet
* PURPOSE:       Initializes a color use set which can then be added to
*                a group of color use sets.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                      DESCRIPTION/UNITS
*   Input  char *      color_use_db_name         The color use string
*                                                as it appears in the
*                                                ColorValue table.
*   Input  char *      color_use_display_string  The color use string as
*                                                it appears on the 
*                                                color thresholds window.
*   Input  int         num_color_db_names        The number of color names
*                                                and thresholds in this set.
*   Input  double      threshold_values [ ]      The color thresholds.
*   Input  char *      color_names [ ]           The color name associated with
*                                                each threshold.
*   Input  char *      missing_color_name        The color to use for missing
*                                                data. 
*   Input  char *      default_color_name        The color to use for default.
*   Input  int         default_duration          The duration in seconds
*                                                of the data represented 
*                                                by this color set.
*
* RETURNS:
*   DATA TYPE          NAME                        DESCRIPTION
*   NamedColorUseSet * pNamedColorUseSet           The create color use set.
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*   loadColorThresholdSet                   NA          Creates and loads
*                                                       a color threshold
*                                                       array.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE             NAME                 DESCRIPTION
*   NamedColorUseSet *    pNamedColorUseSet    Points the newly created
*                                              named color use set. 
*   ColorThresholdArray * pThresholdArray      Points the newly created
*                                              color value threshold array.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                             
*    Returns NULL if memory could not be allocated for the color use set.
*
********************************************************************************
*/
NamedColorUseSet * initializeNamedColorUseSet ( 
                             const char * color_use_db_name,
                             const char * color_use_display_string,
                             int num_color_db_names,
                             const double threshold_values [ ],
                             const char * color_names [ ],
                             const char * missing_color_name, 
                             const char * default_color_name,
                             int default_duration )
{
   NamedColorUseSet * pNamedColorUseSet = NULL;
   ColorThresholdArray * pThresholdArray = NULL;

   /* Allocate memory for a NamedColorUseSet structure. */
   pNamedColorUseSet = ( NamedColorUseSet * ) malloc ( 
                                              sizeof ( NamedColorUseSet ) );

   if ( pNamedColorUseSet == NULL )
   {
      printf ( "Could not allocate memory for Color Set.\n" );
      return NULL;
   }

   pNamedColorUseSet->color_use_db_name = strdup ( color_use_db_name );
   pNamedColorUseSet->color_use_display_string = 
                                          strdup ( color_use_display_string );
   pNamedColorUseSet->default_duration = default_duration;
   
   pThresholdArray = loadColorThresholdSet ( 
                             &pNamedColorUseSet->threshold_array,
                             missing_color_name,
                             default_color_name,
                             threshold_values,
                             color_names,
                             num_color_db_names );

   if ( pThresholdArray == NULL )
   {
      printf ( "Could not allocate memory for Color Threshold Array.\n" );
      return NULL;
   }

   return pNamedColorUseSet ;
}


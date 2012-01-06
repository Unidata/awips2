
/*******************************************************************************
* FILENAME:            get_climate_source.c
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       October 22, 2007
* ORGANIZATION:        OHD-11/HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    10/22/2007   B. Lawrence       First Coding 
********************************************************************************
*/
#include <string.h>

#include "gageqc_gui.h"

/*******************************************************************************
* MODULE NAME:        getClimateSource
* PURPOSE:            Translates the typesource associated with climate
*                     data to a source name which can be displayed when
*                     a station is edited.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  char *      pParamCode	    Contains the paramater code (PEDTSEP)
*                                           of the climate data associated with a
*                                           station.
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   char *      pSourceName                 The name of the source of the climate
*                                           data.
* APIs UTILIZED:
*   N/A
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME                         DESCRIPTION
*   char * previousClimateTypeSource        The climate type source supplied 
*                                           in the last call to this function.
*   char * previousClimateSourceName        The name of the last found
*                                           climate type source. 
*   char * pClimateTypeSources []           Array of accepted type sources.
*   char * pClimateSourceNames []           Names corresponding to array of
*                                           accepted type sources.
*   int i;                                  Loop index.
*   int status;                             Holds return value for string 
*                                           comparisons.
*
* DATA FILES AND/OR DATABASE:
*   N/A
*
* ERROR HANDLING:
*   Returns the string "Unknown" if the supplied climate type source
*   is not known. 
********************************************************************************
*/

const char * getClimateSource ( const char * pParamCode )
{
   static const char * previousClimateTypeSource = NULL;
   static const char * previousClimateSourceName = NULL;
   static const char * pClimateTypeSources [NUM_CLIMATE_SOURCES] = 
                                           { "PB", "RZ" }; 
   static const char * pClimateSourceNames [NUM_CLIMATE_SOURCES + 1] = 
                                           { "PRISM", "NCDC", "Unknown" };
   int i;
   int status;

   if ( previousClimateTypeSource != NULL && 
        previousClimateSourceName != NULL )
   {
      status = strncmp ( previousClimateTypeSource, &pParamCode[3], 2);

      if ( status == 0 )
      {
         return previousClimateSourceName;
      }
   }

   for ( i = 0; i < NUM_CLIMATE_SOURCES; ++i )
   {
      status = strncmp ( &pParamCode[3], pClimateTypeSources[i], 2 );

      if ( status == 0 )
      {
          previousClimateTypeSource = pClimateTypeSources[i];
          previousClimateSourceName = pClimateSourceNames[i];
          return previousClimateSourceName;
      }
   }

   previousClimateTypeSource = NULL;
   previousClimateSourceName = NULL;

   return pClimateSourceNames [NUM_CLIMATE_SOURCES];

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/get_climate_source.c,v $";
 static char rcs_id2[] = "$Id: get_climate_source.c,v 1.1 2007/10/23 17:51:09 lawrence Exp $";}
/*  ===================================================  */

}

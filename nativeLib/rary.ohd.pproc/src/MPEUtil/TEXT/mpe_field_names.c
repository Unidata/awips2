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
                                      "HEIGHT", "INDEX", "PRISM",
                                      "MAX_TEMP_PRISM", "MIN_TEMP_PRISM",
                                      "RFCMOSAIC", "SGMOSAIC", "SRMOSAIC",
                                      "SRGMOSAIC", "RFCBMOSAIC", "RFCMMOSAIC",
                                      "QMOSAIC", "LQMOSAIC", "MLQMOSAIC" } ;

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

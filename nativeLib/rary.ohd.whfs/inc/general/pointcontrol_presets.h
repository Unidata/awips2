/*******************************************************************************
* FILENAME:
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

#ifndef POINTCONTROL_PRESETS_H 
#define POINTCONTROL_PRESETS_H




#include "PointDataPresets.h"
#include "pointcontrol_options.h"
#include "TokenizeOptionString.h"

/* Function Prototypes. */
PointDataPresets * get_PointDataPresetsList ( ) ;
PointDataPresets * get_PointDataPresetsHead ( ) ;
void free_PointDataPresets ( ) ;
int set_pc_options_using_presets ( int pos ) ;

void pc_set_options_from_optionvalue_pairs ( OptionValuePair * pOptionValuePair );
//FilterOperation getFilterOperationFromPresetString(const char * presetCodeString);
char * build_pointdata_preset_string_from_options ( );
void set_pc_options ( OptionValuePair * pOptionValuePair );

#endif /* #ifndef POINTCONTROL_PRESETS_H */

#ifndef HV_COLOR_THRESHOLD_H
#define HV_COLOR_THRESHOLD_H

#include "ColorThreshold.h"
#include "NamedColorSetGroup.h"
#include "HvColorList.h"
#include "pointcontrol_options.h"


#define COLOR_USE_NAME_LENGTH 20



NamedColorSetGroup * get_default_hv_colors ( );

void loadColorThresholdArrayByPcOptions(const pc_options_struct * pc_options );

void loadColorThresholdArray(ColorThresholdArray *colorArray,
                 char *colorUseName,
                 time_t duration,
                 const NamedColorSetGroup *defaultNamedColorSetGroupPtr,
                 Widget widget);

char * determineValueColor(double value,  const pc_options_struct * pc_options);

void writeHydroViewDefaultColorDataFile( char * outputFileName );

#endif /*HV_COLOR_THRESHOLD_H*/

#ifndef NAMED_COLOR_USE_SET_H
#define NAMED_COLOR_USE_SET_H

#include "ColorThreshold.h"
 
typedef struct _NamedColorUseSet
{
   char * color_use_db_name;
   char * color_use_display_string;
   int default_duration;
   ColorThresholdArray threshold_array;
} NamedColorUseSet;

NamedColorUseSet * initializeNamedColorUseSet (  
                                   const char * color_use_db_name,
                                   const char * color_use_display_string,
                                   int num_color_db_names,
                                   const double threshold_values [ ],
                                   const char * color_names [ ],
                                   const char * missing_color_name,
                                   const char * default_color_name,
                                   int default_duration ); 
                                   

#endif /* #ifndef NAMED_COLOR_USE_SET_H */ 

/************************************************
   fcsttrend_info_struct.h
   
   Purpose
   Include the infomation associated with a specified
   forecast trend data.
**************************************************/

#ifndef FCSTTREND_INFO_STRUCT_H
#define FCSTTREND_INFO_STRUCT_H

#include "temp_item_struct.h"
#include "temp_records_struct.h"
#include "cat_and_product_defs.h"

typedef struct
{
/*   format_struct     format;*/
/*   variable_struct   variable;*/
   char              *trend_name[MAX_NUMPHRASES];
   char              *trend_phrase[MAX_NUMPHRASES];
   int               *trend_index;
   int               num_phrase;
} fcsttrend_info_struct;

#endif


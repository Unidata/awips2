#ifndef LOC_SHIFT_H
#define LOC_SHIFT_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "CodeTimer.h"

#define PARAM_CODE_LEN 6

#define INITIAL_SHIFTS 10000
#define LINE_LENGTH 50
typedef struct PdcLocationShift
{
    char lid[LOC_ID_LEN];
    char param_code[PARAM_CODE_LEN + 1];    
    int x_shift;
    int y_shift;
    
} PdcLocationShift;

PdcLocationShift * readShiftFile(char * filePath, int *arraySizeParam);



#endif



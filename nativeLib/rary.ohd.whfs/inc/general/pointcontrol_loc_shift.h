#ifndef LOC_SHIFT_H
#define LOC_SHIFT_H
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "CodeTimer.h"
#include "pointcontrol_report.h"
#include "pointcontrol_mgr.h"

#define PARAM_CODE_LEN 6

#define INITIAL_SHIFTS 10000
typedef struct PdcLocationShift
{
    char lid[LOC_ID_LEN];
    char param_code[PARAM_CODE_LEN + 1];    
    int x_shift;
    int y_shift;
    
} PdcLocationShift;



void loadShiftValues();

void applyShiftValues();


PdcLocationShift * readShiftFile(char * filePath, int *arraySizeParam);
void sortShiftArray(PdcLocationShift * shiftArrayToBeSorted, int arrayLength);

int compareLocationShifts(const void * s1, const void * s2);

PdcLocationShift * findPdcLocationShift(PdcLocationShift * shift_array,
                                      int location_shift_count, 
                                      int *array_slot,
                                      ReportList * report);
void create_shift_key(const char * lid, const char * param_code, char * returned_key);
void getParamCodeFromReport(ReportList * report, char *paramCode);
char getShefDurCodeFromIhfsDurCode(int intDuration);

#endif



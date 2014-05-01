/*
    File: ColorValue.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ColorValue_h
#define ColorValue_h


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include "DbmsAccess.h"
#include "DbmsUtils.h"
#include "List.h"
#include "GeneralUtil.h"
#include "dbmserrs.h"
#include "datetime.h"
#include "time_convert.h"



typedef struct _ColorValue
{
    Node		node;
    char		userid[33];
    char		application_name[21];
    char		color_use_name[16];
    long		duration;
    double		threshold_value;
    char		threshold_unit[2];
    char		color_name[26];
    List		list;
} ColorValue;
/*
    Function Prototypes
*/
    ColorValue* GetColorValue(const char * where);
    ColorValue* SelectColorValue(const char * where);
    int SelectColorValueCount(const char * where);
    int PutColorValue(const ColorValue * structPtr);
    int InsertColorValue(const ColorValue * structPtr);
    int UpdateColorValue(const ColorValue* structPtr, const char *where);
    int DeleteColorValue(const char *where);
    int UpdateColorValueByRecord (const ColorValue * newStructPtr, const ColorValue * oldStructPtr);
    int InsertOrUpdateColorValue(const ColorValue * structPtr);
    int InsertIfUniqueColorValue(const ColorValue * structPtr, bool *isUnique);
    bool ColorValueExists(const ColorValue * structPtr);
    int DeleteColorValueByRecord(const ColorValue * structPtr);
    void GetColorValuePrimaryKeyWhereString (const ColorValue * structPtr, char returnWhereString[] );
    void FreeColorValue(ColorValue * structPtr);
    DbStatus * GetColorValueDbStatus();
    void SetColorValueErrorLogging(int value);
#endif

/*
    File: ColorName.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ColorName_h
#define ColorName_h


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



typedef struct _ColorName
{
    Node		node;
    char		color_name[26];
    List		list;
} ColorName;
/*
    Function Prototypes
*/
    ColorName* GetColorName(const char * where);
    ColorName* SelectColorName(const char * where);
    int SelectColorNameCount(const char * where);
    int PutColorName(const ColorName * structPtr);
    int InsertColorName(const ColorName * structPtr);
    int UpdateColorName(const ColorName* structPtr, const char *where);
    int DeleteColorName(const char *where);
    int UpdateColorNameByRecord (const ColorName * newStructPtr, const ColorName * oldStructPtr);
    int InsertOrUpdateColorName(const ColorName * structPtr);
    int InsertIfUniqueColorName(const ColorName * structPtr, bool *isUnique);
    bool ColorNameExists(const ColorName * structPtr);
    int DeleteColorNameByRecord(const ColorName * structPtr);
    void GetColorNamePrimaryKeyWhereString (const ColorName * structPtr, char returnWhereString[] );
    void FreeColorName(ColorName * structPtr);
    DbStatus * GetColorNameDbStatus();
    void SetColorNameErrorLogging(int value);
#endif

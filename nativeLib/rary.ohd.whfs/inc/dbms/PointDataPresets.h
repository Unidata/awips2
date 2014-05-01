/*
    File: PointDataPresets.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PointDataPresets_h
#define PointDataPresets_h


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



typedef struct _PointDataPresets
{
    Node		node;
    char		preset_id[9];
    char		descr[31];
    short		preset_rank;
    char		preset_string[513];
    List		list;
} PointDataPresets;
/*
    Function Prototypes
*/
    PointDataPresets* GetPointDataPresets(const char * where);
    PointDataPresets* SelectPointDataPresets(const char * where);
    int SelectPointDataPresetsCount(const char * where);
    int PutPointDataPresets(const PointDataPresets * structPtr);
    int InsertPointDataPresets(const PointDataPresets * structPtr);
    int UpdatePointDataPresets(const PointDataPresets* structPtr, const char *where);
    int DeletePointDataPresets(const char *where);
    int UpdatePointDataPresetsByRecord (const PointDataPresets * newStructPtr, const PointDataPresets * oldStructPtr);
    int InsertOrUpdatePointDataPresets(const PointDataPresets * structPtr);
    int InsertIfUniquePointDataPresets(const PointDataPresets * structPtr, bool *isUnique);
    bool PointDataPresetsExists(const PointDataPresets * structPtr);
    int DeletePointDataPresetsByRecord(const PointDataPresets * structPtr);
    void GetPointDataPresetsPrimaryKeyWhereString (const PointDataPresets * structPtr, char returnWhereString[] );
    void FreePointDataPresets(PointDataPresets * structPtr);
    DbStatus * GetPointDataPresetsDbStatus();
    void SetPointDataPresetsErrorLogging(int value);
#endif

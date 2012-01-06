/*
    File: VTECrecord.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECrecord_h
#define VTECrecord_h


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



typedef struct _VTECrecord
{
    Node		node;
    char		record[3];
    char		name[26];
    List		list;
} VTECrecord;
/*
    Function Prototypes
*/
    VTECrecord* GetVTECrecord(const char * where);
    VTECrecord* SelectVTECrecord(const char * where);
    int SelectVTECrecordCount(const char * where);
    int PutVTECrecord(const VTECrecord * structPtr);
    int InsertVTECrecord(const VTECrecord * structPtr);
    int UpdateVTECrecord(const VTECrecord* structPtr, const char *where);
    int DeleteVTECrecord(const char *where);
    int UpdateVTECrecordByRecord (const VTECrecord * newStructPtr, const VTECrecord * oldStructPtr);
    int InsertOrUpdateVTECrecord(const VTECrecord * structPtr);
    int InsertIfUniqueVTECrecord(const VTECrecord * structPtr, bool *isUnique);
    bool VTECrecordExists(const VTECrecord * structPtr);
    int DeleteVTECrecordByRecord(const VTECrecord * structPtr);
    void GetVTECrecordPrimaryKeyWhereString (const VTECrecord * structPtr, char returnWhereString[] );
    void FreeVTECrecord(VTECrecord * structPtr);
    DbStatus * GetVTECrecordDbStatus();
    void SetVTECrecordErrorLogging(int value);
#endif

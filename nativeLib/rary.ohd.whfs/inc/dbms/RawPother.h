/*
    File: RawPother.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RawPother_h
#define RawPother_h


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



typedef struct _RawPother
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    float		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} RawPother;
/*
    Function Prototypes
*/
    RawPother* GetRawPother(const char * where);
    RawPother* SelectRawPother(const char * where);
    int SelectRawPotherCount(const char * where);
    int PutRawPother(const RawPother * structPtr);
    int InsertRawPother(const RawPother * structPtr);
    int UpdateRawPother(const RawPother* structPtr, const char *where);
    int DeleteRawPother(const char *where);
    int UpdateRawPotherByRecord (const RawPother * newStructPtr, const RawPother * oldStructPtr);
    int InsertOrUpdateRawPother(const RawPother * structPtr);
    int InsertIfUniqueRawPother(const RawPother * structPtr, bool *isUnique);
    bool RawPotherExists(const RawPother * structPtr);
    int DeleteRawPotherByRecord(const RawPother * structPtr);
    void GetRawPotherPrimaryKeyWhereString (const RawPother * structPtr, char returnWhereString[] );
    void FreeRawPother(RawPother * structPtr);
    DbStatus * GetRawPotherDbStatus();
    void SetRawPotherErrorLogging(int value);
#endif

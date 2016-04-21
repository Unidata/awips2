/*
    File: RawPP.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RawPP_h
#define RawPP_h


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



typedef struct _RawPP
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} RawPP;
/*
    Function Prototypes
*/
    RawPP* GetRawPP(const char * where);
    RawPP* SelectRawPP(const char * where);
    int SelectRawPPCount(const char * where);
    int PutRawPP(const RawPP * structPtr);
    int InsertRawPP(const RawPP * structPtr);
    int UpdateRawPP(const RawPP* structPtr, const char *where);
    int DeleteRawPP(const char *where);
    int UpdateRawPPByRecord (const RawPP * newStructPtr, const RawPP * oldStructPtr);
    int InsertOrUpdateRawPP(const RawPP * structPtr);
    int InsertIfUniqueRawPP(const RawPP * structPtr, bool *isUnique);
    bool RawPPExists(const RawPP * structPtr);
    int DeleteRawPPByRecord(const RawPP * structPtr);
    void GetRawPPPrimaryKeyWhereString (const RawPP * structPtr, char returnWhereString[] );
    void FreeRawPP(RawPP * structPtr);
    DbStatus * GetRawPPDbStatus();
    void SetRawPPErrorLogging(int value);
#endif

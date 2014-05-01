/*
    File: RawPC.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef RawPC_h
#define RawPC_h


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



typedef struct _RawPC
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
} RawPC;
/*
    Function Prototypes
*/
    RawPC* GetRawPC(const char * where);
    RawPC* SelectRawPC(const char * where);
    int SelectRawPCCount(const char * where);
    int PutRawPC(const RawPC * structPtr);
    int InsertRawPC(const RawPC * structPtr);
    int UpdateRawPC(const RawPC* structPtr, const char *where);
    int DeleteRawPC(const char *where);
    int UpdateRawPCByRecord (const RawPC * newStructPtr, const RawPC * oldStructPtr);
    int InsertOrUpdateRawPC(const RawPC * structPtr);
    int InsertIfUniqueRawPC(const RawPC * structPtr, bool *isUnique);
    bool RawPCExists(const RawPC * structPtr);
    int DeleteRawPCByRecord(const RawPC * structPtr);
    void GetRawPCPrimaryKeyWhereString (const RawPC * structPtr, char returnWhereString[] );
    void FreeRawPC(RawPC * structPtr);
    DbStatus * GetRawPCDbStatus();
    void SetRawPCErrorLogging(int value);
#endif

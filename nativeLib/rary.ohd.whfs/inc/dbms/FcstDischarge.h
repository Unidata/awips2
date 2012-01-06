/*
    File: FcstDischarge.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstDischarge_h
#define FcstDischarge_h


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



typedef struct _FcstDischarge
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    float		probability;
    dtime_t		validtime;
    dtime_t		basistime;
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} FcstDischarge;
/*
    Function Prototypes
*/
    FcstDischarge* GetFcstDischarge(const char * where);
    FcstDischarge* SelectFcstDischarge(const char * where);
    int SelectFcstDischargeCount(const char * where);
    int PutFcstDischarge(const FcstDischarge * structPtr);
    int InsertFcstDischarge(const FcstDischarge * structPtr);
    int UpdateFcstDischarge(const FcstDischarge* structPtr, const char *where);
    int DeleteFcstDischarge(const char *where);
    int UpdateFcstDischargeByRecord (const FcstDischarge * newStructPtr, const FcstDischarge * oldStructPtr);
    int InsertOrUpdateFcstDischarge(const FcstDischarge * structPtr);
    int InsertIfUniqueFcstDischarge(const FcstDischarge * structPtr, bool *isUnique);
    bool FcstDischargeExists(const FcstDischarge * structPtr);
    int DeleteFcstDischargeByRecord(const FcstDischarge * structPtr);
    void GetFcstDischargePrimaryKeyWhereString (const FcstDischarge * structPtr, char returnWhereString[] );
    void FreeFcstDischarge(FcstDischarge * structPtr);
    DbStatus * GetFcstDischargeDbStatus();
    void SetFcstDischargeErrorLogging(int value);
#endif

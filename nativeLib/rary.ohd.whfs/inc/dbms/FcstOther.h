/*
    File: FcstOther.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstOther_h
#define FcstOther_h


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



typedef struct _FcstOther
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
} FcstOther;
/*
    Function Prototypes
*/
    FcstOther* GetFcstOther(const char * where);
    FcstOther* SelectFcstOther(const char * where);
    int SelectFcstOtherCount(const char * where);
    int PutFcstOther(const FcstOther * structPtr);
    int InsertFcstOther(const FcstOther * structPtr);
    int UpdateFcstOther(const FcstOther* structPtr, const char *where);
    int DeleteFcstOther(const char *where);
    int UpdateFcstOtherByRecord (const FcstOther * newStructPtr, const FcstOther * oldStructPtr);
    int InsertOrUpdateFcstOther(const FcstOther * structPtr);
    int InsertIfUniqueFcstOther(const FcstOther * structPtr, bool *isUnique);
    bool FcstOtherExists(const FcstOther * structPtr);
    int DeleteFcstOtherByRecord(const FcstOther * structPtr);
    void GetFcstOtherPrimaryKeyWhereString (const FcstOther * structPtr, char returnWhereString[] );
    void FreeFcstOther(FcstOther * structPtr);
    DbStatus * GetFcstOtherDbStatus();
    void SetFcstOtherErrorLogging(int value);
#endif

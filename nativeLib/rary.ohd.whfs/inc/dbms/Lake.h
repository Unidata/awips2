/*
    File: Lake.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Lake_h
#define Lake_h


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



typedef struct _Lake
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
} Lake;
/*
    Function Prototypes
*/
    Lake* GetLake(const char * where);
    Lake* SelectLake(const char * where);
    int SelectLakeCount(const char * where);
    int PutLake(const Lake * structPtr);
    int InsertLake(const Lake * structPtr);
    int UpdateLake(const Lake* structPtr, const char *where);
    int DeleteLake(const char *where);
    int UpdateLakeByRecord (const Lake * newStructPtr, const Lake * oldStructPtr);
    int InsertOrUpdateLake(const Lake * structPtr);
    int InsertIfUniqueLake(const Lake * structPtr, bool *isUnique);
    bool LakeExists(const Lake * structPtr);
    int DeleteLakeByRecord(const Lake * structPtr);
    void GetLakePrimaryKeyWhereString (const Lake * structPtr, char returnWhereString[] );
    void FreeLake(Lake * structPtr);
    DbStatus * GetLakeDbStatus();
    void SetLakeErrorLogging(int value);
#endif

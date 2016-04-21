/*
    File: Ground.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Ground_h
#define Ground_h


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



typedef struct _Ground
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
} Ground;
/*
    Function Prototypes
*/
    Ground* GetGround(const char * where);
    Ground* SelectGround(const char * where);
    int SelectGroundCount(const char * where);
    int PutGround(const Ground * structPtr);
    int InsertGround(const Ground * structPtr);
    int UpdateGround(const Ground* structPtr, const char *where);
    int DeleteGround(const char *where);
    int UpdateGroundByRecord (const Ground * newStructPtr, const Ground * oldStructPtr);
    int InsertOrUpdateGround(const Ground * structPtr);
    int InsertIfUniqueGround(const Ground * structPtr, bool *isUnique);
    bool GroundExists(const Ground * structPtr);
    int DeleteGroundByRecord(const Ground * structPtr);
    void GetGroundPrimaryKeyWhereString (const Ground * structPtr, char returnWhereString[] );
    void FreeGround(Ground * structPtr);
    DbStatus * GetGroundDbStatus();
    void SetGroundErrorLogging(int value);
#endif

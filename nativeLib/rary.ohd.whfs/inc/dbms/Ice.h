/*
    File: Ice.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Ice_h
#define Ice_h


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



typedef struct _Ice
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
} Ice;
/*
    Function Prototypes
*/
    Ice* GetIce(const char * where);
    Ice* SelectIce(const char * where);
    int SelectIceCount(const char * where);
    int PutIce(const Ice * structPtr);
    int InsertIce(const Ice * structPtr);
    int UpdateIce(const Ice* structPtr, const char *where);
    int DeleteIce(const char *where);
    int UpdateIceByRecord (const Ice * newStructPtr, const Ice * oldStructPtr);
    int InsertOrUpdateIce(const Ice * structPtr);
    int InsertIfUniqueIce(const Ice * structPtr, bool *isUnique);
    bool IceExists(const Ice * structPtr);
    int DeleteIceByRecord(const Ice * structPtr);
    void GetIcePrimaryKeyWhereString (const Ice * structPtr, char returnWhereString[] );
    void FreeIce(Ice * structPtr);
    DbStatus * GetIceDbStatus();
    void SetIceErrorLogging(int value);
#endif

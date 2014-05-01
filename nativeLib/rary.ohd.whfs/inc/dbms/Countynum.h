/*
    File: Countynum.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Countynum_h
#define Countynum_h


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



typedef struct _Countynum
{
    Node		node;
    char		lid[9];
    char		state[3];
    char		county[21];
    List		list;
} Countynum;
/*
    Function Prototypes
*/
    Countynum* GetCountynum(const char * where);
    Countynum* SelectCountynum(const char * where);
    int SelectCountynumCount(const char * where);
    int PutCountynum(const Countynum * structPtr);
    int InsertCountynum(const Countynum * structPtr);
    int UpdateCountynum(const Countynum* structPtr, const char *where);
    int DeleteCountynum(const char *where);
    int UpdateCountynumByRecord (const Countynum * newStructPtr, const Countynum * oldStructPtr);
    int InsertOrUpdateCountynum(const Countynum * structPtr);
    int InsertIfUniqueCountynum(const Countynum * structPtr, bool *isUnique);
    bool CountynumExists(const Countynum * structPtr);
    int DeleteCountynumByRecord(const Countynum * structPtr);
    void GetCountynumPrimaryKeyWhereString (const Countynum * structPtr, char returnWhereString[] );
    void FreeCountynum(Countynum * structPtr);
    DbStatus * GetCountynumDbStatus();
    void SetCountynumErrorLogging(int value);
#endif

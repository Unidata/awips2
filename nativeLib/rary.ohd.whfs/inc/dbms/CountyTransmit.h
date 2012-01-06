/*
    File: CountyTransmit.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CountyTransmit_h
#define CountyTransmit_h


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



typedef struct _CountyTransmit
{
    Node		node;
    char		call_sign[7];
    char		county[21];
    char		state[3];
    List		list;
} CountyTransmit;
/*
    Function Prototypes
*/
    CountyTransmit* GetCountyTransmit(const char * where);
    CountyTransmit* SelectCountyTransmit(const char * where);
    int SelectCountyTransmitCount(const char * where);
    int PutCountyTransmit(const CountyTransmit * structPtr);
    int InsertCountyTransmit(const CountyTransmit * structPtr);
    int UpdateCountyTransmit(const CountyTransmit* structPtr, const char *where);
    int DeleteCountyTransmit(const char *where);
    int UpdateCountyTransmitByRecord (const CountyTransmit * newStructPtr, const CountyTransmit * oldStructPtr);
    int InsertOrUpdateCountyTransmit(const CountyTransmit * structPtr);
    int InsertIfUniqueCountyTransmit(const CountyTransmit * structPtr, bool *isUnique);
    bool CountyTransmitExists(const CountyTransmit * structPtr);
    int DeleteCountyTransmitByRecord(const CountyTransmit * structPtr);
    void GetCountyTransmitPrimaryKeyWhereString (const CountyTransmit * structPtr, char returnWhereString[] );
    void FreeCountyTransmit(CountyTransmit * structPtr);
    DbStatus * GetCountyTransmitDbStatus();
    void SetCountyTransmitErrorLogging(int value);
#endif

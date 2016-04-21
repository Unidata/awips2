/*
    File: Datum.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Datum_h
#define Datum_h


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



typedef struct _Datum
{
    Node		node;
    char		lid[9];
    date_t		ddate;
    double		elev;
    List		list;
} Datum;
/*
    Function Prototypes
*/
    Datum* GetDatum(const char * where);
    Datum* SelectDatum(const char * where);
    int SelectDatumCount(const char * where);
    int PutDatum(const Datum * structPtr);
    int InsertDatum(const Datum * structPtr);
    int UpdateDatum(const Datum* structPtr, const char *where);
    int DeleteDatum(const char *where);
    int UpdateDatumByRecord (const Datum * newStructPtr, const Datum * oldStructPtr);
    int InsertOrUpdateDatum(const Datum * structPtr);
    int InsertIfUniqueDatum(const Datum * structPtr, bool *isUnique);
    bool DatumExists(const Datum * structPtr);
    int DeleteDatumByRecord(const Datum * structPtr);
    void GetDatumPrimaryKeyWhereString (const Datum * structPtr, char returnWhereString[] );
    void FreeDatum(Datum * structPtr);
    DbStatus * GetDatumDbStatus();
    void SetDatumErrorLogging(int value);
#endif

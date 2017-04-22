/*
    File: Discharge.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Discharge_h
#define Discharge_h


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



typedef struct _Discharge
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
} Discharge;
/*
    Function Prototypes
*/
    Discharge* GetDischarge(const char * where);
    Discharge* SelectDischarge(const char * where);
    int SelectDischargeCount(const char * where);
    int PutDischarge(const Discharge * structPtr);
    int InsertDischarge(const Discharge * structPtr);
    int UpdateDischarge(const Discharge* structPtr, const char *where);
    int DeleteDischarge(const char *where);
    int UpdateDischargeByRecord (const Discharge * newStructPtr, const Discharge * oldStructPtr);
    int InsertOrUpdateDischarge(const Discharge * structPtr);
    int InsertIfUniqueDischarge(const Discharge * structPtr, bool *isUnique);
    bool DischargeExists(const Discharge * structPtr);
    int DeleteDischargeByRecord(const Discharge * structPtr);
    void GetDischargePrimaryKeyWhereString (const Discharge * structPtr, char returnWhereString[] );
    void FreeDischarge(Discharge * structPtr);
    DbStatus * GetDischargeDbStatus();
    void SetDischargeErrorLogging(int value);
#endif

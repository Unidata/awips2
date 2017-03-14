/*
    File: Evaporation.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Evaporation_h
#define Evaporation_h


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



typedef struct _Evaporation
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
} Evaporation;
/*
    Function Prototypes
*/
    Evaporation* GetEvaporation(const char * where);
    Evaporation* SelectEvaporation(const char * where);
    int SelectEvaporationCount(const char * where);
    int PutEvaporation(const Evaporation * structPtr);
    int InsertEvaporation(const Evaporation * structPtr);
    int UpdateEvaporation(const Evaporation* structPtr, const char *where);
    int DeleteEvaporation(const char *where);
    int UpdateEvaporationByRecord (const Evaporation * newStructPtr, const Evaporation * oldStructPtr);
    int InsertOrUpdateEvaporation(const Evaporation * structPtr);
    int InsertIfUniqueEvaporation(const Evaporation * structPtr, bool *isUnique);
    bool EvaporationExists(const Evaporation * structPtr);
    int DeleteEvaporationByRecord(const Evaporation * structPtr);
    void GetEvaporationPrimaryKeyWhereString (const Evaporation * structPtr, char returnWhereString[] );
    void FreeEvaporation(Evaporation * structPtr);
    DbStatus * GetEvaporationDbStatus();
    void SetEvaporationErrorLogging(int value);
#endif

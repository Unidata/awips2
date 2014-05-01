/*
    File: Radiation.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Radiation_h
#define Radiation_h


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



typedef struct _Radiation
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
} Radiation;
/*
    Function Prototypes
*/
    Radiation* GetRadiation(const char * where);
    Radiation* SelectRadiation(const char * where);
    int SelectRadiationCount(const char * where);
    int PutRadiation(const Radiation * structPtr);
    int InsertRadiation(const Radiation * structPtr);
    int UpdateRadiation(const Radiation* structPtr, const char *where);
    int DeleteRadiation(const char *where);
    int UpdateRadiationByRecord (const Radiation * newStructPtr, const Radiation * oldStructPtr);
    int InsertOrUpdateRadiation(const Radiation * structPtr);
    int InsertIfUniqueRadiation(const Radiation * structPtr, bool *isUnique);
    bool RadiationExists(const Radiation * structPtr);
    int DeleteRadiationByRecord(const Radiation * structPtr);
    void GetRadiationPrimaryKeyWhereString (const Radiation * structPtr, char returnWhereString[] );
    void FreeRadiation(Radiation * structPtr);
    DbStatus * GetRadiationDbStatus();
    void SetRadiationErrorLogging(int value);
#endif

/*
    File: GateDam.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef GateDam_h
#define GateDam_h


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



typedef struct _GateDam
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
} GateDam;
/*
    Function Prototypes
*/
    GateDam* GetGateDam(const char * where);
    GateDam* SelectGateDam(const char * where);
    int SelectGateDamCount(const char * where);
    int PutGateDam(const GateDam * structPtr);
    int InsertGateDam(const GateDam * structPtr);
    int UpdateGateDam(const GateDam* structPtr, const char *where);
    int DeleteGateDam(const char *where);
    int UpdateGateDamByRecord (const GateDam * newStructPtr, const GateDam * oldStructPtr);
    int InsertOrUpdateGateDam(const GateDam * structPtr);
    int InsertIfUniqueGateDam(const GateDam * structPtr, bool *isUnique);
    bool GateDamExists(const GateDam * structPtr);
    int DeleteGateDamByRecord(const GateDam * structPtr);
    void GetGateDamPrimaryKeyWhereString (const GateDam * structPtr, char returnWhereString[] );
    void FreeGateDam(GateDam * structPtr);
    DbStatus * GetGateDamDbStatus();
    void SetGateDamErrorLogging(int value);
#endif

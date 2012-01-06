/*
    File: Agricultural.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Agricultural_h
#define Agricultural_h


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



typedef struct _Agricultural
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
} Agricultural;
/*
    Function Prototypes
*/
    Agricultural* GetAgricultural(const char * where);
    Agricultural* SelectAgricultural(const char * where);
    int SelectAgriculturalCount(const char * where);
    int PutAgricultural(const Agricultural * structPtr);
    int InsertAgricultural(const Agricultural * structPtr);
    int UpdateAgricultural(const Agricultural* structPtr, const char *where);
    int DeleteAgricultural(const char *where);
    int UpdateAgriculturalByRecord (const Agricultural * newStructPtr, const Agricultural * oldStructPtr);
    int InsertOrUpdateAgricultural(const Agricultural * structPtr);
    int InsertIfUniqueAgricultural(const Agricultural * structPtr, bool *isUnique);
    bool AgriculturalExists(const Agricultural * structPtr);
    int DeleteAgriculturalByRecord(const Agricultural * structPtr);
    void GetAgriculturalPrimaryKeyWhereString (const Agricultural * structPtr, char returnWhereString[] );
    void FreeAgricultural(Agricultural * structPtr);
    DbStatus * GetAgriculturalDbStatus();
    void SetAgriculturalErrorLogging(int value);
#endif

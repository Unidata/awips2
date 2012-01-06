/*
    File: DHRRadar.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DHRRadar_h
#define DHRRadar_h


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



typedef struct _DHRRadar
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		volcovpat;
    short		opermode;
    float		dbzmin;
    float		dbzinc;
    float		dbzcnt;
    short		j_date;
    short		j_time;
    short		mean_field_bias;
    short		sample_size;
    char		grid_filename[21];
    List		list;
} DHRRadar;
/*
    Function Prototypes
*/
    DHRRadar* GetDHRRadar(const char * where);
    DHRRadar* SelectDHRRadar(const char * where);
    int SelectDHRRadarCount(const char * where);
    int PutDHRRadar(const DHRRadar * structPtr);
    int InsertDHRRadar(const DHRRadar * structPtr);
    int UpdateDHRRadar(const DHRRadar* structPtr, const char *where);
    int DeleteDHRRadar(const char *where);
    int UpdateDHRRadarByRecord (const DHRRadar * newStructPtr, const DHRRadar * oldStructPtr);
    int InsertOrUpdateDHRRadar(const DHRRadar * structPtr);
    int InsertIfUniqueDHRRadar(const DHRRadar * structPtr, bool *isUnique);
    bool DHRRadarExists(const DHRRadar * structPtr);
    int DeleteDHRRadarByRecord(const DHRRadar * structPtr);
    void GetDHRRadarPrimaryKeyWhereString (const DHRRadar * structPtr, char returnWhereString[] );
    void FreeDHRRadar(DHRRadar * structPtr);
    DbStatus * GetDHRRadarDbStatus();
    void SetDHRRadarErrorLogging(int value);
#endif

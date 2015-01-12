/*
    File: DPRRadar.h
    Author  : CDBGEN
    Created : Mon Jul 29 08:10:26 EDT 2013 using database hd_ob9eempty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DPRRadar_h
#define DPRRadar_h


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



typedef struct _DPRRadar
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		volcovpat;
    short		opermode;
    float		maxval;
    float		scale;
    float		setoff;
    long		j_end_date;
    long		j_end_time;
    short		mean_field_bias;
    short		precipdetectedflag;
    char		grid_filename[21];
    List		list;
} DPRRadar;
/*
    Function Prototypes
*/
    DPRRadar* GetDPRRadar(const char * where);
    DPRRadar* SelectDPRRadar(const char * where);
    int SelectDPRRadarCount(const char * where);
    int PutDPRRadar(const DPRRadar * structPtr);
    int InsertDPRRadar(const DPRRadar * structPtr);
    int UpdateDPRRadar(const DPRRadar* structPtr, const char *where);
    int DeleteDPRRadar(const char *where);
    int UpdateDPRRadarByRecord (const DPRRadar * newStructPtr, const DPRRadar * oldStructPtr);
    int InsertOrUpdateDPRRadar(const DPRRadar * structPtr);
    int InsertIfUniqueDPRRadar(const DPRRadar * structPtr, bool *isUnique);
    bool DPRRadarExists(const DPRRadar * structPtr);
    int DeleteDPRRadarByRecord(const DPRRadar * structPtr);
    void GetDPRRadarPrimaryKeyWhereString (const DPRRadar * structPtr, char returnWhereString[] );
    void FreeDPRRadar(DPRRadar * structPtr);
    DbStatus * GetDPRRadarDbStatus();
    void SetDPRRadarErrorLogging(int value);
#endif

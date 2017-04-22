/*
    File: DAARadar.h
    Author  : CDBGEN
    Created : Wed Dec 04 19:07:04 EST 2013 using database hd_ob9eempty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DAARadar_h
#define DAARadar_h


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



typedef struct _DAARadar
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		minoff;
    float		maxvalh;
    float		maxvald;
    float		s1_bias_value;
    dtime_t		producttime;
    short		null_product_flag;
    long		coverage_dur;
    char		grid_filename[21];
    List		list;
} DAARadar;
/*
    Function Prototypes
*/
    DAARadar* GetDAARadar(const char * where);
    DAARadar* SelectDAARadar(const char * where);
    int SelectDAARadarCount(const char * where);
    int PutDAARadar(const DAARadar * structPtr);
    int InsertDAARadar(const DAARadar * structPtr);
    int UpdateDAARadar(const DAARadar* structPtr, const char *where);
    int DeleteDAARadar(const char *where);
    int UpdateDAARadarByRecord (const DAARadar * newStructPtr, const DAARadar * oldStructPtr);
    int InsertOrUpdateDAARadar(const DAARadar * structPtr);
    int InsertIfUniqueDAARadar(const DAARadar * structPtr, bool *isUnique);
    bool DAARadarExists(const DAARadar * structPtr);
    int DeleteDAARadarByRecord(const DAARadar * structPtr);
    void GetDAARadarPrimaryKeyWhereString (const DAARadar * structPtr, char returnWhereString[] );
    void FreeDAARadar(DAARadar * structPtr);
    DbStatus * GetDAARadarDbStatus();
    void SetDAARadarErrorLogging(int value);
#endif

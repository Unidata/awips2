/*
    File: DSARadar.h
    Author  : CDBGEN
    Created : Mon Jul 29 08:10:26 EDT 2013 using database hd_ob9eempty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DSARadar_h
#define DSARadar_h


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



typedef struct _DSARadar
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		volcovpat;
    short		opermode;
    float		maxval;
    float		scale;
    float		setoff;
    dtime_t		begin_time;
    dtime_t		end_time;
    short		j_beg_date;
    short		j_beg_time;
    short		j_end_date;
    short		j_end_time;
    short		mean_field_bias;
    short		nullproductflag;
    char		grid_filename[21];
    List		list;
} DSARadar;
/*
    Function Prototypes
*/
    DSARadar* GetDSARadar(const char * where);
    DSARadar* SelectDSARadar(const char * where);
    int SelectDSARadarCount(const char * where);
    int PutDSARadar(const DSARadar * structPtr);
    int InsertDSARadar(const DSARadar * structPtr);
    int UpdateDSARadar(const DSARadar* structPtr, const char *where);
    int DeleteDSARadar(const char *where);
    int UpdateDSARadarByRecord (const DSARadar * newStructPtr, const DSARadar * oldStructPtr);
    int InsertOrUpdateDSARadar(const DSARadar * structPtr);
    int InsertIfUniqueDSARadar(const DSARadar * structPtr, bool *isUnique);
    bool DSARadarExists(const DSARadar * structPtr);
    int DeleteDSARadarByRecord(const DSARadar * structPtr);
    void GetDSARadarPrimaryKeyWhereString (const DSARadar * structPtr, char returnWhereString[] );
    void FreeDSARadar(DSARadar * structPtr);
    DbStatus * GetDSARadarDbStatus();
    void SetDSARadarErrorLogging(int value);
#endif

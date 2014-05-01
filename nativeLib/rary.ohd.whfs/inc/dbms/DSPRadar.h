/*
    File: DSPRadar.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DSPRadar_h
#define DSPRadar_h


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



typedef struct _DSPRadar
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		volcovpat;
    short		opermode;
    float		minval;
    float		maxval;
    float		num_data_lev;
    float		scale_factor;
    dtime_t		begin_time;
    dtime_t		end_time;
    short		j_beg_date;
    short		j_beg_time;
    short		j_end_date;
    short		j_end_time;
    short		mean_field_bias;
    short		sample_size;
    char		grid_filename[21];
    List		list;
} DSPRadar;
/*
    Function Prototypes
*/
    DSPRadar* GetDSPRadar(const char * where);
    DSPRadar* SelectDSPRadar(const char * where);
    int SelectDSPRadarCount(const char * where);
    int PutDSPRadar(const DSPRadar * structPtr);
    int InsertDSPRadar(const DSPRadar * structPtr);
    int UpdateDSPRadar(const DSPRadar* structPtr, const char *where);
    int DeleteDSPRadar(const char *where);
    int UpdateDSPRadarByRecord (const DSPRadar * newStructPtr, const DSPRadar * oldStructPtr);
    int InsertOrUpdateDSPRadar(const DSPRadar * structPtr);
    int InsertIfUniqueDSPRadar(const DSPRadar * structPtr, bool *isUnique);
    bool DSPRadarExists(const DSPRadar * structPtr);
    int DeleteDSPRadarByRecord(const DSPRadar * structPtr);
    void GetDSPRadarPrimaryKeyWhereString (const DSPRadar * structPtr, char returnWhereString[] );
    void FreeDSPRadar(DSPRadar * structPtr);
    DbStatus * GetDSPRadarDbStatus();
    void SetDSPRadarErrorLogging(int value);
#endif

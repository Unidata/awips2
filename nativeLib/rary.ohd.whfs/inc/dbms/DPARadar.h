/*
    File: DPARadar.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DPARadar_h
#define DPARadar_h


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



typedef struct _DPARadar
{
    Node		node;
    char		radid[4];
    dtime_t		obstime;
    short		minoff;
    float		maxvalh;
    float		maxvald;
    float		s1_bias_value;
    dtime_t		producttime;
    short		nisolbin;
    short		noutint;
    short		noutrep;
    float		areared;
    float		biscanr;
    long		block_bins_reject;
    long		clutter_bins_rej;
    long		bins_smoothed;
    float		scan_bins_filled;
    float		high_elev_angle;
    float		scan_rain_area;
    short		nbadscan;
    short		nhourout;
    short		volcovpat;
    short		opermode;
    char		missper[2];
    short		supplmess;
    char		grid_filename[21];
    List		list;
} DPARadar;
/*
    Function Prototypes
*/
    DPARadar* GetDPARadar(const char * where);
    DPARadar* SelectDPARadar(const char * where);
    int SelectDPARadarCount(const char * where);
    int PutDPARadar(const DPARadar * structPtr);
    int InsertDPARadar(const DPARadar * structPtr);
    int UpdateDPARadar(const DPARadar* structPtr, const char *where);
    int DeleteDPARadar(const char *where);
    int UpdateDPARadarByRecord (const DPARadar * newStructPtr, const DPARadar * oldStructPtr);
    int InsertOrUpdateDPARadar(const DPARadar * structPtr);
    int InsertIfUniqueDPARadar(const DPARadar * structPtr, bool *isUnique);
    bool DPARadarExists(const DPARadar * structPtr);
    int DeleteDPARadarByRecord(const DPARadar * structPtr);
    void GetDPARadarPrimaryKeyWhereString (const DPARadar * structPtr, char returnWhereString[] );
    void FreeDPARadar(DPARadar * structPtr);
    DbStatus * GetDPARadarDbStatus();
    void SetDPARadarErrorLogging(int value);
#endif

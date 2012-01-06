// This is a view record !
/*
    File: HvStation.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HvStation_h
#define HvStation_h


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



typedef struct _HvStation
{
    Node		node;
    char		lid[9];
    char		name[51];
    double		lat;
    double		lon;
    char		stream_name[33];
    char		primary_pe[3];
    double		flood_stage;
    double		flood_flow;
    double		action_stage;
    double		action_flow;
    char		disp_class[11];
    char		is_dcp[2];
    char		is_observer[2];
    char		telem_type[11];
    List		list;
} HvStation;
/*
    Function Prototypes
*/
    HvStation* GetHvStation(const char * where);
    HvStation* SelectHvStation(const char * where);
    int SelectHvStationCount(const char * where);
    void FreeHvStation(HvStation * structPtr);
    DbStatus * GetHvStationDbStatus();
    void SetHvStationErrorLogging(int value);
#endif

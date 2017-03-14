// This is a view record !
/*
    File: LocRiverMon.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocRiverMon_h
#define LocRiverMon_h


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



typedef struct _LocRiverMon
{
    Node		node;
    char		lid[9];
    char		name[51];
    char		county[21];
    char		state[3];
    char		hsa[4];
    char		stream[33];
    double		bankfull;
    double		action_stage;
    double		flood_stage;
    double		flood_flow;
    double		action_flow;
    char		primary_pe[3];
    char		proximity[7];
    char		reach[81];
    double		mile;
    double		minor;
    double		moderate;
    double		major;
    List		list;
} LocRiverMon;
/*
    Function Prototypes
*/
    LocRiverMon* GetLocRiverMon(const char * where);
    LocRiverMon* SelectLocRiverMon(const char * where);
    int SelectLocRiverMonCount(const char * where);
    void FreeLocRiverMon(LocRiverMon * structPtr);
    DbStatus * GetLocRiverMonDbStatus();
    void SetLocRiverMonErrorLogging(int value);
#endif

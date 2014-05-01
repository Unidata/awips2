// This is a view record !
/*
    File: ZoneInfo.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ZoneInfo_h
#define ZoneInfo_h


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



typedef struct _ZoneInfo
{
    Node		node;
    char		lid[9];
    char		state[3];
    char		zonenum[4];
    char		descr[21];
    List		list;
} ZoneInfo;
/*
    Function Prototypes
*/
    ZoneInfo* GetZoneInfo(const char * where);
    ZoneInfo* SelectZoneInfo(const char * where);
    int SelectZoneInfoCount(const char * where);
    void FreeZoneInfo(ZoneInfo * structPtr);
    DbStatus * GetZoneInfoDbStatus();
    void SetZoneInfoErrorLogging(int value);
#endif

// This is a view record !
/*
    File: StationList.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef StationList_h
#define StationList_h


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



typedef struct _StationList
{
    Node		node;
    char		lid[9];
    char		name[51];
    char		firstname[13];
    char		lastname[29];
    char		rb[31];
    char		county[21];
    char		wfo[4];
    char		hphone[19];
    char		ophone[19];
    List		list;
} StationList;
/*
    Function Prototypes
*/
    StationList* GetStationList(const char * where);
    StationList* SelectStationList(const char * where);
    int SelectStationListCount(const char * where);
    void FreeStationList(StationList * structPtr);
    DbStatus * GetStationListDbStatus();
    void SetStationListErrorLogging(int value);
#endif

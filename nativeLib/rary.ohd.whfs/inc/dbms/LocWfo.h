// This is a view record !
/*
    File: LocWfo.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocWfo_h
#define LocWfo_h


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



typedef struct _LocWfo
{
    Node		node;
    char		lid[9];
    char		county[21];
    char		state[3];
    char		wfo[4];
    char		primary_back[4];
    char		secondary_back[4];
    List		list;
} LocWfo;
/*
    Function Prototypes
*/
    LocWfo* GetLocWfo(const char * where);
    LocWfo* SelectLocWfo(const char * where);
    int SelectLocWfoCount(const char * where);
    void FreeLocWfo(LocWfo * structPtr);
    DbStatus * GetLocWfoDbStatus();
    void SetLocWfoErrorLogging(int value);
#endif

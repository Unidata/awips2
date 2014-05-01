// This is a view record !
/*
    File: LocClass.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocClass_h
#define LocClass_h


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



typedef struct _LocClass
{
    Node		node;
    char		lid[9];
    char		name[51];
    double		lat;
    double		lon;
    char		wfo[4];
    char		hsa[4];
    long		post;
    char		disp_class[11];
    char		is_dcp[2];
    char		is_observer[2];
    char		telem_type[11];
    List		list;
} LocClass;
/*
    Function Prototypes
*/
    LocClass* GetLocClass(const char * where);
    LocClass* SelectLocClass(const char * where);
    int SelectLocClassCount(const char * where);
    void FreeLocClass(LocClass * structPtr);
    DbStatus * GetLocClassDbStatus();
    void SetLocClassErrorLogging(int value);
#endif

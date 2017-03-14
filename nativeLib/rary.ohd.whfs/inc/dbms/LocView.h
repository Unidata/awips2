// This is a view record !
/*
    File: LocView.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocView_h
#define LocView_h


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



typedef struct _LocView
{
    Node		node;
    char		lid[9];
    char		name[51];
    double		lat;
    double		lon;
    char		rb[31];
    char		state[3];
    char		county[21];
    char		type[5];
    char		wfo[4];
    char		hsa[4];
    long		post;
    char		stream[33];
    char		gsno[11];
    List		list;
} LocView;
/*
    Function Prototypes
*/
    LocView* GetLocView(const char * where);
    LocView* SelectLocView(const char * where);
    int SelectLocViewCount(const char * where);
    void FreeLocView(LocView * structPtr);
    DbStatus * GetLocViewDbStatus();
    void SetLocViewErrorLogging(int value);
#endif

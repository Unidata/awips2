// This is a view record !
/*
    File: HwStages.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef HwStages_h
#define HwStages_h


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



typedef struct _HwStages
{
    Node		node;
    char		lid[9];
    double		fs;
    double		wstg;
    double		ms;
    List		list;
} HwStages;
/*
    Function Prototypes
*/
    HwStages* GetHwStages(const char * where);
    HwStages* SelectHwStages(const char * where);
    int SelectHwStagesCount(const char * where);
    void FreeHwStages(HwStages * structPtr);
    DbStatus * GetHwStagesDbStatus();
    void SetHwStagesErrorLogging(int value);
#endif

// This is a view record !
/*
    File: LocTransmit.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocTransmit_h
#define LocTransmit_h


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



typedef struct _LocTransmit
{
    Node		node;
    char		lid[9];
    char		call_sign[7];
    List		list;
} LocTransmit;
/*
    Function Prototypes
*/
    LocTransmit* GetLocTransmit(const char * where);
    LocTransmit* SelectLocTransmit(const char * where);
    int SelectLocTransmitCount(const char * where);
    void FreeLocTransmit(LocTransmit * structPtr);
    DbStatus * GetLocTransmitDbStatus();
    void SetLocTransmitErrorLogging(int value);
#endif

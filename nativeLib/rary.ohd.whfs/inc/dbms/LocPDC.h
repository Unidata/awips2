// This is a view record !
/*
    File: LocPDC.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LocPDC_h
#define LocPDC_h


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



typedef struct _LocPDC
{
    Node		node;
    char		lid[9];
    char		name[51];
    double		lat;
    double		lon;
    char		hsa[4];
    long		post;
    double		elev;
    char		primary_pe[3];
    double		fs;
    double		fq;
    char		disp_class[11];
    char		is_dcp[2];
    char		is_observer[2];
    char		telem_type[11];
    List		list;
} LocPDC;
/*
    Function Prototypes
*/
    LocPDC* GetLocPDC(const char * where);
    LocPDC* SelectLocPDC(const char * where);
    int SelectLocPDCCount(const char * where);
    void FreeLocPDC(LocPDC * structPtr);
    DbStatus * GetLocPDCDbStatus();
    void SetLocPDCErrorLogging(int value);
#endif

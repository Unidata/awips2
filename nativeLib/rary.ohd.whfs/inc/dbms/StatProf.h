// This is a view record !
/*
    File: StatProf.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef StatProf_h
#define StatProf_h


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



typedef struct _StatProf
{
    Node		node;
    char		lid[9];
    char		name[51];
    char		primary_pe[3];
    char		stream[33];
    double		fs;
    double		wstg;
    double		fq;
    double		action_flow;
    double		zd;
    double		mile;
    char		reach[81];
    char		proximity[7];
    List		list;
} StatProf;
/*
    Function Prototypes
*/
    StatProf* GetStatProf(const char * where);
    StatProf* SelectStatProf(const char * where);
    int SelectStatProfCount(const char * where);
    void FreeStatProf(StatProf * structPtr);
    DbStatus * GetStatProfDbStatus();
    void SetStatProfErrorLogging(int value);
#endif

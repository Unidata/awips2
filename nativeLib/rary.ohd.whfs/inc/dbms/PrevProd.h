// This is a view record !
/*
    File: PrevProd.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PrevProd_h
#define PrevProd_h


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



typedef struct _PrevProd
{
    Node		node;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    char		prodtype[2];
    long		issnum;
    List		list;
} PrevProd;
/*
    Function Prototypes
*/
    PrevProd* GetPrevProd(const char * where);
    PrevProd* SelectPrevProd(const char * where);
    int SelectPrevProdCount(const char * where);
    void FreePrevProd(PrevProd * structPtr);
    DbStatus * GetPrevProdDbStatus();
    void SetPrevProdErrorLogging(int value);
#endif

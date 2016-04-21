/*
    File: FpPrevProd.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FpPrevProd_h
#define FpPrevProd_h


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



typedef struct _FpPrevProd
{
    Node		node;
    char		lid[9];
    char		product_id[11];
    char		prod_categ[4];
    dtime_t		producttime;
    char		office_id[6];
    double		obsvalue;
    dtime_t		obstime;
    double		max_fcstvalue;
    dtime_t		validtime;
    dtime_t		basistime;
    List		list;
} FpPrevProd;
/*
    Function Prototypes
*/
    FpPrevProd* GetFpPrevProd(const char * where);
    FpPrevProd* SelectFpPrevProd(const char * where);
    int SelectFpPrevProdCount(const char * where);
    int PutFpPrevProd(const FpPrevProd * structPtr);
    int InsertFpPrevProd(const FpPrevProd * structPtr);
    int UpdateFpPrevProd(const FpPrevProd* structPtr, const char *where);
    int DeleteFpPrevProd(const char *where);
    int UpdateFpPrevProdByRecord (const FpPrevProd * newStructPtr, const FpPrevProd * oldStructPtr);
    int InsertOrUpdateFpPrevProd(const FpPrevProd * structPtr);
    int InsertIfUniqueFpPrevProd(const FpPrevProd * structPtr, bool *isUnique);
    bool FpPrevProdExists(const FpPrevProd * structPtr);
    int DeleteFpPrevProdByRecord(const FpPrevProd * structPtr);
    void GetFpPrevProdPrimaryKeyWhereString (const FpPrevProd * structPtr, char returnWhereString[] );
    void FreeFpPrevProd(FpPrevProd * structPtr);
    DbStatus * GetFpPrevProdDbStatus();
    void SetFpPrevProdErrorLogging(int value);
#endif

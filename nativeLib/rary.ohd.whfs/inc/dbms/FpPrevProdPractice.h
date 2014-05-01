/*
    File: FpPrevProdPractice.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FpPrevProdPractice_h
#define FpPrevProdPractice_h


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



typedef struct _FpPrevProdPractice
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
} FpPrevProdPractice;
/*
    Function Prototypes
*/
    FpPrevProdPractice* GetFpPrevProdPractice(const char * where);
    FpPrevProdPractice* SelectFpPrevProdPractice(const char * where);
    int SelectFpPrevProdPracticeCount(const char * where);
    int PutFpPrevProdPractice(const FpPrevProdPractice * structPtr);
    int InsertFpPrevProdPractice(const FpPrevProdPractice * structPtr);
    int UpdateFpPrevProdPractice(const FpPrevProdPractice* structPtr, const char *where);
    int DeleteFpPrevProdPractice(const char *where);
    int UpdateFpPrevProdPracticeByRecord (const FpPrevProdPractice * newStructPtr, const FpPrevProdPractice * oldStructPtr);
    int InsertOrUpdateFpPrevProdPractice(const FpPrevProdPractice * structPtr);
    int InsertIfUniqueFpPrevProdPractice(const FpPrevProdPractice * structPtr, bool *isUnique);
    bool FpPrevProdPracticeExists(const FpPrevProdPractice * structPtr);
    int DeleteFpPrevProdPracticeByRecord(const FpPrevProdPractice * structPtr);
    void GetFpPrevProdPracticePrimaryKeyWhereString (const FpPrevProdPractice * structPtr, char returnWhereString[] );
    void FreeFpPrevProdPractice(FpPrevProdPractice * structPtr);
    DbStatus * GetFpPrevProdPracticeDbStatus();
    void SetFpPrevProdPracticeErrorLogging(int value);
#endif

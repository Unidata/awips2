/*
    File: PurgeProduct.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PurgeProduct_h
#define PurgeProduct_h


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



typedef struct _PurgeProduct
{
    Node		node;
    char		product_id[11];
    long		num_versions;
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} PurgeProduct;
/*
    Function Prototypes
*/
    PurgeProduct* GetPurgeProduct(const char * where);
    PurgeProduct* SelectPurgeProduct(const char * where);
    int SelectPurgeProductCount(const char * where);
    int PutPurgeProduct(const PurgeProduct * structPtr);
    int InsertPurgeProduct(const PurgeProduct * structPtr);
    int UpdatePurgeProduct(const PurgeProduct* structPtr, const char *where);
    int DeletePurgeProduct(const char *where);
    int UpdatePurgeProductByRecord (const PurgeProduct * newStructPtr, const PurgeProduct * oldStructPtr);
    int InsertOrUpdatePurgeProduct(const PurgeProduct * structPtr);
    int InsertIfUniquePurgeProduct(const PurgeProduct * structPtr, bool *isUnique);
    bool PurgeProductExists(const PurgeProduct * structPtr);
    int DeletePurgeProductByRecord(const PurgeProduct * structPtr);
    void GetPurgeProductPrimaryKeyWhereString (const PurgeProduct * structPtr, char returnWhereString[] );
    void FreePurgeProduct(PurgeProduct * structPtr);
    DbStatus * GetPurgeProductDbStatus();
    void SetPurgeProductErrorLogging(int value);
#endif

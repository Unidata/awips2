/*
    File: TextProduct.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef TextProduct_h
#define TextProduct_h


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



typedef struct _TextProduct
{
    Node		node;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    char		prodtype[2];
    long		issnum;
    char *		product;
    List		list;
} TextProduct;
/*
    Function Prototypes
*/
    TextProduct* GetTextProduct(const char * where);
    TextProduct* SelectTextProduct(const char * where);
    int SelectTextProductCount(const char * where);
    int PutTextProduct(const TextProduct * structPtr);
    int InsertTextProduct(const TextProduct * structPtr);
    int UpdateTextProduct(const TextProduct* structPtr, const char *where);
    int DeleteTextProduct(const char *where);
    int UpdateTextProductByRecord (const TextProduct * newStructPtr, const TextProduct * oldStructPtr);
    int InsertOrUpdateTextProduct(const TextProduct * structPtr);
    int InsertIfUniqueTextProduct(const TextProduct * structPtr, bool *isUnique);
    bool TextProductExists(const TextProduct * structPtr);
    int DeleteTextProductByRecord(const TextProduct * structPtr);
    void GetTextProductPrimaryKeyWhereString (const TextProduct * structPtr, char returnWhereString[] );
    void FreeTextProduct(TextProduct * structPtr);
    DbStatus * GetTextProductDbStatus();
    void SetTextProductErrorLogging(int value);
#endif

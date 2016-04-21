/*
    File: ProductLink.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ProductLink_h
#define ProductLink_h


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



typedef struct _ProductLink
{
    Node		node;
    char		lid[9];
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} ProductLink;
/*
    Function Prototypes
*/
    ProductLink* GetProductLink(const char * where);
    ProductLink* SelectProductLink(const char * where);
    int SelectProductLinkCount(const char * where);
    int PutProductLink(const ProductLink * structPtr);
    int InsertProductLink(const ProductLink * structPtr);
    int UpdateProductLink(const ProductLink* structPtr, const char *where);
    int DeleteProductLink(const char *where);
    int UpdateProductLinkByRecord (const ProductLink * newStructPtr, const ProductLink * oldStructPtr);
    int InsertOrUpdateProductLink(const ProductLink * structPtr);
    int InsertIfUniqueProductLink(const ProductLink * structPtr, bool *isUnique);
    bool ProductLinkExists(const ProductLink * structPtr);
    int DeleteProductLinkByRecord(const ProductLink * structPtr);
    void GetProductLinkPrimaryKeyWhereString (const ProductLink * structPtr, char returnWhereString[] );
    void FreeProductLink(ProductLink * structPtr);
    DbStatus * GetProductLinkDbStatus();
    void SetProductLinkErrorLogging(int value);
#endif

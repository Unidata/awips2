/*
    File: FcstPtService.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstPtService_h
#define FcstPtService_h


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



typedef struct _FcstPtService
{
    Node		node;
    char		lid[9];
    double		flood_thres;
    short		exceed_prob;
    char		service_type[21];
    date_t		anal_start_date;
    date_t		anal_end_date;
    date_t		impl_date;
    date_t		web_date;
    List		list;
} FcstPtService;
/*
    Function Prototypes
*/
    FcstPtService* GetFcstPtService(const char * where);
    FcstPtService* SelectFcstPtService(const char * where);
    int SelectFcstPtServiceCount(const char * where);
    int PutFcstPtService(const FcstPtService * structPtr);
    int InsertFcstPtService(const FcstPtService * structPtr);
    int UpdateFcstPtService(const FcstPtService* structPtr, const char *where);
    int DeleteFcstPtService(const char *where);
    int UpdateFcstPtServiceByRecord (const FcstPtService * newStructPtr, const FcstPtService * oldStructPtr);
    int InsertOrUpdateFcstPtService(const FcstPtService * structPtr);
    int InsertIfUniqueFcstPtService(const FcstPtService * structPtr, bool *isUnique);
    bool FcstPtServiceExists(const FcstPtService * structPtr);
    int DeleteFcstPtServiceByRecord(const FcstPtService * structPtr);
    void GetFcstPtServicePrimaryKeyWhereString (const FcstPtService * structPtr, char returnWhereString[] );
    void FreeFcstPtService(FcstPtService * structPtr);
    DbStatus * GetFcstPtServiceDbStatus();
    void SetFcstPtServiceErrorLogging(int value);
#endif

/*
    File: FcstPtWatSup.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstPtWatSup_h
#define FcstPtWatSup_h


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



typedef struct _FcstPtWatSup
{
    Node		node;
    char		lid[9];
    char		watsup_method[51];
    char		watsup_coord_agency[65];
    char		frequpd_normal[31];
    char		period_req[31];
    char		watsup_crit[31];
    char		watsup_resp_agency[65];
    char		customer_desc[81];
    date_t		impl_date;
    date_t		web_date;
    List		list;
} FcstPtWatSup;
/*
    Function Prototypes
*/
    FcstPtWatSup* GetFcstPtWatSup(const char * where);
    FcstPtWatSup* SelectFcstPtWatSup(const char * where);
    int SelectFcstPtWatSupCount(const char * where);
    int PutFcstPtWatSup(const FcstPtWatSup * structPtr);
    int InsertFcstPtWatSup(const FcstPtWatSup * structPtr);
    int UpdateFcstPtWatSup(const FcstPtWatSup* structPtr, const char *where);
    int DeleteFcstPtWatSup(const char *where);
    int UpdateFcstPtWatSupByRecord (const FcstPtWatSup * newStructPtr, const FcstPtWatSup * oldStructPtr);
    int InsertOrUpdateFcstPtWatSup(const FcstPtWatSup * structPtr);
    int InsertIfUniqueFcstPtWatSup(const FcstPtWatSup * structPtr, bool *isUnique);
    bool FcstPtWatSupExists(const FcstPtWatSup * structPtr);
    int DeleteFcstPtWatSupByRecord(const FcstPtWatSup * structPtr);
    void GetFcstPtWatSupPrimaryKeyWhereString (const FcstPtWatSup * structPtr, char returnWhereString[] );
    void FreeFcstPtWatSup(FcstPtWatSup * structPtr);
    DbStatus * GetFcstPtWatSupDbStatus();
    void SetFcstPtWatSupErrorLogging(int value);
#endif

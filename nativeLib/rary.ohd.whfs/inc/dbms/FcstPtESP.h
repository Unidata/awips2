/*
    File: FcstPtESP.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstPtESP_h
#define FcstPtESP_h


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



typedef struct _FcstPtESP
{
    Node		node;
    char		lid[9];
    char		snow_method[31];
    char		hydrol_method[31];
    char		reservoir_model[31];
    char		upstream_seg[9];
    char		hydraul_method[31];
    char		flowtype[41];
    char		fcsttype[21];
    char		frequpd_normal[31];
    char		frequpd_flood[31];
    char		frequpd_drought[31];
    char		fcst_horizon[31];
    short		nummonclim;
    short		numdayhyd;
    short		num_elev_zones;
    char		consumptive_use[2];
    char		channel_loss[2];
    char		post_processor[31];
    date_t		impl_date;
    date_t		external_date;
    date_t		web_date;
    List		list;
} FcstPtESP;
/*
    Function Prototypes
*/
    FcstPtESP* GetFcstPtESP(const char * where);
    FcstPtESP* SelectFcstPtESP(const char * where);
    int SelectFcstPtESPCount(const char * where);
    int PutFcstPtESP(const FcstPtESP * structPtr);
    int InsertFcstPtESP(const FcstPtESP * structPtr);
    int UpdateFcstPtESP(const FcstPtESP* structPtr, const char *where);
    int DeleteFcstPtESP(const char *where);
    int UpdateFcstPtESPByRecord (const FcstPtESP * newStructPtr, const FcstPtESP * oldStructPtr);
    int InsertOrUpdateFcstPtESP(const FcstPtESP * structPtr);
    int InsertIfUniqueFcstPtESP(const FcstPtESP * structPtr, bool *isUnique);
    bool FcstPtESPExists(const FcstPtESP * structPtr);
    int DeleteFcstPtESPByRecord(const FcstPtESP * structPtr);
    void GetFcstPtESPPrimaryKeyWhereString (const FcstPtESP * structPtr, char returnWhereString[] );
    void FreeFcstPtESP(FcstPtESP * structPtr);
    DbStatus * GetFcstPtESPDbStatus();
    void SetFcstPtESPErrorLogging(int value);
#endif

/*
    File: FcstPtDeterm.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FcstPtDeterm_h
#define FcstPtDeterm_h


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



typedef struct _FcstPtDeterm
{
    Node		node;
    char		lid[9];
    char		snow_method[31];
    char		hydrol_method[31];
    char		reservoir_model[31];
    char		upstream_seg[9];
    char		hydraul_method[31];
    char		def_issue_crit[51];
    short		hours_qpf;
    char		frequpd_normal[31];
    char		frequpd_flood[31];
    char		frequpd_drought[31];
    char		fcst_horizon[31];
    short		hours_qtf;
    short		hours_qzf;
    short		num_elev_zones;
    char		consumptive_use[2];
    char		channel_loss[2];
    char		fcst_gen_method[31];
    date_t		impl_date;
    date_t		web_date;
    List		list;
} FcstPtDeterm;
/*
    Function Prototypes
*/
    FcstPtDeterm* GetFcstPtDeterm(const char * where);
    FcstPtDeterm* SelectFcstPtDeterm(const char * where);
    int SelectFcstPtDetermCount(const char * where);
    int PutFcstPtDeterm(const FcstPtDeterm * structPtr);
    int InsertFcstPtDeterm(const FcstPtDeterm * structPtr);
    int UpdateFcstPtDeterm(const FcstPtDeterm* structPtr, const char *where);
    int DeleteFcstPtDeterm(const char *where);
    int UpdateFcstPtDetermByRecord (const FcstPtDeterm * newStructPtr, const FcstPtDeterm * oldStructPtr);
    int InsertOrUpdateFcstPtDeterm(const FcstPtDeterm * structPtr);
    int InsertIfUniqueFcstPtDeterm(const FcstPtDeterm * structPtr, bool *isUnique);
    bool FcstPtDetermExists(const FcstPtDeterm * structPtr);
    int DeleteFcstPtDetermByRecord(const FcstPtDeterm * structPtr);
    void GetFcstPtDetermPrimaryKeyWhereString (const FcstPtDeterm * structPtr, char returnWhereString[] );
    void FreeFcstPtDeterm(FcstPtDeterm * structPtr);
    DbStatus * GetFcstPtDetermDbStatus();
    void SetFcstPtDetermErrorLogging(int value);
#endif

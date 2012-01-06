/*
    File: SshpConfig.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SshpConfig_h
#define SshpConfig_h


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



typedef struct _SshpConfig
{
    Node		node;
    char		lid[9];
    char		basin_id[9];
    dtime_t		postingtime;
    char		model_pref[11];
    char		auto_process[2];
    char		source_pref[21];
    char		use_static_evap[2];
    char		use_blend[2];
    char		blend_method[11];
    long		blend_hours;
    List		list;
} SshpConfig;
/*
    Function Prototypes
*/
    SshpConfig* GetSshpConfig(const char * where);
    SshpConfig* SelectSshpConfig(const char * where);
    int SelectSshpConfigCount(const char * where);
    int PutSshpConfig(const SshpConfig * structPtr);
    int InsertSshpConfig(const SshpConfig * structPtr);
    int UpdateSshpConfig(const SshpConfig* structPtr, const char *where);
    int DeleteSshpConfig(const char *where);
    int UpdateSshpConfigByRecord (const SshpConfig * newStructPtr, const SshpConfig * oldStructPtr);
    int InsertOrUpdateSshpConfig(const SshpConfig * structPtr);
    int InsertIfUniqueSshpConfig(const SshpConfig * structPtr, bool *isUnique);
    bool SshpConfigExists(const SshpConfig * structPtr);
    int DeleteSshpConfigByRecord(const SshpConfig * structPtr);
    void GetSshpConfigPrimaryKeyWhereString (const SshpConfig * structPtr, char returnWhereString[] );
    void FreeSshpConfig(SshpConfig * structPtr);
    DbStatus * GetSshpConfigDbStatus();
    void SetSshpConfigErrorLogging(int value);
#endif

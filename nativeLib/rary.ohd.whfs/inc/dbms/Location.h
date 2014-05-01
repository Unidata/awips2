/*
    File: Location.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Location_h
#define Location_h


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



typedef struct _Location
{
    Node		node;
    char		lid[9];
    char		county[21];
    char		coe[4];
    char		cpm[4];
    char		detail[11];
    double		elev;
    char		hdatum[10];
    char		hsa[4];
    char		hu[9];
    double		lat;
    double		lon;
    char		lremark[256];
    date_t		lrevise;
    char		name[51];
    char		network[4];
    char		rb[31];
    char		rfc[6];
    date_t		sbd;
    char		sn[11];
    char		state[3];
    char		waro[4];
    char		wfo[4];
    char		wsfo[4];
    char		type[5];
    char		des[31];
    char		det[31];
    long		post;
    char		stntype[5];
    char		tzone[9];
    List		list;
} Location;
/*
    Function Prototypes
*/
    Location* GetLocation(const char * where);
    Location* SelectLocation(const char * where);
    int SelectLocationCount(const char * where);
    int PutLocation(const Location * structPtr);
    int InsertLocation(const Location * structPtr);
    int UpdateLocation(const Location* structPtr, const char *where);
    int DeleteLocation(const char *where);
    int UpdateLocationByRecord (const Location * newStructPtr, const Location * oldStructPtr);
    int InsertOrUpdateLocation(const Location * structPtr);
    int InsertIfUniqueLocation(const Location * structPtr, bool *isUnique);
    bool LocationExists(const Location * structPtr);
    int DeleteLocationByRecord(const Location * structPtr);
    void GetLocationPrimaryKeyWhereString (const Location * structPtr, char returnWhereString[] );
    void FreeLocation(Location * structPtr);
    DbStatus * GetLocationDbStatus();
    void SetLocationErrorLogging(int value);
#endif

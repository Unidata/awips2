/*
    File: Benchmark.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Benchmark_h
#define Benchmark_h


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



typedef struct _Benchmark
{
    Node		node;
    char		lid[9];
    char		bnum[7];
    double		elev;
    char		remark[256];
    List		list;
} Benchmark;
/*
    Function Prototypes
*/
    Benchmark* GetBenchmark(const char * where);
    Benchmark* SelectBenchmark(const char * where);
    int SelectBenchmarkCount(const char * where);
    int PutBenchmark(const Benchmark * structPtr);
    int InsertBenchmark(const Benchmark * structPtr);
    int UpdateBenchmark(const Benchmark* structPtr, const char *where);
    int DeleteBenchmark(const char *where);
    int UpdateBenchmarkByRecord (const Benchmark * newStructPtr, const Benchmark * oldStructPtr);
    int InsertOrUpdateBenchmark(const Benchmark * structPtr);
    int InsertIfUniqueBenchmark(const Benchmark * structPtr, bool *isUnique);
    bool BenchmarkExists(const Benchmark * structPtr);
    int DeleteBenchmarkByRecord(const Benchmark * structPtr);
    void GetBenchmarkPrimaryKeyWhereString (const Benchmark * structPtr, char returnWhereString[] );
    void FreeBenchmark(Benchmark * structPtr);
    DbStatus * GetBenchmarkDbStatus();
    void SetBenchmarkErrorLogging(int value);
#endif

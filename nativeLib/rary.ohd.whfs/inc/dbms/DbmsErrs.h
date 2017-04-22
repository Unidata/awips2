/*
	File:		DbmsErrs.h

	Date:		2/21/94

	Purpose:	This file contains the definition of
			the error codes related to the "C"
			encapsulation of ESQL.
*/

#ifndef DbmsErrs_h
#define DbmsErrs_h


typedef enum {
	errOk = 0,
	errGeneral,
	errBadArgs,
	errPrepareQid,
	errDescribeQid,
	errOpenCursor,
	errInsertFail
} dbErrResult;


#endif


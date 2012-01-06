/*
 *   Copyright 2002, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: error.h,v 1.2.2.3.6.5 2006/10/19 19:27:04 steve Exp $ */

#ifndef ERROR_H
#define ERROR_H

typedef struct error ErrorObj;  /* defined in error.c */


enum err_level {
    ERR_FAILURE,
    ERR_ERROR = ERR_FAILURE,
    ERR_WARNING,
    ERR_NOTICE,
    ERR_INFO,
    ERR_DEBUG
};


#define ERR_NEW(code, cause, msg) err_new(code, cause, __FILE__, __LINE__, msg)
#define ERR_NEW0	ERR_NEW
#define ERR_NEW1(code, cause, fmt, a1) \
    err_new(code, cause, __FILE__, __LINE__, fmt, a1)
#define ERR_NEW2(code, cause, fmt, a1, a2) \
    err_new(code, cause, __FILE__, __LINE__, fmt, a1, a2)
#define ERR_NEW3(code, cause, fmt, a1, a2, a3) \
    err_new(code, cause, __FILE__, __LINE__, fmt, a1, a2, a3)
#define ERR_NEW4(code, cause, fmt, a1, a2, a3, a4) \
    err_new(code, cause, __FILE__, __LINE__, fmt, a1, a2, a3, a4)
#define ERR_NEW5(code, cause, fmt, a1, a2, a3, a4, a5) \
    err_new(code, cause, __FILE__, __LINE__, fmt, a1, a2, a3, a4, a5)


ErrorObj*
err_new(
    const int		code,
    ErrorObj*		cause,
    const char*		file,
    const unsigned	line, 
    const char*		fmt,
    ...);

int
err_code(
    const ErrorObj *err);

ErrorObj*
err_cause(
    const ErrorObj *err);

const char*
err_message(
    const ErrorObj *err);

void
err_log(
    const ErrorObj *err,
    enum err_level level);

void
err_free(
    ErrorObj *err);

void
err_log_and_free(
    ErrorObj       *err,
    enum err_level level);

#endif

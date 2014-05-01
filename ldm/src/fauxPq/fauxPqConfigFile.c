/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See top level COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: fauxPqConfigFile.c,v 1.7.12.1 2008/04/15 16:34:07 steve Exp $ */


/*
 * A module for controlling the behavior of the "faux" product queue from a
 * configuration file.
 */


#include "ldmconfig.h"

#include <errno.h>
#include <limits.h>
#include <parser.h>     /* XML parser API */
#include <rpc/xdr.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>       /* time(), difftime() */
#include <timestamp.h>
#include <unistd.h>     /* sleep() */

#include "atofeedt.h"
#include "inetutil.h"   /* ghostname() */
#include "ldm.h"
#include "ldm_xlen.h"   /* xlen_prod_i() */
#include "ldmprint.h"   /* s_signaturet() */
#include "md5.h"
#include "ulog.h"

#include "fauxPqConfigFile.h"


typedef enum {
    CLOSED,
    START,
    IN_ROOT_ELT,
    UNKNOWN,
    READY,
    ERROR
} State;


static State          currState = CLOSED;
static State          prevState;
static int            unknownLevel;
static xmlSAXHandler  saxer;
static const xmlChar* rootEltName = (xmlChar*)"FauxProductQueue";
static const xmlChar* feedEltName = (xmlChar*)"feed";
static const xmlChar* typeAttName = (xmlChar*)"type";
static const xmlChar* idAttName = (xmlChar*)"id";
static const xmlChar* sizeAttName = (xmlChar*)"size";
static const xmlChar* delayAttName = (xmlChar*)"delay";
static feedtypet      feedType = EXP;
static char*          prodId = "faux product";
static unsigned int   prodSize = 200000;        /* bytes */
static void*          prodData = NULL;
static prod_info      prodInfo;
static size_t         prodXdrLen;
static void*          prodXdrBuf;
static product        prod;
static unsigned int   delay = 1;                /* seconds */
static MD5_CTX*       md5;
static unsigned int   prodCount = 0;
static time_t         lastReturned = 0;


/*
 * Handles the start of the XML document.
 *
 * user_data                 User-supplied data. (in/out)
 */
/*ARGSUSED0*/
static void myStartDocument(void *user_data)
{
    currState = START;
}


/*
 * Handles the start of an XML element.
 *
 * user_data                 User-supplied data. (in/out)
 * name                      The name of the element. (in)
 * attrs                     XML attributes associated with the element. (in)
 */
/*ARGSUSED0*/
static void myStartElement(
    void*           user_data,
    const xmlChar*  name,
    const xmlChar** attrs)
{
    switch (currState) {

    case START:
        if (xmlStrcmp(name, rootEltName) == 0) {
            currState = IN_ROOT_ELT;
        }
        else {
            uerror("%s:%d: Root element \"%s\" is not \"%s\"", __FILE__,
                __LINE__, name, rootEltName);
            currState = ERROR;
        }
        break;

    case IN_ROOT_ELT:
        if (xmlStrcmp(name, feedEltName) != 0) {
            prevState = currState;
            currState = UNKNOWN;
            unknownLevel = 1;
        }
        else {
            const xmlChar** nameValuePair = attrs;
            for (; *nameValuePair != NULL; nameValuePair += 2) {
                const xmlChar* attName = nameValuePair[0];

                if (xmlStrcmp(attName, typeAttName) == 0) {
                    if (strfeedtypet((const char*)nameValuePair[1], &feedType)
                        != FEEDTYPE_OK) {

                        uerror("%s:%d: Invalid feed type: \"%s\"",
                            __FILE__, __LINE__, nameValuePair[1]);
                        currState = ERROR;
                    }
                }
                else if (xmlStrcmp(attName, idAttName) == 0) {
                    prodId = strdup((char*)nameValuePair[1]);

                    if (prodId == NULL) {
                        uerror(
                            "%s:%d: Couldn't duplicate product ID: \"%s\"",
                            __FILE__, __LINE__, nameValuePair[1]);
                        currState = ERROR;
                    }
                }
                else if (xmlStrcmp(attName, sizeAttName) == 0) {
                    if (sscanf((const char*)nameValuePair[1], "%u", &prodSize)
                        != 1) {

                        uerror("%s:%d: Invalid product size: \"%s\"",
                            __FILE__, __LINE__, nameValuePair[1]);
                        currState = ERROR;
                    }
                }
                else if (xmlStrcmp(attName, delayAttName) == 0) {
                    if (sscanf((const char*)nameValuePair[1], "%u", &delay)
                        != 1) {

                        uerror("%s:%d: Invalid delay time: \"%s\"",
                            __FILE__, __LINE__, nameValuePair[1]);
                        currState = ERROR;
                    }
                }
                else {
                    uerror("%s:%d: Invalid attribute: \"%s\"",
                        __FILE__, __LINE__, attName);
                    currState = ERROR;
                }
            }
        }
        break;

    case UNKNOWN:
        ++unknownLevel;
        break;
    }
}


/*
 * Returns the character data associated with an XM entity.
 *
 * user_data                 User-supplied data. (in/out)
 * name                      The name of the XML entity. (in)
 */
/*ARGSUSED0*/
static xmlEntityPtr myGetEntity(
    void*          user_data,
    const xmlChar* name)
{
    return xmlGetPredefinedEntity(name);
}


/*
 * Handles character data in the body of an XM element.
 *
 * user_data                 User-supplied data. (in/out)
 * ch                        The character data. (in)
 * len                       The amount of data in characters. (in)
 */
/*ARGSUSED0*/
static void myCharacters(
    void*          user_data,
    const xmlChar* ch,
    int            len)
{}


/*
 * Handles the end of an XML element.
 *
 * user_data                 User-supplied data. (in/out)
 * name                      The name of the XML element. (in)
 */
/*ARGSUSED0*/
static void myEndElement(
    void*          user_data,
    const xmlChar* name)
{
    if (currState == UNKNOWN) {
        if (--unknownLevel == 0)
            currState = prevState;
    }
}


/*
 * Handles the end of the XML document.
 *
 * user_data                 User-supplied data. (in/out)
 */
/*ARGSUSED0*/
static void myEndDocument(void *user_data)
{
    if (currState != ERROR)
        currState = READY;
}


/*
 * Handles an XML warning message.
 *
 * user_data                 User-supplied data. (in/out)
 * msg                       The warning message as a printf(3) format. (in)
 * ...                       Arguments referenced by the message.
 */
/*ARGSUSED0*/
static void xmlWarning(
    void*       user_data,
    const char* msg,
    ...)
{
    va_list     args;

    va_start(args, msg);
    vulog(LOG_WARNING, msg, args);
    va_end(args);
}


/*
 * Handles an XML error or fatal error message.
 *
 * user_data                 User-supplied data. (in/out)
 * msg                       The warning message as a printf(3) format. (in)
 * ...                       Arguments referenced by the message.
 */
/*ARGSUSED0*/
static void xmlError(
    void*       user_data,
    const char* msg,
    ...)
{
    va_list     args;

    va_start(args, msg);
    vulog(LOG_ERR, msg, args);
    va_end(args);

    currState = ERROR;
}


/*
 * Returns the next product signature (MD5 checksum).
 *
 * signature               The buffer in which to put the MD5 checksum.
 */
static void nextSignature(signaturet* signature)
{
    MD5Init(md5);
    MD5Update(md5, (const unsigned char*)&prodCount, sizeof(prodCount));
    MD5Final((unsigned char*)*signature, md5);
}


/**
 * Ensures that products aren't returned more often than the configured
 * delay.
 */
static void ensure_delay()
{
    time_t now = time(NULL);

    if (lastReturned != 0) {
        double diff = difftime(now, lastReturned);

        if (diff < delay) {
            (void)sleep((int)(delay - diff + 0.5));
        }
    }

    lastReturned = now;
}


/******************************************************************************
 * Public API:
 ******************************************************************************/


/* 
 * Opens the configuration file.  Any previously opened configuration file will
 * be closed.  The ulog(3) facility will be used to log any problems.
 *
 * pathname          The pathname of the file. (in)
 *
 * Returns:
 *   pq.h:ENOERR     if success.
 *   errno.h:ENOMEM  if out of memory.
 *   errno.h:EINVAL  if the pathname is NULL or if the configuration file was 
 *                   invalid.
 *   (else)          <errno.h> error code.
 */
int cfOpen(const char* pathname)
{
    int              status;

    if (pathname == NULL) {
        uerror("%s:%d: NULL pathname", __FILE__, __LINE__);
        status = EINVAL;
    }
    else {
        if (currState != CLOSED)
            cfClose();

        md5 = new_MD5_CTX();

        if (saxer.startDocument != myStartDocument) {
            saxer.startDocument = myStartDocument;
            saxer.startElement = myStartElement;
            saxer.getEntity = myGetEntity;
            saxer.characters = myCharacters;
            saxer.endElement = myEndElement;
            saxer.endDocument = myEndDocument;
            saxer.warning = xmlWarning;
            saxer.error = xmlError;
            saxer.fatalError = xmlError;
        }

        (void)xmlSAXUserParseFile(&saxer, NULL, pathname);

        if (currState != READY) {
            cfClose();
            status = EINVAL;
        }
        else {
            prodData = realloc(prodData, prodSize);

            if (prodData == NULL) {
                uerror("%s:%d: Couldn't allocate %u bytes for product data",
                    __FILE__, __LINE__, prodSize);
                status = ENOMEM;
            }
            else {
                (void)memset(prodData, 'x', prodSize);

                prodInfo.origin = (char*)ghostname();
                prodInfo.feedtype = feedType;
                prodInfo.ident = prodId;
                prodInfo.sz = prodSize;
                prodXdrLen = xlen_prod_i(&prodInfo);
                prodXdrBuf = realloc(prodXdrBuf, prodXdrLen);

                if (prodXdrBuf == NULL) {
                    uerror("%s:%d: Couldn't allocate %lu bytes for product "
                        "XDR buffer", __FILE__, __LINE__, prodXdrLen);
                    status = errno;
                }
                else {
                    prod.data = prodData;
                    status = ENOERR;
                }

                if (status != ENOERR) {
                    free(prodData);
                    prodData = NULL;
                }
            }
        }
    }

    return status;
}


/*
 * Returns a matching product if appropriate.  Successfully obtained products
 * should be freed using cfFreeProduct(...).
 *
 * mt               The time-matching criteria for finding a product. (in)
 * clss             The class of products to find. (in)
 * info             The product metadata. (out)
 * data             The product data (no metadata). (out)
 * encProd          The entire, XDR-encoded product (including metadata). (out)
 * len              The size of "encProd" in bytes. (out)
 *
 * Returns:
 *   pq.h:ENOERR      if matching product found.
 *   pq.h:PQUEUE_END  if no matching product.
 *   errno.h:EINVAL   if an argument is invalid.
 *   errno.h:ENOENT   if cfOpen(...) hasn't been called or cfClose() has been
 *                    called more recently.
 *   (else)           <errno.h> error code.
 */
/*ARGSUSED0*/
int cfGetProduct(
    pq_match          mt,
    const prod_class_t* clss,
    prod_info*        info,
    void**            data,
    void**            encProd,
    size_t*           len)
{
    int               status;

    if (currState != READY) {
        uerror("%s:%d: Configuration file not ready", __FILE__, __LINE__);

        status = ENOENT;
    }
    else {
        ensure_delay();

        *info = prodInfo;

        status = set_timestamp(&info->arrival);

        if (status  != 0) {
            uerror("%s:%d: Couldn't set product arrival time: %s", __FILE__,
                __LINE__, strerror(errno));
        }
        else {
            XDR      xdr;

            info->seqno = ++prodCount;
            nextSignature(&info->signature);
            prod.info = *info;

            *data = prodData;
            *len = prodXdrLen;
            *encProd = prodXdrBuf;

            xdrmem_create(&xdr, *encProd, *len, XDR_ENCODE);

            if(xdr_product(&xdr, &prod))
            {
                status = ENOERR;
            }
            else {
                uerror("%s:%d: Couldn't XDR_ENCODE product",
                    __FILE__, __LINE__);
                status = EIO;
            }

            xdr_destroy(&xdr);
        }
    }

    return status;
}


/*
 * Frees resources associated with a product obtained via cfGetProduct(...).
 *
 * info             The product metadata. (in/out)
 * data             The product data (no metadata). (in/out)
 * encProd          The entire, XDR-encoded product (including metadata). 
 *                  (in/out)
 */
/*ARGSUSED0*/
void cfFreeProduct(
    prod_info*        info,
    void**            data,
    void**            encProd)
{}


/*
 * Closes the configuration file and releases any resources.
 */
void cfClose()
{
    if (currState != CLOSED) {
        free_MD5_CTX(md5);

        if (prodXdrBuf != NULL) {
            free(prodXdrBuf);
            prodXdrBuf = NULL;
        }

        if (prodData != NULL) {
            free(prodData);
            prodData = NULL;
        }

        currState = CLOSED;
    }
}

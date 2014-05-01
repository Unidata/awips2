/* 
 * Grammar for LDM configuration-file.
 */

%{
/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: parser.y,v 1.1.2.2 2008/05/15 21:26:41 steve Exp $ */

#include "ldmconfig.h"

#include "acl.h"
#include "atofeedt.h"
#include "error.h"
#include "globals.h"
#include "ldm.h"
#include "ldmprint.h"
#include "RegularExpressions.h"
#include "ulog.h"
#include "wordexp.h"

#include <limits.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>

#if YYDEBUG
extern int yydebug;
#endif

static int	line = 0;
static unsigned	ldmPort = LDM_PORT;
static int      execute = 1;

static const char*	scannerGetPath(void);
static int		scannerPush(const char*);
static int		scannerPop(void);

static void
yyerror(const char *msg)
{
    err_log_and_free(
	ERR_NEW3(0, NULL,
	    "Error near line %d, file \"%s\": %s",
	    line, scannerGetPath(), msg),
	ERR_FAILURE);
}

#if __STDC__
extern int yyparse(void);
#endif


static int
decodeFeedtype(
    feedtypet*	ftp,
    const char*	string)
{
    feedtypet	ft;
    int		error;
    int		status = strfeedtypet(string, &ft);

    if (status == FEEDTYPE_OK) {
#if YYDEBUG
	if(yydebug)
	    udebug("feedtype: %#x", ft);
#endif
	*ftp = ft;
	error = 0;
    }
    else {
	err_log_and_free(
	    ERR_NEW4(0, NULL, 
		"Invalid feedtype expression \"%s\" near line %d, file \"%s\": "
		    "%s",
		string, line, scannerGetPath(), strfeederr(status)),
	    ERR_FAILURE);

	error = 1;
    }

    return error;
}


static int
decodeRegEx(
    regex_t** const	regexpp,
    const char* 	string)
{
    int		error = 1;		/* failure */
    char* const	clone = strdup(string);

    if (NULL == clone) {
	err_log_and_free(
	    ERR_NEW4(0, NULL, 
		"Couldn't clone regular-expression \"%s\" near line %d, "
		    "file \"%s\": %s",
		string, line, scannerGetPath(), strerror(errno)),
	    ERR_FAILURE);
    }
    else {
	regex_t*	regexp = (regex_t*)malloc(sizeof(regex_t));

	if (NULL == regexp) {
	    err_log_and_free(
		ERR_NEW3(0, NULL, 
		    "Couldn't allocate %lu bytes for \"regex_t\""
			"near line %d, file \"%s\"",
		    (unsigned long)sizeof(regex_t), line, scannerGetPath()),
		ERR_FAILURE);
	}
	else {
	    if (re_vetSpec(clone)) {
		/*
		 * Pathological regular expression.
		 */
		err_log_and_free(
		    ERR_NEW3(0, NULL, 
			"Adjusted pathological regular expression \"%s\" "
			    "near line %d, file \"%s\"",
			string, line, scannerGetPath()),
		    ERR_WARNING);
	    }

	    error = regcomp(regexp, clone, REG_EXTENDED|REG_ICASE|REG_NOSUB);

	    if (!error) {
		*regexpp = regexp;
	    }
	    else {
		char	buf[132];

		(void)regerror(error, regexp, buf, sizeof(buf));
		err_log_and_free(
		    ERR_NEW4(0, NULL,
			"Couldn't compile regular-expression \"%s\" near "
			    " line %d, file \"%s\": %s",
			clone, line, scannerGetPath(), buf),
		    ERR_FAILURE);
	    }

	    if (error)
		free(regexp);
	}				/* "regexp" set */

	free(clone);
    }					/* "clone" set */

    return error;
}


static int
decodeHostSet(
    host_set** const	hspp,
    const char*		string)
{
    regex_t*	regexp;
    int		error = decodeRegEx(&regexp, string);

    if (!error) {
	char* dup = strdup(string);

	if (NULL == dup) {
	    err_log_and_free(
		ERR_NEW4(0, NULL, 
		    "Couldn't clone string \"%s\" near line %d, "
			"file \"%s\": %s",
		    string, line, scannerGetPath(), strerror(errno)),
		ERR_FAILURE);
	}
	else {
	    host_set*	hsp = new_host_set(HS_REGEXP, dup, regexp);

	    if (NULL == hsp) {
		err_log_and_free(
		    ERR_NEW4(0, NULL, 
			"Couldn't create host-set for "
			    "\"%s\" near line %d, file \"%s\": %s",
			dup, line, scannerGetPath(), strerror(errno)),
		    ERR_FAILURE);

		error = 1;
	    }
	    else {
#if YYDEBUG
		if(yydebug)
		    udebug("hostset: \"%s\"", dup);
#endif
		*hspp = hsp;
		error = 0;
	    }

	    if (error)
		free(dup);
	}				/* "dup" set */

	if (error)
	    regfree(regexp);

	free(regexp);
    }				/* "regexp" set */

    return error;
}


static int
decodeSelection(
    feedtypet* const	ftp,
    regex_t** const	regexpp,
    const char* const	ftString,
    const char*	const	regexString)
{
    feedtypet	ft;
    int		error;

    error = decodeFeedtype(&ft, ftString);

    if (!error) {
	error = decodeRegEx(regexpp, regexString);

	if (!error) {
#if YYDEBUG
	    if(yydebug)
		udebug("prodIdPat: \"%s\"", regexString);
#endif
	    *ftp = ft;
	}
    }				/* feedtype decoded */

    return error;
}


static void
warnIfPathological(
    const char*  const	re)
{
    if (re_isPathological(re)) {
	/*
	 * Pathological regular expression.
	 */
	err_log_and_free(
	    ERR_NEW3(0, NULL, 
		"Pathological regular expression \"%s\" "
		    "near line %d, file \"%s\"",
		re, line, scannerGetPath()),
	    ERR_WARNING);
    }
}


/*
 * Arguments:
 *	feedtypeSpec	String specification of feedtype.  May not be NULL.
 *			Caller may free upon return.
 *	hostPattern	ERE of allowed hosts.  May not be NULL.  Caller may
 *			free upon return.
 *	okPattern	ERE that product-identifiers must match in order for
 *			the associated data-products to be transferred.  Caller
 *			may free upon return.
 *	notPattern	ERE that product-identifiers must NOT match in order for
 *			the associated data-products to be transferred.  May
 *			be null to indicate that such matching should be
 *			disabled.  Caller may free upon return.
 * Returns:
 *	0		Success.
 *	else		Failure.
 */
static int
decodeAllowEntry(
    const char* const	feedtypeSpec,
    const char* const	hostPattern,
    const char* const	okPattern,
    const char* const	notPattern)
{
    feedtypet	ft;
    int		errCode = decodeFeedtype(&ft, feedtypeSpec);

    if (!errCode) {
	host_set*	hsp;

	errCode = decodeHostSet(&hsp, hostPattern);

	if (!errCode) {
	    ErrorObj*	errObj;

	    warnIfPathological(okPattern);

	    if (notPattern)
		warnIfPathological(notPattern);

	    errObj = acl_addAllow(ft, hsp, okPattern, notPattern);

	    if (errObj) {
		err_log_and_free(
		    ERR_NEW2(0, errObj, 
			"Couldn't add ALLOW entry near line %d, file \"%s\"",
			line, scannerGetPath()),
		    ERR_FAILURE);

		errCode = -1;
	    }

	    if (errCode)
		free_host_set(hsp);
	}				/* "hsp" allocated */
    }					/* "ft" set */

    return errCode;
}


static int
decodeRequestEntry(
    const char* const	feedtypeSpec,
    const char* const	prodPattern,
    char* const		hostSpec)
{
    feedtypet	ft;
    regex_t*	regexp;
    int		errCode =
	decodeSelection(&ft, &regexp, feedtypeSpec, prodPattern);

    if (!errCode) {
	RequestEntry*	entry;

	if (errCode = requestEntry_get(&entry, ft, prodPattern, regexp)) {
	    err_log_and_free(
		ERR_NEW3(0, NULL, 
		    "Couldn't get request-entry near line %d, file \"%s\": %s",
		    line, scannerGetPath(), strerror(errCode)),
		ERR_FAILURE);
	}
	else {
	    const char*	hostId = strtok(hostSpec, ":");

	    if (NULL == hostId) {
		err_log_and_free(
		    ERR_NEW3(0, NULL, 
			"Invalid hostname specification \"%s\" near "
			    "line %d, file \"%s\"",
			hostSpec, line, scannerGetPath()),
		    ERR_FAILURE);

		errCode = EINVAL;
	    }
	    else {
		unsigned	localPort;
		const char*	portSpec = strtok(NULL, ":");

		if (NULL == portSpec) {
		    localPort = ldmPort;
		}
		else {
		    char*	suffix = "";
		    long	port;

		    errno = 0;
		    port = strtol(portSpec, &suffix, 0);

		    if (0 == errno && 0 == *suffix &&
			0 < port && 0xffff >= port) {

			localPort = (unsigned)port;
		    }
		    else {
			err_log_and_free(
			    ERR_NEW3(0, NULL, 
				"Invalid port specification \"%s\" near "
				    "line %d, file \"%s\"",
				portSpec, line, scannerGetPath()),
			    ERR_FAILURE);

			errCode = EINVAL;
		    }
		}			/* have port specification */

		if (0 == errCode) {
		    if ((errCode =
			    requestEntry_addHost(entry, hostId, localPort))) {

			err_log_and_free(
			    ERR_NEW3(0, NULL, 
				"Couldn't add host to request-entry near "
				    "line %d, file \"%s\": %s",
				line, scannerGetPath(), strerror(errCode)),
			    ERR_FAILURE);
		    }
		}			/* "localPort" set */
	    }				/* valid hostname */
	}				/* got request "entry" */

	if (errCode)
	    regfree(regexp);

	free(regexp);
    }					/* "ft" & "regexp" set */

    return errCode;
}


#if YYDEBUG
#define printf udebug
#endif

#ifdef __hpux
/* otherwise, they define it to 'int' in spite of the union typedef */
#define YYSTYPE YYSTYPE
#endif

%}

%union  {
		char	string[2000];
        }


%token ALLOW_K
%token ACCEPT_K
%token REQUEST_K
%token EXEC_K
%token INCLUDE_K

%token <string>	STRING

%start table

%%
table:		/* empty */
		| table entry
		;

entry:		  allow_entry
		| accept_entry
		| request_entry
		| exec_entry
		| include_stmt
		;

allow_entry:	ALLOW_K STRING STRING
		{
		    int errCode = decodeAllowEntry($2, $3, ".*", NULL);

		    if (errCode)
			return errCode;
		}
		| ALLOW_K STRING STRING STRING
		{
		    int errCode = decodeAllowEntry($2, $3, $4, NULL);

		    if (errCode)
			return errCode;
		}
		| ALLOW_K STRING STRING STRING STRING
		{
		    int errCode = decodeAllowEntry($2, $3, $4, $5);

		    if (errCode)
			return errCode;
		}
		;

accept_entry:	ACCEPT_K STRING STRING STRING
		{
		    feedtypet	ft;
		    regex_t*	regexp;
		    int		error = decodeSelection(&ft, &regexp, $2, $3);

		    if (!error) {
			host_set*	hsp;

			error = decodeHostSet(&hsp, $4);

			if (!error) {
			    char*	patp = strdup($3);

			    if (NULL == patp) {
				err_log_and_free(
				    ERR_NEW3(0, NULL, 
					"Couldn't clone string \"%s\" "
					    "near line %d, file \"%s\": %s",
					$3, scannerGetPath(), strerror(errno)),
				    ERR_FAILURE);

				error = 1;
			    }
			    else {
				error =
				    accept_acl_add(ft, patp, regexp, hsp, 1);

				if (error) {
				    err_log_and_free(
					ERR_NEW3(0, NULL, 
					    "Couldn't add ACCEPT entry "
						"near line %d, file \"%s\": %s",
					    line, scannerGetPath(),
					    strerror(error)),
					ERR_FAILURE);
				}

				if (error)
				    free(patp);
			    }		/* "patp" set */

			    if (error)
				free_host_set(hsp);
			}		/* "*hsp" set */

			if (error)
			    regfree(regexp);

			free(regexp);
		    }			/* "ft" & "regexp" set */

		    if (error)
			return error;
		}
		;

request_entry:	REQUEST_K STRING STRING STRING
		{
		    int	errCode = decodeRequestEntry($2, $3, $4);

		    if (errCode)
			return errCode;
		}
		| REQUEST_K STRING STRING STRING STRING
		{
		    int	errCode = decodeRequestEntry($2, $3, $4);

		    if (errCode)
			return errCode;
		}
		;

exec_entry:	EXEC_K STRING
		{
		    wordexp_t	words;
		    int		error;

		    (void)memset(&words, 0, sizeof(words));

		    error = wordexp($2, &words, 0);

		    if (error) {
			err_log_and_free(
			    ERR_NEW4(0, NULL,
				"Couldn't decode command \"%s\" near "
				    " line %d, file \"%s\": %s",
				$2, line, scannerGetPath(), strerror(errno)),
			    ERR_FAILURE);
		    }
		    else {
#if YYDEBUG
			if(yydebug)
			    udebug("command: \"%s\"", $2);
#endif
			if (execute) {
			    error = exec_add(&words);

			    if (error) {
				err_log_and_free(
				    ERR_NEW3(0, NULL,
					"Couldn't add EXEC entry near "
					    " line %d, file \"%s\": %s",
					line, scannerGetPath(),
					strerror(errno)),
				    ERR_FAILURE);
				wordfree(&words);
			    }
			}
		    }			/* "words" set */

		    if (error)
			return error;
		}
		;

include_stmt:	INCLUDE_K STRING
		{
		    if (scannerPush($2))
			return -1;
		}

%%

#include "scanner.c"

/*
 * Returns:
 *	 0	More input
 *	!0	No more input
 */
int
yywrap(void)
{
    return scannerPop();
}

/*
 * Arguments:
 *	conf_path	Pathname of configuration-file.
 *	doSomething	Whether or not to actually do something or just
 *			parse the configuration-file.
 *	defaultPort	The default LDM port.
 * Returns:
 *	0		Success.
 *	!0		Failure.
 */
int
read_conf(
    const char* const	conf_path,
    int			doSomething,
    unsigned		defaultPort)
{
    int			status = -1;	/* failure */

    if (scannerPush(conf_path)) {
	log_add("Couldn't open LDM configuration-file");
    }
    else {
	/* yydebug = 1; */
	ldmPort = defaultPort;
	execute = doSomething;
	status = yyparse();

	if (status != 0) {
	    udebug("yyparse returns %d", status);
	}
	else {
	    if (doSomething) {
		status = invert_request_acl(defaultPort);

		if (status != 0) {
		    log_errno();
		    log_add("Problem requesting data");
		}
	    }
	}
    }

    return status;
}

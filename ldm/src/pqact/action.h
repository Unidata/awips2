/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: action.h,v 1.67.16.2 2004/08/09 14:46:28 steve Exp $ */
#ifndef _ACTION_H_
#define _ACTION_H_

#include "ldm.h"
#include "filel.h"

#ifdef __cplusplus
struct actiont {
	char *name;
#define LDM_ACT_TRANSIENT 1
	int flags;
	/* executed in "processProduct" */
	int (*prod_action)(product *prod, int argc, char **argv);
};
typedef struct actiont actiont;
extern "C" int close_all(void);
extern "C" int atoaction(const char *str, actiont *result);
extern "C" char * s_actiont(actiont *act);
#elif defined(__STDC__)
struct actiont {
	char *name;
#define LDM_ACT_TRANSIENT 1
	int flags;
	/* executed in "processProduct" */
	int (*prod_action)(const product *prod, int argc, char **argv,
		const void *xprod, size_t xlen);

};
typedef struct actiont actiont;
extern int close_all(void);
extern int atoaction(const char *str, actiont *result);
extern char * s_actiont(actiont *act);
#else /* Old Style C */
struct actiont {
	char *name;
#define LDM_ACT_TRANSIENT 1
	int flags;
	/* executed in "processProduct" */
	int (*prod_action)();
};
typedef struct actiont actiont;
extern int close_all();
extern int atoaction();
extern char * s_actiont();
#endif

#endif /* !_ACTION_H_ */

/*
 *	Copyright 1996, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: libvers.c,v 1.1 2004/09/15 20:04:52 dsa Exp $ */

#include "netcdf_inc/netcdf.h"

/*
 * A version string.
 */
#define SKIP_LEADING_GARBAGE 33	/* # of chars prior to the actual version */
#define XSTRING(x)	#x
#define STRING(x)	XSTRING(x)
static const char nc_libvers[] =
	"\044Id: \100(#) netcdf library version " STRING(VERSION) " of "__DATE__" "__TIME__" $";

const char *
nc_inq_libvers(void)
{
	return &nc_libvers[SKIP_LEADING_GARBAGE];
}

#if MAKE_PROGRAM /* TEST JIG */
#include <stdio.h>

main()
{
	(void) printf("Version: %s\n", nc_inq_libvers());
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/netcdf_main/RCS/libvers.c,v $";
 static char rcs_id2[] = "$Id: libvers.c,v 1.1 2004/09/15 20:04:52 dsa Exp $";}
/*  ===================================================  */

}
#endif

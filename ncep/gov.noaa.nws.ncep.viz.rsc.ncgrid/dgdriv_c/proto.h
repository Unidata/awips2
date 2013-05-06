/************************************************************************
 * proto.h                                                              *
 *                                                                      *
 * This include file contains function prototypes for all the c code in *
 * the common nawips libraries.                                         *
 **                                                                     *
 * E. Safford/GSC       10/00   Created                                 *
 * A. Hardy/NCEP	 7/03   Added proto_textlib			*
 * S. Danz/AWC    	 3/06   Added proto_cmd.h and proto_cap.h	*
 * R. Tian/SAIC		 8/06	Added proto_dg.h,proto_cmm.h,proto_grc.h*
 * R. Tian/SAIC		10/06	Added proto_na.h			*
 * E. Safford/SAIC	12/07	rm proto_nmaplib and proto_xw, programs	*
 *				 that need these should include them    *
 *				 directly.				*
 ***********************************************************************/

#ifndef	PROTO
#define PROTO

#include "proto_cgemlib.h"
#include "proto_cmm.h"
#include "proto_dg.h"
#include "fortran_wrappers.h"
#include "proto_gemlib.h"
#include "proto_grc.h"
#include "proto_na.h"
#include "proto_textlib.h"

#endif 		/* PROTO */

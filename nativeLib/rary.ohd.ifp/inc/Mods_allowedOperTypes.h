
/* ***************************************************************************************

	Mods_allowedOperTypes.h


	Coded by:       George Smith
	Affiliation:    NWS/Office of Hydrology/Hydrologic Research Laboratory
	Date:           9/24/94
	Modified:       


   *************************************************************************************** */

#ifndef Mods_allowedOperTypes_h
#define Mods_allowedOperTypes_h

#define MAX_ALLOWED_MOD_OPERATIONS 16

static	char *allowed_mod_operations[MAX_ALLOWED_MOD_OPERATIONS] = 
                                   {"SAC-SMA",      /*   1 */
				    "UNIT-HG",      /*   2 */
				    "ADJUST-Q",     /*  14 */
				    "SNOW-17",      /*  19 */
				    "API-CONT",     /*  24 */
				    "RES-SNGL",     /*  26 */
				    "API-MKC",      /*  29 */
                                    "SNOW-43",      /*  31 */
				    "API-CIN",      /*  33 */
				    "API-SLC",      /*  34 */
				    "API-HAR",      /*  35 */
				    "XIN-SMA",      /*  36 */
				    "BASEFLOW",     /*  38 */
                                    "API-HAR2",     /*  41 */
				    "API-HFD",     /*  43 */
					"DHM-OP"};     /* 64 */

#endif

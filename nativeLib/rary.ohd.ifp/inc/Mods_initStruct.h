/****************************************************************/
/*								*/
/*	FILE:		Mods_initStruct.h			*/
/*								*/
/*	Include file for configuring the Mods interface		*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		10/19/94				*/
/*	Modified:	10/20/94 - (TEA)			*/
/*			10/31/94 - (TEA) - added Units to	*/
/*				modLimitsDef struct		*/
/*			21 Oct. 95 - D. Page -                  */
/*                              added TIME_PERIODS define	*/
/****************************************************************/

#ifndef Mods_initStruct_h
#define Mods_initStruct_h

#define		DIMENSIONLESS	0
#define		LENGTH_UNITS	1
#define		FLOW_UNITS	2
#define		API_UNITS	3
#define		SAC_UNITS	4
#define         TIME_PERIODS    5

typedef struct
	{
	char		name[20];			/* Mod name			*/
	float           lower_warning_limit;		/* Warning -	lower limit	*/
	float           upper_warning_limit;		/* Warning -	upper limit	*/
	float           lower_error_limit;		/* Error   -	lower limit	*/
	float           upper_error_limit;		/* Error   -	upper limit	*/
	int             lower_warning_inclusive;	/* Limit inclusive - TRUE/FALSE	*/
	int             upper_warning_inclusive;	/* Limit inclusive - TRUE/FALSE	*/
	int             lower_error_inclusive;		/* Limit inclusive - TRUE/FALSE	*/
	int             upper_error_inclusive;		/* Limit inclusive - TRUE/FALSE	*/
	int		units;				/* --- SEE ABOVE ---		*/
	}       modLimitsDef;

typedef struct
	{
	int		num;		/* Number of array elements; should	*/
					/* equal the number of Mod types...	*/
	modLimitsDef	**array;	/* Array of pointers to structures	*/
	}	Mod_limitsStruct;


	/* gfs 950219 */

#include "mod_data.h"

	/* gfs 950219 */

#endif

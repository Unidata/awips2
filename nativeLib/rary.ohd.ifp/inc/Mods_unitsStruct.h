/****************************************************************/
/*								*/
/*	FILE:		Mods_unitsStruct.h			*/
/*								*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/21/94				*/
/*	Modified:						*/
/*								*/
/****************************************************************/

#ifndef Mods_unitsStruct_h
#define Mods_unitsStruct_h

int             SAC_Units;	/* If 0: ENGLISH, 1: METRIC     */
int             API_Units;	/* If 0: ENGLISH, 1: METRIC     */
int             General_Units;	/* If 0: ENGLISH, 1: METRIC     */
int             NWSRFS_Units;	/* If 0: ENGLISH, 1: METRIC     */



/*--------------------------------------------------------------*/
/* Structure to hold Flags for			*/
/*--------------------------------------------------------------*/
typedef struct
	{
	int     General_Units;
	int     NWSRFS_Units;
	int     API_Units;
	int     SAC_Units;
	}       unitsStruct_t, *unitsStruct_p;
	
#endif
 

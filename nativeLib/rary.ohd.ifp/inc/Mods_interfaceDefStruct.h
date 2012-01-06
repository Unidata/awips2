/****************************************************************/
/*								*/
/*	FILE:		Mods_interfaceDefStruct.h		*/
/*								*/
/*	Include file for configuring the Mods interface to	*/
/*	match the available Operations Table and Mods for	*/
/*	the Operations						*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		09/28/94				*/
/*								*/
/****************************************************************/

#ifndef Mods_interfaceDefStruct_h
#define Mods_interfaceDefStruct_h

#include "Mods_defStruct.h"

typedef struct
	{
	int		num;			/* Number of array elements		*/
	Mod_defStruct	**array;		/* An array of pointers to structures	*/
	}       Mod_interfaceDefStruct;
 
#endif

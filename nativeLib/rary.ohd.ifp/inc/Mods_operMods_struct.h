/****************************************************************/
/*								*/
/*	FILE:		Mods_operMods_struct.h			*/
/*								*/
/*	Include file for the info about which operations	*/
/*		a mod can be applied to				*/
/*								*/
/*								*/
/*	Coded by:	George Smith				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/19/94				*/
/*	Modified:		                                */
/*								*/
/****************************************************************/

#ifndef Mods_operMods_Struct_h
#define Mods_operMods_Struct_h

#define MAX_OPERS_PER_MOD 20
#define MAX_NUMBER_OF_MODS 40

typedef struct
	{
	 int        number_of_opers;
	 int        oper_numbers[MAX_OPERS_PER_MOD];
	 char       oper_types[MAX_OPERS_PER_MOD][9];
	} mod_oper_struct;

typedef struct
	{
	int             number_of_mods;
	int             mod_numbers[MAX_NUMBER_OF_MODS];
	char            mod_names[MAX_NUMBER_OF_MODS][9];
	mod_oper_struct *mod_oper_info[MAX_NUMBER_OF_MODS];
	}       operMod_struct;

#endif

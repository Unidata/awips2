/**************************************************************/
/*                                                            */
/*       File:	Mods_ofsData_struct.h                         */
/*                                                            */
/*	Include file for configuring the Mods interface to    */
/*	match the available Operations Table and Mods for     */
/*	the Operations					      */
/*                                                            */
/*	Coded by:	Tom Adams                             */
/*			NWS * Office of Hydrology * HRL       */
/*	Date:		09/08/94                              */
/*                                                            */
/*                                                            */
/**************************************************************/

#ifndef Mods_ofsData_struct_h
#define Mods_ofsData_struct_h

typedef struct
	{
	float	*p_float_array;
	char	**p_char_array;
	float	*ts_float_array;
	char	**ts_char_array;
	float	*d_float_array;
	int	*t_array;
	}       ofsData_struct;
 
#endif

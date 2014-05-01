/****************************************************************/
/*								*/
/*	FILE:		Mods_optionsOpMenuStruct.h		*/
/*								*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/09/94				*/
/*	Modified:	11/10/94				*/
/*								*/
/*	NOTE:	This structure and data held by this structure	*/
/*		are created & destroyed dynamically, depending	*/
/*		on whether or not the selected Mod has an	*/
/*		Option field; there is a one-to-one mapping	*/
/*		between keywords & values, although the value	*/
/*		may be and usually is the Keyword itself. Con-	*/
/*		sequently, a value of -9999 indicates that the	*/
/*		Keyword is the value...				*/
/*								*/
/****************************************************************/

#ifndef Mods_optionsOpMenuStruct_h
#define Mods_optionsOpMenuStruct_h

/*--------------------------------------------------------------*/
/* Structure to hold data for the Options OptionMenu	 	*/
/*--------------------------------------------------------------*/
typedef struct
	{
	int			num;		/* Number of Items				*/
	char			**keyword;	/* Array of NWSRFS User's Manual Keywords	*/
	int			*value;		/* Array of values if the option is different	*/
						/* from the Keyword				*/
	pullDownMenu_struct	*menuData;	/* Pointer to a structure - OptionMenu menu	*/
						/* items...					*/
	}       OptionsOpMenuStruct_t, *OptionsOpMenuStruct_p;
	
#endif
 

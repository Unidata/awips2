/****************************************************************/
/*								*/
/*	FILE:		Mods_defStruct.h			*/
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
/*	Modified:	11/21/94 - (TEA) - added Field #12	*/
/*                      22 Oct. 95 - D. Page added Field #13    */
/*		  	4/5/99 - AV added Field #14		*/
/*		  	3/09/00 - AV            		*/
/*								*/
/*	NOTE:	See the written documentation, "Defining new	*/
/*		Run-time Modifications in NWSRFS-IFP".		*/
/*		Fields A & B are used for identification,	*/
/*		purposes for the most part...			*/
/*								*/
/*		(1) Valid Mod name are given in the NWSRFS	*/
/*		    Manual					*/
/*								*/
/*		(2) See "mods_info.h" for Mod Type definitions,	*/
/*		    but they look like this:			*/
/*								*/
/*			#define         Mod_format_A1   1	*/
/*			#define         Mod_format_A2   2	*/
/*			#define         Mod_format_B1   3	*/
/*			#define         Mod_format_B2   4	*/
/*			#define         Mod_format_B3   5	*/
/*								*/
/*		(3) Field #10 identifies whether the list	*/
/*		    shows Operation Type & Name or Time-series	*/
/*		    ID, Data Type, & Delta T			*/
/*								*/
/*		(4) Field #11 identifies what kind of Popup	*/
/*		    data window (if any) is associated with	*/
/*		    a Mod:					*/
/*								*/
/*			NONE			0		*/
/*			SETQMEAN_TYPE		1		*/
/*			TIME_SERIES_TYPE	2		*/
/*								*/
/*		(5) Field #12 identifies if the Operation/Time-	*/
/*		    series List has Single or Multiple		*/
/*		    Selection behavior:				*/
/*								*/
/*			SINGLE_SELECT_TYPE	0		*/
/*			MULTIPLE_SELECT_TYPE	1		*/
/*								*/
/*              (6) Field #13 determines the number of          */
/*                  time periods from the start of run the      */
/*                  start date of the mod should be offset      */
/*                                                              */ 
/*		(6) The remaining structure member values are	*/
/*		    either ON or OFF or INSENSITIVE		*/
/*								*/
/*	        (7) Field # 14 determines Mods options: 	*/
/*	            SEGMENT     0				*/
/*	            FGROUP      1				*/
/*	            RANGE       2				*/
/****************************************************************/

#ifndef Mods_defStruct_h
#define Mods_defStruct_h

#define		MOD_DEF_LENGTH		200

#define		OFF			0
#define		ON			1
#define		INSENSITIVE		2

#define		OP_TYPE			0
#define		TS_TYPE			1

#define		NONE			0
#define		SETQMEAN_TYPE		1
#define		TIME_SERIES_TYPE	2 
#define		SACCO_TYPE		3 /*aivo 2/9/01*/

#define		SINGLE_SELECT_TYPE	0
#define		MULTIPLE_SELECT_TYPE	1

#define		SEGMENT			0
#define		FGROUP  		1
#define		RANGE 			2


typedef struct
	{
	char	name[20];	/* Mod name					   -  A	*/
	int	type;		/* Mod type					   -  0	*/
	int	value;		/* Mod Value:			ON/OFF 		   -  1	*/
	int	arrowButtons;	/* Mod Date Arrow Buttons:	ON/OFF 		   -  2	*/
	int	modStartDate;	/* Mod Start date:		ON/OFF/INSENSITIVE -  3	*/
	int	modEndDate;	/* Mod End date:		ON/OFF/INSENSITIVE -  4	*/
	int	modValidDate;	/* Mod Valid date:		ON/OFF/INSENSITIVE -  5	*/
	int	OpTimeSeries;	/* Mod Operation/Time Series:	ON/OFF 		   -  6	*/
	int	TSDates;	/* Mod Time-series dates:	ON/OFF 		   -  7	*/
	int	entryDialog;	/* Mod Data entry Dialog:	ON/OFF 		   -  8	*/
	int	modOption;	/* Mod option 			ON/OFF 		   -  9	*/
	int	OpTSType;	/* Mod Operation/TS type	OP_/TS_TYPE	   - 10	*/
	int	DataWinType;	/* Mod Data Window Popup type	See Note above	   - 11	*/
	int	OpTSSelectType;	/* Operation/TS List selection type, See Note	   - 12	*/
	int     modStartDateOffset; /* Number of time periods start date of mod    - 13 */
				    /* should be offset from start date of run          */
        int     modFGroup;      /* Mods options    SEGMENT/FGROUP/RANGE            - 14 */
        int     UHGList;        /* AiV 5/4/04 List of UHGCDATE mod                 - 15 */
	}       Mod_defStruct;
 
#endif

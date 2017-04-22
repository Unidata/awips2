/********************************************************************************/
/*										*/
/*	FILE:		Mods_everythingStruct.h					*/
/*										*/
/*	Include file for configuring the Mods interface to match the available	*/
/*	Operations Table and Mods for the Operations				*/
/*										*/
/*	NOTE:		The term 'static' applied to Widgets and structures	*/
/*			holding Widgets refers to the fact that the Widgets	*/
/*			have been created in XDesigner (GUI Builder) and can	*/
/*			not be altered by the programmer except by going into	*/
/*			XDesigner - one must be cognizant of references to	*/
/*			structure members when making GUI changes in XDesigner	*/
/*										*/
/*	Coded by:	Tom Adams (TEA)						*/
/*			NWS * Office of Hydrology * HRL				*/
/*	Date:		09/22/94						*/
/*	Modified:	09/22/94 - (TEA)					*/
/*			09/28/94 - (TEA) - added Mod_interfaceDefStruct		*/
/*			10/03/94 - (TEA) - added #include statments		*/
/*			10/24/94 - (TEA) - added #include "Mods_initStruct.h"	*/
/*					and 'mod_limits_data'			*/
/*			11/20/94 - George Smith (GFS) - 			*/
/*					 added #include Mods_operMods_struct.h  */
/*			11/21/94 - (TEA) - added #include "Mods_unitsStruct.h"	*/
/*			4 Oct. 1995 - D. Page - added *fromFileMods_str		*/
/*						and *ofsMods_str		*/
/*			9 Oct. 1995 - D. Page -	added *Mod_globalPrefs          */		
/*                     29 Oct. 1995 - D. Page - added currentModSaved           */
/*                     30 Oct. 1995 - D. Page - added previousModName           */
/*	       	       31 Oct. 1995 - D. Page - added currentModChanged  	*/
/*			6 Nov. 1995 - D. Page - added opTSdata			*/
/*		       11 Nov. 1995 - D. Page - added ofsModsSaved, 		*/
/*						fromFileModsSaved		*/
/*										*/
/********************************************************************************/

#ifndef Mods_everythingStruct_h
#define Mods_everythingStruct_h

#include "Mods_widgetStruct.h"
#include "Mods_config.h"
#include "Mods_ofsData_struct.h"
#include "Mods_interfaceDefStruct.h"
#include "Mods_initStruct.h"
#include "DateHandling.h"
#include "Mods_flags.h"
#include "Mods_optionsOpMenuStruct.h"
#include "Mods_dialogStruct.h"
#include "Mods_operMods_struct.h"  		/* Added by gfs - 11/20/94 */
#include "Mods_unitsStruct.h"
#include "Mods_info.h"
#include "ifp_struct.h"
#include "Mods_globalPrefs.h"                   /* Added by page - 10/09/95 */
#include "Mods_opTSDataStruct.h"                /* Added by page - 11/06/95 */
#include "show_sacco.h"	
#include "UhgType.h"
   			
    
typedef struct
	{
	char			*ModSettingsPath;	/* Path to find ALL Mod Settings &	*/
							/* interface initialization files	*/
							/* Definitions that affect the GUI	*/
	char			SegmentName[20];	/* Name of the current Segment (Basin)	*/
	char			ModName[20];		/* Name of the currently selected Mod	*/
	char                    previousModName[20];    /* Name of the previously selected Mod  */
	int			ModIndex;		/* Index identifying current ModArray	*/
							/* position				*/
	ModInfo			*ModArray[MAX_MODS];	/* Array of pointers to structures for	*/
							/* Mod data to be written to a file	*/
	ofsData_struct		*ofsData;		/* OFS data arrays			*/
	ifp_modsShell_p		widgetData;		/* All the 'static' Widgets		*/
	viewerShell_p		viewerWidgets;		/* Viewer 'static' Widgets		*/
	setQMeanShell_p		setQMeanWidgets;	/* SETQMEAN 'static' Widgets		*/
	pullDownMenu_struct	*opsMenu;		/* Operations Menu Widgets, etc.	*/
	pullDownMenu_struct	*modsMenu;		/* Mods Menu Widgets, etc.		*/
	Mod_interfaceDefStruct	*interfaceDefs;		/* Mods interface definitions data	*/
	Mod_defStruct		*selectedModDef;	/* Mod definition for the currently	*/
							/* selected Mod...			*/
	Mod_limitsStruct	*modLimits;		/* Pointer to a struct holding an array	*/
							/* of range limits & number of array	*/
							/* elements...				*/
	modLimitsDef		*modValueLimits;	/* Mod limits for the currently		*/
							/* selected Mod...			*/
	modLimitsDef		*scaleValueLimits;	/* Mod limits for the currently		*/
							/* selected Mod - adjusted values...	*/
	datesStruct_p		ModDates;		/* Pointer to a struct to hold the Mods	*/
							/* Start, End, and Valid dates		*/
	Mod_flagsStruct_p	flags;			/* Pointer to a structure		*/
	OptionsOpMenuStruct_p	Options;		/* Pointer to a structure holding data	*/
							/* for Option OptionMenu		*/
	dialogWidgetStruct_p	dialogStruct;		/* Structure holding MessageBox Widgets	*/
							/* of Dialogs...			*/
	operMod_struct		*operModData;		/* Operations to which each mod applies */
	unitsStruct_p		units;			/* Pointer to a structure: Flags set	*/
							/* for identifying what units are being	*/
							/* used...				*/
	mod_data		*ModSettings;		/* Pointer to a structure holding Oper-	*/
							/* ation & Time-series info.		*/
	mods_plot_struct	*modsPlotData;		/* Pointer to a structure used in TS	*/
							/* Mods for plotting & changing TS	*/
        char                    *fromFileMods_str;      /* Original text in fromFileMods file   */
	char                    *ofsMods_str;           /* Original text in ofsMods file        */
        char                    *fromFilefgMods_str;    /* Original text in fromFileMods file   */
	char                    *ofsfgMods_str;         /* Original text in ofsfgroup Mods file        */
	Mod_globalPrefs_t       *Mod_globalPrefs;       /* pointer to structure for global prefs*/
	int                     currentModSaved;        /* flag for if the current mod was saved*/
        int                     currentModChanged;      /* flag for if mod was changed          */
        OpTSTypeStruct_p        opTSdata;               /* holds the operation/time series list */
        int                     ofsModsSaved;           /* flag for if the ofs Mods were saved  */
        int                     fromFileModsSaved;      /* flag for if the file mods were saved */
        int                     fromFilefgModsSaved;      /* flag for if the file mods were saved */
        int                     fgroupModsSaved;        /* flag for if the file mods were saved */
        int                     fgroupModsselected;        /* flag for if the fgroup mods were saved */
        int                     ofsfgModsSaved;           /* flag for if the ofs fgroup Mods were saved  */
        int                     rangeModsSaved;           /* flag for if the ofs fgroup Mods were saved  */
        int                     create_flag;              /* AiV 5/4/04 */
        int                     mp_doneClick;             /* AiV 5/4/04 */
        WaterLevelType		*WaterLvl;
		
        }       Mods_everythingStruct;
 
#endif

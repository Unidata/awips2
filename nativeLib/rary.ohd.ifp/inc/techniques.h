
/* ********************************************************************************

	techniques.h
		Include file for the NWSRFS Interactive Forecast
		Program Universal & Non-universal 'Techniques'

	Coded:  3/20/91
	By:     Tom Adams - NWS/Office of Hydrology/HRL

   ******************************************************************************** */

#ifndef techniques_h
#define techniques_h

typedef struct
	{
	int             mod_units;                      /* English/Metric units...      */
	int             mod_sac_units;                  /* English/Metric units...      */
	int             mod_api_units;                  /* English/Metric units...      */
	int             metric_units;                   /* Gen. English/Metric units... */
	int             mod_warning;                    /* Show Mod warnings...         */
	int             future_precip;                  /* Quant. Precip. used:  Yes/No */
	char            input_time_zone_code[5];        /* ....NEW....                  */
	int             output_daylight_savings;        /* Output Daylight Sav.: Yes/No */
	int             output_time_zone;               /* Output Time Zone code        */
	char            mod_time_zone_code[5];
	}       univ_techniques_struct;


univ_techniques_struct          *universalTechniques;
univ_techniques_struct          *temp_univ_Techniques;

int     non_univ_techs_All_segs_selected;       /* A flag that indicates whether or     */
						/*  not all the segments have been      */
						/*  selected in the Non-universal       */
						/*  techniques popup...                 */
#define ENGLISH_UNITS   0
#define METRIC_UNITS    1

#define ON              1
#define OFF             0
#define NO_CHANGE       2

#define ZONE_Z          0       /* Zulu: Greenwich, England     */
#define ZONE_EST       -5       /* Eastern Standard Time        */
#define ZONE_CST       -6       /* Central Standard Time        */
#define ZONE_MST       -7       /* Mountain Standard Time       */
#define ZONE_PST       -8       /* Pacific Standard Time        */
#define ZONE_AST       -9       /* Alaskan Standard Time        */
#define ZONE_NST       -10      /* Nome Standard Time           */
#define ZONE_HST       -11      /* Hawaiian Standard Time       */


#endif

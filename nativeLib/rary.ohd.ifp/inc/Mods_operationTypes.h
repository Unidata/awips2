
/* ***************************************************************************************

	Mods_opertationTypes.h


	Coded by:       Tom Adams
	Affiliation:    NWS/Office of Hydrology/Hydrologic Research Laboratory
	Date:           9/12/94
	Modified:       9/12/94
                        24/Sept/94 to add additional operation types - gfs - hrl


   *************************************************************************************** */

#ifndef Mods_operationTypes_h
#define Mods_operationTypes_h

	char *operationTypes[65] = {"SAC-SMA",      /*   1 */
				    "UNIT-HG",      /*   2 */
				    "REDO-UHG",     /*   3 */
				    "CLEAR-TS",     /*   4 */
				    "SAC-PLOT",     /*   5 */
				    "MEAN-Q",       /*   6 */
				    "LAG/K",        /*   7 */
				    "CHANLOSS",     /*   8 */
				    "MUSKROUT",     /*   9 */
				    "ADD/SUB",      /*  10 */
				    "LAY-COEF",     /*  11 */
				    "INSQPLOT",     /*  12 */
				    "TATUM",        /*  13 */
				    "ADJUST-Q",     /*  14 */
				    "WEIGH-TS",     /*  15 */
				    "STAT-QME",     /*  16 */
				    "WY-PLOT",      /*  17 */
				    "PLOT-TS",      /*  18 */
				    "SNOW-17",      /*  19 */
				    "CHANGE-T",     /*  20 */
				    "DWOPER",       /*  21 */
				    "HFS",          /*  22 */
				    "STAGE-Q",      /*  23 */
				    "API-CONT",     /*  24 */
				    "PLOT-TUL",     /*  25 */
				    "RES-SNGL",     /*  26 */
				    "LIST-FTW",     /*  27 */
				    "CHANLEAK",     /*  28 */
				    "API-MKC",      /*  29 */
				    "MERGE-TS",     /*  30 */
				    "SNOW-43",      /*  31 */
				    "FFG",          /*  32 */
				    "API-CIN",      /*  33 */
				    "API-SLC",      /*  34 */
				    "API-HAR",      /*  35 */
				    "XIN-SMA",      /*  36 */
				    "LIST-MSP",     /*  37 */
				    "BASEFLOW",     /*  38 */
				    "LOOKUP",       /*  39 */
                                    "WATERBAL",     /*  40 */
                                    "API-HAR2",     /*  41 */
                                    "RSNWELEV",     /*  42 */
                                    "API-HFD",      /*  43 */
                                    "SARROUTE",     /*  44 */
                                    "DELTA-TS",     /*  45 */
                                    "NOMSNG",       /*  46 */
                                    "PEAKFLOW",     /*  47 */
                                    "MULT/DIV",     /*  48 */
                                    "BEGASSIM",     /*  49 */
                                    "ASSIM",        /*  50 */
			            "SSARRESV",     /*  51 */
                                    "SUMPOINT",     /*  52 */
                                    "LOOKUP3",      /*  53 */
                                    "SWB-NILE",     /*  54 */
				    "FLDWAV",       /*  55 */
                                    "GLACIER",      /*  56 */
				    "CONS_USE",     /*  57 */
				    "RES-J",        /*  58 */
				    "TIDEREV",      /*  59 */
                                    "ADJUST-T",      /*  60 */
                                    "STAGEREV",     /*  61 */
                                    "ADJUST-H",     /*  62 */
                                    "SET-TS", /*  63 */
                                    "DHM-OP", /*  64 */
                                    "not_assigned"};/*  65 */
                                    

#endif

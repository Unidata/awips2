/******************************************************************/
/*                                                                */
/*	FILE:		Mods_initializeInterface.c                */
/*                                                                */
/*	Run-time Modifications Interface initialization	          */
/*	module						          */
/*                                                                */
/*	Coded by:	Tom Adams                                 */
/*			NWS * Office of Hydrology * HRL           */
/*	Date:		09/08/94                                  */
/*			03/15/00  AV   added SACCO in the Array   */
/*                                                                */
/*      NOTE:	This file contains functions that complete        */
/*		the interface setup - these include GUI	          */
/*		elements that change dynamically and can not      */
/*		be created statically in a GUI builder,           */
/*		such as the available Operations or Mods for      */
/*		an individual Segment (basin).		          */
/*                                                                */
/******************************************************************/
#ifndef Mods_menuStruct_h
#define Mods_menuStruct_h

xs_menu_struct ModStruct_array[] =
	{
	{"AEICQN",   NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"AESCCHNG", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"AIADJ",    NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"APICQN",   NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"APICBASF", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"BASEF",    NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"BFRCHNG",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"CBASEF",   NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"CHGBLEND", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"IGNORETS", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"MFC",      NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"RAINSNOW", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"ROCHNG",   NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"ROMULT",   NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"RRICHNG",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"RRIMULT",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"SACBASEF", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"SACCO",    NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"SETMSNG",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"SETQMEAN", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"SWITCHTS", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"UADJ",     NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"UHGCHNG",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},        
	{"WEADD",    NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"WECHNG",   NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"ZERODIFF", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"UHGCDATE", NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"DPRECIP",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"DSACST",  NULL, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL}	
	};


/*------------------------------*/
/*	ALL MODS AVAILABLE	*/
/*------------------------------*/
/* AEICQN   */
/* AESCCHNG */
/* AIADJ    */
/* APICBASF */
/* APICCO   */
/* APICQN   */
/* BASEF    */
/* BFRATE   */
/* BFRCHNG  */
/* BUBLSHFT */
/* CBASEF   */
/* CBFRATE  */
/* CHGBLEND */
/* IGNORETS */
/* MATCHNG  */
/* MFC      */
/* QCSHIFT  */
/* QPSHIFT  */
/* RAINSNOW */
/* ROCHNG   */
/* ROMULT   */
/* RRICHNG  */
/* RRIMULT  */
/* SACBASEF */
/* SACCO    */
/* SETMSNG  */
/* SETQMEAN */
/* SSARREG  */
/* SWITCHTS */
/* TSADD    */
/* TSCHNG   */
/* TSMULT   */
/* TSREPL   */
/* UADJ     */
/* UCBASEF  */
/* UCBFRATE */
/* UHGADJ   */
/* UHGCHNG  */
/* WEADD    */
/* WECHNG   */
/* WEUPDATE */
/* XINCO    */
/* ZERODIFF */
/* UHGCDATE */ /*AV added for UHG CHANGE MOD wiht start date and end date*/
/* DPRECIP  */
/* DSACST  */

/*------------------------------*/
/*	46 - Total		*/
/*------------------------------*/

#endif

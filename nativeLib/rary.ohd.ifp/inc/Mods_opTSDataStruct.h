/****************************************************************/
/*								*/
/*	FILE:		Mods_opTSDataStruct.h			*/
/*								*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/09/94				*/
/*	Modified:						*/
/*								*/
/****************************************************************/

#ifndef Mods_opTSDataStruct_h
#define Mods_opTSDataStruct_h

/*--------------------------------------------------------------*/
/* Structure to hold XmString array for the Operation/Time-	*/
/* series List							*/
/*--------------------------------------------------------------*/
typedef struct
	{
	int		num;			/* Number of Items, 20 Maximum	*/
	XmString	xmStringList[20];
	}       OpTSTypeStruct_t, *OpTSTypeStruct_p;
	
#endif
 

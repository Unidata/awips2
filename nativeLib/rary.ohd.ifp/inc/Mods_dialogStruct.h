/****************************************************************/
/*								*/
/*	FILE:		Mods_dialogStruct.h			*/
/*								*/
/*	Include file for holding the MessageBox Widgets of the	*/
/*	Error, Warning, and Message Dialogs for setting the	*/
/*	Run-time Modifications					*/
/*								*/
/*								*/
/*	Coded by:	Tom Adams (TEA)				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		11/15/94				*/
/*	Modified:	D. Page - 29 Oct. 1995			*/
/*			added modNotSavedMessageBox and		*/
/*			modTheSameMessageBox			*/
/*			D. Page - 11 Nov. 1995 - added		*/
/*			modViewerNotSavedMB			*/
/*  Modified by Guoxian Zhou --Nov. 24, 2003                    */
/*         added ErrorMessageBox                                */      
/*								*/
/****************************************************************/

#ifndef Mods_dialogStruct_h
#define Mods_dialogStruct_h

typedef struct
	{
	Widget	valueWarningMessageBox;
	Widget	valueErrorMessageBox;
	Widget  modNotSavedMessageBox;
	Widget  modTheSameMessageBox;
	Widget  modViewerNotSavedMB;
	Widget  modDateErrorMessageBox;/*Added by gzhou 11/26/2003*/
	}       dialogWidgetStruct_t, *dialogWidgetStruct_p;
 
#endif
 

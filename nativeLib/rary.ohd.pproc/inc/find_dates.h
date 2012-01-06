/*******************************************************************************
* FILENAME:             find_dates.h
* GENERAL INFORMATION:
* DESCRIPTION:          Contains the prototypes for the 
*						get_dates_RFCW,
*						GetCurrentDate_RFCW,
*						GetEarlierDate_RFCW(int hrs)
*                       routines in the find_dates.c file.
* ORIGINAL AUTHOR:      Moria Shebsovich
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell-Linux
* MODIFICATION HISTORY:
* DATE                      PROGRAMMER         DESCRIPTION/REASON
* February 11, 2002          Moria Shebsovich  Original Coding.
********************************************************************************
*/

#ifndef FIND_NAMES_H
#define FIND_NAMES_H

void get_dates_RFCW();

void GetCurrentDate_RFCW();

void GetEarlierDate_RFCW(int hrs);

#endif /* #ifndef FIND_NAMES_H */

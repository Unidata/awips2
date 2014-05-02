/*******************************************************************************
* FILENAME:             update_flig_RFCW.h
* GENERAL INFORMATION:  Contains the prototype for the update_flig_RFCW
*                       routine.
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        December 15, 2005
* ORGANIZATION:         OHD11, HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/15/2005   Bryon Lawrence    Modified the update_flig_RFCW prototype
*                                  to contain the ignore_radar_flag argument. 
********************************************************************************
*/
#ifndef UPDATE_FLIG_RFCW_H
#define UPDATE_FLIG_RFCW_H

/*prototype for function update_flig_RFCW() */
void update_flig_RFCW ( short ignore_radar_flag, char * rid , 
                        char * datetime );

void update_flig_dp_RFCW ( short ignore_radar_flag, char * rid ,
                        char * datetime );

#endif /* #ifndef UPDATE_FLIG_RFCW_H */

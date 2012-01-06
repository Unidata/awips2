/*******************************************************************************
* FILENAME:            update_rawPP.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototype for the update_rawPP routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       January 12, 2005
* ORGANIZATION:        OHD/HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   1/12/05      Bryon Lawrence    Original coding
********************************************************************************
*/

#ifndef UPDATE_RAWPP_H
#define UPDATE_RAWPP_H

void update_rawPP ( const char * obstime,
                    const char * lid,
                    const char * ts,
                    char shef_qual_code,
                    int dur,
                    float value );

#endif /* #ifndef UPDATE_RAWPP_H */

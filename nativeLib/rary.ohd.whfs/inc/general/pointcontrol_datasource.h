/*******************************************************************************
* FILENAME:             pointcontrol_datasource.h
* GENERAL INFORMATION:
* DESCRIPTION:          Contains the prototype for the get_DatasrcList
*                       routine.  Also contains the manifest constants
*                       used by this routine. 
*
* ORIGINAL AUTHOR:      Unknown
* CREATION DATE:        Unknown
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/31/03     Bryon Lawrence    Added documentation.  Split the pointcontrol
*                                  library into the PdcEngine and PdcGui
*                                  libraries.
********************************************************************************
*/

#ifndef POINTCONTROL_DATASOURCE_H
#define POINTCONTROL_DATASOURCE_H

/* Manifest constants. */
#define DCP_STRING "DCP"
#define OBSERVER_STRING "Observer"
#define UNDEFINED_STRING "Undefined"

/* Function prototypes. */
int get_AdHocDataSourceList();
#endif /* #ifndef POINTCONTROL_DATASOURCE_H */

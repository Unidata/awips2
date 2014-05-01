/*******************************************************************************
* FILENAME:             gage_pp_rm_file.h
* GENERAL INFORMATION:  
* DESCRIPTION:          Contains the prototype for the gage_pp_rm_file routine.
*                       This routine deletes the PP/PC precip files after
*                       the gage precipitation processor has finished
*                       reading and processing them.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        July 30, 2004
* ORGANIZATION:         HSEB / OHD
* MACHINE:
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   7/30/2004    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef GAGE_PP_RM_FILE_H
#define GAGE_PP_RM_FILE_H

void gage_pp_rm_file ( const char * rmfile ) ;

#endif /* #ifndef GAGE_PP_RM_FILE_H */

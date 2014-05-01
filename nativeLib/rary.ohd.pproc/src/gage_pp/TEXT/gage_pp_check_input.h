/*******************************************************************************
* FILENAME:            gage_pp_check_input.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*         DESCRIPTION: This file contains prototype of gage_pp_check_input 
*                      routine defined within the gage_pp_check_input.c file.
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       July 7, 2004 
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        7/?/2004     Moria Shebsovich  Original Coding
*          1        8/9/2004     Bryon Lawrence    Changed routine name
*                                                  from check_input to
*                                                  gage_pp_check_input        
*********************************************************************************/

#ifndef GAGE_PP_CHECK_INPUT_H
#define GAGE_PP_CHECK_INPUT_H

/* Function prototype. */
int gage_pp_check_input (  const char * data_dir ,
                           const char * stopfilename ,
                           char   ** infiles ,
                           int    MAXFILES ,
                           int    * numfound ) ;
 
#endif /* #ifndef GAGE_PP_CHECK_INPUT_H */



/*******************************************************************************
* FILENAME:            ReadAndSwap.h
* DESCRIPTION:         Contains the prototype and error code information 
*                      for the ReadAndSwap routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 16, 2001
* ORGANIZATION:        HSEB / OHD
* MACHINE:             HP-UX / Redhat Linux
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
* 11/16/01     Bryon Lawrence    Original Coding
********************************************************************************
*/
#ifndef READ_AND_SWAP_H
#define READ_AND_SWAP_H

#define READ_AND_SWAP_BAD_VALUE -1 
#define READ_AND_SWAP_IO_ERROR  -2
#define READ_AND_SWAP_OS_ERROR  -3

size_t ReadAndSwap ( void * ptr , size_t size , size_t nitems , 
                     FILE * stream ) ;

#endif /* #ifndef READ_AND_SWAP_H */

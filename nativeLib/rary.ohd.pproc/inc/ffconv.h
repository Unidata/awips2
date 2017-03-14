/*******************************************************************************
* FILENAME:             ffconv.h
* DESCRIPTION:          Contains the prototype for the ffconv routine. This
*                       routine converts from an unsigned char value to 
*                       a short integer value.
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        September 3, 2003
* ORGANIZATION:         OHD / HSEB
* MACHINE O/S:          HP-UX, Redhat Linux             
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   9/3/2003     Bryon Lawrence    Original Coding
*   3/22/2004    Bryon Lawrence    Modified this header file to contain all
*                                  of the variable used by the ffconv 
*                                  function.
********************************************************************************
*/

#ifndef FFCONV_H
#define FFCONV_H

/* Variables used by function ffconv. */
#define fill_value -998
#define lower_thres  201
#define mmfactor .33
#define miss_value -999
#define upper_thres  253
#define xmrg_factor 100 

/*  fill value for unsigned char    */

#define fill_value_uchar 0

short int MPEUtil_ffconv ( unsigned char uchar ) ;

#endif /* #ifndef FFCONV_H */

/*******************************************************************************
* FILENAME:             read_product_dt.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       read_product_dt routine in the read_product_dt.ec
*                       source file.
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 7, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                 PROGRAMMER         DESCRIPTION/REASON
* February 7, 2002     Moria Shebsovich   Original Coding 
********************************************************************************
*/

#ifndef READ_PRODUCT_DT_H
#define READ_PRODUCT_DT_H

void read_product_dt ( char rad[4], char * datetime, int * n );
void read_daaproduct_dt ( char rad[4], char * datetime, int * n );
 
#endif /* #ifndef READ_PRODUCT_DT_H */
 

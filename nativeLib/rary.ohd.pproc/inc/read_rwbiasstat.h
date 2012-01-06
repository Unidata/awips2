/*******************************************************************************
* FILENAME:             read_rwbiasstat.h
* GENERAL INFORMATION:
* DESCRIPTION:          This file contains the prototype for the 
*                       read_rwbiasstat routine in the read_rwbiasstat.ec
*                       source file.
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 7, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                 PROGRAMMER          DESCRIPTION/REASON
* February 7, 2002     Moria Shebsovich    Original Coding 
********************************************************************************
*/

#ifndef READ_RWBIASSTAT_H
#define READ_RWBIASSTAT_H

void MPEUtil_read_rwbiasstat ( float * min_gr_value,
                       int * npair_bias_select,
                       int * npair_svar_update,
                       int * std_cut,
                       int * lag_cut,
                       int * init_span,
                       int * bias_qc_opt,
                       int * num_span,
                       float mem_span_values[10],
                       long int * ircbia ) ;
		       
#endif /* #ifndef READ_RWBIASSTAT_H */

/*******************************************************************************
* FILENAME:            read_rwbiastat.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          read_rwbiastat
* DESCRIPTION:         Reads a record from the RWBiasStat table. 
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 3, 2004
* ORGANIZATION:        HSEB/OHD
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/3/2004    Bryon Lawrence    Converted to use DBgen 
********************************************************************************
*/

#include "read_rwbiasstat.h"
#include "RWBiasStat.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    read_rwbiasstat
* PURPOSE:        Reads a record from the RWBiasStat table in the IHFS 
*                 database.  This table defines memory span durations
*                 the parameters used in computing the mean field bias for
*                 a radar site.
*
*                 There is only one record stored in the RWBiasStat table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME              DESCRIPTION/UNITS
*   Output float *     min_gr_value      The minimum precipitation amount
*                                        for a gage value or radar bin value
*                                        to be included in the MPE mean-field
*                                        bias or local bias calculation.
*   Output int *       npair_bias_select The minimum number of nonzero 
*                                        gage/radar pairs required before
*                                        a bias value can be selected for
*                                        use in MPE.
*   Output int *       npair_svar_update The number of positive gage/radar
*                                        pairs necessary to update the state
*                                        variables for the mean-field bias
*                                        calculations in MPE.
*   Output int *       std_cut           The number of standard deviations
*                                        from the best-fit regression line
*                                        within which the positive gage value 
*                                        and the radar value must fall for the
*                                        gage/radar pair to be included in the
*                                        mean-field bias calculation of 
*                                        mpe_fieldgen.
*   Output int *       lag_cut           The cut off lag for storm-by-storm
*                                        reinitialization of spatial averages
*                                        of positive gage and radar rainfall.
*                                        If no state variable data is found
*                                        within lag_cut number of hours,
*                                        then the bias values are reset to 1.0
*                                        and all other state variable data
*                                        are reset to 0.0.
*   Output int *       init_span         Memory span index of choice for
*                                        reinitialization. 
*   Output int *       bias_qc_opt       Quality control option for bad gage
*                                        data in the mean-field bias
*                                        adjustment.
*   Output int *       num_span          The number of memory spans contained
*                                        within the mem_span_values array. Used
*                                        for the mean field bias calculation.
*   Output float [ ]   mem_span_values   Contains the memory span values.
*   Output long int *  ircbia            Flag indicating to the calling routine
*                                        if an error was encountered.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME             HEADER FILE   DESCRIPTION
*   FreeRWBiasStat   RWBiasStat.h  Deallocates the memory used by the linked
*                                  list of RWBiasStat structures.
*   GetRWBiasStat    RWBiasStat.h  Retrieve the records from the RWBiasStat
*                                  table.  Each record is a separate node
*                                  in the linked list.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE    NAME             DESCRIPTION
*   RWBiasStat * pRWBiasStatHead  Points to the head of the linked list of
*                                 RWBiasStat rows.
*   RWBiasStat * pRWBiasStatNode  Points to a node in the linked list of
*                                 RWBiasStat rows.
*
* DATA FILES AND/OR DATABASE:
* Requires an open connection to the IHFS database.   Reads
* the RWBiasStat table.
*
* ERROR HANDLING:
*  ircbia is set to -1 if the record could not be retrieved from the
*  RWBiasStat table. Otherwise, it is set 0.
*
* CALLING FUNCTION:
*    read_static 
********************************************************************************
*/

void MPEUtil_read_rwbiasstat(float *min_gr_value,
                     int *npair_bias_select,
                     int *npair_svar_update,
                     int *std_cut,
                     int *lag_cut,
                     int *init_span,
                     int *bias_qc_opt,
                     int *num_span,
                     float mem_span_values[10],
                     long int *ircbia)

{
   RWBiasStat * pRWBiasStatHead = NULL ;
   RWBiasStat * pRWBiasStatNode = NULL ;

   * ircbia = 0 ;
   
   /*--------------------------------------*/
   /*   read from RWBiasStat table         */
   /*--------------------------------------*/

   /* There is no where clause.  There is only one row of information
      in this table. */
   pRWBiasStatHead = GetRWBiasStat ( "" ) ;

   if ( pRWBiasStatHead != NULL )
   {
      pRWBiasStatNode = ( RWBiasStat * ) ListFirst ( & pRWBiasStatHead->list );

      * min_gr_value = pRWBiasStatNode->min_gr_value_bias;
      * npair_bias_select = pRWBiasStatNode->npair_bias_select;
      * npair_svar_update = pRWBiasStatNode->npair_svar_update;
      * std_cut = pRWBiasStatNode->std_cut;
      * lag_cut = pRWBiasStatNode->lag_cut;
      * init_span = pRWBiasStatNode->init_span;
      * bias_qc_opt = pRWBiasStatNode->bias_qc_opt;
      * num_span = pRWBiasStatNode->num_span;

      mem_span_values[0] = pRWBiasStatNode->mem_span1;
      mem_span_values[1] = pRWBiasStatNode->mem_span2;
      mem_span_values[2] = pRWBiasStatNode->mem_span3;
      mem_span_values[3] = pRWBiasStatNode->mem_span4;
      mem_span_values[4] = pRWBiasStatNode->mem_span5;
      mem_span_values[5] = pRWBiasStatNode->mem_span6;
      mem_span_values[6] = pRWBiasStatNode->mem_span7;
      mem_span_values[7] = pRWBiasStatNode->mem_span8;
      mem_span_values[8] = pRWBiasStatNode->mem_span9;
      mem_span_values[9] = pRWBiasStatNode->mem_span10;
   }
   else
   {
      * ircbia = -1 ;
   }

   /* Deallocate the RWBiasStat linked list. */
   if ( pRWBiasStatHead != NULL )
   {
      FreeRWBiasStat ( pRWBiasStatHead ) ;
      pRWBiasStatHead = NULL ; 
   }

   return ;
}

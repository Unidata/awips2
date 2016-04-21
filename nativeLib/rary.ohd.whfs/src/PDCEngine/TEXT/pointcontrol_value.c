/*******************************************************************************
* FILENAME:            pointcontrol_value.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          process_value_option_menu
* DESCRIPTION:         Processes the currently selected option in the
*                      pointcontrol value option menu.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 2003
* ORGANIZATION:        OHD/HSEB
* MACHINE O/S:         HP-UX / Redhat Linux    
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        5/2003       Bryon Lawrence    Original Coding
********************************************************************************
*/

#include "pointcontrol_derive.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_options.h"
#include "pointcontrol_report.h"
#include "pointcontrol_value.h"
#include "rating_util.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    process_value_option_menu 
* PURPOSE:        Processes the pointcontrol value option menu.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

/* This routine requires that there be a new data member added the pc report
   structure.  This new data member must be called VALUE2.

   This routine also requires that a new member be added to the pc_options
   structure.  This new data member must be called derived_stage_flow. */
 
void process_value_option_menu ( ReportList * reportHead ,
                                 const pc_options_struct * pc_options ,
                                 int retrieval_required )
{
   float flow ;
   float stage ;

   static int valuetype = 0 ;
   static int fldlevel = 0 ;
   static int derive_stage_flow = 0 ;

   enum PeType pe_type ; 

   ReportList * pReportNode = NULL ;
   rs_info_struct * pRsInfo = NULL ;

   if ( reportHead == NULL )
   {
      return ;
   }

   /* Only pass through this routine if the PE starts with a 'H' or a 'Q'
      or if the PE is "PRIMARY".  That is, all of the river stage or flow
      PEs will be passed through this routine.  This filtering should be
      done here, not in the main routine. */
   if ( ( * ( pc_options->selectedAdHocElementString ) != 'H' ) &&
        ( * ( pc_options->selectedAdHocElementString ) != 'Q' ) &&
        ( strcmp ( pc_options->selectedAdHocElementString , "Pr" ) != 0 ) )
   {
      return ;
   }


   /* Set the "previous" variables to reflect what is currently being 
      requested. */
   fldlevel = pc_options->fldlevel ;
   derive_stage_flow = pc_options->derive_stage_flow ;
   valuetype = pc_options->valuetype ;

   /* Begin testing the setting of the Value Pulldown Option Menu. */ 

   /* The first case is where the user just wants to display the
      requested value. This is the simplest case.  It corresponds
      to "Value" item on the "Value" pull down option menu. Also,
      when the user is displaying the change in a value, it does not
      make any sense to be able to display the flood stage or the
      flood departure. */
   if ( ( pc_options->time_mode == VALUE_CHANGE ) ||
        ( ( valuetype == TYPE_VALUE ) &&
        ( fldlevel == 0 ) &&
        ( derive_stage_flow == 0 ) ) )
   {
      /* Not much else to do.  The value is already contained within
         the report structure. */
      return ;
   }

   if ( ( valuetype == TYPE_VALUE ) &&
        ( fldlevel == 1 ) )
   {
      /* Loop over each report in the report list and
         call get_rs_info to retrieve the flood level if
         it is available. */
      pReportNode = ( ReportList * ) ListFirst ( & reportHead->list ) ;

      while ( pReportNode != NULL )
      {
         pRsInfo = get_rs_info ( pReportNode->lid ) ;

         if ( strcmp ( pReportNode->lid , "TRLO2" ) == 0
              || strcmp ( pReportNode->lid , "AWIP2" ) == 0 
              || strcmp ( pReportNode->lid , "AWIP3" ) == 0 )
         {
            if ( pRsInfo != NULL )
            {
               fprintf ( stdout , "In routine \"process_value_option_menu\":\n"
                                  "The values of %s for pe %s are:\n"
                                  "fs = %10.2f, fq = %10.2f\n" ,
                                  pReportNode->lid , 
                                  pReportNode->pe ,
                                  pRsInfo->fs ,
                                  pRsInfo->fq ) ;
            }
            else
            {
               fprintf ( stdout , "In routine \"process_value_option_menu\":\n"
                                  "The values of %s are NULL\n" ,
                                  pReportNode->lid ) ;
            }

         }

         if ( ( pRsInfo != NULL ) && ( pRsInfo->fs != MISSING_VAL ) )
         {
            if ( * ( pReportNode->pe ) == '-' )
            {
               strcpy ( pReportNode->pe , pRsInfo->pe ) ;
            }

            if ( * ( pReportNode->pe ) == 'H' )
            {
               pReportNode->value2 = pRsInfo->fs ;
            }
            else
            {
               pReportNode->value2 = pRsInfo->fq ;
            }
          
         }

         pReportNode = ( ReportList * ) ListNext ( & pReportNode->node ) ;
 
      }

      return ;
   }

   /* Test if the option to draw the current value over the derived stage
      or flow has been selected. */
   if ( ( valuetype == TYPE_VALUE ) &&
        ( derive_stage_flow == 1 ) )
   {

      /* Only enter this logic if the PE is "HG", "QR" or "PRIMARY". 
         We already know that the PE is either "H*" or "Q*"
         or "PRIMARY", so the code only needs to check on the second
         character of the PE to verify that it is a "G" for "HG" or an
         "R" for "QR" or "PRIMARY". */

      if ( ( * ( pc_options->selectedAdHocElementString ) == 'H' ) &&
           ( * ( pc_options->selectedAdHocElementString + 1 ) == 'G' ) )
      {
         pe_type = HG ;
      }
      else if ( ( * ( pc_options->selectedAdHocElementString ) == 'Q' ) &&
                ( * ( pc_options->selectedAdHocElementString + 1 ) == 'R' ) )
      {
         pe_type = QR ;
      }
      else if ( ( * ( pc_options->selectedAdHocElementString ) == 'P' ) )
      {
         pe_type = PRIMARY ;
      }
      else
      {
         return ;
      }
         
      /* Loop over the reports in the ReportList Linked List. */
      pReportNode = ( ReportList * ) ListFirst ( & reportHead->list ) ;

      while ( pReportNode != NULL )
      {
         
         /*For a PE of HG:
         If there is a corresponding rating curve then interpolate the flow
         and set VALUE2 to it.
         Otherwise, the VALUE2 is set to missing.
         exit */

         switch ( pe_type ) 
         {
            case HG :
               
               /* Check to determine if there is a rating curve.
                  If there is, then a corresponding flow can
                  be derived. */
               if ( pReportNode->value != MISSING_VAL )
               {
                  flow = stage2discharge_buff ( pReportNode->lid ,
                                                pReportNode->value ) ;

                  if ( flow != RATING_CONVERT_FAILED )
                  {
                     pReportNode->value2 = flow ;
                  }
                     
               }

               break ;

            /* For a PE of QR:
               Loop over each report in the report list.
               If there is a corresponding rating curve, then interpolate
               the stage and set VALUE1 to it. 
               Otherwise, set VALUE2 to missing.
               exit */

            case QR :

               /* Check to determine if there is a rating curve.
                  If there is, then it a corresponding stage can
                  be derived. */
               if ( pReportNode->value != MISSING_VAL )
               {
                  stage = discharge2stage_buff ( pReportNode->lid ,
                                                 pReportNode->value ) ;

                  if ( stage != RATING_CONVERT_FAILED )
                  {
                     pReportNode->value2 = stage ;
                  }
                     
               }

               break ;

            case PRIMARY :

               if ( ( * ( pReportNode->pe ) == 'H' ) &&
                    ( * ( pReportNode->pe + 1 ) == 'G' ) )
               {
                  /* Check to determine if there is a rating curve.
                     If there is, then it a corresponding flow can
                     be derived. */
                  if ( pReportNode->value != MISSING_VAL )
                  {
                     flow = stage2discharge_buff ( pReportNode->lid ,
                                                   pReportNode->value ) ;

                     if ( flow != RATING_CONVERT_FAILED )
                     {
                        pReportNode->value2 = flow ;
                     }
                     
                  }

               }
               else if ( ( * ( pReportNode->pe ) == 'Q' ) &&
                         ( * ( pReportNode->pe + 1 ) == 'R' ) )
               {
                  /* Check to determine if there is a rating curve.
                     If there is, then it a corresponding stage can
                     be derived. */
                  if ( pReportNode->value != MISSING_VAL )
                  {
                     stage = discharge2stage ( pReportNode->lid ,
                                              pReportNode->value ) ;

                     if ( stage != RATING_CONVERT_FAILED )
                     {
                        pReportNode->value2 = stage ;
                     }
                     
                  }

               }
   
               break ;
         
         }

         pReportNode = ( ReportList * ) ListNext ( & pReportNode->node ) ;
 
      }

   }

   /* If the value type is "departure", then find the floodlevel from the
      riverstat table and subtract it from the primary value stored in each
      node of the linked list of ReportList structures. */
   if ( ( valuetype == TYPE_DEPART ) &&
        ( fldlevel == 0 ) )
   {
      /* Loop over each report in the report list and
         call get_rs_info to retrieve the flood level if
         it is available. */
      pReportNode = ( ReportList * ) ListFirst ( & reportHead->list ) ;

      while ( pReportNode != NULL )
      {
         pRsInfo = get_rs_info ( pReportNode->lid ) ;

         if ( pRsInfo != NULL ) 
         {
            if ( * ( pReportNode->pe ) == '-' )
            {
               strcpy ( pReportNode->pe , pRsInfo->pe ) ;
            }

            if ( * ( pReportNode->pe ) == 'H' )
            {
               if ( pRsInfo->fs != MISSING_VAL )
               {
                  if ( pReportNode->value != MISSING_VAL )
                  {
                     pReportNode->value -= pRsInfo->fs ;
                  }
               }
               else
               {
                  pReportNode->value = MISSING_VAL ;
               }
            }
            else
            {
               if ( pRsInfo->fq != MISSING_VAL )
               {
                  if ( pReportNode->value != MISSING_VAL )
                  {
                     pReportNode->value -= pRsInfo->fq ;
                  }
               }
               else
               {
                  pReportNode->value = MISSING_VAL ;
               }
            }
         }
         else
         {
            pReportNode->value = MISSING_VAL ;
         }

         pReportNode = ( ReportList * ) ListNext ( & pReportNode->node ) ;
      }

      return ;
   }

   /* If the value type is "departure" and the user wishes
      to display both the departure and the floodlevel values, then 
      find the floodlevel from the riverstat table and subtract it from 
      the primary value stored in each node of the linked list of 
      ReportList structures. */
   if ( ( valuetype == TYPE_DEPART ) &&
        ( fldlevel == 1 ) )
   {
      /* Loop over each report in the report list and
         call get_rs_info to retrieve the flood level if
         it is available. */
      pReportNode = ( ReportList * ) ListFirst ( & reportHead->list ) ;

      while ( pReportNode != NULL )
      {
         pRsInfo = get_rs_info ( pReportNode->lid ) ;

         if ( pRsInfo != NULL )
         {
            if ( * ( pReportNode->pe ) == '-' )
            {
               strcpy ( pReportNode->pe , pRsInfo->pe ) ;
            }

            if ( * ( pReportNode->pe ) == 'H' )
            {
               if ( pRsInfo->fs != MISSING_VAL )
               {
                  if ( pReportNode->value != MISSING_VAL )
                  {
                     pReportNode->value -= pRsInfo->fs ;
                  }

                  pReportNode->value2 = pRsInfo->fs ;
               }
               else
               {
                  pReportNode->value = MISSING_VAL ;
               }
            }
            else
            {
               if ( pRsInfo->fq != MISSING_VAL )
               {
                  if ( pReportNode->value != MISSING_VAL )
                  {
                     pReportNode->value -= pRsInfo->fq ;
                  }

                  pReportNode->value2 = pRsInfo->fq ;
               }
               else
               {
                  pReportNode->value = MISSING_VAL ;
               }
            }
         }
         else
         {
            pReportNode->value = MISSING_VAL ;
         }

         pReportNode = ( ReportList * ) ListNext ( & pReportNode->node ) ;
      }
   }
}

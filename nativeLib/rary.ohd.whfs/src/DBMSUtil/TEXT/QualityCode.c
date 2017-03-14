/************************************************************
   File: QualityCode.c
   
   Purpose:
   Contains functions for the management and use of the
   32-bit quality control code used in the IHFS database.   
   
   Last Updated: 05-05-2003 (Gautam Sood)
                 - Set BIT17 to the Outlier bit.
                 - Added appropriate code to build_qc_descr, 
                   set_qcbit, set_qccode, check_qccode for the 
                   Outlier bit.  
                 - Added the function set_qcnotquest_summary to
                   set the notquest_qc bit.
                 
                 05-19-2003 (Gautam Sood)
                 - Modified the set_qcnotquest_summary function.
                 - Added QC_ROC_PASSED clause to set_qccode.

                 09-15-2003 (Gautam Sood)
                 - Added 2 new bits (Bit 16 = SCC_QC (Spatial Consistency Check) )
                                    (Bit 15 = MSC_QC (Multi Sensor Check) )
                 - Modified the set_qccode, set_qcnotquest_summary,
                   build_qc_descr, and check_qccode functions to 
                   include the 2 new bits. 

   ************************************************************/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "QualityCode.h"


/************************************************************
   
   build_qc_where()
   
   Builds a string for the SQL where clause pertaining
   to the quality code, using the specified request.
   
   **********************************************************/

void build_qc_where(int 	request,
                    char 	*qc_where_subclause)   
{  
   
   /* The data value is valid as per all checks. */
   
   if (request == QC_PASSED)
      sprintf(qc_where_subclause, "%s%d", 
	      "quality_code >= ", GOOD_QUESTIONABLE_THRESHOLD);
   
   
   /* The gross range certainty check did not pass. The data is not usable. */
   
   else if (request == QC_NOT_PASSED)
      sprintf(qc_where_subclause, "%s%d", "quality_code < ", 
	      GOOD_QUESTIONABLE_THRESHOLD);
   
   
   /* The data value is invalid because it failed at least one
      certainty check */
   
   else if (request == QC_FAILED)
      sprintf(qc_where_subclause, "%s%d", "quality_code < ", 
	      QUESTIONABLE_BAD_THRESHOLD);
   
   
   /* The data value is questionable.  It passed all certainty checks  
      but failed at least one quality check. */
   
   else if (request == QC_QUESTIONABLE)
      sprintf(qc_where_subclause, "quality_code BETWEEN %d AND %d",
	      QUESTIONABLE_BAD_THRESHOLD, 
	      (GOOD_QUESTIONABLE_THRESHOLD - 1) ); 
   
   
   /* The data is deemed full valid or is questionable, but it is 
      not deemed bad with certainty. */
   
   else if (request == QC_NOT_FAILED)
      sprintf(qc_where_subclause, "%s%d", "quality_code >= ",
	      QUESTIONABLE_BAD_THRESHOLD);
   
   
   /* Processing of an invalid request submitted by the calling function. */
   
   else 
   {
      fprintf(stderr, "Invalid request made in build_qc_where() function.\n");
      sprintf(qc_where_subclause, "%s", "INVALID REQUEST"); 
   }
   
   return;
   
}


/**************************************************************
   
   build_qc_descr()
   
   Form a phrase briefly describing the quality of a
   value based on its quality code value.
   
   **************************************************************/

void build_qc_descr(long 	quality_code,
                    char 	*qc_descr)
{
   int passed_certainty, passed_questionable;
   int num_failed = 0;
   
   
   /* for convenience, determine some bit settings here
      for use later */
   
   passed_certainty    = check_qcbit(CERTAINTY_QC, quality_code);
   passed_questionable = check_qcbit(NOTQUEST_QC, quality_code);
   
   
   /* first check whether the quality is fully successfull */
   
   if (passed_certainty)
   {
      if (passed_questionable)
	 strcpy(qc_descr, "Passed All Tests");
      
      
      /* now check whether the data is considered questionable */
      
      else
      {
	 strcpy(qc_descr, "Questionable: Failed");
	 
	 if (check_qcbit(EXTERN_NOTQUEST_QC,  quality_code) == FALSE_QC)
	 {
	    strcat(qc_descr, " External Test");
	    num_failed++; 
	 }

	 if (check_qcbit(MANUAL_QC, quality_code) == FALSE_QC)
	 {
	 if (num_failed > 0) strcat(qc_descr, ",");
	 strcat(qc_descr, " Manual Review");   
	 num_failed++; 
	 }
      
	 if (check_qcbit(REASONRANGE_QC, quality_code) == FALSE_QC)
	 {
	    if (num_failed > 0) strcat(qc_descr, ",");
	    strcat(qc_descr, " Reasonable Range Test");
	    num_failed++; 
	 }
	 
	 if (check_qcbit(ROC_QC, quality_code) == FALSE_QC)
	 {
	    if (num_failed > 0) strcat(qc_descr, ",");
	    strcat(qc_descr, " Rate-of-Change Test");
	    num_failed++; 
	 }

         if (check_qcbit(OUTLIER_QC, quality_code) == FALSE_QC)
         {
            if (num_failed > 0) strcat(qc_descr, ",");
            strcat(qc_descr, " Outlier Test");
            num_failed++;
         }
 
         if (check_qcbit(SCC_QC, quality_code) == FALSE_QC)
         {
            if (num_failed > 0) strcat(qc_descr, ",");
            strcat(qc_descr, " Spatial Consistency Test");
            num_failed++;
         }
 
         if (check_qcbit(MSC_QC, quality_code) == FALSE_QC)
         {
            if (num_failed > 0) strcat(qc_descr, ",");
            strcat(qc_descr, " Multi Sensor Test");
            num_failed++;
         }
      }
   }
   
   
   /* check if the data is bad */
   
   else
   {
      strcpy(qc_descr, "Bad: Failed");
      
      if (check_qcbit(EXTERN_QC,  quality_code) == FALSE_QC)
      {
	 strcat(qc_descr, " External Test");
	 num_failed++; 
      }
      
      if (check_qcbit(MANUAL_QC, quality_code) == FALSE_QC)
      {
	 if (num_failed > 0) strcat(qc_descr, ",");
	 strcat(qc_descr, " Manual Review");   
	 num_failed++; 
      }
      
      if (check_qcbit(GROSSRANGE_QC, quality_code) == FALSE_QC)
      {
	 if (num_failed > 0) strcat(qc_descr, ",");
	 strcat(qc_descr, " Gross Range Test"); 
	 num_failed++; 
      }
   }  
   
   
   return;
   
}


/**************************************************************
   
   build_qc_symbol()
   
   Return a symbol summarizing the quality of a
   value based on its quality code value.
   
   **************************************************************/

void build_qc_symbol(long 	quality_code,
                     char 	*qc_symbol)
{   
   
   if (check_qccode(QC_PASSED,            quality_code) == TRUE_QC)
      strcpy(qc_symbol, "G");   
   
   else if (check_qccode(QC_QUESTIONABLE, quality_code) == TRUE_QC)
      strcpy(qc_symbol, "Q");
   
   else if (check_qccode(QC_FAILED,       quality_code) == TRUE_QC)
      strcpy(qc_symbol, "B");
   
   else
      strcpy(qc_symbol, "?");
   
   
   return;
   
}


/**************************************************************
   
   check_qccode()
   
   Checks whether the quality control code matches some 
   user specified condition
   
   ************************************************************/

int check_qccode(int	bit_grp_descr,
                 long	quality_code)
{  
   
   int returnValue; 
   
   /* Processing of bit patterns representing bit patterns */
   
   switch (bit_grp_descr)
   {         
      case QC_DEFAULT:
	 
	 if (quality_code == DEFAULT_QC_VALUE)
	    returnValue = TRUE_QC;
	 
	 else 
	    returnValue = FALSE_QC; 
	 
	 break;
	 
	 
      case QC_PASSED:
	 
	 if (quality_code > GOOD_QUESTIONABLE_THRESHOLD)
	    returnValue = TRUE_QC; 
	 
	 else 
	    returnValue = FALSE_QC; 
	 
	 break;
	 
	 
      case QC_QUESTIONABLE:
	 
	 if ((quality_code >= QUESTIONABLE_BAD_THRESHOLD) && 
	     (quality_code <  GOOD_QUESTIONABLE_THRESHOLD))
	    returnValue = TRUE_QC; 
	 
	 else 
	    returnValue = FALSE_QC;
	 
	 break;
	 
	 
      case QC_ROC_PASSED:
      
         returnValue = check_qcbit(ROC_QC, quality_code);
	 
	 break;
 
      case QC_OUTLIER_PASSED:
  
         returnValue = check_qcbit(OUTLIER_QC, quality_code);
    
         break;
	 
      case QC_SCC_PASSED:
         
         returnValue = check_qcbit(SCC_QC, quality_code);
         
         break;

      case QC_MSC_PASSED:
 
         returnValue = check_qcbit(MSC_QC, quality_code);
 
         break;
	 
      case QC_FAILED:
	 
	 if (quality_code < QUESTIONABLE_BAD_THRESHOLD)
	    returnValue = TRUE_QC; 
	 
	 else
	    returnValue = FALSE_QC; 
	 
	 break;
	 
	 
      case QC_NOT_FAILED:
	 
	 if (quality_code >= QUESTIONABLE_BAD_THRESHOLD)
	    returnValue = TRUE_QC; 
	 
	 else 
	    returnValue = FALSE_QC;
	 
	 break;
	 
	 
      case QC_NOT_PASSED:
	 
	 if ((quality_code <= GOOD_QUESTIONABLE_THRESHOLD))
	    returnValue = TRUE_QC; 
	 
	 else 
	    returnValue = FALSE_QC;
	 
	 break;
	 
	 
      default:  
	 
	 fprintf(stderr, "%s",
		 "Invalid request made in check_qc_code() function.\n");
	 returnValue = INVALID_QC_REQUEST; 
	 
	 break; 	 
   } 
   
   
   return(returnValue); 
   
} 


/****************************************************************
   set_qccode
   
   Sets the quality control code  
   **************************************************************/

int set_qccode(int	bit_grp_descr,
               long	*quality_code)
   
{ 
   
   int returnValue = VALID_QC_REQUEST;
   
   switch (bit_grp_descr)
   {         
      
      case QC_DEFAULT:
	 
	 /* Set the quality control code to the default value. */
	 
	 *quality_code = DEFAULT_QC_VALUE;
	 break;
	 
	 
      case QC_GROSSRANGE_FAILED:
	 
	 /* Set the quality control code so that the specific bit
	    for gross range check indicates it failed, and therefore
	    set the summary certainty bit to indicated the failure. */
	 
	 set_qcbit(GROSSRANGE_QC, 0, quality_code);
	 set_qcbit(CERTAINTY_QC,  0, quality_code);
	 break; 
	 
	 
      case QC_REASONRANGE_FAILED:
	 
	 /* Set the quality control code so that the specific bit
	    for reasonableness range check indicates failure, and therefore
	    set the summary questionanle bit to indicate the failure. */
	 
	 set_qcbit(REASONRANGE_QC, 0, quality_code);
	 set_qcbit(NOTQUEST_QC,    0, quality_code);
	 break; 
	 
	 
      case QC_ROC_FAILED:
	 
	 /* Set the quality control code so that the specific bit
	    for roc check indicates it failed, and therefore
	    set the summary questionable bit to indicate the failure. */
	 
	 set_qcbit(ROC_QC,       0, quality_code);
	 set_qcbit(NOTQUEST_QC,  0, quality_code);
	 break; 

      case QC_ROC_PASSED:

         /* Set the quality control code so that the specific bit
            for roc check indicates that it passed, and therefore
            set the summary questionable bit to indicate the passing. */

         set_qcbit(ROC_QC,  1, quality_code);
         set_qcnotquest_summary(quality_code);
         break;

	 
      case QC_OUTLIER_FAILED:
 
         /* Set the quality control code so that the specific bit
            for outlier check indicates it failed, and therefore
            set the summary questionable bit to indicate the failure. */
    
         set_qcbit(OUTLIER_QC,   0, quality_code);
         set_qcbit(NOTQUEST_QC,  0, quality_code);
         break; 	 
    
      case QC_OUTLIER_PASSED:
   
         /* Set the quality control code so that the specific bit
            for outlier check indicates that it passed, and therefore
            set the summary questionable bit to indicate the passing. */
 
         set_qcbit(OUTLIER_QC,  1, quality_code);
         set_qcnotquest_summary(quality_code);
         break;

      case QC_SCC_FAILED:
         
         /* Set the quality control code so that the specific bit
            for spatial consistency check indicates that it failed, and
            therefore set the summary questionable bit to indicate the failure. */
        
         set_qcbit(SCC_QC, 0, quality_code);
         set_qcbit(NOTQUEST_QC,   0, quality_code);
         break;

      case QC_SCC_PASSED:

         /* Set the quality control code so that the specific bit
            for spatial consistency check indicates that it passed, and therefore
            set the summary questionable bit to indicate the passing. */
  
         set_qcbit(SCC_QC, 1, quality_code);
         set_qcnotquest_summary(quality_code);
         break;

      case QC_MSC_FAILED:

         /* Set the quality control code so that the specific bit
            for multi sensor check indicates that it failed, and therefore
            set the summary questionable bit to indicate the failure. */
 
         set_qcbit(MSC_QC, 0, quality_code);
         set_qcbit(NOTQUEST_QC, 0, quality_code);
         break;

      case QC_MSC_PASSED:

         /* Set the quality control code so that the specific bit
            for multi sensor check indicates that it passed, and therefore
            set the summary questionable bit to indicate the passing. */

         set_qcbit(MSC_QC, 1, quality_code);
         set_qcnotquest_summary(quality_code);
         break;
          
      case QC_EXTERN_FAILED:
	 
	 /* Set the quality control code so that the specific bit
	    for external check indicates it failed, and therefore
	    set the summary certainty bit to indicate the failure. */
	 
	 set_qcbit(EXTERN_QC,    0, quality_code);
	 set_qcbit(CERTAINTY_QC, 0, quality_code);
	 break; 
	 	 
	 
      case QC_EXTERN_QUEST:
	 
	 /* Set the quality control code so that the specific bit
	    for external check indicates it is questioable, and therefore
	    set the summary questionable bit to indicate the failure. */
	 
	 set_qcbit(EXTERN_NOTQUEST_QC,    0, quality_code);
	 set_qcbit(NOTQUEST_QC,           0, quality_code);
	 break; 
	 
	 
      case QC_MANUAL_PASSED:
	 
	 /* When manually updating a value, assume that the value
	    is set to a good value, in which case we should set it 
	    to the default. */
	 
	 set_qccode(QC_DEFAULT,       quality_code);
	 break; 
	 
	 
      case QC_MANUAL_QUEST:
	 
	 /* Set the quality control code so that the specific bit for
	    manual check indicates it is questionable, and therefore
	    set the summary certainty bit to indicate the questionable state. */
	 
	 set_qcbit(MANUAL_QC,    0, quality_code);
	 set_qcbit(NOTQUEST_QC,  0, quality_code);
	 set_qcbit(CERTAINTY_QC, 1, quality_code);
	 break; 
	 
	 
      case QC_MANUAL_FAILED:
	 
	 /* Set the quality control code so that the specific bit
	    for manual check indicates it failed, and therefore
	    set the summary certainty bit to indicate the failure. */
	 
	 set_qcbit(MANUAL_QC,    0, quality_code);
	 set_qcbit(CERTAINTY_QC, 0, quality_code);
	 break; 
	 
	 
      case QC_MANUAL_NEW:
	 
	 /* When manually inserting a value, assume that the value
	    is set to a good value, in which case we should set it 
	    to the default. */
	 
	 set_qccode(QC_DEFAULT,       quality_code);
	 break;
	  
	 
      default:  
	 
	 /* An invalid bit value was processed.
	    The quality control code remains unchanged. */
	 
	 fprintf(stderr, "%s",
		 "Invalid request made in set_qccode() function.\n");
	 returnValue = INVALID_QC_REQUEST; 
	 break; 
	 
   }  
   
   
   return(returnValue); 
   
} 


/************************************************************
   
   check_qcbit()
   
   Check if the specified bit of the quality code is set.
   
   **********************************************************/

int check_qcbit(int 	bit_position, 
                long	quality_code)   
{  
   
   /* Variable used to set a specific bit in bit string to 1;
      initialized as 0000000000000000 0000000000000001 */
   
   unsigned int mask = 1;
   unsigned int bitwise_AND_result; 
   
   
   /* The mask is employed to set a specific bit to the value of 1 in a 
      32-bit string while the value of the other bits. The mask is leftwardly 
      shifted to the bit position of the bit being checked. */
   
   mask = mask << bit_position; 
   
   
   /* To check the setting of the specified bit, the mask variable is 
      compared to the bit string of quality control variable.  The end 
      result yields an integer value.*/
   
   bitwise_AND_result = quality_code & mask; 
   
   
   /* The following conditional block checks the setting of the specified bit.  
      bit.  The check determined by the return value of the bitwise AND
      operation. If the return value is greater than zero (0), the specified
      bit is set. */
   
   if (bitwise_AND_result > 0)
      return(TRUE_QC);   
   
   else
      return(FALSE_QC); 
   
}


/************************************************************
   
   set_qcbit()
   
   Sets the specified bit of the quality code to the 
   specified value.
   
   **********************************************************/

int set_qcbit(int 	bit_position, 
              int 	setting, 
              long 	*quality_code)    
{   
   
   /* Variable used to set a specific bit in bit string to 1;
      initialized as 0000000000000000 0000000000000001 */
   
   unsigned int mask = 1;
   int bitwise_inclusive_OR_result;                                      				      
   int bitwise_AND_result; 				       
   int status;
   
   
   /* Ensure that a valid bit position is requested. */
   
   if (bit_position < SIGN_QC)
   {
      status = VALID_QC_REQUEST;
      
      
      /* if setting the bit ON */
      
      if (setting == 1)
      {
	 /* The mask is employed to set a specific bit to the value of 1 in a
	    32-bit string while hiding the value of the other bits.  The mask
	    is leftwardly shifted to the bit position of the bit
	    being referenced. */
	 
	 mask = mask << bit_position; 
	 
	 
	 /* The bitwise inclusive OR operation is used to set the specified
	    bit.  Upon completion, the bit is written to quality_code memory
	    location. */
	 
	 bitwise_inclusive_OR_result = *quality_code | mask;  
	 
	 *quality_code = bitwise_inclusive_OR_result;
      }
      
      
      /* if setting the bit OFF. first build a mask that has all
	 ones except for the bit in question, then AND the mask with
	 the existing value to turn off the single bit. */
      
      else
      {	
	 mask = ALL_ONES ^ (mask << bit_position);
	 
	 bitwise_AND_result = *quality_code & mask;  
	 
	 *quality_code = bitwise_AND_result;
      }
   }
   
   else
   {
      fprintf(stderr, "%s", "Invalid request made in set_qcbit() function.\n");
      status = INVALID_QC_REQUEST;
   }
   
   
   return(status); 
   
}  


/************************************************************

   set_qcnotquest_summary()

   Set's the notquest_qc bit according to whether it passed the
   following tests:
     External QC Not-QUESTIONABLE Indicator (BIT20)
     Reasonable Range Test                  (BIT19)
     Rate-of-change Test                    (BIT18)
     Outlier Test                           (BIT17)   
     Spatial Consistency Test               (BIT16)
     Multi Sensor Test                      (BIT15)

   **********************************************************/


void   set_qcnotquest_summary(long *quality_code)
{
   
   if ( (check_qcbit(EXTERN_NOTQUEST_QC, *quality_code) == FALSE_QC) || 
                (check_qcbit(REASONRANGE_QC, *quality_code) == FALSE_QC) ||
                (check_qcbit(ROC_QC, *quality_code) == FALSE_QC) ||
                (check_qcbit(OUTLIER_QC, *quality_code) == FALSE_QC) ||
                (check_qcbit(SCC_QC, *quality_code) == FALSE_QC) ||
                (check_qcbit(MSC_QC, *quality_code) == FALSE_QC) )

      set_qcbit(NOTQUEST_QC,  0, quality_code);
   else
      set_qcbit(NOTQUEST_QC, 1, quality_code);
   return; 
}

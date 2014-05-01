#ifndef QUALITY_CODE_H
#define QUALITY_CODE_H


/* lengths for string values returned by functions.  these lengths
   can be used by calling functions to ensure that the returned
   string will fit into the variable sized by this constant */

#define MAXLEN_QCWHERE   40
#define MAXLEN_QCDESCR  120  
#define MAXLEN_QCSYMBOL   2


/* function prototypes of functions intended for public use */
 
void build_qc_where(int  request,
                    char *qc_where_subclause);
 
void build_qc_descr(long quality_code,
                    char *qc_descr);

void build_qc_symbol(long quality_code,
                     char *qc_sumbol);


/* for check functions, the return arg is a TRUE/FALSE indicator;
   for set functions, the return arg is an INVALID/VALID REQUEST indicator. */

int check_qccode(int 	bit_grp_descr,
                 long 	quality_code); 

int set_qccode(int 	bit_grp_descr,
               long 	*quality_code); 


/***************************************************************** 
   definition of symbolic constants used as bit group descriptors 
 *****************************************************************/

/* these requests are for checking a value.
   they are valid for building a where clause or for checking the qc code */

#define   QC_PASSED                           101
#define   QC_QUESTIONABLE                     102
#define   QC_FAILED                           103

#define   QC_NOT_PASSED                       104
#define   QC_NOT_FAILED                       105


/* this request is for setting the qc_code to an inital value */
 
#define   QC_DEFAULT                          110


/* these requests are for setting the qc code, which is done
   by setting a specific bit and its associated summary bit */

#define   QC_GROSSRANGE_FAILED                120

#define   QC_MANUAL_PASSED                    121
#define   QC_MANUAL_QUEST                     122
#define   QC_MANUAL_FAILED                    123

#define   QC_MANUAL_NEW                       124

#define   QC_EXTERN_QUEST                     125
#define   QC_EXTERN_FAILED                    126

#define   QC_REASONRANGE_FAILED               127

#define   QC_ROC_PASSED                       128 
#define   QC_ROC_FAILED                       129

#define   QC_OUTLIER_PASSED                   130
#define   QC_OUTLIER_FAILED                   131 

#define   QC_SCC_PASSED                       132
#define   QC_SCC_FAILED                       133

#define   QC_MSC_PASSED                       134
#define   QC_MSC_FAILED                       135

/************************************************************
  Definition of symbolic constants representing status codes  
 ************************************************************/
 
#define   FALSE_QC                              0
#define   TRUE_QC                               1

#define   INVALID_QC_REQUEST                   -1
#define   VALID_QC_REQUEST                      1


/********************************************************************
  The remainder of this file contains definitions for internal use.
  Definition of symbolic constants used as bit descriptors  
 *******************************************************************/
 
#define   SIGN_QC                 31 
#define   CERTAINTY_QC            30 
#define   NOTQUEST_QC             29
#define   TEST_RUN_QC             28
#define   BIT27                   27
#define   BIT26                   26
#define   BIT25                   25
#define   BIT24                   24
#define   EXTERN_QC               23
#define   MANUAL_QC               22
#define   GROSSRANGE_QC           21
#define   EXTERN_NOTQUEST_QC      20
#define   REASONRANGE_QC          19
#define   ROC_QC	          18
#define   OUTLIER_QC              17
#define   SCC_QC                  16
#define   MSC_QC                  15
#define   BIT14                   14
#define   BIT13                   13
#define   BIT12                   12
#define   BIT11                   11
#define   BIT10                   10
#define   BIT9                     9
#define   BIT8                     8
#define   BIT7                     7
#define   BIT6                     6
#define   BIT5                     5
#define   BIT4                     4
#define   BIT3                     3
#define   BIT2                     2
#define   BIT1                     1
#define   BIT0                     0


/*********************************************************************
  Bit pattern 0110111111111111 1111111111111111 yields 1,879,048,191.    
 *********************************************************************/

#define   DEFAULT_QC_VALUE             1879048191 

/*********************************************************************
  Bit pattern 0110000000000000 0000000000000000 yields 1,610,612,736.   
 *********************************************************************/

#define   GOOD_QUESTIONABLE_THRESHOLD  1610612736 
                                                 
/*********************************************************************
  Bit pattern 0100000000000000 0000000000000000 yields 1,073,741,824.   
 *********************************************************************/

#define   QUESTIONABLE_BAD_THRESHOLD   1073741824
 
/*********************************************************************
  Bit pattern 0111111111111111 1111111111111111 yields 2,147,483,647.
  = 2**31 - 1   
 *********************************************************************/

#define   ALL_ONES 2147483647



/* function prototypes for functions for internal use by these qc 
   utility functions. */

int check_qcbit(int 	bit_position,
                long 	quality_code); 

int set_qcbit(int 	bit_position,
              int 	setting,
	      long 	*quality_code); 

void set_qcnotquest_summary(long *quality_code);


#endif

/*********************************************************************
   check_conditions.c
   
   check_conditions()  
   load_condition_data()
   
   MODIFICATION HISTORY   DATE     PROGRAMMER     DESCRIPTION
   check_conditions()    05/2004  Jingtao Deng   Add vtecinfo as argument
   load_condition_data() 05/2004  Jingtao Deng   Add vtecinfo as argument
   *******************************************************************/

#include <string.h>            /* library string functions */
#include <stdio.h>             /* standard io library functions */

#include "check_conditions.h"


/*********************************************************************
   check_conditions()
   
   PURPOSE
   Loads the data into the conditional expression and evaluates the 
   condition to determine whether it is true or false
      
   NOTES
   
   *******************************************************************/

void check_conditions(const	int			id_index,
		          	fp_struct		*fp,
		          	grp_struct		*grp,
		                int                     numcnty,
		                county_struct           *cnty,
		                misc_struct		*misc,
		                vtecinfo_struct         *vtecinfo,
		                pcc_struct		*pcc,
		      const	int			product_section,
		     		template_info_struct	*template_info)
   
{
   int stacknum;
   
   /* loop on the number of stacks for this template */
   
   for (stacknum = 0; stacknum < template_info->num_conditions; ++stacknum)
   {      
      /* initialize the include flag */
      
      template_info->include_phrase[stacknum] = TRUE;	 
      
      
      /* load the data into the variables contained in the conditional
	 expressions of the template */
      
      load_condition_data(id_index, fp, grp, numcnty, cnty, misc, vtecinfo, pcc,
			  stacknum, product_section, template_info);
      
      
      /* evaluate the conditional expression specified associated with
	 the phrases in the template assuming no data missing;
         if data was missing in condition, do not evaluate the condition
	 and issue message */
      
      if (template_info->include_phrase[stacknum] == TRUE)
	 eval_condit(stacknum, template_info);            
   }
   
   return;
}


/*********************************************************************
   load_condition_data()
   
   PURPOSE
   Loads the data into the variables specified in the 
   stack used for the conditional expression.
   
   NOTES
   Data are always loaded, even if the value is missing.
   
   *******************************************************************/

void load_condition_data(const int			id_index,
			       fp_struct		*fp,
			       grp_struct		*grp,
			       int                      numcnty,
			       county_struct            *cnty,
			       misc_struct		*misc,
			       vtecinfo_struct          *vtecinfo,
			       pcc_struct		*pcc,
			 const int			stacknum,
			 const int			product_section,
			       template_info_struct	*template_info)
{
   extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];  
   
   template_item_struct *template_item;
   int 		index, varindex;
   int 		curtype;
   values 	rawvalue;
   char 	dummystring[1];
   char		dqcode[SHEF_QC_LEN + 1];
   char		locid[LOC_ID_LEN + 1];
   int          filter_qc = 1;
   
   /* get the stack address */
   
   template_item = template_info->bottom_condition_stack[stacknum];
   
   
   /* loop on the number of items in the stack */
   
   for (index = 0; index < template_info->stacksize[stacknum]; index++)
   {      
      /* get the type from the stack */
      
      curtype =  template_item[index].type;
      varindex = template_item[index].varinfo->varindex;
      
      /* if the stack item is a variable that needs to have a value 
	 assigned, then load the value */
      
      if (isvar(curtype) == TRUE)
      {
	 /* check that the variable is allowed to be in the product
	    section being considered */
	 
	 if (check_var_access(TEMPLATE_VARIABLES_TABLE[varindex].accesslist,
			      product_section) != TRUE)
	 {
	    template_info->include_phrase[stacknum] = FALSE;
	    log_msg(VAR_NOT_ALLOWED, TEMPLATE_VARIABLES_TABLE[varindex].name);
	    break;
	 }
	
	 
	 /* get the value of the variable; 
	    no data should ever be returned in the dummy string 
	    as long strings are not allowed on the stack;
	    don't need to pass in the stage time arguments as the variables 
	    that need them are not also not permitted on the stac.
	    the shef qualifier code is returned but not used */
	 
	 if (id_index != MISSINGVAL)
	    strcpy(locid, fp[id_index].id);
	 else
	    memset(locid, 0, LOC_ID_LEN + 1);
	 
	 /*judge the variable is PE variable or standard variable*/
	 
	 if (strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name,
		       "<PEVal>") == 0 ||
	     strcmp(TEMPLATE_VARIABLES_TABLE[varindex].name,
		       "<PETime>") == 0)
	 {	       
	     load_pe_value(filter_qc, locid, *template_item[index].varinfo, 
		           &rawvalue, dqcode, dummystring);
	 }
	 else
	 {
	     load_variable_value(fp, grp, numcnty, cnty, misc, vtecinfo, pcc, 
			     template_item[index].varinfo, id_index, 
			     locid, NULL, 0, &rawvalue, dqcode, dummystring);
	 }
	 
	 /* now load the returned value into the item stack and
	    set the type appropriately */
	 
	 if (curtype == VAR_INT)
	 {
	    template_item[index].type    = RPF_INT;
	    template_item[index].value.i = rawvalue.i;	    	    
	 }
	 
	 else if (curtype == VAR_FLT)
	 {
	    template_item[index].type    = RPF_FLT;
	    template_item[index].value.f = rawvalue.f;
	 }
	 
	 else if (curtype == VAR_STR) 
	 {
	    template_item[index].type    = RPF_STR;
	    strcpy(template_item[index].value.s, rawvalue.s);
	 }
	 
	 else if (curtype == VAR_TIM)
	 {
	    template_item[index].type    = RPF_TIM;
	    template_item[index].value.t = rawvalue.t;
	 }
	 
	 
	 /* currently, date variables are not even permitted in the
	    conditional expression but leave this in for the future */
	 
	 else if (curtype == VAR_DAT)
	 {
	    template_item[index].type = RPF_DAT;
	    strcpy(template_item[index].value.d, rawvalue.d);
	 }
      }
   }

   
   return;
}

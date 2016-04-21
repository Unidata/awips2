/*********************************************************************
   eval_condit.c
   
   eval_condit()
   evaluate_rel_expr()
   evaluate_log_expr()
   
   *******************************************************************/

#include <string.h>            /* library string functions */
#include <stdio.h>             /* standard io library functions */

#include "eval_condit.h"


/*********************************************************************
   eval_condit()
   
   PURPOSE
   Evaluates the condition represented in the stack to see if 
   the overall result is true or false.
   
   NOTES
   This function assumes that the stack contains valid entries in
   a valid order! It does not check the validity of the stack
   instructions; this is done in verify_condition_syntax().
   
   Note that all relational expressions must be enclosed in parens,
   while logical expressions can be but are not required to be in
   parentheses.
   
   The functions first processes all expressions contained in parenthesis,
   then it processes any remaining logical expressions.  Note that the
   stack is processed from bottom to top; i.e. from left to right in
   terms of the original expression.
     
   *******************************************************************/

void eval_condit(const	int			stacknum,
			template_info_struct	*template_info)   
{
   template_item_struct *template_item;
   int 		index, stacksize;
   int 		curtype, num_to_pop;
   int 		oper_type;
   values 	booleanresult;
   int 		leftparen_pos = MISSINGVAL;
   int 		rightparen_pos;
   int 		oper_pos;
   int 		boolean_pos;
   
   
   /* get the stack address */
   
   template_item = template_info->bottom_condition_stack[stacknum];
   
   
   /* check if condition need not be evaluated because the condition is
      is a single boolean which has been translated into a integer */
   
   if (template_info->stacksize[stacknum] == 1 && 
       template_item[0].type == RPF_INT)
   {
      if (template_item[0].value.i == TRUE)
	 template_info->include_phrase[stacknum] = TRUE;
      else
	 template_info->include_phrase[stacknum] = FALSE;
      return;
   }
   
   
   /* initialize the index to the stack position, and set a
      convenient local value for the stack */
   
   stacksize = template_info->stacksize[stacknum];
   index = 0;
   

   /*-------------------------------------------------------------------*/
   /* loop thru stack and search for all parenthetical expressions */
   
   do
   {
      /* get the type of the current slot */
      
      curtype =  template_item[index].type;
      
      
      /* if a left paren, note its position and increment the index */
      
      if (curtype == LEFT_PAREN)
      {
	 leftparen_pos = index;
	 index++;
      }
      
      
      /* if a right paren is found, then act upon the contents
	 of the expression inside the parentheses if it is a
	 relational expression */
      
      else if (curtype == RIGHT_PAREN)
      {
	 rightparen_pos = index;
	 
	 
	 /* if parens have only one item inside them - which had
	    better be a boolean value resulting from previous
	    relational and logical expression evaluations - then
	    simply strip off the extraneous parentheses */
	 
	 if (rightparen_pos - leftparen_pos == 2)
	 {
	    /* replace the left paren stack item with the boolean
	       result value that is located one slot away on the stack */
	    
	    boolean_pos = leftparen_pos + 1;
	  /*  expr_pushstack(template_item[boolean_pos].type,
			   template_item[boolean_pos].value,
			   template_item[boolean_pos].varinfo->varindex,
			   leftparen_pos, template_item);*/
			   
           expr_pushstack(template_item[boolean_pos].type,
			   template_item[boolean_pos].value,
			   *template_item[boolean_pos].varinfo,
			   leftparen_pos, template_item); 			   
	    
	    
	    /* pop the positions made obsolete and decrement the stack size;
	       note that the stacksize is adjusted here and not 
	       inside the popstack function */
	    
	    num_to_pop = 2;
	    expr_popstack(boolean_pos, num_to_pop, stacksize, template_item);
	    stacksize = stacksize - num_to_pop;
	    index = 0;	    
	 }
	 
	 
	 /* only process the expression if it has 5 items total;
	    (i.e. left paren, leftvalue, operator, rightvalue, right paren).
	    need this check to avoid processing a logical
	    expressions that is not a simply binary operation */ 
	 	 
	 else if (rightparen_pos - leftparen_pos == 4) 
	 {
	    /* find the operator type, which is assumed to be two 
	       slots away from the left parenthesis */
	    
	    oper_pos =  leftparen_pos + 2;
	    oper_type = template_item[oper_pos].type;
	    
	    if (isrelop(oper_type) == TRUE || isstrop(oper_type) == TRUE ||
		islogop(oper_type) == TRUE)
	    {	    
	       if (islogop(oper_type) == TRUE)
		  booleanresult.i = evaluate_log_expr(template_item, oper_pos);
	       else		  
		  booleanresult.i = evaluate_rel_expr(template_item, oper_pos);
	       
	       
	       /* replace the left paren stack item with the result value;
		  then pop the positions made obsolete; decrement the stack
		  size by the number popped; reset the index to
		  start at the beginning again; note that the number of
		  slots to pop is 4 as each relational expression is five
		  slots (i.e. left paren, leftvalue, operator, rightvalue,
		  right paren) and becomes one slot (boolean result) */
	       
	       /*expr_pushstack(RPF_BOOLEAN, booleanresult, 0,
			      leftparen_pos, template_item);*/
	       expr_pushstack(RPF_BOOLEAN,booleanresult,
	                      *template_item[oper_pos].varinfo,
			      leftparen_pos,template_item);		      
	       num_to_pop = 4;	       
	       expr_popstack(leftparen_pos + 1, num_to_pop, stacksize,
			     template_item);
	       stacksize = stacksize - num_to_pop;
	       index = 0;
	    }
	 }
	 
	 /* this is for the case where the item is a terminating
	    right paren but there are logical expressions with
	    no explicit parens */
	 
	 else
	    index++;
	 
      }  /* end of if right_paren block */
	
      
      /* if the item is not a parens, increment the index  */
      
      else
	 index++;           
   } while (index < stacksize && stacksize > 1);
   
   /*-------------------------------------------------------------------*/
   /* at this point, all that should remain of the stack is either 
      a single boolean value, or there will be the two surrounding parens,
      with one or more boolean values, and if more than one boolean value,
      then the logical operators. */
   
   if (stacksize > 1)
   {
      index = 0;
      do
      {
	 curtype =  template_item[index].type;
	 
	 if (islogop(curtype) == TRUE)
	 {	 
	    booleanresult.i = evaluate_log_expr(template_item, index);
	    
	    
	    /* replace the left boolean item with the result value; then pop
	       the positions made obsolete; decrement the stack size;
	       note that the number of slots to pop is 2 as each expression
	       is three slots (i.e. leftvalue, operator, rightvalue) and
	       becomes one slot (boolean result) */
	    
	    /*expr_pushstack(RPF_BOOLEAN, booleanresult, 0,
			   index - 1, template_item);*/
	    expr_pushstack(RPF_BOOLEAN,booleanresult,*template_item[0].varinfo,
	                   index - 1, template_item);
			    		   
	    num_to_pop = 2;	       
	    expr_popstack(index, num_to_pop, stacksize, template_item);
	    stacksize = stacksize - num_to_pop;
	    index = 0;
	 }
	 
	 /* if the item is not a parens, increment the index  */
	 
	 else
	    index++;           
      } while (index < stacksize && stacksize > 1);
      
      
      /* all that should remain in the stack are three items - left 
	 paren, boolean, and right paren. compress the stack as 
	 done earlier in this function */
      
      /*expr_pushstack(template_item[1].type,
		     template_item[1].value,
		     template_item[1].varinfo->varindex,
		     0, template_item);*/
    		     
      expr_pushstack(template_item[1].type,
		     template_item[1].value,
		     *template_item[1].varinfo,
		     0, template_item);
    		     
      num_to_pop = 2;
      expr_popstack(1, num_to_pop, stacksize, template_item);
      stacksize = stacksize - num_to_pop;
   }

   /*-------------------------------------------------------------------*/
   
   /* set the stack size to 1, since all that should remain is a boolean; 
      and set the include flag depending on the result of the evaluation */
   
   template_info->stacksize[stacknum] = 1;
   
   if (template_item[0].value.i == TRUE)
      template_info->include_phrase[stacknum] = TRUE;
   else
      template_info->include_phrase[stacknum] = FALSE;      
   
   return;
}


/*********************************************************************
   evaluate_rel_expr()
   
   PURPOSE
   Evaluates a relational expression and returns the result. 
   
   NOTES
   
   *******************************************************************/
int evaluate_rel_expr(const	template_item_struct	*template_item,
		      const	int 			index)
{
   int 		curtype;
   long int 	below, above;
   float 	belownum, abovenum;
   int 		result;
   
   
   /* get the type again and set the neighboring stack indices */
   
   curtype = (int )template_item[index].type;
   
   below  = index - 1;
   above  = index + 1;
   
   result = FALSE;
   
   
   /* the relational operator is a numeric relational operator */
   
   if (isrelop(curtype) == TRUE)
   {
      /* load in the values left and right of the relational
	 operator */
      
      if (template_item[below].type == RPF_INT)
	 belownum = (float )template_item[below].value.i;
      
      else if (template_item[below].type == RPF_FLT)
	 belownum = template_item[below].value.f;
      
      else if (template_item[below].type == RPF_TIM)
	 belownum = (float )template_item[below].value.t;
	 
      else
         belownum = MISSINGVAL;
      
      
      if (template_item[above].type == RPF_INT)
	 abovenum = (float )template_item[above].value.i;
      
      else if (template_item[above].type == RPF_FLT)
	 abovenum = template_item[above].value.f;
      
      else if (template_item[above].type == RPF_TIM)
	 abovenum = (float )template_item[above].value.t;
	 
      else
         abovenum = MISSINGVAL;
      
      
      
      /* check if either of the two values are the missing 
	 value indicator token.  if so, then assume that either
	 a EQ or NE test is requested.  continue with testing. */
      
      if ((int )belownum == MISSING_TOKEN || (int )abovenum == MISSING_TOKEN)
      {
	 if (curtype == RPF_EQ)
	 {
	    if (((int )belownum == MISSING_TOKEN &&
	         (int )abovenum == MISSINGVAL) ||
	        ((int )abovenum == MISSING_TOKEN &&
	         (int )belownum == MISSINGVAL))
	       result = TRUE;
	    else
	       result = FALSE;
	 }
	 
	 else if (curtype == RPF_NE)
	 {
	    if (((int )belownum == MISSING_TOKEN &&
	         (int )abovenum != MISSINGVAL) ||
	        ((int )abovenum == MISSING_TOKEN &&
	         (int )belownum != MISSINGVAL))
	       result = TRUE;
	    else
	       result = FALSE;
	 }
	 
	 else
	 {
	    log_msg(INVALID_MSGCONDITION, "use EQ or NE only");
	    result = FALSE;
	 }
      }
      
      
      /* if either data value missing and the missing indicator
         token is not used in the expression, then the
         expression evaluates to FALSE always */
         
      else if ((int )belownum == MISSINGVAL || (int )abovenum == MISSINGVAL)
      {
        result = FALSE;
      }
      
      
      /* perform the specified operation and get the result. */
      
      else if (curtype == RPF_GT)
      {
	 if (belownum > abovenum)
	    result = TRUE;
	 else
	    result = FALSE;
      }
      
      else if (curtype == RPF_GE)
      {
	 if (belownum >= abovenum)
	    result = TRUE;
	 else
	    result = FALSE;
      }
      
      else if (curtype == RPF_EQ)
      {
	 if (belownum == abovenum)
	    result = TRUE;
	 else
	    result = FALSE;
      }
      
      else if (curtype == RPF_NE)
      {
	 if (belownum != abovenum)
	    result = TRUE;
	 else
	    result = FALSE;
      }
      
      else if (curtype == RPF_LE)
      {
	 if (belownum <= abovenum)
	    result = TRUE;
	 else
	    result = FALSE;
      }
      
      else if (curtype == RPF_LT)
      {
	 if (belownum < abovenum)
	    result = TRUE;
	 else
	    result = FALSE;
      }
   }
   
   
   /* the relational operator is a string operator */
   
   else if (isstrop(curtype) == TRUE)
   {
      result = strcmp(template_item[below].value.s,
		      template_item[above].value.s);
      if (curtype == RPF_SEQ)
      {
	 if (result == 0)
	    result = TRUE;
	 else
	    result = FALSE;
      }
      else if (curtype == RPF_SNE)
      {
	 if (result == 0)
	    result = FALSE;
	 else
	    result = TRUE;
      }
   }
   
   return(result);
}


/*********************************************************************
   evaluate_log_expr()
   
   PURPOSE
   Evaluates a logical expression and returns the result. 
   
   NOTES
   
   *******************************************************************/
int evaluate_log_expr(const	template_item_struct	*template_item,
		      const	int 			index)
{
   int curtype;
   int below, above;
   int belowboolean, aboveboolean;
   int result;
   
   
   /* get the type again and set the neighboring stack indices */
   
   curtype = template_item[index].type;
   below = index - 1;
   above = index + 1;
   
   
   /* load in the values left and right of the logical operator */
   
   belowboolean = template_item[below].value.i;
   aboveboolean = template_item[above].value.i;
   
   
   /* perform the specified operation and get the result */
   
   if (curtype == RPF_AND)
   {
      if (belowboolean == TRUE && aboveboolean == TRUE)
	 result = TRUE;
      else
	 result = FALSE;
   }
   
   else if (curtype == RPF_OR)
   {
      if (belowboolean == TRUE || aboveboolean == TRUE)
	 result = TRUE;
      else
	 result = FALSE;
   }
   
   else
      result = FALSE;
   
   return(result);
}

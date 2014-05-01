/*********************************************************************
 rpf_stack.c
 
 expr_pushstack()
 expr_popstack()
 free_template_memory()
  
   ********************************************************************/

#include <stdlib.h>        /* standard library functions */

#include "rpf_stack.h"

/*********************************************************************
   expr_pushstack()
   
   PURPOSE
   This function pushes a set of information onto the stack used 
   for the conditional expression.
   
   NOTES
   Neither the size of the stack nor the pointer (or index) to 
   the current stack position are managed by this function.

   
   ********************************************************************/
void expr_pushstack(const 	itemtypes		type,
		    const	values 			value,
		    varinfo_struct			varinfo,
		    const	int			slot,
		    template_item_struct 	        *template_item)
{
   /* load the stack with the information that defines
      the slots contents */
   
   template_item[slot].type = type;
   template_item[slot].value = value;
   *(template_item[slot].varinfo) = varinfo;
   
   return;
}

/*********************************************************************
   expr_popstack()
   
   PURPOSE
   This function pops a set of information out of the stack
   and compresses the stack used for the conditional expression.
   Either 1 or more items can be popped at a time.
   
   NOTES
   Neither the size of the stack nor the pointer (or index) to 
   the current stack position are managed by this function.
   
   This function can pop multiple stack items with one call.
   The slot number is the lower slot in the stack if the num_to_pop
   is more than 1.
   ********************************************************************/
void expr_popstack(const int			slot,
		   const int 			num_to_pop,
		   const int			stacksize,
			 template_item_struct 	*template_item)
{
   int to_slot, from_slot;
   
   
   /* starting at the slot to pop, loop until the end of the
      new stack is reached */
   
   for (to_slot = slot; to_slot < stacksize - num_to_pop; ++to_slot)
   {
      /* define the slot copying from */
      
      from_slot = to_slot + num_to_pop;
      
      
      /* copy the stack info */
      
      template_item[to_slot].type = template_item[from_slot].type;
      template_item[to_slot].value = template_item[from_slot].value;
      *(template_item[to_slot].varinfo) = 
                                     *(template_item[from_slot].varinfo);
   }
   
   return;
}


/*********************************************************************
   free_template_memory()
   
   PURPOSE
   This function frees the memory allocated for a templates
   stack information.
   
   NOTES
   The memory was originally allocated in the read_condition and 
   read_phrase functions.
   
   ********************************************************************/

void free_template_memory(template_info_struct *template_info)
{
   int 	i;
   void *address;
   
   
   /* deallocate the memory used to store the conditional expressions
      in the variable sized stacks */
   
   for (i = 0; i < template_info->num_conditions; ++i)
   {
      address = template_info->bottom_condition_stack[i];
      if (address == NULL)
      {
         log_msg(FREE_NULL_MEMORY, "in free_template_memory(), bottom stack.\n");
      }
      else
      {
         free(address);
	 address = NULL;
      }
   }
   
  
   /* deallocate the memory used to store the variable length 
      phrases in the template */
   
   for (i = 0; i < template_info->num_phrases; ++i)
   {
      address = template_info->phrase[i];
      if (address == NULL)
      {
         log_msg(FREE_NULL_MEMORY, "in free_template_memory(), phrase.\n");
      }
      else
      {
	 free(address);
	 address = NULL;
      }
   }
   
   
   /* reset the counts */
   
   template_info->num_phrases = 0;
   template_info->num_conditions = 0;
  
   return;
}

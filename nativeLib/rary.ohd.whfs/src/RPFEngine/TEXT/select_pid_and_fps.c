/*********************************************************************
   select_pid_and_fps.c
   
   select_pid_and_fps()
   determine_issuance_number()
   determine_expiretime()
   round_expiretime()
   check_cnty_included()
   
   ********************************************************************/

#include <stdio.h>
#include <string.h>

#include "select_pid_and_fps.h"      /* function prototypes */

/*********************************************************************
   select_pid_and_fps()
   
   PURPOSE
   Functions that allow the determination of:
   (1) the recommended type of product to generate, and
   (2) the recommended forecast points to include in the product.
   This functions then sets these values to be the recommended values.
     
   ********************************************************************/
void select_pid_and_fps(int		numfps,
			fp_struct	*fp,
			int		numgrps,
			grp_struct	*grp,
			pcc_struct	*pcc,
			misc_struct	*misc,
			vtecinfo_struct	*vtecinfo)   
{
   int i, cnt;
   
   
   /* determine the recommended product to generate and the forecast
      points to include. this function manages the settings for the
      case where all points in a group can be included if at least
      one point is included. */
   
   rec_vtecproduct(numfps, fp, numgrps, grp, misc, vtecinfo);
      
   
   /* set the product category to the recommended product */
   
   strcpy(pcc->product.prod_categ,
	  convert_prodindex_to_id(misc->rec_prod_index));
   
   
   /* initialize the value of the actual points and groups included
      to be the recommended points */
      
   for (i = 0; i < numfps; i++)
   {
      misc->fps_included[i] = misc->rec_fps_included[i];
   }
       
   cnt = 0;
   for (i = 0; i < numgrps; i++)
   {
      misc->grps_included[i] = misc->rec_grps_included[i];
      if (misc->grps_included[i] == TRUE)
	 cnt++;
   }     
   misc->numgrps_included = cnt;
   
      
   return;
}


/*********************************************************************
   determine_issuance_number()
   
   PURPOSE
   To determine the issuance number for the product being generated.
   
 
   ********************************************************************/
void determine_issuance_number(int		numfps,
			       fp_struct	*fp,
			       int		prod_categ_index,
			       misc_struct	*misc)
{    
   misc->issnum = 0;
   
      
   return;
}


/************************************************************************
   determine_expiretime()
   
   PURPOSE
   Determine expiration time based on the product id.
   
   **********************************************************************/
void determine_expiretime(int		prod_categ_index,
			  misc_struct	*misc,
			  pcc_struct    *pcc)
{
   
   /* update the system time */
   
   time(&misc->system_time);
   
   
   /* note that the expire time is set by a relative value */
   
   misc->expire_set = UGC_RELATIVE; 
   
         
   /* use the pcc->product.expiration_time if it is not missingval, otherwise
      use the default expire time based on the product vategory */
   
   if (pcc->product.expiration_time != MISSINGVAL)
      misc->expire_time = misc->system_time + 
                         pcc->product.expiration_time*3600;
			 
   else
   {      
     if (prod_categ_index == FLS)         
        misc->expire_time = misc->system_time + misc->fls_expirehrs*3600;
   
     else if (prod_categ_index == FLW)
        misc->expire_time = misc->system_time + misc->flw_expirehrs*3600;
   
     else
         misc->expire_time = misc->system_time + misc->rvs_expirehrs*3600;           
   }
   
      
   return;
}
	  
   
/************************************************************************
   round_expiretime()
   
   PURPOSE
   Round the expiration time to the lowest (i.e. round down) hour.
   
   ************************************************************************/

void round_expiretime(misc_struct *misc)
{
   struct tm *time_struct;
      
   time_struct = gmtime(&misc->expire_time);
   time_struct->tm_sec = 0;
   time_struct->tm_min = 0;
   misc->expire_time = gm_mktime(time_struct);
   
   return;
}


/*****************************************************************
   check_cnty_included()
   
   Purpose
   Check which counties have at least one forecast point
   and update the count of included counties.
   *******************************************************************/

void check_cnty_included(county_struct	*cnty,
			 int         	numcnty,
		         misc_struct	*misc)  
{
   int i, j, fpindex;
   int cnt = 0;
   
   for (j = 0; j < numcnty; j++)
   {  
      misc->cnty_included[j] = FALSE;	 
      for (i = 0; i < cnty[j].numfps; i++)
      {
         fpindex = cnty[j].fpindex[i];
	 
	 if (misc->fps_included[fpindex] == TRUE)
	 {
      	    misc->cnty_included[j] = TRUE;	    
	 } 	  
      }
      
      if (misc->cnty_included[j] == TRUE)
         cnt++;
   }
   
   misc->numcnty_included = cnt;
   
   return;
} 


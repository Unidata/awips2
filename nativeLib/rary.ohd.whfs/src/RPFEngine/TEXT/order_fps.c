/*********************************************************************
   order_fps.c
   
   order_fps()
   order_fp_alpha()
   order_fp_grpalpha()
   order_fp_grpfp()
   order_fp_action()
   
   order_grps()
   order_grp_alpha()
   order_grp_grp()
   
   The same order choice values are used for both grp/fp ordering
   and fp ordering.
   
   *******************************************************************/

#include "order_fps.h"

/*********************************************************************
   order_fps()
   
   PURPOSE
   Orders the forecast points in the order specified.
      
   NOTES
   The returned array is ordered in the standard forecast point
   order implied by the master list of points.
   
   *******************************************************************/

void order_fps(const	int		numfps,
	       const	fp_struct	*fp,
	       const	int		numgrps,
	       const    grp_struct	*grp,
	               vtecinfo_struct *vtecinfo,
	       const	int		order_choice,
	       		int		*fporder)
{   
   int i;
   
   /*initialize fporder */
   
   for (i=0; i< numfps; i++)
      fporder[i] = MISSINGVAL;
      
   /* if the default order is selected, then simply use the
      order specified in the group id list */
   
   if (order_choice == ORDER_DEFAULT)     
      order_fp_alpha(numfps, fp, numgrps, grp, fporder);

   
   /* order the forecast points by the severity of the group, then
      within the group, order the forecast points in the 
      order specified in the group id list */
   
   else if (order_choice == ORDER_GROUP_DEFAULT)
      order_fp_grpalpha(numfps, fp, numgrps, grp, fporder); 
   
   
   /* sort by forecast groups, then within the forecast groups,
      sort the forecast points */
   
   else if (order_choice == ORDER_GROUP_FP)
      order_fp_grpfp(numfps, fp, numgrps, grp, fporder);
      
   /*sort the forecast points by action code*/
   
   else if (order_choice == ORDER_BY_ACTION)
      order_fp_action(numfps, fp, numgrps, grp, vtecinfo, fporder);   
   
   return;
}


/*********************************************************************
   order_fp_alpha()
   
   PURPOSE
   Orders the forecast points in the default order.
   
   NOTES
   The alogorithm used relies on the fact that the groups and points are 
   retrieved and loaded in an ordered manner.
   
   *******************************************************************/

void order_fp_alpha(const int		numfps,
		    const fp_struct	*fp,
		    const int		numgrps,
		    const grp_struct	*grp,
		          int		*fporder)
{
   int i, j;
   int ordernum;
   int fpindex;
   
   ordernum = 0;
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 fporder[ordernum] = fpindex;
	 ordernum++;
      }
   }
   
   return;
}


/*********************************************************************
   order_fp_grp()
   
   PURPOSE
   Orders the forecast points by group severity,, then
   by default forecast point order within the group.
   
   NOTES
   
   *******************************************************************/
void order_fp_grpalpha(const int		numfps,
		       const fp_struct		*fp,
		       const int		numgrps,
		       const grp_struct		*grp,
			     int		*fporder)
{
   int ordernum;
   int i, j;
   int grpcat;
   
   
   ordernum = 0;
   
   
   /* loop on the possible stage categories */
   
   for (grpcat = RECORD; grpcat >= NONFLOOD; grpcat--)
   {
      /* loop on the groups of forecast points */
      
      for (i = 0; i < numgrps; i++)
      {
	 /* if the group category matches the current loop category,
	    then set the order for its forecast points  */
	 
	 if (grp[i].max_omf_cat == grpcat)
	 {
	    for (j = 0; j < grp[i].numfps; j++)
	    {
	       fporder[ordernum] = grp[i].fpindex[j];
	       ordernum++;
	    }
	 }
      }  
   }     /* end of loop on the possible categories */
   
   
   /* now fill in any unaccounted for groups;
      loop on the groups of forecast points and for those not considered
      above, then loop on the group's forecast points and put
      them into the order list */
   
   for (i = 0; i < numgrps; i++)
   {      
      /* note that this if check is the inverse of the check above since
	 a category is either between RECORD and NONFLOOD or is NULLCAT */
      
      if (grp[i].max_omf_cat == NULLCAT)
      {
	 for (j = 0; j < grp[i].numfps; j++)
	 {
	    fporder[ordernum] = grp[i].fpindex[j];
	    ordernum++;
	 }
      }
   }   /* end of loop on forecast groups */
   
   return;
}


/*********************************************************************
   order_fp_grpfp()
   
   PURPOSE
   Orders the forecast points by group severity, then within group by
   forecast point severity.
   
   NOTES
   
   *******************************************************************/
void order_fp_grpfp(const int		numfps,
		    const fp_struct	*fp,
		    const int		numgrps,
		    const grp_struct	*grp,
		          int		*fporder)
{
   int i, j;
   int ordernum;
   int fpindex, grpcat, fpcat;
   
   
   ordernum = 0;
   
   
   /* loop on the possible stage categories */
   
   for (grpcat = RECORD; grpcat >= NONFLOOD; grpcat--)
   {
      /* loop on the forecast groups */
      
      for (i = 0; i < numgrps; i++)
      {
	 /* if the group category matches the current loop category,
	    then check its forecast points */
	 
	 if (grp[i].max_omf_cat == grpcat)
	 {
	    /* do a similar loop for the categories and
	       the individual forecast points */
	    
	    for (fpcat = RECORD; fpcat >= NONFLOOD; fpcat--)
	    {
	       for (j = 0; j < grp[i].numfps; j++)
	       {
		  fpindex = grp[i].fpindex[j];
		  if (fp[fpindex].omf_cat == fpcat)
		  {
		     fporder[ordernum] = fpindex;
		     ordernum++;
		  }
	       }
	    }   /* end of for loop on the possible categories */
	    
	    
	    /* now fill in any unaccounted for forecast points in the group.
	       this ensures that all forecast points that are in
	       a group are ordered together even if there is no
	       data for the group */
	    
	    for (j  = 0; j < grp[i].numfps; j++)
	    {
	       fpindex = grp[i].fpindex[j];
	       if (fp[fpindex].omf_cat == NULLCAT)
	       {
		  fporder[ordernum] = fpindex;
		  ordernum++;
	       }
	    }   
	 }      /* end of if on matching categories of group */	 	 
      } 
   }     /* end of loop on possible categories */     
   
   
   /* now fill in any unaccounted for groups;
      loop on the groups of forecast points and for those not considered
      above, then loop on the group's forecast points and put
      them into the order list */
   
   for (i = 0; i < numgrps; i++)
   {
      if (grp[i].max_omf_cat == NULLCAT)
      {
	 for (j = 0; j < grp[i].numfps; j++)
	 {
	    fporder[ordernum] = grp[i].fpindex[j];
	    ordernum++;
	 }
      }
   }  /* end of loop on the forecast groups */

   return;
}


/*********************************************************************
   order_fp_action()
   
   PURPOSE
   Orders the forecast points by the order of the action codes specified
   in 10-922.
   
   NOTES
   Forecast points will be ordered by action codes as COR, CAN, EXP, EXT, CON,
   NEW and ROU
   
   *******************************************************************/

void order_fp_action(const int		numfps,
		    const fp_struct	*fp,
		    const int		numgrps,
		    const grp_struct	*grp,
		    vtecinfo_struct    *vtecinfo,
		          int		*fporder)
{
   int i, j;
   int ordernum;
   int fpindex;
   
   ordernum = 0;
   
   /*get fp which has "COR" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "COR") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
   
   /*get fp which has "CAN" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "CAN") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
   
   /*get fp which has "EXP" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "EXP") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
   
   /*get fp which has "NEW" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "NEW") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
   
   /*get fp which has "EXT" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "EXT") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
   
   /*get fp which has "CON" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "CON") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
      
   
   /*get fp which has "ROU" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      for (j = 0; j < grp[i].numfps; j++)
      {
	 fpindex = grp[i].fpindex[j];
	 if (strcmp(vtecinfo[fpindex].action, "ROU") == 0)
	 {
	     fporder[ordernum] = fpindex;
	     ordernum++;
	 }    
      }
   }
   
   return;
}

/*********************************************************************
   order_grps()
   
   PURPOSE
   Orders the forecast groups in the order specified.
      
   
   *******************************************************************/

void order_grps(const	int		numgrps,
	        const   grp_struct	*grp,
		vtecinfo_struct        *vtecinfo,
	        const	int		order_choice,
	       		int		*grporder)
{  
   int i;
   
   /*initialize grporder*/
   
   for (i =0; i< numgrps; i++)
      grporder[i] = MISSINGVAL;
       
   /* if the default order is selected, then simply use the
      order specified in the group id list */
   
   if (order_choice == ORDER_DEFAULT)     
      order_grp_alpha(numgrps, grp, grporder);

   /* order the group be the vtec action */
   
   else if (order_choice == ORDER_BY_ACTION)
      order_grp_action(numgrps, grp, vtecinfo, grporder);
   
   /* order the forecast points by the severity of the group */
   
   else if (order_choice == ORDER_GROUP_DEFAULT)
      order_grp_grp(numgrps, grp, grporder); 
             
   else
      order_grp_grp(numgrps, grp, grporder); 
         
   return;
}


/*********************************************************************
   order_grp_alpha()
   
   PURPOSE
   Orders the forecast points in the default order.
   
   NOTES
   The alogorithm used relies on the fact that the groups and points are 
   retrieved and loaded in an ordered manner.
   
   *******************************************************************/

void order_grp_alpha(const int		numgrps,
		     const grp_struct	*grp,
		    	   int		*grporder)
{
   int i;
   
   for (i = 0; i < numgrps; i++)
      grporder[i] = i;
   
   return;
}


/*********************************************************************
   order_grp_grp()
   
   PURPOSE
   Orders the forecast groups by their categorical severity.
      
   NOTES
   
   *******************************************************************/

void order_grp_grp(const	int		numgrps,
	           const	grp_struct	*grp,
		            	int		*grporder)
{
   int ordernum;
   int i;
   int grpcat;
   
   ordernum = 0;
   
   
   /* loop on the possible stage categories */
   
   for (grpcat = RECORD; grpcat >= NONFLOOD; grpcat--)
   {
      /* loop on the forecast groups */
      
      for (i = 0; i < numgrps; i++)
      {
	 /* if the group category matches the current loop category,
	    then set the order */
	 
	 if (grp[i].max_omf_cat == grpcat)
	 {
	    grporder[ordernum] = i;
	    ordernum++;
	 }
      }  
   }     /* end of loop on the possible categories */
   
   
   /* now fill in any unaccounted for groups;
      loop on the groups of forecast points and for those not considered
      above, then loop on the group's forecast points and put
      them into the order list */
   
   for (i = 0; i < numgrps; i++)
   {      
      /* note that this if check is the complement of the check above since
	 a category is either between RECORD and NONFLOOD or is NULLCAT */
      
      if (grp[i].max_omf_cat == NULLCAT)
      {
	 grporder[ordernum] = i;
	 ordernum++;
      }
   }   
   
   return;
}

/*********************************************************************
   order_grp_action()
   
   PURPOSE
   Orders the group by the vtec action codes
   
   NOTES
   
   Forecast groups will be ordered by action codes as COR, CAN, EXP, EXT, CON,
   NEW and ROU
   *******************************************************************/
void order_grp_action(const int		numgrps,
		     const grp_struct	*grp,
		     vtecinfo_struct   *vtecinfo,
		    	   int		*grporder)
{              
   int i;
   int ordernum;
     
   ordernum = 0;
   
   /*get grp which has "COR" action*/
   
   for (i = 0; i < numgrps; i++)
   {      
       if (strcmp(vtecinfo[i].action, "COR") == 0)
       {
	   grporder[ordernum] = i;
	   ordernum++;
       }    
      
   }
   
   /*get grp which has "CAN" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      
      if (strcmp(vtecinfo[i].action, "CAN") == 0)
      {
	  grporder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get grp which has "EXP" action*/
   
   for (i = 0; i < numgrps; i++)
   {      
      if (strcmp(vtecinfo[i].action, "EXP") == 0)
      {
	  grporder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get grp which has "EXT" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      if (strcmp(vtecinfo[i].action, "EXT") == 0)
      {
	  grporder[ordernum] =  i;
	  ordernum++;
      }    

   }
   
   /*get grp which has "CON" action*/
   
   for (i = 0; i < numgrps; i++)
   {     
      if (strcmp(vtecinfo[i].action, "CON") == 0)
      {
	  grporder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get grp which has "NEW" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      if (strcmp(vtecinfo[i].action, "NEW") == 0)
      {
	  grporder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get grp which has "ROU" action*/
   
   for (i = 0; i < numgrps; i++)
   {
      if (strcmp(vtecinfo[i].action, "ROU") == 0)
      {
	  grporder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   return;

}
/*********************************************************************
   order_cntys()
   
   PURPOSE
   Orders the forecast counties in the order specified.
      
   
   *******************************************************************/
void order_cntys(const	int		numcntys,
	        const   county_struct	*cnty,
		vtecinfo_struct        *vtecinfo,
	        const	int		order_choice,
	       		int		*cntyorder)
{   
   /* if the default order is selected, then simply use the
      order specified in the county list */
   
   if (order_choice == ORDER_DEFAULT)     
      order_cnty_alpha(numcntys, cnty, cntyorder);

   /* order the group be the vtec action */
   
   else if (order_choice == ORDER_BY_ACTION)
      order_cnty_action(numcntys, cnty, vtecinfo, cntyorder);  
      
   else                     
      order_cnty_alpha(numcntys, cnty, cntyorder);
      
   return;
}

/*********************************************************************
   order_cnty_alpha()
   
   PURPOSE
   Orders the counties in the default order.   
   
   *******************************************************************/

void order_cnty_alpha(const int		    numcntys,
		     const county_struct   *cnty,
		    	   int		   *cntyorder)
{
   int i;
   
   for (i = 0; i < numcntys; i++)
      cntyorder[i] = i;
   
   return;
}

/*********************************************************************
   order_cnty_action()
   
   PURPOSE
   Orders the counties by the vtec action codes
   
   NOTES
   
   Counties will be ordered by action codes as COR, CAN, EXP, EXT, CON,
   NEW and ROU
   *******************************************************************/
void order_cnty_action(const int	    numcntys,
		     const county_struct   *cnty,
		     vtecinfo_struct   *vtecinfo,
		    	   int		*cntyorder)
{              
   int i;
   int ordernum;
     
   ordernum = 0;
   
   /*get county which has "COR" action*/
   
   for (i = 0; i < numcntys; i++)
   {      
       if (strcmp(vtecinfo[i].action, "COR") == 0)
       {
	   cntyorder[ordernum] = i;
	   ordernum++;
       }    
      
   }
   
   /*get couty which has "CAN" action*/
   
   for (i = 0; i < numcntys; i++)
   {      
      if (strcmp(vtecinfo[i].action, "CAN") == 0)
      {
	  cntyorder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get county which has "EXP" action*/
   
   for (i = 0; i < numcntys; i++)
   {      
      if (strcmp(vtecinfo[i].action, "EXP") == 0)
      {
	  cntyorder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get county which has "EXT" action*/
   
   for (i = 0; i < numcntys; i++)
   {
      if (strcmp(vtecinfo[i].action, "EXT") == 0)
      {
	  cntyorder[ordernum] =  i;
	  ordernum++;
      }    

   }
   
   /*get county which has "CON" action*/
   
   for (i = 0; i < numcntys; i++)
   {     
      if (strcmp(vtecinfo[i].action, "CON") == 0)
      {
	  cntyorder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get county which has "NEW" action*/
   
   for (i = 0; i < numcntys; i++)
   {
      if (strcmp(vtecinfo[i].action, "NEW") == 0)
      {
	  cntyorder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   
   /*get county which has "ROU" action*/
   
   for (i = 0; i < numcntys; i++)
   {
      if (strcmp(vtecinfo[i].action, "ROU") == 0)
      {
	  cntyorder[ordernum] = i;
	  ordernum++;
      }    
      
   }
   return;

}

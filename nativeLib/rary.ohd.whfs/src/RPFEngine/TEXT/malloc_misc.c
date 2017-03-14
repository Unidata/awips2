/*********************************************************************
   malloc_misc.c
   
   malloc_miscmain()
   malloc_miscsubs()
   
   malloc_vtecinfo()
      
   malloc_pcc()
   malloc_pccsubs()
   
   malloc_templatenames()
   
 
   --------------------
   
   free_memory()
   
   free_fpgrpcnty()
   
   free_miscsubs()  
   free_vtecinfo()
   free_pcc()
   free_pccnames()
   free_templatenames()
   
   
   NOTES:
   The fp info, grp info, and pccnames info are malloc'ed in separate
   functions not part of this file, while the free functions for these
   structures are included herein.
   
   
   ********************************************************************/

#include <stdlib.h>

#include "malloc_misc.h"


/*********************************************************************
   malloc_miscmain()
   
   PURPOSE
   Allocate the memory for the main part of the misc structure.  
   
   ********************************************************************/
void malloc_miscmain(misc_struct 	**misc)
{
   *misc = (misc_struct *)malloc(sizeof(misc_struct));
   if (*misc == NULL) log_msg(FAILED_MALLOC, "of misc in malloc_misc");
      
   return;
}


/*********************************************************************
   malloc_miscsubs()
   
   PURPOSE 
   Allocate the subordinate memory for the misc information.  
   
   ********************************************************************/

void malloc_miscsubs(int		numfps,
		     int		numgrps, 
		     int                numcnty,
		     misc_struct 	*misc)
{     
   /*------------------------------------------------------------*/
   /* sized by forecast points */
      
   misc->fps_included = (int *)malloc(sizeof(int) * numfps);
   if (misc->fps_included == NULL) 
      log_msg(FAILED_MALLOC, "of misc->fps_included in malloc_misc");

   misc->rec_fps_included = (int *)malloc(sizeof(int) * numfps);
   if (misc->rec_fps_included == NULL) 
      log_msg(FAILED_MALLOC, "of misc->rec_fps_included in malloc_misc");
   
        
   /*--------------------------------------------------------------------*/
   /* sized by forecast groups */
   
   misc->grps_included = (int *)malloc(sizeof(int) * numgrps);
   if (misc->grps_included == NULL) 
      log_msg(FAILED_MALLOC, "of misc->grps_included in malloc_misc");

   misc->rec_grps_included = (int *)malloc(sizeof(int) * numgrps);
   if (misc->rec_grps_included == NULL) 
      log_msg(FAILED_MALLOC, "of misc->rec_grps_included in malloc_misc");
   
  
   /*-----------------------------------------------------------------*/
   /* sized by county */
  
   misc->cnty_included = (int *)malloc(sizeof(int) * numcnty);
   if (misc->cnty_included == NULL)
      log_msg(FAILED_MALLOC, "of misc->cnty_included in malloc_misc");  
   
   
   return;
}


/*********************************************************************
  malloc_vtecinfo()
  
  PURPOSE
  Allocate the memory for the vtec information for the product 
  being generated.  Assume that VTEC events are segmented by forecast 
  points.
  
  
   ********************************************************************/
void malloc_vtecinfo(int		numfps,
		     vtecinfo_struct	**vtecinfo)
{
    
   *vtecinfo = (vtecinfo_struct *)malloc(sizeof(vtecinfo_struct) * numfps);
   if (*vtecinfo == NULL)
      log_msg(FAILED_MALLOC, "of *vtecinfo in malloc_vtecinfo");
   
   return;
}


/*********************************************************************
  malloc_pcc()
  
  PURPOSE
  Allocate the memory for the pcc information.  This is done once, 
  outside of the function where the information is loaded because it
  is possible that the pcc information is reloaded.  
  
   ********************************************************************/
void malloc_pcc(int		numfps,
		int		numgrps, 
		pcc_struct 	**pcc)
{
   *pcc = (pcc_struct *)malloc(sizeof(pcc_struct));
   if (*pcc == NULL) log_msg(FAILED_MALLOC, "of pcc in malloc_pcc");
   
   malloc_pccsubs(numfps, numgrps, *pcc);
   
   return;
}


/*********************************************************************
  malloc_pccsubs()
  
  PURPOSE
  Allocate the subordinate memory for the pcc information.  
  
  ********************************************************************/
void malloc_pccsubs(int		numfps,
		    int		numgrps, 
		    pcc_struct 	*pcc)
{
   int i;
   
   /* product-wide section */
   
   pcc->product.include_fp = (int *)malloc(sizeof(int)*numfps);
   if (pcc->product.include_fp == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->product.include_fp in malloc_pcc");
   
   
   /* summary section */
   
   pcc->summary.template = (char **)malloc(sizeof(char *) * numgrps);
   if (pcc->summary.template == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->summary.template in malloc_pcc");

   for (i = 0; i < numgrps; i++)
   {
      pcc->summary.template[i] = (char *)malloc(MAXLEN_TEMPLATENAME+1);
      if (pcc->summary.template[i] == NULL) 
	 log_msg(FAILED_MALLOC, "of pcc->summary.template[] in malloc_pcc");
   }

   
   /* basis, tabular section */
   
   
   /* roundup subsection */
   
   pcc->roundup.template = (char **)malloc(sizeof(char *) * numfps);
   if (pcc->roundup.template == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->roundup.template in malloc_pcc");

   for (i = 0; i < numfps; i++)
   {
      pcc->roundup.template[i] = (char *)malloc(MAXLEN_TEMPLATENAME+1);
      if (pcc->roundup.template[i] == NULL) 
	 log_msg(FAILED_MALLOC, "of pcc->roundup.template[] in malloc_pcc");
   }

   
   /* impact subsection */
   
   pcc->impact.lower_stage = (float *)malloc(sizeof(float) * numfps);
   if (pcc->impact.lower_stage == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->impact.lower_stage in malloc_pcc");

   pcc->impact.upper_stage = (float *)malloc(sizeof(float) * numfps);
   if (pcc->impact.upper_stage == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->impact.upper_stage in malloc_pcc");

   pcc->impact.range_set = (int *)malloc(sizeof(int) * numfps);
   if (pcc->impact.range_set == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->impact.range_set in malloc_pcc");
   
   pcc->impact.refstage = (float *)malloc(sizeof(float) * numfps);
   if (pcc->impact.refstage == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->impact.refstage in malloc_pcc");
   
   pcc->impact.reftype = (int *)malloc(sizeof(int) * numfps);
   if (pcc->impact.reftype == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->impact.reftype in malloc_pcc");   
   

   /* comparison subsection */
   
   pcc->comparison.compare_date = (char **)malloc(sizeof(char *) * numfps);
   if (pcc->comparison.compare_date == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->comparison.compare_date in malloc_pcc");

   for (i = 0; i < numfps; i++)
   {
      pcc->comparison.compare_date[i] = (char *)malloc(DATE_LEN + 1); 
      if (pcc->comparison.compare_date[i] == NULL) 
	 log_msg(FAILED_MALLOC, "of pcc->comparison.compare_date[] in malloc_pcc");
   }

   pcc->comparison.compare_stage = (float *)malloc(sizeof(float) * numfps);
   if (pcc->comparison.compare_stage == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->comparison.compare_stage in malloc_pcc");

   pcc->comparison.compare_set = (int *)malloc(sizeof(int) * numfps);
   if (pcc->comparison.compare_set == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->comparison.compare_set in malloc_pcc");
   
   pcc->comparison.refstage = (float *)malloc(sizeof(float) * numfps);
   if (pcc->comparison.refstage == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->comparison.refstage in malloc_pcc");

   pcc->comparison.reftype = (int *)malloc(sizeof(int) * numfps);
   if (pcc->comparison.reftype == NULL) 
      log_msg(FAILED_MALLOC, "of pcc->comparison.reftype in malloc_pcc");
   
   
   return;
}


/*********************************************************************
  malloc_templatenames()
  
  PURPOSE
  Allocate the memory for the template names information.
  
  ********************************************************************/
void malloc_templatenames(templatenames_struct **template_names)
{
   
   *template_names = 
      (templatenames_struct *)malloc(sizeof(templatenames_struct));
   if (*template_names == NULL) 
      log_msg(FAILED_MALLOC, "of template_names in malloc_templatenames");
   
   return;
}




/*********************************************************************
   
   MALLOC FUNCTIONS ABOVE -
   FREE FUNCTIONS BELOW - 
   
*********************************************************************/


/*********************************************************************
  free_memory()
  
  PURPOSE
  Free all the the memory allocated by RiverPro.
  This is only called at the end of the program.
  The free_xxx functions called by this function are also called
  directly/independently by other functions where appropriate.
  
  
   ********************************************************************/

void free_memory(int			numfps, 
		 fp_struct		*fp,
		 int			numgrps,
		 grp_struct		*grp,
		 int                    numcnty,
		 county_struct          *cnty,
		 vtecinfo_struct	*vtecinfo,
		 misc_struct		*misc,
		 pcc_struct		*pcc,
		 templatenames_struct	*templatenames)
{         
   
   /* free the template names memory */
   
   free_templatenames(templatenames);
   
   
   /* free the vtec memory */
  
   free_vtecinfo(vtecinfo);

   
   /* free the allocated forecast point and group memory */
   
   free_fpgrpcnty(numfps, fp, numgrps, grp, numcnty, cnty); 
   
   
   /* free the misc info. note that since the misc main
      memory is permanent, it is not freed in the subordinate
      function, but is freed here instead.  */
   
   free_miscsubs(numfps, numgrps, numcnty, misc);   
   free(misc);
   
   
   /* free the pcc info  */
   
   free_pcc(numfps, numgrps, pcc);
   
   
   /* note that the pccnames info is freed independently */
        
   
   return;      
}



/*********************************************************************
  free_fpgrpcnty()
  
  PURPOSE
  Allocate the memory for the information in the fp,grp and cnty structure.  
  
  ********************************************************************/
void free_fpgrpcnty(int			numfps, 
		    fp_struct		*fp,
		    int			numgrps,
		    grp_struct		*grp,
		    int			numcnty,
		    county_struct	*cnty)
{
   int i;
   
   
   /* free any stage values that may be loaded for a forecast point,
      then free the fp structure. */
   
   for (i = 0; i < numfps; i++)
   {
      if (fp[i].obsH != NULL)
	 free(fp[i].obsH);
      if (fp[i].fcstH != NULL)
	 free(fp[i].fcstH);
      
      fp[i].numobsH = fp[i].numfcstH = 0;
      fp[i].use_obsH = 0;
   }
     
   free(fp); 
   
   
   /* free the group data */
   
   for (i = 0; i < numgrps; i++)
   {
     if (grp[i].fpindex != NULL)
        free(grp[i].fpindex);
   }
   if  (grp != NULL)
     free(grp);
   
   
   /* free the county data */
   
   for (i = 0; i < numcnty; i++)
   {
     if (cnty[i].fpindex != NULL)
        free(cnty[i].fpindex);
   }
   if (cnty != NULL)
     free(cnty);
  
   return;
}


/*********************************************************************
  free_miscsubs()
  
  PURPOSE
  Allocate the memory for the subordinate information in the misc
  structure.
  
   ********************************************************************/
void free_miscsubs(int 		numfps, 
		   int 		numgrps,
		   int          numcnty,
		   misc_struct 	*misc)
{
    
    
   /* free forecast point info */
   
   free(misc->fps_included); 
   free(misc->rec_fps_included);
    
   
   /* free forecast group info */
   
   free(misc->grps_included);
   free(misc->rec_grps_included);
       
   
   /* free forecast group info */
   
   free(misc->cnty_included);
   
   
   return;
}


/*********************************************************************
  free_vtecinfos()
  
  PURPOSE
  Allocate the memory for the vtec information..  
  
   ********************************************************************/
void free_vtecinfo(vtecinfo_struct	*vtecinfo)
{
    
   if (vtecinfo != NULL)
      free(vtecinfo);
    
   return;
}


/*********************************************************************
  free_pcc()
  
  PURPOSE
  Allocate the memory for the pcc information..  
  
   ********************************************************************/
void free_pcc(int		numfps,
	      int		numgrps,
	      pcc_struct 	*pcc)
{
   int i;
   
     
   free(pcc->product.include_fp); 
   
   for (i = 0; i < numgrps; i++)
      free(pcc->summary.template[i]);
   free(pcc->summary.template);
   
   
   for (i = 0; i < numfps; i++)
      free(pcc->roundup.template[i]);   
   free(pcc->roundup.template);
   
   free(pcc->impact.lower_stage);
   free(pcc->impact.upper_stage);
   free(pcc->impact.range_set); 
   free(pcc->impact.refstage);
   free(pcc->impact.reftype);
   
   for (i = 0; i < numfps; i++)
      free(pcc->comparison.compare_date[i]);
   free(pcc->comparison.compare_date);
   
   free(pcc->comparison.compare_stage);   
   free(pcc->comparison.compare_set);
   free(pcc->comparison.refstage);
   free(pcc->comparison.reftype);
   
   return;
}


/*********************************************************************
  free_pccnames()
  
  PURPOSE
  Free the memory for the pcc names information.
  
  ********************************************************************/
void free_pccnames(pccnames_struct *pccnames)
{
   pccnames_struct	*pccname;   
   
   while ((pccname = (pccnames_struct *)ListLast(&pccnames->list)))
   {
      ListDelete(&pccnames->list, &pccname->node);
      free((pccnames_struct *)pccname);
   }
   
   return;
}


/*********************************************************************
  free_templatenames()
  
  PURPOSE
  Allocate the memory for the templatename information..  
  
   ********************************************************************/
void free_templatenames(templatenames_struct	*templatenames)
{
    
   if (templatenames != NULL)
      free(templatenames);
   
   return;
}






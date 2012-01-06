/*********************************************************************
   verify_templatenames.c
   
   verify_templatenames()
   verify_hdrtemplate() 
   verify_headltemplate()  
   verify_bastemplate()
   verify_sumtemplate()
   verify_tabtemplate()
   verify_rndtemplate()
   verify_imptemplate()
   verify_cmptemplate()
   verify_ctatemplate()
   
   MODIFICATION HISTORY
    FUNCTION              DATE     PROGRAMMER    DESCRIPTION    
   verify_headltemplate() 04/2004  Jingtao Deng  New routine to handle
                                                 headline section.
						 
   verify_sumtemplate()   04/2004  Jingtao Deng  Remove prologue part.						 
   ********************************************************************/

#include <string.h>                /* string library functions */

#include "verify_templatenames.h"  /* function prototypes */

/*********************************************************************
   verify_templatenames()
   
   PURPOSE
   Verify that the template names specified in the pcc structure
   are valid names of templates. 
   
   NOTES
   
   ********************************************************************/

void verify_templatenames(const int			numfps,
			  const fp_struct		*fp,
			  const	int			numgrps,
			  const grp_struct		*grp,
			  const templatenames_struct	*templatenames,
			        pcc_struct		*pcc)
{
   
   verify_hdrtemplate(templatenames, pcc);
   
   verify_bastemplate(templatenames, pcc);
   
   verify_headltemplate(templatenames, pcc);
   
   verify_sumtemplate(numgrps, grp, templatenames, pcc);
   
   verify_tabtemplate(templatenames, pcc);
   
   verify_rndtemplate(numfps, fp, templatenames, pcc);
   
   verify_imptemplate(templatenames, pcc);
   
   verify_cmptemplate(templatenames, pcc);
   
   verify_ctatemplate(templatenames, pcc);
   
   return;
}


/*********************************************************************
   verify_hdrtemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the header section is a valid template name.

   
   NOTES
   
   ********************************************************************/
   
void verify_hdrtemplate(const	templatenames_struct	*templatenames,
		        	pcc_struct 		*pcc)
{
   int found, i;
   
   
   /* only bother checking if this is NOT for an nwr product */
   
   if (pcc->product.nwr_flag)
      return;
   
   
   /* check the template name specified for the header section;
      if not found, issue message */
   
   i = 0;
   found = FALSE;
   while (found == FALSE && i < templatenames->header.number)
   {
      if (strcmp(pcc->header.template,
		 templatenames->header.name[i]) == 0) found = TRUE;
      i++;
   }
   if (found == FALSE) 
      log_msg(HEADER_TEMPLATE_NOTFOUND, pcc->header.template);

   return;
}


/*********************************************************************
   verify_bastemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the basis section is a valid template name.

   
   NOTES
   
   ********************************************************************/
   
void verify_bastemplate(const 	templatenames_struct 	*templatenames,
		      	    	pcc_struct 		*pcc)
{
   int found, i;
   
   /* check the template name specified for the basis section;
      if not found, issue message */
   
   if (pcc->basis.includeflag == TRUE)
   {
      i = 0;
      found = FALSE;
      while (found == FALSE && i < templatenames->basis.number)
      {
	 if (strcmp(pcc->basis.template,
		    templatenames->basis.name[i]) == 0) found = TRUE;
	 i++;
      }
      if (found == FALSE)
      {
	 log_msg(BASIS_TEMPLATE_NOTFOUND, pcc->basis.template);
	 pcc->basis.includeflag = FALSE;
      }
   }

   return;
}

/*********************************************************************
   verify_headltemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the headline section is a valid template name.

   
   NOTES
   
   ********************************************************************/
   
void verify_headltemplate(const templatenames_struct 	*templatenames,
		      	 pcc_struct     	      *pcc)
{
   int found, i;
   
   /* check the template name specified for the headline section;
      if not found, issue message */
   
   if (pcc->headline.includeflag == TRUE)
   {
      i = 0;
      found = FALSE;
      while (found == FALSE && i < templatenames->headline.number)
      {
	 if (strcmp(pcc->headline.template,
		    templatenames->headline.name[i]) == 0) found = TRUE;
	 i++;
      }
      if (found == FALSE)
      {
	 log_msg(HEADLINE_TEMPLATE_NOTFOUND, pcc->headline.template);
	 pcc->headline.includeflag = FALSE;
      }
   }

   return;
}

/*********************************************************************
   verify_sumtemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the summary section is a valid template name.

   
   NOTES
   
   ********************************************************************/
void verify_sumtemplate(const	int			numgrps,
			const	grp_struct 		*grp,
			const	templatenames_struct 	*templatenames,
		      	    	pcc_struct 		*pcc)
{
   int found, i, index;
   
   /* check the template names specified for the summary section;
      if not found, issue message */
   
   if (pcc->summary.includeflag == TRUE)
   {
	 	 
      /* check the prologue template */
      
      /*remove this prologue*/
      /*
      if (pcc->summary.include_prologueflag == TRUE)
      {
	 i = 0;
	 found = FALSE;
	 while (found == FALSE && i < templatenames->summary.number)
	 {
	    if (strcmp(pcc->summary.prologue_template,
		       templatenames->summary.name[i]) == 0) found = TRUE;
	    i++;
	 }
	 if (found == FALSE)
	 {
	    log_msg(SUMMARY_TEMPLATE_NOTFOUND, pcc->summary.prologue_template);
	    pcc->summary.include_prologueflag = FALSE;
	 }
      }
      
      */
      /* check the forecast group templates */
      
      for (index = 0; index < numgrps; index++)
      {
	 i = 0;
	 found = FALSE;
	 while (found == FALSE && i < templatenames->summary.number)
	 {
	    if (strcmp(pcc->summary.template[index],
		       templatenames->summary.name[i]) == 0) found = TRUE;
	    i++;
	 }
	 if (found == FALSE)
	 {
	    log_msg(SUMMARY_TEMPLATE_NOTFOUND, pcc->summary.template[index]);
	    pcc->summary.includeflag = FALSE;
	 }
      }
            
   }

   return;
}


/*********************************************************************
   verify_tabtemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the tabular section is a valid template name.
  
   NOTES
   
   ********************************************************************/
   
void verify_tabtemplate(const	templatenames_struct 	*templatenames,
		    		pcc_struct 		*pcc)
{
   int found, i;
   
   /* check the template name specified for the tabular section;
      if not found, issue message */
   
   if (pcc->tabular.includeflag == TRUE)
   {
      i = 0;
      found = FALSE;
      while (found == FALSE && i < templatenames->tabular.number)
      {
	 if (strcmp(pcc->tabular.template,
		    templatenames->tabular.name[i]) == 0) found = TRUE;
	 i++;
      }
      if (found == FALSE)
      {
	 log_msg(TABULAR_TEMPLATE_NOTFOUND, pcc->tabular.template);
	 pcc->tabular.includeflag = FALSE;
      }
   }

   return;
}


/*********************************************************************
   verify_rndtemplate()
   
   PURPOSE
   Verify that the template names specified in the pcc structure
   for the roundup subsection are valid names of templates.
   
   NOTES
   
   ********************************************************************/
   
void verify_rndtemplate(const	int 		 	numfps,
			const	fp_struct		*fp,
			const	templatenames_struct 	*templatenames,
		      	     	pcc_struct 		*pcc)
{
   int found, i, index;

   /* check the template name specified for each forecast point
      for the roundup subsection; if not found, issue message */
   
   if (pcc->roundup.includeflag == TRUE)
   {
      for (index = 0; index < numfps; index++)
      {
	 i = 0;
	 found = FALSE;
	 while (found == FALSE && i < templatenames->roundup.number)
	 {
	    if (strcmp(pcc->roundup.template[index],
		       templatenames->roundup.name[i]) == 0) found = TRUE;
	    i++;
	 }
	 if (found == FALSE)
	 {
	    log_msg(ROUNDUP_TEMPLATE_NOTFOUND, pcc->roundup.template[index]);
	 }
      }
   }
   
   return;
}


/*********************************************************************
   verify_imptemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the impact subsection is a valid template name.

   
   NOTES
   
   ********************************************************************/
   
void verify_imptemplate(const	templatenames_struct 	*templatenames,
		            	pcc_struct 	 	*pcc)
{
   int found, i;
   
   /* check the template name specified for the impact subsection;
      if not found, issue message */
   
   if (pcc->impact.includeflag == TRUE)
   {
      i = 0;
      found = FALSE;
      while (found == FALSE && i < templatenames->impact.number)
      {
	 if (strcmp(pcc->impact.template,
		    templatenames->impact.name[i]) == 0) found = TRUE;
	 i++;
      }
      if (found == FALSE)
      {
	 log_msg(IMPACT_TEMPLATE_NOTFOUND, pcc->impact.template);
	 pcc->impact.includeflag = FALSE;
      }
   }

   return;
}


/*********************************************************************
   verify_cmptemplate()
   
   PURPOSE
   Verify that the template name specified in the pcc structure
   for the comparison subsection is a valid template name.

   
   NOTES
   
   ********************************************************************/
   
void verify_cmptemplate(const	templatenames_struct 	*templatenames,
		      	   	pcc_struct 		*pcc)
{
   int found, i;
   
   /* check the template name specified for the comparison subsection;
      if not found, issue message */
   
   if (pcc->comparison.includeflag == TRUE)
   {
      i = 0;
      found = FALSE;
      while (found == FALSE && i < templatenames->comparison.number)
      {
	 if (strcmp(pcc->comparison.template,
		    templatenames->comparison.name[i]) == 0) found = TRUE;
	 i++;
      }
      if (found == FALSE)
      {
	 log_msg(COMP_TEMPLATE_NOTFOUND, pcc->comparison.template);
	 pcc->comparison.includeflag = FALSE;
      }
   }

   return;
}


/*********************************************************************
   verify_ctatemplate()
   
   PURPOSE
   Verify that the template names specified in the pcc structure
   for the call-to-action section are valid names of templates.

   
   NOTES
   
   ********************************************************************/
   
void verify_ctatemplate(const	templatenames_struct 	*templatenames,
		           	pcc_struct  	 	*pcc)
{
   int found, i, index;
   /* check the template name specified for each call-to-action ;
      if not found, issue message */
   
   if (pcc->cta.includeflag == TRUE)
   {
      for (index = 0; index < pcc->cta.num_ctas; index++)
      {
	 i = 0;
	 found = FALSE;
	 while (found == FALSE && i < templatenames->cta.number)
	 {
	    if (strcmp(pcc->cta.template[index],
		       templatenames->cta.name[i]) == 0) found = TRUE;
	    i++;
	 }
	 if (found == FALSE) 
	 {
	    log_msg(CTA_TEMPLATE_NOTFOUND, pcc->cta.template[index]);
	    pcc->cta.includeflag = FALSE;
	 }
      }
   }

   return;
}


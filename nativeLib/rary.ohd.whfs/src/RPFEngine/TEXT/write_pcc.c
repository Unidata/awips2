/************************************************************************
   write_pcc_product.c

   write_pcc_product()
   write_pcc_header()
   write_pcc_summary()
   write_pcc_basis()
   write_pcc_tabular()
   write_pcc_roundup()
   write_pcc_impact()
   write_pcc_comparison()
   write_pcc_cta()
   
   PURPOSE   
   Writes the pcc information for the product to a file.
   
   MODIFICATION HISTORY
     FUNCTION            DATE    PROGRAMMER     DESCRIPTION
   write_pcc_headline()  04/2004 Jingtao Deng   add writing headline section
                                                to pcc files.
   write_pcc_summary()  04/2004  Jingtao Deng   remove writting prologueflag
                                                and HEADER_TEMPLATE into pcc
						files.						
   *********************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "write_pcc.h"

/************************************************************************
   write_pcc_product()
   
   PURPOSE
   Writes the pcc information for the product to a file.
   
   NOTES
   No function was added to rpf_converts to handle the
   conversion of the section index and subsection index to the
   corresponding string, even though some exist for the 
   reverse operation.  The function to convert the fporder index
   to a string is not used because it is intended for the gui display
   of the string.
   
   *********************************************************************/

void write_pcc_product(FILE 		*file_ptr,
		       pcc_struct 	*pcc,
		       int		numfps,
		       fp_struct	*fp,
		       misc_struct	*misc,
		       int		fps_switch)
{
   int i;
   char string[MAXLEN_STRING];
   int cnt, slen;
   
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     PRODUCT SECTION\n");
   fprintf(file_ptr, "#\n");
   
   
   /* write the section beginning line */ 
   
   fprintf(file_ptr, "SECTION: PRODUCT\n");
   
   
   /* write the product identifier and cccnnnxxx and nwr flag */
   
   fprintf(file_ptr, "PRODUCT_TYPE: %s\n", pcc->product.prod_categ);
   fprintf(file_ptr, "PRODUCT_ID: %s\n",   pcc->product.product_cnx);
   
   strcpy(string, convert_index_to_nwrflag(pcc->product.nwr_flag));
   fprintf(file_ptr, "NWR_FLAG: %s\n", string);
   
   
   /* write the segmentation mode option for forecast points */ 
   
   sprintf(string, "SEGMENT: %s",
      convert_index_to_segment_mode(pcc->product.segment_mode));
   fprintf(file_ptr, "%s\n", string);
      
   
   /* add to write VTEC product  (02/15/02) */
   
   strcpy(string, convert_index_to_vtecflag(pcc->product.vtec_flag));
   fprintf(file_ptr, "VTEC_FLAG: %s\n", string);    
   
   
   /* write VTEC OTE mode to pcc file */
   
   strcpy(string, convert_index_to_vtecOTEmode(pcc->product.vtec_cat));
   fprintf(file_ptr, "VTEC_OTEMODE: %s\n", string);
   
   
   /* write VTEC phenomena to pcc file */
   
   fprintf(file_ptr, "VTEC_PHENOM: %s\n", pcc->product.vtec_phenom);
   
   
   /* write VTEC default significance to pcc file */
   
   fprintf(file_ptr, "VTEC_DEFAULT_SIGNIF: %s\n", pcc->product.vtec_default_signif);
   
   
   /* write TIMEZONE_FLAG to pcc file */
   
   strcpy(string, convert_index_to_timezoneflag(pcc->product.timeZoneFlag));
   fprintf(file_ptr, "TIMEZONE_FLAG: %s\n", string);             
   
   
   /* write UGC_MODE to pcc file*/
   
   strcpy(string, convert_index_to_ugcmode(pcc->product.ugc_mode));
   fprintf(file_ptr, "UGC_MODE: %s\n", string);
   
   
   /* write EXPIRATION_TIME to pcc file*/
      
   sprintf(string, "%5.2f", pcc->product.expiration_time);
   if (string != NULL)
     fprintf(file_ptr, "EXPIRATION_TIME: %s\n", string);
   
   
   /* write the points to include if specified.
      if no point included, still need to output the line. */
   
   cnt = 0;
   if (fps_switch == TRUE)
   {
      strcpy(string, "INCLUDE_POINTS: ");
      for (i = 0; i < numfps; i++)
      {
	 if (misc->fps_included[i] == TRUE)
	 {
	    slen = strlen(string);
	    if (slen > 65)
	    {
	       strcat(string, " &\n");
	       fprintf(file_ptr, string);
	       strcpy(string, "");
	    }
	    strcat(string, fp[i].id);
	    strcat(string, ",");
	    cnt++;
	 }
      }
      
      if (cnt > 0)
      {
	 i = strlen(string);
	 strcpy(&string[i-1], "\0");  /* also get rid of last comma */
	 fprintf(file_ptr, "%s\n", string);
      }
      
      else
	 fprintf(file_ptr, "%s\n", string);
   }
   
   
   /* write the include sections line;
      get rid of the trailing comma and write the line */
   
   sprintf(string, "INCLUDE_SECTIONS: ");
   for (i = 0; i < pcc->product.num_sections; i++)
   {
      if (pcc->product.section_index[i]      == BASIS)
	 strcat(string, " BASIS,");
      else if (pcc->product.section_index[i] == HEADLINE)
         strcat(string, " HEADLINE,");	 
      else if (pcc->product.section_index[i] == SUMMARY)
	 strcat(string, " SUMMARY,");
      else if (pcc->product.section_index[i] == TABULAR)
	 strcat(string, " TABULAR,");
      else if (pcc->product.section_index[i] == POINT_SPECIFIC)
	 strcat(string, " POINT_SPECIFIC,");
      else if (pcc->product.section_index[i] == CALL_TO_ACTION)
	 strcat(string, " CALL_TO_ACTION,");
   }
   
   i = strlen(string);
   strcpy(&string[i - 1], "\0");
   fprintf(file_ptr, "%s\n", string);
   
   
   /* write the include subsections line;
      get rid of the trailing comma and write the line*/
   
   sprintf(string, "INCLUDE_SUBSECTIONS: ");
   for (i = 0; i < pcc->product.num_ps_subsections; i++)
   {
      if (pcc->product.ps_subsection_index[i]      == DATA_ROUNDUP)
	 strcat(string, " DATA_ROUNDUP,");
      else if (pcc->product.ps_subsection_index[i] == IMPACT_STATEMENT)
	 strcat(string, " IMPACT_STATEMENT,");
      else if (pcc->product.ps_subsection_index[i] == HISTORICAL_COMPARISON)
	 strcat(string, " HISTORICAL_COMPARISON,");
   }
   
   i = strlen(string);
   strcpy(&string[i - 1], "\0");
   fprintf(file_ptr, "%s\n", string);
   
   
   /* write the tabular within segment flag */
   
   if (pcc->product.tabular_within_flag == TRUE)
      fprintf(file_ptr, "TABULAR_WITHIN: YES\n");
   else
      fprintf(file_ptr, "TABULAR_WITHIN: NO\n");
   
   
   /* write the point-specific order option for forecast points */ 
   
   fprintf(file_ptr, "GRPFP_ORDER: %s\n",
                  convert_index_to_fporder(pcc->product.grpfp_order));
   
   
   
   /* write the product text case type */
   
   fprintf(file_ptr, "TEXT_CASE: %s\n", 
	   convert_index_to_textcase(pcc->product.tcase));
      
   
   /* write the endsection keyword to terminate  */
   
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}


/************************************************************************
   write_pcc_header()
   
   PURPOSE
   Writes the pcc information for the header section to a file.
   
   *********************************************************************/

void write_pcc_header(FILE 		*file_ptr,
		      pcc_struct 	*pcc)
{
     
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     HEADER SECTION\n");
   fprintf(file_ptr, "#\n");
      
   fprintf(file_ptr, "SECTION: HEADER\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->header.template);
   
   fprintf(file_ptr, "NWR_HEADER: ");
   fprintf(file_ptr, " %s  %d  %s  %s  %d  %d  %s\n",
	   pcc->header.nwr_msg_format,
	   pcc->header.nwr_periodicity,
	   pcc->header.nwr_active, pcc->header.nwr_delete,
	   pcc->header.nwr_confirm, pcc->header.nwr_interrupt,
	   convert_index_to_nwralert(pcc->header.nwr_alert_index));
   
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}
 

/************************************************************************
   write_pcc_headline()
   
   PURPOSE
   Writes the pcc information for the headline section to a file.
   
   *********************************************************************/

void write_pcc_headline(FILE 		*file_ptr,
		       pcc_struct 	*pcc)
{
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     HEADLINE SECTION\n");
   fprintf(file_ptr, "#\n");
   
   fprintf(file_ptr, "SECTION: HEADLINE\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->headline.template);
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}
/************************************************************************
   write_pcc_summary()
   
   PURPOSE
   Writes the pcc information for the summary section to a file.
   
   *********************************************************************/

void write_pcc_summary(FILE 		*file_ptr,
		       pcc_struct 	*pcc,
		       int		numgrps,
		       grp_struct	*grp,
		       misc_struct	*misc)
{
   int i;
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     SUMMARY SECTION\n");
   fprintf(file_ptr, "#\n");
      
   fprintf(file_ptr, "SECTION: SUMMARY\n");     

   
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->summary.default_template);
   
   
   /* if any groups use a different template, note that */
   
   for (i = 0; i < numgrps; i++)
   {
      if (misc->grps_included[i] == TRUE &&
	  strcmp(pcc->summary.default_template,
		 pcc->summary.template[i]) != 0)
	 fprintf(file_ptr, "SPECIAL_TEMPLATE: %s %s\n",
		 grp[i].id, pcc->summary.template[i]); 
   }
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}
  

/************************************************************************
   write_pcc_basis()
   
   PURPOSE
   Writes the pcc information for the basis section to a file.
   
   *********************************************************************/

void write_pcc_basis(FILE 		*file_ptr,
		     pcc_struct 	*pcc)
{
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     BASIS SECTION\n");
   fprintf(file_ptr, "#\n");
   
   fprintf(file_ptr, "SECTION: BASIS\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->basis.template);
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}


/************************************************************************
   write_pcc_tabular()
   
   PURPOSE
   Writes the pcc information for the tabular section to a file.
   
   *********************************************************************/

void write_pcc_tabular(FILE 		*file_ptr,
		       pcc_struct 	*pcc, 
		       int		numfps, 
		       fp_struct	*fp,
		       misc_struct	*misc)
{
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     TABULAR SECTION\n");
   fprintf(file_ptr, "#\n");
   
   fprintf(file_ptr, "SECTION: TABULAR\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->tabular.template);
         
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}
  

/************************************************************************
   write_pcc_roundup()
   
   PURPOSE
   Writes the pcc information for the roundup subsection to a file.
   
   *********************************************************************/

void write_pcc_roundup(FILE 		*file_ptr,
		       pcc_struct 	*pcc,
		       int		numfps,
		       fp_struct	*fp,
		       misc_struct 	*misc)
{
   int i;

   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     DATA ROUNDUP SECTION\n");
   fprintf(file_ptr, "#\n");
   
   fprintf(file_ptr, "SUBSECTION: DATA_ROUNDUP\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->roundup.default_template);
   
   
   /* if any points use a template different than
      the default, then note that */
   
   for (i = 0; i < numfps; i++)
   {
      if (misc->fps_included[i] && 
	  strcmp(pcc->roundup.default_template, pcc->roundup.template[i]) != 0)
	 fprintf(file_ptr, "SPECIAL_TEMPLATE: %s %s\n", fp[i].id, pcc->roundup.template[i]); 
   }
   
   fprintf(file_ptr, "ENDSUBSECTION:\n");
   return;
}
 

/************************************************************************
   write_pcc_impact()
   
   PURPOSE
   Writes the pcc information for the impact subsection to a file.
   
   *********************************************************************/

void write_pcc_impact(FILE 		*file_ptr,
		      pcc_struct 	*pcc,
		      int		numfps,
		      fp_struct		*fp,
		      misc_struct	*misc)
{
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     IMPACT STATEMENT SECTION\n");
   fprintf(file_ptr, "#\n");
   
   fprintf(file_ptr, "SUBSECTION: IMPACT_STATEMENT\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->impact.template);
            
   
   /* load in the reference stage/flow */
   
    if (pcc->impact.refstage_index == CUR_OBSSTAGE)
      fprintf(file_ptr, "REFERENCE_VALUE_TYPE: OBS\n");
    
   else if (pcc->impact.refstage_index == MAX_FCSTSTAGE)
      fprintf(file_ptr, "REFERENCE_VALUE_TYPE: MAXFCST\n");
    
   else if (pcc->impact.refstage_index == MAX_STAGE)
      fprintf(file_ptr, "REFERENCE_VALUE_TYPE: MAX\n");
   
    
   fprintf(file_ptr, "STAGE_WINDOW: %8.2f %8.2f\n", pcc->impact.stage_lwindow,
	   pcc->impact.stage_uwindow);
   
   fprintf(file_ptr, "FLDSTAGE_FILTER: %8.2f \n", 
	   pcc->impact.fs_filter_offset);
   
   fprintf(file_ptr, "FLOW_LOWER_WINDOW: %9.2f \n", 
                      pcc->impact.flow_lwindow);
      
   fprintf(file_ptr, "FLOW_UPPER_WINDOW: %9.2f \n", 
                      pcc->impact.flow_uwindow);
		      
   fprintf(file_ptr, "FLDFLOW_OFFSET_FILTER: %9.2f \n", 
	   pcc->impact.fq_filter_offset);
	   
   /* load in the search type.  these types must agree
      with those found in the rpf_convert functions!!! */
   
   if (pcc->impact.search_type ==  CLOSEST_IN_STGWINDOW)
      fprintf(file_ptr, "SEARCH_TYPE: CLOSEST_IN_STGWINDOW\n");
   
   else if (pcc->impact.search_type ==  HIGHEST_IN_STGWINDOW) 
      fprintf(file_ptr, "SEARCH_TYPE: HIGHEST_IN_STGWINDOW\n");
   
   else if (pcc->impact.search_type ==  BELOW_UPPER_STGWINDOW) 
      fprintf(file_ptr, "SEARCH_TYPE: BELOW_UPPER_STGWINDOW\n");
            
   fprintf(file_ptr, "ENDSUBSECTION:\n");
   
   return;
}


/************************************************************************
   write_pcc_comparison()
   
   PURPOSE
   Writes the pcc information for the comparison subsection to a file.
   
   *********************************************************************/

void write_pcc_comparison(FILE 		*file_ptr,
			  pcc_struct 	*pcc,
			  int 		numfps,
			  fp_struct	*fp,
			  misc_struct	*misc)
{
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     CREST COMPARISON SECTION\n");
   fprintf(file_ptr, "#\n");
   
   fprintf(file_ptr, "SUBSECTION: HISTORICAL_COMPARISON\n");
   fprintf(file_ptr, "TEMPLATE: %s\n", pcc->comparison.template);
            
   
   /* load in the search type */
   
   if (pcc->comparison.search_type == RECENT_IN_WINDOWS)
      fprintf(file_ptr, "SEARCH_TYPE: RECENT_IN_WINDOWS\n");
   
   else if (pcc->comparison.search_type ==  CLOSEST_IN_WINDOWS)
      fprintf(file_ptr, "SEARCH_TYPE: CLOSEST_IN_WINDOWS\n");
   
   else if (pcc->comparison.search_type ==  RECENT_IN_STGWINDOW)
      fprintf(file_ptr, "SEARCH_TYPE: RECENT_IN_STGWINDOW\n");
   
   else if (pcc->comparison.search_type ==  CLOSEST_IN_STGWINDOW)
      fprintf(file_ptr, "SEARCH_TYPE: CLOSEST_IN_STGWINDOW\n");
   
   else if (pcc->comparison.search_type ==  HIGHEST_IN_STGWINDOW) 
      fprintf(file_ptr, "SEARCH_TYPE: HIGHEST_IN_STGWINDOW\n");

   
   /* load in the reference stage/flow */
   
   if (pcc->comparison.refstage_index == CUR_OBSSTAGE)
      fprintf(file_ptr, "REFERENCE_VALUE_TYPE: OBS\n");
   
   else if (pcc->comparison.refstage_index == MAX_FCSTSTAGE)
      fprintf(file_ptr, "REFERENCE_VALUE_TYPE: MAXFCST\n");
   
   else if (pcc->comparison.refstage_index == MAX_STAGE)
      fprintf(file_ptr, "REFERENCE_VALUE_TYPE: MAX\n");
   
   
   fprintf(file_ptr, "STAGE_WINDOW: %8.2f %8.2f\n",
	   pcc->comparison.stage_lwindow, pcc->comparison.stage_uwindow);
   
   fprintf(file_ptr, "FLDSTAGE_FILTER: %8.2f \n", 
	   pcc->comparison.fs_filter_offset);
	   
   fprintf(file_ptr, "FLOW_LOWER_WINDOW: %9.2f \n", 
                      pcc->comparison.flow_lwindow);
      
   fprintf(file_ptr, "FLOW_UPPER_WINDOW: %9.2f \n", 
                      pcc->comparison.flow_uwindow);
		      
   fprintf(file_ptr, "FLDFLOW_OFFSET_FILTER: %9.2f \n", 
	   pcc->comparison.fq_filter_offset);	   
   
   fprintf(file_ptr, "TIME_WINDOW: %d\n", pcc->comparison.lookback_years);
            
   fprintf(file_ptr, "ENDSUBSECTION:\n");
   
   return;
}


/************************************************************************
   write_pcc_cta()
   
   PURPOSE
   Writes the pcc information for the call-to-action section to a file.
   
   *********************************************************************/
void write_pcc_cta(FILE 	*file_ptr,
		   pcc_struct 	*pcc)
{
   char string[MAXLEN_STRING];
   int i;
   
   fprintf(file_ptr, "#--------------------------------------\n");
   fprintf(file_ptr, "#     CALL-TO-ACTION SECTION\n");
   fprintf(file_ptr, "#\n");
   
   if (pcc->cta.num_ctas <= 0) return;
   
   fprintf(file_ptr, "SECTION: CALL_TO_ACTION\n");
   
   strcpy(string, "TEMPLATES: ");
   for (i = 0; i < pcc->cta.num_ctas; i++)
   {
      strcat(string, pcc->cta.template[i]);
      strcat(string, ",");
   }
   i = strlen(string);
   strcpy(&string[i-1], "\0");
   fprintf(file_ptr, "%s\n", string);
   
   
   /* write the skiplines flag */
   
   if (pcc->cta.skipline_flag == TRUE)
      fprintf(file_ptr, "SKIPLINE: YES\n");
   else
      fprintf(file_ptr, "SKIPLINE: NO\n");
        
   fprintf(file_ptr, "ENDSECTION:\n");
   
   return;
}

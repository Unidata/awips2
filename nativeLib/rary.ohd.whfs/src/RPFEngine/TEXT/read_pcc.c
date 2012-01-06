/**********************************************************************
   read_pcc.c
   
   read_pcc()
   init_pcc()
   read_pcc_product_info()
   read_pcc_header_info()
   read_pcc_summary_info()
   read_pcc_basis_info()
   read_pcc_tabular_info()
   read_pcc_cta_info()
   read_pcc_roundup_info()
   read_pcc_impact_info()
   read_pcc_comparison_info()
   
   get_include_points()
   get_include_ctas()
   set_includeflags()
   check_crestdate()
   
   MODIFICATION HISTORY  
     FUNCTION                 DATE    PROGRAMMER    DESCRIPTION
   read_pcc_headline_info()  04/2004 Jingtao Deng   read headline structure
                                                     from pcc files.
   read_pcc_summary_info()   04/2004 Jingtao Deng   remove reading summary
                                                    prologue flag and
						    HEADER_TEMPLATE						     
   *******************************************************************/

#include <string.h>                  /* string library functions */
#include <stdio.h>                   /* standard io library functions */
#include <stdlib.h>                  /* standard library functions */
#include <libgen.h>		     /* for basename() function */
#include <ctype.h>

#include "read_pcc.h"                /* function prototypes */


/*********************************************************************
   read_pcc_info()
   
   PURPOSE
   Read the information in the product content control file
   and load it into the structure.
   
   ********************************************************************/

void read_pcc(int		numfps,
	      fp_struct		*fp,
	      int		numgrps,
	      grp_struct 	*grp,
	      char		pcc_file[],
	      pcc_struct 	*pcc)
   
{
   FILE *file_ptr;
   char fileline[MAXLEN_STRING];
   char msgstr[200];
   char *keyword, *keyword_value;
   char *fgets_ptr;
   char  *basefilename;
   
   
   basefilename = basename((char *)pcc_file);
   sprintf(msgstr, "Reading pcc info from file: %s", basefilename);
   log_msg("", msgstr);
   
   
   /* open the pcc file */
   
   file_ptr = fopen(pcc_file, "r");
   if(file_ptr == NULL)
   {
     log_msg(FILE_OPENERR, pcc_file);
   }  
   
   /* initialize all the information in the pcc structure */
   
   init_pcc(numfps, numgrps, pcc);
   
   
   /* loop until the end of the file is reached */
   
   for(;;)
   {
      /* get a line from the input pcc file;
	 if end-of-file, then exit the loop, thereby exiting the function */      
      
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL) break;
      convert_str_to_upcase(fileline);      
      
      
      /* the only lines that are accepted by this function are comment
	 or blank lines and lines with a primary keyword definition;
	 if not either one, then error occurs;
	 check if the is comment or blank line */
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 if (keyword != NULL)
	 {
	    
	    /* get the section value using the string token function */
	    
	    keyword_value = strtok(NULL, " \t\n");
	    
	    if (keyword_value != NULL)
	    {
	       
	       /* process the info if the keyword is SECTION by 
		  calling the appropriate function to read the information
		  pertaining to the section */
	       
	       if (strcmp(keyword, "SECTION") == 0)
	       {    
		  if (strcmp(keyword_value, "PRODUCT") == 0 )
		     read_pcc_product_info(numfps, fp, file_ptr, pcc);
		  
		  else if (strcmp(keyword_value, "HEADER") == 0)
		     read_pcc_header_info(file_ptr, pcc);
		     
		  else if (strcmp(keyword_value, "HEADLINE") == 0)
		     read_pcc_headline_info(file_ptr, pcc);   
		  
		  else if (strcmp(keyword_value, "SUMMARY") == 0)
		     read_pcc_summary_info(numgrps, grp, file_ptr, pcc);
		  
		  else if (strcmp(keyword_value, "BASIS") == 0)
		     read_pcc_basis_info(file_ptr, pcc);
		  
		  else if (strcmp(keyword_value, "TABULAR") == 0)
		     read_pcc_tabular_info(numfps, fp, file_ptr, pcc);
		  
		  else if (strcmp(keyword_value, "CALL_TO_ACTION") == 0)
		     read_pcc_cta_info(file_ptr, pcc);
		  
		  else
		     log_msg(BADPRIMARYKEYWORD_VALUE, keyword_value);
	       }
	       
	       
	       /* process the info if the keyword is SUBSECTION */
	       
	       else if(strcmp(keyword, "SUBSECTION") == 0)
	       {
		  if (strcmp(keyword_value, "DATA_ROUNDUP") == 0)
		     read_pcc_roundup_info(numfps, fp, file_ptr, pcc);
		  
		  else if (strcmp(keyword_value, "IMPACT_STATEMENT") == 0)
		     read_pcc_impact_info(numfps, fp, file_ptr, pcc);
		  
		  else if (strcmp(keyword_value, "HISTORICAL_COMPARISON")  == 0)
		     read_pcc_comparison_info(numfps, fp, file_ptr, pcc);
		  
		  else
		     log_msg(BADPRIMARYKEYWORD_VALUE, keyword_value);
	       }
	       
	       /* if the keyword is not valid then issue error
		  message  */
	       else
		  log_msg(BADPRIMARY_KEYWORD, keyword);	 
	    }
	 }      
      }
      
   }
   
   /* close the pcc file */
     
   fclose(file_ptr);
         
   /* set a flag for each of the product section/subsections that
      indicates whether it is included in the product */
   
   set_includeflags(pcc);
   
   return;
}


/*********************************************************************
   init_pcc()
   
   PURPOSE
   Initializes the information in the pcc structure for all
   product sections/subsections.
   
   NOTES
   The include flags are not set in this function; they are set in the
   set_includeflags function.
   Editted by Hank Herr (1/11/2001) for segmentation
   
   MODIFICATION
   Add vtec_flag for VTEC product structure - Jingtao Deng (02/15/02)
   Initialize timeZoneFlag to FALSE - Erb (2/25/2002)
   Initialize vtec_cat for VTEC product mode O, T or E - Jingtao Deng (10/28/2003)
   Initialize ugc_flag and expiration_time - Jingtao Deng (Mar. 2004)
   
   ********************************************************************/

void init_pcc(int 		numfps,
	      int 		numgrps,
	      pcc_struct 	*pcc)
{
   int i;
   
   
   /* initialize the product-wide info;
      note that the default product type is an RVS */
   
   strcpy(pcc->product.prod_categ, "RVS");
   strcpy(pcc->product.product_cnx, "CCCCNNNXXX");  
   pcc->product.include_fp_flag    = FALSE;
   init_array(pcc->product.include_fp, TRUE, numfps);
   pcc->product.num_sections       = 0;
   pcc->product.num_ps_subsections = 0;
   init_array(pcc->product.section_index, MISSINGVAL, MAX_SECTIONS);
   init_array(pcc->product.ps_subsection_index, MISSINGVAL,
	      MAX_PS_SUBSECTIONS);
   pcc->product.tabular_within_flag = FALSE;
   pcc->product.grpfp_order        = ORDER_DEFAULT;
   pcc->product.tcase              = CASE_FORCEUPPER;
   
   pcc->product.nwr_flag      = FALSE;  
   pcc->product.segment_mode  = SEGMENT_MODE_NONE;
   pcc->product.vtec_flag     = FALSE;
   pcc->product.vtec_cat      = VTEC_CAT_OPERATIONAL ;
   strcpy(pcc->product.vtec_phenom, "FL");
   strcpy(pcc->product.vtec_default_signif, "W");
   
   pcc->product.timeZoneFlag  = FALSE;
   
   pcc->product.ugc_mode = UGC_BY_COUNTY;
   pcc->product.expiration_time = MISSINGVAL;
   
   
   /* initialize the header section info */
   
   strcpy(pcc->header.template, "DEFAULT");
   strcpy(pcc->header.nwr_msg_format, "T_ENG");
   pcc->header.nwr_periodicity = 0;
   strcpy(pcc->header.nwr_active, "A");
   strcpy(pcc->header.nwr_delete, "D");
   pcc->header.nwr_confirm     = 0;
   pcc->header.nwr_interrupt   = 0;
   pcc->header.nwr_alert_index = NWR_ALERT_NEITHER; 
   
   
   /* initialize the headline section info */
   
   strcpy(pcc->headline.template, "DEFAULT");
   
   /* initialize the summary section info */
     
   strcpy(pcc->summary.default_template, "DEFAULT");
   for (i = 0; i < numgrps; i++)
      strcpy(pcc->summary.template[i], "DEFAULT");
   
   
   /* initialize the basis section info */
   
   strcpy(pcc->basis.template, "DEFAULT");
   
   
   /* initialize the tabular section info */
   
   strcpy(pcc->tabular.template, "DEFAULT");
   
   
   /* initialize the call to action section info */
   
   pcc->cta.num_ctas = 0;
   pcc->cta.skipline_flag = TRUE;
    
   
   /* initialize the data roundup subsection info */
   
   strcpy(pcc->roundup.default_template, "DEFAULT");
   for (i = 0; i < numfps; i++)
      strcpy(pcc->roundup.template[i], "DEFAULT");
   
   
   /* initialize the impact statement subsection info */
   
   init_array(pcc->impact.range_set, FALSE, numfps);
   strcpy(pcc->impact.template, "DEFAULT");
   pcc->impact.refstage_index   = MAX_STAGE;
   pcc->impact.fs_filter_offset = -2.0;
   pcc->impact.stage_lwindow    = -2.0;
   pcc->impact.stage_uwindow    = +2.0;
   pcc->impact.search_type      = CLOSEST_IN_STGWINDOW;
   pcc->impact.fq_filter_offset = 10.0;
   pcc->impact.flow_lwindow    = 10.0;
   pcc->impact.flow_uwindow    = 10.0;
   
   /* initialize the historical comparison subsection info */
   
   init_array(pcc->comparison.compare_set, FALSE, numfps);
   strcpy(pcc->comparison.template, "DEFAULT");
   pcc->comparison.refstage_index   = MAX_STAGE;
   pcc->comparison.fs_filter_offset = -2.0;
   pcc->comparison.stage_lwindow    = -2.0;
   pcc->comparison.stage_uwindow    = +2.0;
   pcc->comparison.lookback_years   = 10;
   pcc->comparison.fq_filter_offset = 10.0;
   pcc->comparison.flow_lwindow    = 10.0;
   pcc->comparison.flow_uwindow    = 10.0;
   pcc->comparison.search_type      = CLOSEST_IN_WINDOWS;
   
   return;
}


/*********************************************************************
   read_pcc_product_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the productwide info and load it into the structure.
   
   NOTES
   Add reading product mode for VTEC (O,T,E) by Jingtao Deng (10/28/2003)
   
   ********************************************************************/

void read_pcc_product_info(int		numfps,
			   fp_struct	*fp,
			   FILE		*file_ptr,
			   pcc_struct	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   int i, j, startpos;
   int type_specified, id_specified, nwrflag_specified;
   int vtecflag_specified, timeZoneFlag_specified;
   
   
   /* set a flag indicating certain info not given */
   
   type_specified = id_specified = nwrflag_specified = FALSE;
   vtecflag_specified = timeZoneFlag_specified = FALSE;

   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Product section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 
	 /* process the info depending on the keyword value */
	 
	 /* load in the product type to use;
	    the value is passed onto the misc structure outside
	    of this process of reading the pcc info */
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "PRODUCT_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) != 3)
		     log_msg(BADPRODID, keyword_value);
		  else
		  {
		     strcpy(pcc->product.prod_categ, keyword_value);
		     type_specified = TRUE;
		  }
	       }  
	    } 
	    
	    
	    /* load in the product id - i.e. cccnnnxxx.
	       insist that the id be between 8-10 chars.
	       it should not be more than 9 characters for NWR products. */
	    
	    else if (strcmp(keyword, "PRODUCT_ID") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL && 
		   strlen(keyword_value) >= 8 &&
		   strlen(keyword_value) <= 10)
	       {
		  strcpy(pcc->product.product_cnx, keyword_value);
		  id_specified = TRUE;
	       }
	       else
		  log_msg(BADPRODCNX, keyword_value);
	       
	    }
	    
	    
	    /* set the points to be included in the product;
	       the values are passed onto the misc structure outside
	       of this process of reading the pcc info*/
	    
	    else if(strcmp(keyword, "INCLUDE_POINTS") == 0)
	    {
	       startpos = strlen(fileline) + 1;
	       get_include_points(numfps, fp, &fileline[startpos], file_ptr, 
				  pcc->product.include_fp);
	       pcc->product.include_fp_flag = TRUE;
	    }
	    
	    
	    /* load in the sections to include and their order */
	    
	    else if (strcmp(keyword, "INCLUDE_SECTIONS") == 0)
	    {
	       /* loop until no more sections are specified. */
	       
	       do
	       {
		  /* get the value(s) of the keyword. */
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  if (keyword_value != NULL) 
		  {
		     i = convert_sectionname_to_index(keyword_value);
		     if (i != MISSINGVAL)
		     {
			j = pcc->product.num_sections;
			
			if (pcc->product.num_sections + 1 <= MAX_SECTIONS)
			{
			   pcc->product.section_index[j] = i;
			   ++pcc->product.num_sections;
			}
			else
			   log_msg(BADKEYWORD_VALUE, keyword);
		     }
		  }
	       } while(keyword_value != NULL);	    
	    }
	    
	    
	    /* load in the point-specific subsections to include */ 
	    
	    else if (strcmp(keyword, "INCLUDE_SUBSECTIONS") == 0)
	    {
	       /* loop until no more subsections are specified. */
	       
	       do
	       {
		  /* get the value(s) of the keyword using the
		     string token function. */
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  if (keyword_value != NULL) 
		  {
		     i = convert_subsectionname_to_index(keyword_value);
		     if (i != MISSINGVAL)
		     {
			j = pcc->product.num_ps_subsections;
			
			if (pcc->product.num_ps_subsections + 1 <=
			    MAX_PS_SUBSECTIONS)
			{
			   pcc->product.ps_subsection_index[j] = i;
			   ++pcc->product.num_ps_subsections;
			}
			else
			   log_msg(BADKEYWORD_VALUE, keyword);
		     }
		  }
	       } while(keyword_value != NULL);
	    }
	    
	    
	    /* load in the tabular within segment flag */
	    
	    else if (strcmp(keyword, "TABULAR_WITHIN") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       
	       if (keyword_value != NULL && strcmp(keyword_value, "YES") == 0)
		  pcc->product.tabular_within_flag = TRUE;
	       else
		  pcc->product.tabular_within_flag = FALSE;
	       
	    }
	    
	    
	    /* load in the point-specific order option for forecast points */ 
	    
	    else if (strcmp(keyword, "GRPFP_ORDER") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  pcc->product.grpfp_order = 
		     convert_fporder_to_index(keyword_value);
	       } 
	    }
	    
	    
	    /* load in the case type for product text */ 
	    
	    else if (strcmp(keyword, "TEXT_CASE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  pcc->product.tcase = convert_textcase_to_index(keyword_value);
	       }
	    }
	    
	    
	    /* load in the nwr flag */
	    
	    else if (strcmp(keyword, "NWR_FLAG") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) < 0)
		     log_msg(BADNWRFLAG, keyword_value);
		  else
		  {
		     pcc->product.nwr_flag = convert_nwrflag_to_index(keyword_value);
		     nwrflag_specified = TRUE;
		  }
	       }	
	    }
	    
	    
	    /* load in the segmentation mode */ 
	    
	    else if (strcmp(keyword, "SEGMENT") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  pcc->product.segment_mode = 
		     convert_segment_mode_to_index(keyword_value);
	       } 
	    }
	    
	    
	    /* load in the master VTEC switch */
	    
	    else if (strcmp(keyword, "VTEC_FLAG") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) < 0)
		     log_msg(BADVTECFLAG, keyword_value);
		  else
		  {
		     pcc->product.vtec_flag = 
			convert_vtecflag_to_index(keyword_value);
		     vtecflag_specified = TRUE;
		  }
	       }	
	    }    		 
	    
	    
	    /* read in product mode (O,T,E) for VTEC */
	    
	    else if (strcmp(keyword, "VTEC_OTEMODE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {	            
		  pcc->product.vtec_cat = 
		     convert_vtecOTEmode_to_index(keyword_value);
	       }
	       else
	       {
		  log_msg(BAD_OTE_MODE, "");
	       } 	  
	    }
	    
	    /* read in phenom code for VTEC */
	    
	    else if (strcmp(keyword, "VTEC_PHENOM") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL && 
		   strlen(keyword_value) == VTEC_PHENOM_LEN) 
	       {	            
		  strcpy(pcc->product.vtec_phenom, keyword_value);
	       }
	       else
	       {
		  log_msg(BAD_PHENOM_CODE, "");
	       } 	  
	    }
	    
	    
	    /* read in default significance code for VTEC */
	    
	    else if (strcmp(keyword, "VTEC_DEFAULT_SIGNIF") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL && 
		   strlen(keyword_value) == VTEC_SIGNIF_LEN) 
	       {	            
		  strcpy(pcc->product.vtec_default_signif, keyword_value);
	       }
	       else
	       {
		  log_msg(BAD_SIGNIF_CODE, "");
	       } 	  
	    }
	    
	    /* read in if an individual station's time zone should be used */
	    
	    else if (strcmp(keyword, "TIMEZONE_FLAG") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) < 0)
		     log_msg(BADTIMEZONEFLAG, keyword_value);
		  else
		  {
		     pcc->product.timeZoneFlag = 
			convert_timezoneflag_to_index(keyword_value);
		     timeZoneFlag_specified = TRUE;
		  }
	       }  
	    }
	    
	    /*read in UGC mode */
	    
	    else if (strcmp(keyword, "UGC_MODE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
	          if (strlen(keyword_value) < 0)
		     log_msg(BAD_UGC_MODE, keyword_value);
		  else
		  {
		     pcc->product.ugc_mode = 
		              convert_ugcmode_to_index(keyword_value);
		  }
	       }
	    }
	    
	    /*read in product expiration look-forward time*/
	    
	    else if (strcmp(keyword, "EXPIRATION_TIME") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {	          
	          if (strlen(keyword_value) < 0)		  
		     log_msg(BAD_EXPTIME, keyword_value);		     		     		     		     
		     
		  else 
		     pcc->product.expiration_time = atof(keyword_value);
		                 
               }
	    }   				     
	       	  	         
	    /* use the endsection keyword to terminate the loop */
	    
	    else if(strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error  */
	    
	    else
	       log_msg(BADKEYWORD, keyword);	    
	 }
      }
   }
   
   
   /* log a message if certain fields not specified */
   
   if (type_specified == FALSE)
      log_msg(NO_PRODUCTTYPE, "");
   
   if (id_specified == FALSE)
      log_msg(NO_PRODUCTID, "");
   
   if (nwrflag_specified == FALSE)
      log_msg(NO_NWRFLAG, "");
   
   if (vtecflag_specified == FALSE)   
      log_msg(NO_VTECFLAG, "");         
   
   if (timeZoneFlag_specified == FALSE)   
      log_msg(NO_TIMEZONEFLAG, "");
   
   
   return;
}


/*********************************************************************
   read_pcc_header_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the header info and load it into the structure.
   
   ********************************************************************/

void read_pcc_header_info(FILE		*file_ptr,
			  pcc_struct	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   int	value;
   
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Header section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 
	 /* process the info depending on the keyword value */
	 /* load in the template to use */
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		     strcpy(pcc->header.template, keyword_value);
	       }	  
	    }
	    
	    
	    /* set the NWR header attributes */
	    
	    else if (strcmp(keyword, "NWR_HEADER") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  
		  if (strlen(keyword_value) == NWR_FORMAT_LEN)
		     strcpy(pcc->header.nwr_msg_format, "T_ENG");
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for message format");
		  
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  
		  if (keyword_value != NULL && strlen(keyword_value) > 0)
		  {
		     value = atoi(keyword_value);
		     if (value >= 0 && value < 1440)
			pcc->header.nwr_periodicity = value;
		     else
			log_msg(INVALID_NWR_HEADERINFO, "for periodicity");
		  }		  
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for periodicity");
		  
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  
		  if (strlen(keyword_value) == CODE_LEN && 
		      keyword_value != NULL)
		     strcpy(pcc->header.nwr_active, keyword_value);
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for active");
		  
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  
		  if (strlen(keyword_value) == CODE_LEN &&
		      keyword_value != NULL)
		     strcpy(pcc->header.nwr_delete, keyword_value);
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for delete");
		  
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  
		  if (strlen(keyword_value) > 0 &&
		      keyword_value != NULL)
		  {
		     value = atoi(keyword_value);
		     if (value == 0 || value == 1)
			pcc->header.nwr_confirm = value;
		     else
			log_msg(INVALID_NWR_HEADERINFO, "for confirm");
		  }
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for confirm");
		  
		  
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  
		  if (strlen(keyword_value) > 0 &&
		      keyword_value != NULL)
		  {
		     value = atoi(keyword_value);
		     if (value == 0 || value == 1)
			pcc->header.nwr_interrupt = value;
		     else
			log_msg(INVALID_NWR_HEADERINFO, "for interrupt");
		  }
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for interrupt");
		  
		  
		  keyword_value = strtok(NULL, ", \t\n");
		  if (strlen(keyword_value) > 0 &&
		      keyword_value != NULL)
		     pcc->header.nwr_alert_index = 
			convert_nwralert_to_index(keyword_value);
		  else
		     log_msg(INVALID_NWR_HEADERINFO, "for alert");
		  
	       }	   
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if(strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 } 
      }
   }
   
   return;
}


/*********************************************************************
   read_pcc_headline_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the headline info and load it into the structure.
   
   ********************************************************************/
void read_pcc_headline_info(FILE	*file_ptr,
			 pcc_struct  	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Headline section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */
	 
	 /* load in the template to use */
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		     strcpy(pcc->headline.template, keyword_value);
	       }  	   
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if (strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error  */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 }     	 
      }
   }
   
   return;
}

/*********************************************************************
   read_pcc_summary_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the summary info and load it into the structure.
   
   ********************************************************************/
void read_pcc_summary_info(int		numgrps,
			   grp_struct	*grp,
			   FILE		*file_ptr,
			   pcc_struct  	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   int i;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Summary section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */
	 
	 /* load in the default template to use for the groups */
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		  {
		     if (strstr(keyword_value, "PROLOGUE") != NULL)
			log_msg(INVALID_SUMMARY_TEMP, keyword_value);
		     else
		     {
			strcpy(pcc->summary.default_template, keyword_value);
			for(i = 0; i < numgrps; ++i)
			   strcpy(pcc->summary.template[i], keyword_value);
		     }
		  }
	       }	
	    }
	    
	    	    
	    /* read the names of any special templates for
	       particular forecast groups. the template can not
	       have the phrase prologue in it */
	    
	    else if (strcmp(keyword, "SPECIAL_TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", ");
	       if (keyword_value != NULL)
	       {
		  i = convert_grpid_to_index(keyword_value, numgrps, grp);
		  if (i != MISSINGVAL)
		  {
		     keyword_value = strtok(NULL, " \n");
		     if (keyword_value != NULL)
		     {
			if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
			   log_msg(EXCEED_TEMPLATENAME, keyword_value);
			else
			{
			   if (strstr(keyword_value, "PROLOGUE") != NULL)
			      log_msg(INVALID_SUMMARY_TEMP, keyword_value);
			   else
			      strcpy(pcc->summary.template[i], keyword_value);
			}
		     }  
		  }
	       }	 
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if (strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 } 
      }
   }
   
   return;
}


/*********************************************************************
   read_pcc_basis_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the basis info and load it into the structure.
   
   ********************************************************************/
void read_pcc_basis_info(FILE		*file_ptr,
			 pcc_struct  	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Basis section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */
	 
	 /* load in the template to use */
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		     strcpy(pcc->basis.template, keyword_value);
	       }  	   
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if (strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error  */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 }     	 
      }
   }
   
   return;
}


/*********************************************************************
   read_pcc_tabular_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the tabular info and load it into the structure.
   
   ********************************************************************/
void read_pcc_tabular_info(int		numfps,
			   fp_struct	*fp,
			   FILE		*file_ptr,
			   pcc_struct  	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Tabular section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */
	 /* load in the template to use */
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		     strcpy(pcc->tabular.template, keyword_value);
	       } 	   
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if(strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 }     
      }
   }
   return;
}


/*********************************************************************
   read_pcc_cta_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the callto action info and load it into the structure.
   
   ********************************************************************/
void read_pcc_cta_info(FILE		*file_ptr,
		       pcc_struct	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {	 
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Call-to-action section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */	 
	 /* load in the templates to use*/
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATES") == 0)
	       get_include_ctas(fileline, pcc);
	    
	    
	    /* load in the skip line between multiple templates flag */
	    
	    else if (strcmp(keyword, "SKIPLINE") == 0)
	    {
	       keyword_value = strtok(NULL, ", \t\n");
	       
	       if (keyword_value != NULL && strcmp(keyword_value, "YES") == 0)
		  pcc->cta.skipline_flag = TRUE;
	       else
		  pcc->cta.skipline_flag = FALSE;
	       
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if(strcmp(keyword, "ENDSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error  */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	    
	 }
      }
   }
   
   return;
}


/*********************************************************************
   read_pcc_roundup_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the data roundup info and load it into the structure.
   
   ********************************************************************/
void read_pcc_roundup_info(int		numfps,
			   fp_struct 	*fp,
			   FILE		*file_ptr,
			   pcc_struct	*pcc)
{
   char fileline[MAXLEN_STRING];
   char *fgets_ptr;
   char *keyword, *keyword_value;
   int i;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Data Roundup subsection");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */	 
	 /* load in the template to use*/
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		  {
		     strcpy(pcc->roundup.default_template, keyword_value);
		     for(i = 0; i < numfps; ++i)
			strcpy(pcc->roundup.template[i], keyword_value);
		  }
	       }	
	    }
	    
	    
	    /* read the names of any special templates for
	       particular forecast points */
	    
	    else if (strcmp(keyword, "SPECIAL_TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, ", ");
	       if (keyword_value != NULL)
	       {
		  i = convert_fpid_to_index(keyword_value, numfps, fp);
		  if (i != MISSINGVAL)
		  {
		     keyword_value = strtok(NULL, " \n");
		     if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
			log_msg(EXCEED_TEMPLATENAME, keyword_value);
		     else
			strcpy(pcc->roundup.template[i], keyword_value);
		  }
	       }	
	    }
	    
	    
	    /* use the endsection keyword to terminate the loop */
	    
	    else if(strcmp(keyword, "ENDSUBSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 } 
      }
   }
   
   return;
}


/*********************************************************************
   read_pcc_impact_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the impact statement info and load it into the structure.
   
   ********************************************************************/
void read_pcc_impact_info(int		numfps,
			  fp_struct 	*fp,
			  FILE		*file_ptr,
			  pcc_struct	*pcc)
{
   char 	fileline[MAXLEN_STRING];
   char 	*fgets_ptr;
   char 	*keyword, *keyword_value;
   int 		i;
   float 	f1, f2;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Impact Statement subsection");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */	 
	 
	 /* load in the template to use*/
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		     strcpy(pcc->impact.template, keyword_value);
	       }  	   
	    }
	    
	    
	     /* read the reference stage type to use for determining
	       which stage value to use as a reference. Note only read this
	       keyword, not write to pcc file since use REFERENCE_VALUE_TYPE
	       replace it */
	    
	    else if (strcmp(keyword, "REFERENCE_STAGE_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  pcc->impact.refstage_index = 
		     convert_refstage_to_index(keyword_value);
	       }	   
	    }
	    
	    /* read the reference value type to use for determining
	       which stage/flow value to use as a reference */
	    
	    else if (strcmp(keyword, "REFERENCE_VALUE_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  pcc->impact.refstage_index = 
		     convert_refstage_to_index(keyword_value);
	       }	   
	    }
	    
	    
	    /* read the offset from the flood stage below which
	       a reference stage will not be considered as having
	       an impact statement associated with it */
	    
	    else if (strcmp(keyword, "FLDSTAGE_FILTER") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  f1 = atof(keyword_value);
		  if (f1 >= -10.0 && f1 <= 0.0)
		     pcc->impact.fs_filter_offset = f1;
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
	       } 	   
	    }
	    
	    
	    /* read the type of search to use */
	    
	    else if (strcmp(keyword, "SEARCH_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  i = convert_searchtype_to_index(keyword_value);
		  pcc->impact.search_type = i;
	       }	
	    }
	    
	    
	    /* read the stage window to use for determining the
	       window around the given stage that is used to define
	       the range of previous stages which are considered comparable */
	    
	    else if (strcmp(keyword, "STAGE_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  f1 = atof(keyword_value);
		  keyword_value = strtok(NULL, " \t\n");
		  if (keyword_value != NULL)
		  {
		     f2 = atof(keyword_value);
		     if ((f1 >= -5.0 && f1 <= 0.0) && (f2 >= 0.0 && f2 <= 5.0))
		     {
			pcc->impact.stage_lwindow = f1;
			pcc->impact.stage_uwindow = f2;
		     }
		     else
			log_msg(BADKEYWORD_VALUE, keyword_value);
		  }
	       } 	    
	    }
	    
	    /* read the flow percentage window to use for determining the
	            window around the given flow that is used to define
	           the range of previous flow which are considered comparable */
	    
	    else if (strcmp(keyword, "FLOW_LOWER_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {		 
		  f1 = atof(keyword_value);
		  
		  if (f1 >= 0.0 && f1 <= 100.0)		     
		     pcc->impact.flow_lwindow = f1;				     
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
		  
	       } 	    
	    }
	    
	    else if (strcmp(keyword, "FLOW_UPPER_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {		 
		  f2 = atof(keyword_value);
		  
		  if (f2 >= 0.0)		     
		     pcc->impact.flow_uwindow = f2;				     
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
		  
	       } 	    
	    }
	    /* read the offset from the flood flow below which
	             a reference stage will not be considered as having
	             an impact statement associated with it */
	    
	    else if (strcmp(keyword, "FLDFLOW_OFFSET_FILTER") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  f1 = atof(keyword_value);
		  if (f1 >= 0.0 && f1 <= 100.0)
		     pcc->impact.fq_filter_offset = f1;
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
	       } 	   
	    }
	    /* use the endsection keyword to terminate the loop */
	    
	    else if(strcmp(keyword, "ENDSUBSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error  */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	    
	 } 
      }
   }
   
   return;
}


/*********************************************************************
   read_pcc_comparison_info()
   
   PURPOSE
   Read the information in the product content control file
   relating to the historical comparison info and load it into the structure.
   
   ********************************************************************/
void read_pcc_comparison_info(int		numfps,
			      fp_struct		*fp,
			      FILE		*file_ptr,
			      pcc_struct 	*pcc)
{
   char 	fileline[MAXLEN_STRING];
   char 	*fgets_ptr;
   char 	*keyword, *keyword_value;
   int 		i;
   float 	f1, f2;
   
   /* loop until reach the ENDSECTION keyword */
   
   for(;;)
   {
      /* get a line from the input pcc file; if end-of-file without
	 hitting ENDSECTION keyword, then log error */
            
      fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
      if (fgets_ptr == NULL)
      {
	 log_msg(ENDSECTION_KEYWORD_MISSING, "for Comparison section");
	 return;
      }
      convert_str_to_upcase(fileline);      
      
      if (fileline[0] != COMMENT_SYMBOL && fileline[0] != '\n')
      { 
         /* find the location of the character which defines the
	    end of the keyword; if keyword not specified or not found
	    issue an error message */
	 
	 keyword = strtok(fileline, ":\n");
	 
	 /* process the info depending on the keyword value */	 
	 
	 /* load in the template to use*/
	 
	 if (keyword != NULL)
	 {
	    if (strcmp(keyword, "TEMPLATE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  if (strlen(keyword_value) > MAXLEN_TEMPLATENAME)
		     log_msg(EXCEED_TEMPLATENAME, keyword_value);
		  else
		     strcpy(pcc->comparison.template, keyword_value);
	       }   	   
	    }
	    
	     /* read the reference stage to use for determining
	       which stage value to use as a reference for comparison.
	       Note only read this
	       keyword, not write to pcc file since use REFERENCE_VALUE_TYPE
	       replace it */
	    
	    else if (strcmp(keyword, "REFERENCE_STAGE_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  i = convert_refstage_to_index(keyword_value);
		  pcc->comparison.refstage_index = i;
	       } 	
	    }
	    
	    /* read the reference value to use for determining
	       which stage/flow value to use as a reference for comparison */
	    
	    else if (strcmp(keyword, "REFERENCE_VALUE_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  i = convert_refstage_to_index(keyword_value);
		  pcc->comparison.refstage_index = i;
	       } 	
	    }
	    
	    
	    /* read the offset from the flood stage below which
	       a reference stage will not be considered as having
	       an comparison statement associated with it */
	    
	    else if (strcmp(keyword, "FLDSTAGE_FILTER") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  f1 = atof(keyword_value);
		  if (f1 >= -10.0 && f1 <= 0.0)
		     pcc->comparison.fs_filter_offset = f1;
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
	       } 	   
	    }
	    
	    
	    /* read the type of search to use */
	    
	    else if (strcmp(keyword, "SEARCH_TYPE") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  i = convert_searchtype_to_index(keyword_value);
		  pcc->comparison.search_type = i;
	       } 	
	    }
	    
	    
	    /* read the upper and lower stage window values to use for 
	       determining the window around the given stage that is used to 
	       define the range of historical stages which are 
	       considered comparable */
	    
	    else if (strcmp(keyword, "STAGE_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  f1 = atof(keyword_value);
		  keyword_value = strtok(NULL, " \t\n");
		  if (keyword_value != NULL)
		  {
		     f2 = atof(keyword_value);
		     if ((f1 >= -5.0 && f1 <= 0.0) && (f2 >= 0.0 && f2 <= 5.0))
		     {
			pcc->comparison.stage_lwindow = f1;
			pcc->comparison.stage_uwindow = f2;
		     }
		     else
			log_msg(BADKEYWORD_VALUE, keyword_value);
		  }     
	       }	   
	    }
	    
	    
	    /* read the time lookback used for determining the
	       years since the current date that a comparison crest
	       is considered */
	    
	    else if (strcmp(keyword, "TIME_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  i = atoi(keyword_value);
		  if (i >= 0 && i < 300)
		     pcc->comparison.lookback_years = i;
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
	       }		   
	    }
	    
	    /* read the flow percentage window to use for determining the
	            window around the given flow that is used to define
	           the range of previous flow which are considered comparable */
	    
	    else if (strcmp(keyword, "FLOW_LOWER_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {		 
		  f1 = atof(keyword_value);
		  
		  if (f1 >= 0.0 && f1 <= 100.0)		     
		     pcc->comparison.flow_lwindow = f1;				     
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
		  
	       } 	    
	    }
	    
	    else if (strcmp(keyword, "FLOW_UPPER_WINDOW") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {		 
		  f2 = atof(keyword_value);
		  
		  if (f2 >= 0.0)		     
		     pcc->comparison.flow_uwindow = f2;				     
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
		  
	       } 	    
	    }
	    /* read the offset from the flood flow below which
	             a reference stage will not be considered as having
	             an impact statement associated with it */
	    
	    else if (strcmp(keyword, "FLDFLOW_OFFSET_FILTER") == 0)
	    {
	       keyword_value = strtok(NULL, " \t\n");
	       if (keyword_value != NULL)
	       {
		  f1 = atof(keyword_value);
		  if (f1 >= 0.0 && f1 <= 100.0)
		     pcc->comparison.fq_filter_offset = f1;
		  else
		     log_msg(BADKEYWORD_VALUE, keyword_value);
	       } 	   
	    }
	
	    
	    /* use the endsection keyword to terminate the loop */ 
	    
	    else if(strcmp(keyword, "ENDSUBSECTION") == 0)
	       break;
	    
	    
	    /* if invalid keyword, then log error */
	    
	    else
	       log_msg(BADKEYWORD, keyword);
	 }     
      }
   }
   
   return;
}


/*********************************************************************
   get_include_points()
   
   PURPOSE
   Read the info in the pcc file relating to which forecast points
   to include for the particular section and load it into the
   structure.
   
   ********************************************************************/
void get_include_points(int		numfps,
			fp_struct	*fp,
			char 		*original_line,
			FILE		*file_ptr,
			int		include_fp[])
{
   char *token;
   int 	index;
   char *fgets_ptr;
   char fileline[MAXLEN_STRING];
   
   /* initialize the include forecast points array to be false */
   
   init_array(include_fp, FALSE, numfps);
   
   /* copy the original input line into the local variable fileline.
      this is needed because later in this function, if there are continuation
      lines, it will read the new lines into the fileline variable, and without
      a local variable, overwriting of memory may occur...note that the original
      line passed in is actually the pcc file with the INCLUDE_POINTS: part
      stripped off the front of it. */
   
   if (original_line != NULL)  
      strcpy(fileline, original_line);
   else
      return;
   
   
   /* get the first token from the line following the keyword
      specification */
   
   token = strtok(fileline, " ,\t\n");
   
   /* if the first token is the keyword ALL, set all points to
      be included */
   
   if (token != NULL)
   {
      if (strcmp(token, "ALL") == 0)
      {
	 for (index = 0; index < numfps; ++index)
	    include_fp[index] = TRUE;
      }
      
      /* if the first token is not the keyword all, then assume that
	 a list of forecast points follows */
      
      else
      {
	 do
	 {
	    /* get the index for the specified forecast point */
	    
	    index = convert_fpid_to_index(token, numfps, fp);
	    
	    
	    /* if the index is not missing, then a valid forecast 
	       point was found so set the include flag */
	    
	    if (index != MISSINGVAL) include_fp[index] = TRUE;
	    
	    
	    /* get the next token */
	    
	    token = strtok(NULL, " ,\t\n");
	    
	    
	    /* if the next token is a continuation symbol, then
	       read the next line and continue; note that this 
	       method of handling continuation lines is different 
	       than that used for the templates or impact statements.
	       this is to allow for an open-ended very long list */
	    
	    if (token != NULL)
	    {
	       if (token[0] == CONT_SYMBOL)
	       {	          
		  fgets_ptr = fgets(fileline, MAXLEN_STRING, file_ptr);
		  
		  convert_str_to_upcase(fileline);
		  
		  
		  /* get the first token on the new line */
		  
		  token = strtok(fileline, " ,\t\n");
	       }
	    }
	 } while (token != NULL);
      }
   }
   return;
}


/*********************************************************************
   get_include_ctas()
   
   PURPOSE
   Read the info in the pcc file relating to which call-to-action
   statements to include.
   
   ********************************************************************/

void get_include_ctas(char		fileline[],
		      pcc_struct	*pcc)
   
{
   char *template_name;
   int cnt;
   
   cnt = 0;
   
   /* loop on the number of entries still in the fileline and
      get the name of the template; if the template name is not 
      null, load it in and increment count; otherwise, exit function */
   
   for (;;)
   {
      template_name = strtok(NULL, " ,\n");
      if (template_name != NULL)
      {
	 if (strlen(template_name) > MAXLEN_TEMPLATENAME)
	    log_msg(EXCEED_TEMPLATENAME, template_name);
	 
         else
	 {
	    if (cnt + 1 <= MAX_CTAS)
	       strcpy(pcc->cta.template[pcc->cta.num_ctas++], template_name);
	    else
	    {
	       log_msg(EXCEED_MAXCTAS, template_name);
	       break;
	    }
	 }
	 
      }
      
      /* no more templates so quit loop */
      
      else
	 break;
   }
   
   return;
}


/*********************************************************************
   set_includeflags()
   
   PURPOSE
   Set the flags indicating whether a particular product 
   section/subsection is included in the product.
   
   ********************************************************************/

void set_includeflags(pcc_struct *pcc)
   
{
   int i;
   
   
   /* initialize */
   
   pcc->summary.includeflag    = FALSE;
   pcc->headline.includeflag   = FALSE;
   pcc->basis.includeflag      = FALSE;
   pcc->tabular.includeflag    = FALSE;
   pcc->roundup.includeflag    = FALSE;
   pcc->impact.includeflag     = FALSE;
   pcc->comparison.includeflag = FALSE;
   pcc->cta.includeflag        = FALSE;
   
   
   /* loop on the number of product sections included */
   
   for (i = 0; i < pcc->product.num_sections; i++)
   {
      if (pcc->product.section_index[i] == SUMMARY)
	 pcc->summary.includeflag = TRUE;
      
      else if (pcc->product.section_index[i] == HEADLINE)
         pcc->headline.includeflag = TRUE;	 
      
      else if (pcc->product.section_index[i] == BASIS)
	 pcc->basis.includeflag = TRUE;
      
      else if (pcc->product.section_index[i] == TABULAR)
	 pcc->tabular.includeflag = TRUE;
      
      else if (pcc->product.section_index[i] == CALL_TO_ACTION)
	 pcc->cta.includeflag = TRUE;
   }
   
   
   /* loop on the number of product subsections included */
   
   for (i = 0; i < pcc->product.num_ps_subsections; i++)
   {
      if (pcc->product.ps_subsection_index[i] == DATA_ROUNDUP)
	 pcc->roundup.includeflag = TRUE;
      
      else if (pcc->product.ps_subsection_index[i] == IMPACT_STATEMENT)
	 pcc->impact.includeflag = TRUE;
      
      else if (pcc->product.ps_subsection_index[i] == HISTORICAL_COMPARISON)
	 pcc->comparison.includeflag = TRUE;
   }
   
   return;
}


/***********************************************************************
   check_crestdate()
   
   PURPOSE
   Verifys the syntax of the date given as mm/dd/yyyy format.
   
   NOTES
   This code note used (since special crests no longer recognized
   in pcc file) but left in for possible future use.
   
   ***********************************************************************/
void check_crestdate(char dait[])
   
{
   int day, month, year;
   int numread;
   int error_flag;
   
   
   /* initialize */
   
   error_flag = FALSE;
   
   
   /* extract the day, month, and year fields */
   
   numread = sscanf(dait, "%d/%d/%d", &month, &day, &year);
   if (numread != 3)
   {
      log_msg(INVALID_DATE, dait);
      error_flag = TRUE;
   }
   
   if (day < 0 || day > 31 || month < 1 || month > 12 ||
       year < 1700 || year > 2100)
   {
      log_msg(INVALID_DATE, dait);
      error_flag = TRUE;
   }
   
   if (error_flag == TRUE) strcpy(dait, "01/01/1970");
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

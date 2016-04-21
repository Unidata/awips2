/*********************************************************************
   create_product.c    
   create_product()
   
   create_nwr_product()
   create_segment_product()
   create_regular_product()
   
   create_header_nwr()
   create_header_section()
   
   create_nwr_product_body()
   create_segment_product_body()
   create_regular_product_body()   
    
   create_segment_section()
   
   create_summary_body() 
   create_summary_body_county()
   
   create_basis_section()
   
   create_pointspecific_section()
   create_pointspecific_segment_section()
   create_roundup_subsection()
   create_impact_subsection()
   create_comparison_subsection()
     
   create_cta_section()

   get_impact_chosen()
   
   MODIFICATION HISTORY
   FUNCTION                  DATE     PROGRAMMER    DESCRIPTION
   create_headline_section() 04/2004  Jingtao Deng  Add it to create
                                                    headline section in the
                                                    product. This section is an
						    overview of info for fcst pts
						    based on events.
						   
   create_summary_prologue() 04/2004 Jingtao Deng  Remove this function.
   
   create_segment_section() 05/2004 Jingtao Deng  Add create_MND_datetime()
                                                  after	H-VTEC line or UGC line
						  if VTEC is not selected.

   most of functions        05/2004 Jingtao Deng  Add vtecinfo as argument.
   *******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */

#include "create_product.h" 

extern char paramdir[];
extern char productdir[];
extern char file_suffix[];

#define TEST_LINE     "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...\n\n"
#define TEST_LINE_END "THIS IS A TEST MESSAGE.  DO NOT TAKE ACTION BASED ON THIS.\n\n"

int last_is_bulletorindent = FALSE;

/*********************************************************************
   create_product()
   
   PURPOSE
   Create the selected river forecast product by managing the calls
   to each of the functions that create the various product
   sections.
      
   NOTES   
   The template information structure is allocated in this function
   so that every use of it in the subordinate functions (called by this
   function and that create the specific product sections) uses the same
   memory locations.  This is done even though there is no actual info
   in the structure that is shared between different subordinate 
   functions. This template includes memory that is allocated 
   dynamically to hold the conditional expression stack and the template
   phrases; this memory is freed when the the template information
   is being reloaded with new information.
   
   *******************************************************************/

void create_product(int			numfps,
		    fp_struct		*fp,
		    int			numgrps,
		    grp_struct		*grp,
		    int                 numcnty,
		    county_struct       *cnty,
		    vtecinfo_struct	*vtecinfo,
		    pcc_struct		*pcc,
		    misc_struct		*misc,
		    char		output_file[])  
{
   
   FILE 		*outfile_ptr;
   char 		filename[MAXLEN_FILENAME];
   template_info_struct *template_info = NULL;
   
   
   /* compute the detailed info for the group and county info.
      this is only needed for product creation purposes, so do it here. */
   
   compute_grp_county_full_info(fp, numgrps, grp, numcnty, cnty);
   
   
   /* always set which counties are included, even though this info
      is only used when segmenting by counties. */   
   
   check_cnty_included(cnty, numcnty, misc);
   
      
   /* update the system time.  only define this once for a given product 
      creation, to ensure that all product components have a
      consistent time.  */
   
   time(&misc->system_time);
   
   
   /* allocate the storage for the template info.
      initialize the number of conditions and phrases stored in
      the template info structure; this should be here so the 
      free memory calls realize there is nothing to free */
   
   template_info = (template_info_struct *)malloc(sizeof(template_info_struct));
   if (template_info == NULL) 
      log_msg(FAILED_MALLOC, "for template_info_struct in create_product");  
   template_info->num_conditions = template_info->num_phrases = 0;
   
   
   /* open the output product work file */
   
   sprintf(filename, "%s/%s.%s", productdir, output_file, file_suffix);
   outfile_ptr = fopen(filename, "w");
   if (outfile_ptr == NULL) log_msg(FILE_OPENERR, filename);
   
   
   /* generate the product using methods depending on whether the product is
      and NWR product, segmented product, or a regular product */ 
   
   if(pcc->product.nwr_flag)
      create_nwr_product(numfps, fp, numgrps, grp, numcnty, cnty, 
			 misc, vtecinfo, pcc,
			 outfile_ptr, template_info);
   
   else 
   {
      if (pcc->product.segment_mode != SEGMENT_MODE_NONE)
	 create_segment_product(numfps, fp, numgrps, grp, numcnty, cnty,
				vtecinfo, pcc, misc,
				outfile_ptr, template_info);
      
      else
	 create_regular_product(numfps, fp, numgrps, grp, numcnty, cnty,
				pcc, misc, vtecinfo,
				outfile_ptr, template_info);
      
      
      /* if in test or practice mode, add phrase at end of product */
      
      if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
      {
	 write_line_text(TEST_LINE_END, outfile_ptr);
      }      
   }
   
   
   /* close the output product file */
   
   fclose(outfile_ptr);
   
   
   /* free memory */ 
   
   free_template_memory(template_info);
   free(template_info);  
   
   
   return;   
} 


/*****************************************************************
   create_nwr_product()
   
   PURPOSE
   Create the product with the NOAA Weather Radio (NWR) structure
   
   **************************************************************/
void create_nwr_product(int                   numfps,
                        fp_struct             *fp,
			int                   numgrps,
			grp_struct            *grp,
			int                   numcnty,
			county_struct         *cnty,
			misc_struct           *misc,
			vtecinfo_struct       *vtecinfo,
                        pcc_struct            *pcc,
                        FILE                  *outfile_ptr,
			template_info_struct  *template_info)
{
   char             tower_wildcard = TRUE;
   int              unique_tower_cnt, tower_cnt;
   int		    num_products, product_cnt;
   int              loc_cnt, prod_index;
   NWRTransmitter   *nwrtransmitPtr = NULL, *ntransPtr = NULL;
   LocTransmit      *loctransPtr = NULL;
   char             where[300], outstr[500]; 
   
   
   /* check if the pcc definition specifies use of the wildcard
      tower product designation. */ 
   
   if (strcmp(&(pcc->product.product_cnx[6]), NWR_TOWER_WILDCARD) == 0)
      tower_wildcard = TRUE;
   else
      tower_wildcard = FALSE;
   
   
   /* if the wildcard was used, then consider all active towers
      for the wfo. */ 
   
   if (tower_wildcard)
   {      
      log_msg("", "Creating NWR products using wildcard approach...");
      
      
      /* use the almost-same where clause to get the data and to
	 count of unique product_codes. issue message if counts differ */
      
      sprintf(where,
	      " WHERE use_transmitter != 'F' AND call_sign IN "
	      " (SELECT distinct(call_sign) from loctransmit) ");
      
      unique_tower_cnt = count_unique_prodcodes(where);	 
      strcat(where, " ORDER BY call_sign ");		 
      nwrtransmitPtr= GetNWRTransmitter(where);
      
      if (nwrtransmitPtr == NULL)
	 tower_cnt = 0;
      else
	 tower_cnt = ListCount(&nwrtransmitPtr->list);
      
      if (tower_cnt == 0)
      {
	 sprintf(outstr, "%s No active NWR towers associated with these\n",
		 NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
	 sprintf(outstr, "%s forecast points.  Product not written.\n",
		 NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
	 
	 log_msg(NO_TOWERS_HSA, "");
	 num_products = 0;	 
      }
      
      else
      {
	 /* check that no duplicate product codes exist */
	 
	 if (unique_tower_cnt != tower_cnt)
	 {
	    sprintf(outstr,
		    "%s Duplicate product codes exist for transmitters!!\n",
		    NWR_COMMENT);
	    write_line_text(outstr, outfile_ptr);
	    
	    sprintf(outstr,
		    "%s There are %d transmitters, but only %d unique codes.\n", 
		    NWR_COMMENT, tower_cnt, unique_tower_cnt);	       
	    write_line_text(outstr, outfile_ptr);
	    
	    sprintf(outstr,
		    "%s Fix product code definitions in HydroBase ASAP!!\n%s\n", 
		    NWR_COMMENT, NWR_COMMENT);	       
	    write_line_text(outstr, outfile_ptr);
	    
	    log_msg(DUP_NWR_PRODCODES, "");
	 }
	 
	 sprintf(outstr,
		 "%s This file contains a section for each NWR tower considered.\n"
		 "%s Upon issuance, each tower product is split apart and issued.\n",
		 NWR_COMMENT, NWR_COMMENT);	    
	 write_line_text(outstr, outfile_ptr);
      }
   }  
   
   
   /* if no wildcard specified, then assume a specific tower's product
      code was specified.  if a match was not found, then issue
      an error message and do NOT write a product.  also, check here if 
      there are multiple towers with the same product codes */ 
   
   else	 
   {
      sprintf(where,
	      " WHERE transmit_prod_code='%s' AND use_transmitter != 'F' ",
	      &(pcc->product.product_cnx[6]));
      nwrtransmitPtr= GetNWRTransmitter(where);
      
      if (nwrtransmitPtr == NULL)
	 tower_cnt = 0;
      else
	 tower_cnt = ListCount(&nwrtransmitPtr->list);
      
      if (tower_cnt == 0)
      {
	 sprintf(outstr,
		 "%s Specified product code does not match any tower code.\n",
		 NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
	 sprintf(outstr,
		 "%s Product code is given as the last 3 characters of product id: %s\n",
		 NWR_COMMENT, pcc->product.product_cnx);
	 sprintf(outstr,
		 "%s Define the 3-character code for appropriate tower using HydroBase.\n",
		 NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
	 sprintf(outstr, "%s Product not written.\n", NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
	 log_msg(TOWERCODE_NOT_FOUND, pcc->product.product_cnx);
      }
      
      else
      {
	 log_msg("", "Creating NWR products using specific tower approach...");
	 if (tower_cnt > 1)
	 {
	    sprintf(outstr, "%s Duplicate product code for multiple towers!!\n",
		    NWR_COMMENT);
	    write_line_text(outstr, outfile_ptr);
	    
	    sprintf(outstr, "%s Only one product written.\n", NWR_COMMENT);
	    write_line_text(outstr, outfile_ptr);
	    
	    sprintf(outstr,
		    "%s Fix product code definitions in HydroBase ASAP!!\n%s\n", 
		    NWR_COMMENT, NWR_COMMENT);
	    write_line_text(outstr, outfile_ptr);
	    
	    log_msg(DUP_NWR_PRODCODES, pcc->product.product_cnx);
	    tower_cnt = 1;
	 }
	 
	 sprintf(outstr,
		 "%s This file contains a single product for the NWR tower specified\n"
		 "%s by virtue of the last three characters of the product id.\n",
		 NWR_COMMENT, NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
      }
   }
      
   
   /* get the location-transmitter association information */
   
   loctransPtr = GetLocTransmit("");
   
   /* check that locations are covered by towers. */
   if (tower_cnt > 0)
   {
      log_msg("", "Checking NWR location coverage...");     
      check_locs_covered(numfps, fp, misc, nwrtransmitPtr, loctransPtr,
			 outfile_ptr); 
   }
   
   /* set the first pointer and set the number of products for use later. */
   
   num_products = tower_cnt;
   
   if (nwrtransmitPtr != NULL)
      ntransPtr = (NWRTransmitter *) ListFirst(&nwrtransmitPtr->list);            
   
   
   /* loop on the number of products to possibly create a product for,
      based on the number of towers, and write the text to the output file. */
   
   for (product_cnt = 0; product_cnt < num_products; product_cnt++)
   {     
      /* if a nwr product, check how many locations are "in" this
	 tower area, and log important information to output file. */
      
      loc_cnt = check_numloc_in_tower(numfps, fp, misc, ntransPtr,
				      loctransPtr, outfile_ptr);
      
      
      /* build the product sections if at least one location for
	 this tower is included in this product. */
      
      if (loc_cnt > 0)
      {
	 prod_index = convert_prodid_to_index(pcc->product.prod_categ);	 
	 if (misc->issuance_set == 0)
	    determine_issuance_number(numfps, fp, prod_index, misc);
	    
	 if (misc->expire_set == MISSINGVAL || misc->expire_set == UGC_RELATIVE)
	    determine_expiretime(prod_index, misc, pcc);
	 
	 
	 /* create the header section accordingly */
	 
	 log_msg("", "Creating NWR header section...");
	 create_header_nwr(numfps, fp, numgrps, grp, pcc, misc,
			   ntransPtr, loctransPtr, tower_wildcard,
			   outfile_ptr);
	 
	 
	 /* define the special locations for the NWR reference data.
	    this is a klugy workaround to avoid having to pass this
	    arguments all through the functions in order to support
	    the opccasional need for the data later */	 
	 
	 misc->loctransPtr = loctransPtr;
	 misc->nwrtransPtr = ntransPtr;	
	 
	 
	 /* create the body of the product */
	 
	 create_nwr_product_body(numfps, numgrps, numcnty,
				 fp, grp, cnty, pcc, misc, vtecinfo,
				 outfile_ptr, template_info);
	 
	 
	 /* reset the convenience pointers since we moving on to
	    the next transmitter (i.e. tower) */
	 
	 misc->loctransPtr = NULL;
	 misc->nwrtransPtr = NULL;
      }
      
      
      /* get the next tower's info */
      
      ntransPtr = (NWRTransmitter *) ListNext(&ntransPtr->node);
      
   }  /* end of for loop on num products, i.e. towers */
      
   
   /* free memory */
   
   if (nwrtransmitPtr != NULL) FreeNWRTransmitter(nwrtransmitPtr);   
   if (loctransPtr    != NULL) FreeLocTransmit(loctransPtr);
   
   return;	 
}


/***********************************************************
   create_segment_product()
   
   PURPOSE
   Create product with a segment structure
   
   ***********************************************************/
void create_segment_product(int                   numfps,
			    fp_struct             *fp,
			    int                   numgrps,
			    grp_struct            *grp,
			    int                   numcnty,
			    county_struct         *cnty, 
			    vtecinfo_struct	  *vtecinfo,
			    pcc_struct            *pcc,
			    misc_struct           *misc,
			    FILE                  *outfile_ptr,
			    template_info_struct  *template_info)
{
    int	num_products, prod_index;
    
    
    /* not creating an NWR product so only one product is being created */
    
    num_products = 1;
    
    
    /* get relevant information */
    
    prod_index = convert_prodid_to_index(pcc->product.prod_categ);
    
    if (misc->issuance_set == 0)
       determine_issuance_number(numfps, fp, prod_index, misc);
    
    if (misc->expire_set == MISSINGVAL || misc->expire_set == UGC_RELATIVE)
       determine_expiretime(prod_index, misc, pcc);
    
    
    /* create the segment header section */
    
    log_msg("", "Creating header section...");
    create_header_section(numfps, fp, numgrps, grp, numcnty, cnty, 
			  pcc, misc,vtecinfo,template_info, outfile_ptr); 
    
    
    /* create the body of the segmented product */
    
    create_segment_product_body(numfps, fp, numgrps, grp,
				numcnty, cnty, vtecinfo,
				pcc, misc,
				outfile_ptr, template_info);
    
        
    return;	 
}


/********************************************************
   create_regular_product()
   
   PURPOSE
   Create product with a conventional structure
   
   *********************************************************/
void create_regular_product(int                   numfps,
			    fp_struct             *fp,
			    int                   numgrps,
			    grp_struct            *grp,
			    int                   numcnty,
			    county_struct         *cnty,                        
                            pcc_struct            *pcc,
			    misc_struct           *misc,
			    vtecinfo_struct       *vtecinfo,
			    FILE                  *outfile_ptr,
			    template_info_struct  *template_info)
{
   int	num_products, prod_index;
   
   
   /* not creating an NWR product so only one product is being created*/
   
   num_products = 1;
   
   
   /* get relevant information */
   
   prod_index = convert_prodid_to_index(pcc->product.prod_categ);
   
   if (misc->issuance_set == 0)
      determine_issuance_number(numfps, fp, prod_index, misc);
      
   if (misc->expire_set == MISSINGVAL || misc->expire_set == UGC_RELATIVE)
      determine_expiretime(prod_index, misc, pcc);
   
   
   /* create the header section */
   
   log_msg("", "Creating header section...");
   create_header_section(numfps, fp, numgrps, grp, numcnty, cnty, 
			 pcc, misc, vtecinfo, template_info, outfile_ptr); 
	
   
   /* create the body of the product */
   
   create_regular_product_body(numfps, numgrps, numcnty,
			       fp, grp, cnty, pcc, misc, vtecinfo,
			       outfile_ptr, template_info);
   
      
   return;	 
}


/*********************************************************************
   create_header_nwr()
   
   PURPOSE
   Create the header section of the NWR output product,. 
         
   *******************************************************************/
void create_header_nwr(int			numfps,
		       fp_struct		*fp,
		       int			numgrps,
		       grp_struct		*grp,
		       pcc_struct  		*pcc,
		       misc_struct		*misc,
		       NWRTransmitter		*ntransPtr,
		       LocTransmit		*loctransPtr,
		       int			tower_wildcard,
		       FILE 			*outfile_ptr) 
{
#define NWR_TIME_LEN   10
#define NWR_PERIOD_LEN 8

   struct tm 	*tm_ptr;
   time_t	expire_timet;
   int		num_hrs, num_mins;
   char msg_format[NWR_FORMAT_LEN + 1];
   char product_id[PRODUCT_LEN + 1];
   char create_time[NWR_TIME_LEN + 1];
   char effective_time[NWR_TIME_LEN + 1];
   char periodicity[NWR_PERIOD_LEN + 1];
   char mrd[] = "";                    /* blank mrd for now */
   char active_flag[CODE_LEN + 1];
   char delete_flag[CODE_LEN + 1];
   char confirm_flag[CODE_LEN + 1];
   char interrupt_flag[CODE_LEN + 1];
   char alert_tone[CODE_LEN + 1];
   char area_codes[MAXLEN_LONGSTR];
   char area_delimiter[] = "c";         /* fixed */
   char expire_time[NWR_TIME_LEN + 1];
   char	outstr[500];
   char formatstr[40];
   
   
   /* use system clock to define the creation date and the
      effective date. use silly formatstr to avoid SCCS pokage. */
   
   tm_ptr = gmtime(&misc->system_time);
   sprintf(formatstr, "%s", "%y%m%d");
   strcat(formatstr, "%H");
   strcat(formatstr, "%M");
   strftime(create_time, NWR_TIME_LEN + 1, formatstr, tm_ptr);
   
   strcpy(effective_time, create_time);
   
   
   /* set the expiration time. use this silly way
      to set up the format string to avoid SCCS pokage. */
   
   expire_timet = misc->expire_time; 
   tm_ptr = gmtime(&expire_timet);
   
   sprintf(formatstr, "%s", "%y%m%d");
   strcat(formatstr, "%H");
   strcat(formatstr, "%M");
   strftime(expire_time, NWR_TIME_LEN + 1, formatstr, tm_ptr);
   
   
   /* build the product id based on whether using the wildcard
      which requires filling in the special product code */
   
   strcpy(product_id, pcc->product.product_cnx);
   if (tower_wildcard)
      strncpy(&product_id[6], ntransPtr->transmit_prod_code, 3); 
   
   
   /* build the listening area codes, i.e. UGC string.
      provide the tower being considered and the list of 
      assocations between towers and locations. */
   
   build_ugc_nwr(numfps, fp, misc, ntransPtr, loctransPtr, area_codes);
   
   
   /* copy the strings that are ready to go */
   
   strcpy(msg_format,  pcc->header.nwr_msg_format);
   strcpy(active_flag, pcc->header.nwr_active);
   strcpy(delete_flag, pcc->header.nwr_delete);
   
   
   /* convert the booleans to strings */
   
   if (pcc->header.nwr_confirm)
      strcpy(confirm_flag, "C");
   else
      strcpy(confirm_flag, " ");
   
   if (pcc->header.nwr_interrupt)
      strcpy(interrupt_flag, "I");
   else
      strcpy(interrupt_flag, " ");
   
   
   /* load the settings that are index coded */
   
   if (pcc->header.nwr_alert_index == NWR_ALERT_BOTH)
      strcpy(alert_tone, "A");
   else if (pcc->header.nwr_alert_index == NWR_ALERT_SAME_ONLY)
      strcpy(alert_tone, " ");
   else
      strcpy(alert_tone, "N");
   
   
   /* convert the periodicity string to DDHHMMSS, it is stored in minutes.
      assume it is less than one day. */
   
   num_hrs  = pcc->header.nwr_periodicity / 60;
   num_mins = (pcc->header.nwr_periodicity - (num_hrs*60));
   if (num_hrs + num_mins == 0)   /* fixed 2-8-2000 */
      strcpy(periodicity, "");
   else      
      sprintf(periodicity, "00%2.2d%2.2d00", num_hrs, num_mins);
   
   
   /* create the header; the length of the header is
      5+9+10+10+8+mrd+5+codes+1+10 = 58+mrd+codes */
   
   sprintf(outstr, "%s\n", NWR_COMMENT);
   write_line_text(outstr, outfile_ptr);
   
   sprintf(outstr, "%-5s%-9s%-10s%-10s%-8s",
	   msg_format, product_id, create_time, effective_time, periodicity);
   write_line_text(outstr, outfile_ptr);
   
   sprintf(outstr, "%s", mrd);
   write_line_text(outstr, outfile_ptr);
   
   sprintf(outstr, "%-1s%-1s%-1s%-1s%-1s",
	   active_flag, delete_flag, confirm_flag, interrupt_flag, alert_tone);
   write_line_text(outstr, outfile_ptr);
   
   sprintf(outstr, "%s%-1s%-10s\n\n", area_codes, area_delimiter, expire_time);
   write_line_text(outstr, outfile_ptr);
   

   if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
   {
      write_line_text(TEST_LINE_END, outfile_ptr);
   }
   
   
   return;
}


/*********************************************************************
   create_header_section()
   
   PURPOSE
   Create the header section of the output product. 
   
   *******************************************************************/
void create_header_section(int			numfps,
			   fp_struct		*fp,
			   int			numgrps,
			   grp_struct		*grp,
			   int                  numcnty,
			   county_struct        *cnty,
			   pcc_struct  		*pcc,
			   misc_struct		*misc,
			   vtecinfo_struct      *vtecinfo,
			   template_info_struct	*template_info,
			   FILE 		*outfile_ptr)			    
{
   int 	template_type;
   char template_file[MAXLEN_FILENAME];
   char template_name[MAXLEN_TEMPLATENAME];
   int 	phrasenum;
   char *newphrase;
   int  product_type_line_found = FALSE;
   
   
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s", 
	   paramdir, HEADER_TEMPLATEFILE, misc->selected_office); 
   strcpy(template_name, pcc->header.template);
   template_type = PHRASE_WITH_SUBSTITUTION;
   
   
   /* get and load all the information for the template */
   
   buf_template(template_file, template_name, template_type, 
		misc->system_time, template_info);
   
   
   /* loop on each of the phrases and load all the data into
      the phrases for this template;  
      note that the id index is set to 0 since no substitution supported
      for the header section; then output the phrase itself */
   
   for (phrasenum = 0; phrasenum < template_info->num_phrases; ++phrasenum)
   {
      load_phrase_data(0, HEADER, fp, grp, numcnty, cnty, misc, vtecinfo, 
		       phrasenum, template_info, pcc, &newphrase); 
      
      
      /* if test or practice mode, prepend and append the product type line
	 with TEST line indicators.  assume that the first line without the
	 words bulletin or urgent or dash indicating a UGC is the product 
	 type line */
      
      if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat) && 
	  (strstr(newphrase, "BULLETIN") == NULL &&
	   strstr(newphrase, "URGENT")   == NULL &&
	   strstr(newphrase, "-")        == NULL &&
	   product_type_line_found == FALSE))
      {
	 write_phrase_text(pcc->product.tcase, "TEST...", outfile_ptr);	 
	 write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);      	 
	 write_phrase_text(pcc->product.tcase, "...TEST", outfile_ptr);
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 
	 product_type_line_found = TRUE;
      }
      
      else
      {
	 /* note that there is no support provided for bulleted or indented
	    strings in the header section. */
	 
	 write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);      
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
      }
   }

   
   /* put a trailing blank line automatically */
   
   write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
   
   
   /* add a test line after the header */
   
   if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
   {
      write_line_text(TEST_LINE, outfile_ptr);
   }
   
   return;  
}


/***************************************************************
   create_nwr_product_body()
   
   PURPOSE
   Create the product body with the sections and subsections,
   such as summary, basis, pointspecfic, CTA etc for a product
   with an NWR structure.
   
   ****************************************************************/
void create_nwr_product_body(int		  numfps,
                             int		  numgrps,
			     int                  numcnty,
		    	     fp_struct	          *fp,			     
			     grp_struct	          *grp,
			     county_struct        *cnty,
			     pcc_struct 	  *pcc,
			     misc_struct	  *misc,
			     vtecinfo_struct      *vtecinfo,
			     FILE		  *outfile_ptr,
			     template_info_struct *template_info)
{
   int	i;
   
   
   /* loop on the number of sections specified 
      and create the appropriate section */
   
   for (i = 0; i < pcc->product.num_sections; ++i)
   {
      /* build the summary section */ 
                  
      if (pcc->product.section_index[i] == SUMMARY && 
	  pcc->summary.includeflag == TRUE)
      {   	       		      
         /* create the summary body */
	 
	 log_msg("", "Creating summary body section...");
	 create_summary_body(fp, numgrps, grp, numcnty, cnty,  pcc, misc,
          	             vtecinfo,template_info, outfile_ptr, MISSINGVAL);
         
      }
      
      /* build the headline section */
      
      if (pcc->product.section_index[i] == HEADLINE && 
	  pcc->headline.includeflag == TRUE)
      {
         log_msg("", "Creating headline section...");
	 create_headline_section(numfps,fp, numgrps, grp, numcnty, cnty, 
	                            pcc, misc, vtecinfo, template_info, outfile_ptr); 
      }
      
      /* build the basis section */
      
      if (pcc->product.section_index[i] == BASIS &&
	  pcc->basis.includeflag == TRUE)
      {
	 log_msg("", "Creating basis section...");
	 create_basis_section(fp, pcc, misc, template_info, outfile_ptr); 
      }
      
      
      /* build the tabular section.  allow the tabular section to be 
	 included in the NWR products at the users risk.  it is available
	 to allow the flexibility provided by the PE-type variables. */ 
      
      else if (pcc->product.section_index[i] == TABULAR &&
	       pcc->tabular.includeflag == TRUE)
      {  
	 log_msg("", "Creating tabular section...");
	 create_tabular_section(numfps, fp, numgrps, grp, numcnty, cnty, 
				pcc, misc, vtecinfo, outfile_ptr);
      }   
      
      /* build the point_specific section */ 
      
      else if (pcc->product.section_index[i] == POINT_SPECIFIC)
      {
	 create_pointspecific_section(numfps, fp, numgrps, grp, numcnty, cnty, 
				      pcc, misc, vtecinfo, template_info, 
				      outfile_ptr);
      }  
      
      
      /* build the call-to-action section */
      
      else if (pcc->product.section_index[i] == CALL_TO_ACTION &&
	       pcc->cta.includeflag == TRUE)
      {
	 log_msg("", "Creating call-to-action section...");
	 create_cta_section(fp, pcc, misc, template_info, outfile_ptr);
      }                
   }  
   
   return;
}

	     
/***************************************************************
   create_segment_product_body()
   
   PURPOSE
   Create the product body such as summary,basis,pointspecfic,
   CAT etc for segment structure.
   
   ****************************************************************/
void create_segment_product_body(int		      	numfps,
		    		 fp_struct	      	*fp,     
                        	 int		      	numgrps,
				 grp_struct	      	*grp,
				 int                  	numcnty,
				 county_struct        	*cnty,
				 vtecinfo_struct	*vtecinfo,
				 pcc_struct 	      	*pcc,
				 misc_struct	      	*misc,
				 FILE		      	*outfile_ptr,
				 template_info_struct 	*template_info)
{          	 
   int	i;  
   int  segment_section_created = FALSE;
   int	grpindex_used = MISSINGVAL;
   
   
   /* if VTEC mode enabled, then load the info for the VTEC codes.
      this processes only the included areas, and assumes that the
      full time series data have already been
      loaded for those areas (i.e. pts, grps, or counties). */
   
   if (pcc->product.vtec_flag == TRUE)
   {
      load_vtecinfo_lines(numfps, fp, numgrps, grp, numcnty, cnty,
			 pcc, misc, vtecinfo);
   }
   
   /* loop on the number of sections specified 
      and create the included section */ 
   
   for (i = 0; i < pcc->product.num_sections; ++i)
   {
      /* build the summary section */
      
      if (pcc->product.section_index[i] == SUMMARY && 
	  pcc->summary.includeflag == TRUE)
      {  	 	 
         /* only create the summary body here if the segment mode is for
	    forecast points. if segmenting by forecast group or county,
	    the summary body is included inside the segment section. */
	 
         if (pcc->product.segment_mode == SEGMENT_MODE_POINT)
         {
	    /* the grpindex_used argument value tells it to produce
	       for all groups, i.e. not just for one specific group.  */
	    
	    log_msg("", "Creating summary body section...");
	    create_summary_body(fp, numgrps, grp, numcnty, cnty, pcc, 
	                        misc, vtecinfo, template_info, outfile_ptr,
				grpindex_used);
         }
      }
      
      /* build the headline section. the headline section is always placed
         outside of the segment */ 
      
      if (pcc->product.section_index[i] == HEADLINE &&
	  pcc->headline.includeflag == TRUE)
      {
	 log_msg("", "Creating headline section...");
	 create_headline_section(numfps, fp, numgrps, grp, numcnty, cnty, 
	                            pcc, misc, vtecinfo, template_info, outfile_ptr); 
      }
      
      
      /* build the basis section. the basis section is always placed
         outside of the segment */ 
      
      if (pcc->product.section_index[i] == BASIS &&
	  pcc->basis.includeflag == TRUE)
      {
	 log_msg("", "Creating basis section...");
	 create_basis_section(fp, pcc, misc, template_info, outfile_ptr); 
      }
      
      
            
      /* build the tabular section.  allow the tabular section to be 
	 included in the NWR products at the users risk.  it is available
	 to allow the flexibility provided by the PE-type variables. */
      
      else if (pcc->product.section_index[i] == TABULAR &&
	       pcc->tabular.includeflag == TRUE)
      {
         /* for segments, put the segment blocks where the
	    tabular section was requested, assuming the point-specific
	    sections was not already encountered. */
	    
         if (pcc->product.tabular_within_flag == TRUE)
         {	 
	 
	    if (segment_section_created == FALSE)
	    {
	       log_msg("", "Creating segment section at tabular location...");
	       create_segment_section(numfps, fp, numgrps, grp, 
			   	      numcnty, cnty, vtecinfo, 
				      pcc, misc,
				      outfile_ptr, template_info);
	       segment_section_created = TRUE;
	    }
	 }
	 
	 
         /* build the tabular section if instructed to be outside the segment. */ 
	    
	 else
	 {
	    log_msg("", "Creating tabular section...");
	    create_tabular_section(numfps, fp, numgrps, grp, numcnty, cnty, 
				   pcc, misc, vtecinfo, outfile_ptr);
 	 }
      }
      
      
      /* build the point_specific section */
      
      else if (pcc->product.section_index[i] == POINT_SPECIFIC)
      {	 
         /* for segments, put the segment blocks where the
	    point specific section was requested, unless the 
	    tabular section was already encountered */
	 
	 if (segment_section_created == FALSE)
	 {         
	    log_msg("", "Creating segment section at point-specific location...");
	    create_segment_section(numfps, fp, numgrps, grp,
				   numcnty, cnty, vtecinfo,
				   pcc, misc,
				   outfile_ptr, template_info);
	    segment_section_created = TRUE;
	 }   
      }
      
      
      /* build the call-to-action section */
      
      else if (pcc->product.section_index[i] == CALL_TO_ACTION &&
	       pcc->cta.includeflag == TRUE)
      {
	 log_msg("", "Creating call-to-action section...");
	 create_cta_section(fp, pcc, misc, template_info, outfile_ptr);
      }     
   }  
   
   return;
}


/****************************************************************
   create_regular_product_body()
   
   PURPOSE
   Create the product body such as summary, basis, pointspecfic,
   CTA etc for a product with a regular structure.
   
   ****************************************************************/
void create_regular_product_body(int		      numfps,
                                 int		      numgrps,
				 int                  numcnty,
		    		 fp_struct	      *fp,			     
				 grp_struct	      *grp,
				 county_struct        *cnty,
				 pcc_struct 	      *pcc,
				 misc_struct	      *misc,
				 vtecinfo_struct      *vtecinfo,
				 FILE		      *outfile_ptr,
				 template_info_struct *template_info)
{				  
   int	i;   
   
   /* loop on the number of sections specified 
      and create the appropriate section */
   
   for (i = 0; i < pcc->product.num_sections; ++i)
   {
      /* build the summary section */
      
      if (pcc->product.section_index[i] == SUMMARY && 
	  pcc->summary.includeflag == TRUE)
      {  	 	
         /* create summary body */
         
	 log_msg("", "Creating summary body section...");
	 create_summary_body(fp, numgrps, grp, numcnty, cnty, pcc,
			     misc, vtecinfo, template_info, outfile_ptr, MISSINGVAL);         
      }
      
      /* build the headline section */
      
      if (pcc->product.section_index[i] == HEADLINE &&
	  pcc->headline.includeflag == TRUE)
      {
	 log_msg("", "Creating headline section...");
	 create_headline_section(numfps, fp, numgrps, grp, numcnty, cnty, 
	                            pcc, misc, vtecinfo, template_info, outfile_ptr); 
      }
      
      /* build the basis section */
      
      if (pcc->product.section_index[i] == BASIS &&
	  pcc->basis.includeflag == TRUE)
      {
	 log_msg("", "Creating basis section...");
	 create_basis_section(fp, pcc, misc, template_info, outfile_ptr); 
      }
      
      
      /* build the tabular section.  allow the tabular section to be 
	 included in the NWR products at the users risk.  it is available
	 to allow the flexibility provided by the PE-type variables. */
      
      else if (pcc->product.section_index[i] == TABULAR &&
	       pcc->tabular.includeflag == TRUE)
      {         
	 log_msg("", "Creating tabular section...");
	 create_tabular_section(numfps, fp, numgrps, grp, numcnty, cnty, 
				pcc, misc, vtecinfo, outfile_ptr);       
      }
      
      
      /* build the point_specific section */
      
      else if (pcc->product.section_index[i] == POINT_SPECIFIC)
      {	 
	 create_pointspecific_section(numfps, fp, numgrps, grp, numcnty, cnty, 
				      pcc, misc, vtecinfo, template_info, 
				      outfile_ptr);
      } 
      
      /* build the call-to-action section */
      
      else if (pcc->product.section_index[i] == CALL_TO_ACTION &&
	       pcc->cta.includeflag == TRUE)
      {
	 log_msg("", "Creating call-to-action section...");
	 create_cta_section(fp, pcc, misc, template_info, outfile_ptr);
      }
      
   }  
   
   return;
} 


/*********************************************************************
   create_segment_section()
   
   PURPOSE
   Builds the segment section of the output product.  Segmenting is
   by forecast point, forecast group, or by county.  The segment sections
   can include tabular and point-specific sections; if segmenting by
   forecast group or county, it can also include the summary body.
   
   *******************************************************************/
void create_segment_section(int			numfps,
			    fp_struct		*fp,
			    int			numgrps,
			    grp_struct		*grp,
			    int                 numcnty,
			    county_struct       *cnty,
			    vtecinfo_struct	*vtecinfo,
			    pcc_struct		*pcc,
			    misc_struct		*misc,
			    FILE		*outfile_ptr,
			    template_info_struct *template_info)
{
   
   char	SEGMENT_END[] = "$$";
   int	i, j, k, l, n; 
   int  fpindex, grpindex, cntyindex;   
   int  fpindex2;
   int  *fporder  = NULL;
   int	*grporder = NULL;
   int  *cntyorder = NULL;
   int  tabular_created_flag; 
   char longstring[MAXLEN_LONGSTR];
   char	Pvtecline[MAXLEN_STRING] = "";
   char	Hvtecline[MAXLEN_STRING] = "";
   int  *temp_fps_inc   = NULL;      /* used in creating tabular section */
   int  *actual_fps_inc = NULL;      /* used in creating tabular section */
   char	msgstr[120];
   int  point_mode, group_mode, county_mode;
   char county_zone_flag = 'C';   
   char datetime_str[MAXLEN_STRING];
   int  prod_index;
   
   
   /* malloc space for temp_fps_inc array */

   temp_fps_inc = (int *)malloc(sizeof(int) * numfps);
   if (temp_fps_inc == NULL)
      log_msg(FAILED_MALLOC, "of temp_fps_inc in create_segment_section");
   
   
   /* null the datetime_str */
   
   memset(datetime_str, 0, MAXLEN_STRING);
   
   prod_index = convert_prodid_to_index(pcc->product.prod_categ);
   
   
   /* set a convenience variable based on the segment mode */
   
   group_mode = point_mode = county_mode = FALSE;
   
   if (pcc->product.segment_mode == SEGMENT_MODE_POINT)
   {
      log_msg("", "Creating segment section in forecast point mode...");
      point_mode = TRUE;
   }
   else if (pcc->product.segment_mode == SEGMENT_MODE_GROUP)
   {
      log_msg("", "Creating segment section in forecast group mode...");
      group_mode = TRUE;
   }
   else if (pcc->product.segment_mode == SEGMENT_MODE_COUNTY)
   {
      log_msg("", "Creating segment section in forecast county mode...");
      county_mode = TRUE;
   }   
      
	   
   /* if segmented by forecast point or group, define the order of the
      forecast points. */
   
   fporder = grporder = NULL;	 
   fpindex = grpindex = cntyindex = MISSINGVAL;
  
   if (point_mode || group_mode)
   {
     fporder = (int *)malloc(sizeof(int) * numfps);
     if (fporder == NULL) 
      log_msg(FAILED_MALLOC, "of fporder in create_segment_section");
   
     order_fps(numfps, fp, numgrps, grp, vtecinfo, pcc->product.grpfp_order, fporder);
   } 
   
   
                                            
   /* if segmenting by forecast point --------------------------- */
   
   if (point_mode)
   {
      /* set the tabular_created_flag to false, so that we know
	 we still have to do the tabular section for this
	 group/point (if it is included) */
      
      tabular_created_flag = FALSE;	 	 
      
      
      /* produce a set of info for each forecast point, one at a time */
      
      for (j = 0; j < numfps; j ++)
      {
	 fpindex = fporder[j];
	 
	 if (fpindex == MISSINGVAL)
	    break;
	    
	 /* check if this forecast point should be processed */
	 
	 if (misc->fps_included[fpindex])
	 {
	    sprintf(msgstr, "Processing forecast point %s (%d)", 
		    fp[fpindex].id, fpindex);
	    log_msg("", msgstr);
	    
	    
	    /* create and print the UGC line accordingly.  there can be
	       delays between the time the product is created and the time the
	       product is issued.  Because the UGC can depend on the current
	       time, and on vtec attributes, the actual UGC expire time is 
	       managed by coding the expire time at product creation, then
	       substituting final info at issuance time. */
	    
	    if (pcc->product.ugc_mode == UGC_BY_COUNTY)
	       county_zone_flag = 'C';
	    else if (pcc->product.ugc_mode == UGC_BY_ZONE)
	       county_zone_flag = 'Z';  
	        	    
	    	
	    build_ugc_segment(grp, fp, pcc, misc, vtecinfo,
	                      grpindex, fpindex, county_zone_flag,
			      longstring);
	    write_phrase_text(pcc->product.tcase, longstring, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    
	    
	    /* create the VTEC info for forecast point mode segment */
	    
	    if (pcc->product.vtec_flag == TRUE)
	    {
	       create_vteclines(vtecinfo, misc, fpindex, fp,
				Pvtecline, Hvtecline);
				
	       write_phrase_text(pcc->product.tcase, Pvtecline, outfile_ptr);
	       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	       
	       write_phrase_text(pcc->product.tcase, Hvtecline, outfile_ptr);
	       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	       
	    }
	    
	    
	    /* create MND date/time, local current system will be used for this.
               The time format is like 530 PM CDT FRI APR 6 2003*/
	       
	    create_MND_datetime(pcc, fp, fpindex, misc, datetime_str);
	    write_phrase_text(pcc->product.tcase, datetime_str, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    
   	    
	    /* add the test line if needed */
	    
	    if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
	    {
	       write_line_text(TEST_LINE, outfile_ptr);
	    }
	    
	    
	    /* loop on the number of sections specified 
	       and create the appropriate section */
	    
	    for (k = 0; k < pcc->product.num_sections; ++k)
	    {      
	       /* build the tabular section */
	       
	       if (pcc->product.section_index[k]      == TABULAR &&
		   pcc->tabular.includeflag           == TRUE    &&
		   pcc->product.tabular_within_flag   == TRUE    &&
		   tabular_created_flag               == FALSE)
	       {
		  log_msg("", "Creating segment tabular section...");
		  
		  
		  /* create the temp_fps_inc array with only one fpindex
		     being true. */
		  
		  for (l = 0; l < numfps; l ++)
		  {
		     if (l == fpindex)
			temp_fps_inc[l] = TRUE;
		     else
			temp_fps_inc[l] = FALSE;
		  }
		  
		  
		  /* swap int* pointers so that misc now has the temporary
		     fps_included array, and then create the tabular section.
		     the temp pointers fool the create_tabular_section into only
		     ncluding those points being considered for the segment */
		  
		  actual_fps_inc = misc->fps_included;
		  misc->fps_included = temp_fps_inc;
		  create_tabular_section(numfps, fp, numgrps, grp, numcnty, cnty,
					 pcc, misc, vtecinfo, outfile_ptr);
		  misc->fps_included = actual_fps_inc;
		  
	       }  /* end of if creating tabular section */ 
	       
	       
	       /* build the point_specific section */
	       
	       else if (pcc->product.section_index[k] == POINT_SPECIFIC)
	       {
		  log_msg("", "Creating segment point-specific section...");
		  create_pointspecific_segment_section(fp, numgrps, grp,
						       numcnty, cnty,
						       pcc, misc, vtecinfo,
						       template_info,
						       outfile_ptr, fpindex);
	       }
	    }  /* end of looop on number of sections */ 
	    
	    
   	    
	    /* add the test line if needed */
	    
	    if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
	    {
	       write_line_text(TEST_LINE_END, outfile_ptr);
	    }
	    
	    
	    /* print the segment terminator */
	    
	    write_phrase_text(pcc->product.tcase, SEGMENT_END, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    
	 } /* end of if on forecast point included */
      }    /* end of loop on number of forecast points */     
   }       /* end of if check for point_mode */
   
   
   /* if segmenting by forecast group --------------------------- */
   
   else if (group_mode)
   {
      /* find the order of the groups. this is used for ordering the
	 groups in the summary section.*/
      
      grporder = (int *)malloc(sizeof(int) * numgrps);
      if (grporder == NULL) 
         log_msg(FAILED_MALLOC, "of grporder in create_segment_section");
      
      order_grps(numgrps, grp, vtecinfo, pcc->product.grpfp_order, grporder);
      
      
      /* loop thru all groups, or enter just once if in point mode */
      
      for (i = 0; i < numgrps; i ++)
      {  
	 /* set the tabular_created_flag to false, so that we know
	    we still have to do the tabular section for this
	    group/point (if it is included) */
	 
	 tabular_created_flag = FALSE;
	 
	 
	 /* if the segment mode is group, then create the summary body section.
	    it is not created inside the segment when in point segment mode. */
	 
	 /* initialize fpindex which will be used in build_ugc_segment()*/
	 
	 fpindex = MISSINGVAL;
	 
	 /* get the group index */
	 
	 grpindex = grporder[i];
	 	 	 
	 if (grpindex == MISSINGVAL)
	    break;
	    
	 /* if this group is not included, then skip to the next group */
	 
	 if (misc->grps_included[grpindex] == FALSE)
	    continue;  
	 
	 sprintf(msgstr, "Processing forecast group %s (%d)", 
		 grp[grpindex].id, grpindex);
	 log_msg("", msgstr);
	 
	 
	 /* create and print the UGC line accordingly*/
	    
	 if (pcc->product.ugc_mode == UGC_BY_COUNTY)
	    county_zone_flag = 'C';
	 else if (pcc->product.ugc_mode == UGC_BY_ZONE)
	    county_zone_flag = 'Z'; 
	 
	 
	 if (misc->expire_set == MISSINGVAL || misc->expire_set == UGC_RELATIVE)
	    determine_expiretime(prod_index, misc, pcc);
	   	    
	 
	 /* when building the ugc for group mode, note that it ignores 
	    some of the args needed for vtec forecast point mode */
	 
	 build_ugc_segment(grp, fp, pcc, misc, vtecinfo,
			   grpindex, fpindex, county_zone_flag, 
			   longstring); 
	 write_phrase_text(pcc->product.tcase, longstring, outfile_ptr);
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 
	 	 
	 /* create MND date/time, local current system will be used for this.
               The time format is like 530 PM CDT FRI APR 6 2003*/
	       
	 create_MND_datetime(pcc,fp,fpindex, misc, datetime_str);
	 write_phrase_text(pcc->product.tcase, datetime_str, outfile_ptr);
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 
	 
	 /* write the test line if needed */
	 
	 if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
	 {
	    write_line_text(TEST_LINE, outfile_ptr);
	 }
	 
	 
	 /* produce a summary body for this group */
	 
	 if (pcc->summary.includeflag == TRUE)
	 {
	    create_summary_body(fp, numgrps, grp, numcnty, cnty,
				pcc, misc, vtecinfo,
				template_info, outfile_ptr, grpindex);  
	 }
	 
	 	 
	 /* loop on the number of sections specified 
		  and create the appropriate section */
	       
	 for (k = 0; k < pcc->product.num_sections; ++k)
	 {      
             /* build the tabular section */
		  
	     if (pcc->product.section_index[k]      == TABULAR &&
		 pcc->tabular.includeflag           == TRUE    &&
		 pcc->product.tabular_within_flag   == TRUE    &&
		 tabular_created_flag               == FALSE)
	     {
		 log_msg("", "Creating segment tabular section...");
		     
		     
	         /* set flag to true, so we do tabular only once per group */
		     
	         tabular_created_flag = TRUE;
		     
	         /* create a temporary misc->fps_included to
	  	    include only those points in this group */
		     
	         for (l = 0; l < numfps; l ++)
	         {
	  	    /* If the l-th fp is in the current forecast group and
		       the fp is included in the product, then set the temp
		       copy to TRUE */
		    
		    if ((strcmp(grp[grpindex].id, fp[l].grpid) == 0) &&
		        (misc->fps_included[l]                 == TRUE))
		       temp_fps_inc[l] = TRUE;		     
		    else
		       temp_fps_inc[l] = FALSE;
		 }
		     
		     
		     
		  /* Swap int* pointers so that misc now has the 
		     temp fps_included array, and create the tabular section.
		     the temp pointers fool the create_tabular_section into only
		     ncluding those points being considered for the segment*/

		  actual_fps_inc = misc->fps_included;
		  misc->fps_included = temp_fps_inc;
		  create_tabular_section(numfps, fp, numgrps, grp, numcnty, cnty,
					 pcc, misc, vtecinfo, outfile_ptr);
		  misc->fps_included = actual_fps_inc;
		     
	       }  /* end of if creating tabular section */ 
		  
		  
	       /* build the point_specific section. the called function
		  controls which, if any, subsections of the point specific
		  sections are included. */
		  
	       else if (pcc->product.section_index[k] == POINT_SPECIFIC)
	       {
	          for (j = 0; j < numfps; j ++)
	          {
	             fpindex = fporder[j];
	    
	             if (fpindex == MISSINGVAL)
		        break;
			
	             /* check if this forecast point should be processed.
	             if point either isn't in the group or isn't included,
	             then skip to next point in the group */
	    
	             if ( (strcmp(grp[grpindex].id, fp[fpindex].grpid) == 0) &&
		        (misc->fps_included[fpindex] == TRUE) )
	             {
	       
		        log_msg("", "Creating segment point-specific section...");
		        create_pointspecific_segment_section(fp, numgrps, grp,
						  	     numcnty, cnty,
							     pcc, misc,vtecinfo,
							     template_info,
							     outfile_ptr,
                                                             fpindex);
		     }
	          }  	 	       
	        } /* end of if creating pointspecif section */
	 }      /*end of the loop of the num_sections */
	 
   	    
	 /* add the test line if needed */
	    
	 if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
	 {
	    write_line_text(TEST_LINE_END, outfile_ptr);
	 }
	    
	 
	 /* if in group mode, then print the segment terminator now */
	 
	 write_phrase_text(pcc->product.tcase, SEGMENT_END, outfile_ptr);
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 
      }  /* end of loop on number of forecast groups */
   }     /* end of if control for group mode */
   
   
   /* if segmenting by county ------------------------------------------ */
   
   else if (county_mode)
   {
     
      if (misc->numcnty_included == 0)
      {
	 sprintf(msgstr, "ERROR... No counties are included");
	 log_msg("", msgstr);
      }	 
      
      /* find the order of the counties.*/
      
      cntyorder = (int *)malloc(sizeof(int) * numcnty);
      if (cntyorder == NULL) 
         log_msg(FAILED_MALLOC, "of cntyorder in create_segment_section");
      
      order_cntys(numcnty, cnty, vtecinfo, pcc->product.grpfp_order, cntyorder);

      for (i = 0; i < numcnty; i++)
      {
	 /* set the tabular_created_flag to false, so that we know we still 
	    have to do the tabular section for this county */
	 	 
	 cntyindex = cntyorder[i];
	 tabular_created_flag = FALSE;	 	 
	 
	 /* if this county is not included, then skip to the next county */
	 
	 if (misc->cnty_included[cntyindex] == FALSE)
	    continue;
	 
	 else
	 {
	    sprintf(msgstr, "Processing forecast points in county %s (%s)",
		    cnty[cntyindex].county, cnty[cntyindex].state);
	    log_msg("", msgstr);
	    
	    
	    /* print the UGC line */
	    
	    log_msg("", "Creating UGC line...");
	    	    
	    if (misc->expire_set == MISSINGVAL || misc->expire_set == UGC_RELATIVE)
	       determine_expiretime(prod_index, misc, pcc);
	    
	    build_ugc_segment_county(cnty, cntyindex, misc,longstring);
	    write_phrase_text(pcc->product.tcase, longstring, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    	    
	    
	    /* create MND date/time, local current system will be used for this.
               The time format is like 530 PM CDT FRI APR 6 2003*/
	       
	    create_MND_datetime(pcc,fp,fpindex, misc, datetime_str);
	    write_phrase_text(pcc->product.tcase, datetime_str, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    
	    
	    /* create the test line if needed */
	    
	    if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
	    {
	       write_line_text(TEST_LINE, outfile_ptr);
	    }
	    
	    
	    /* produce a summary body for this county */
	    
	    log_msg("", "Creating summary body line...");
	    if (pcc->summary.includeflag == TRUE)
	    {
	       create_summary_body_county(fp, numgrps, grp, numcnty, cnty, 
					  pcc, misc, vtecinfo,
					  template_info, outfile_ptr,
					  cntyindex);	       	       
	    }  
	    
	    
	    /* loop on the number of sections specified and create the
	       appropriate section */
		  
	    for (j = 0; j < pcc->product.num_sections; ++j)
	    {

	       /* build the tabular section */

	       if (pcc->product.section_index[j]    == TABULAR &&
		   pcc->tabular.includeflag         == TRUE    &&
		   pcc->product.tabular_within_flag == TRUE    &&
		   tabular_created_flag             == FALSE)
	       {
		  log_msg("", "Creating segment county tabular section...");

		  /* set flag so tabular done only once per county */

		  tabular_created_flag = TRUE;


		  /* create a temporary misc->fps_included to include
		     only those points in the county */

		  for (n = 0; n < numfps; n++)
		  {
		     temp_fps_inc[n] = FALSE;
		  }

		  for (l = 0; l < cnty[cntyindex].numfps; l++)
		  {			
		     fpindex2 = cnty[cntyindex].fpindex[l];
		     if (misc->fps_included[fpindex2] == TRUE)
			temp_fps_inc[fpindex2] = TRUE;
		     else
			temp_fps_inc[fpindex2] = FALSE;
		  }


		  /* swap int* around so that misc now has the temp
		     fps_included array and create the tabular section */

		  actual_fps_inc = misc->fps_included;
		  misc->fps_included = temp_fps_inc;

		  create_tabular_section(numfps, fp, numgrps, grp, numcnty,
					 cnty, pcc, misc, vtecinfo, outfile_ptr);

		  misc->fps_included = actual_fps_inc;

	       } /* end of if creating tabular section */


	       /* build the point_specific section */

	       else if (pcc->product.section_index[j] == POINT_SPECIFIC)
	       {
	          for (k = 0; k < cnty[cntyindex].numfps; k++)
	          {
	              fpindex = cnty[cntyindex].fpindex[k];
	       
	       
	              /* check it this forecast point should be processed */
	       
	              if (misc->fps_included[fpindex] == TRUE)
	              {
			 log_msg("", 
				"Creating segment point-specific county section...");
			
			 create_pointspecific_segment_section(fp,
							     numgrps, grp,
							     numcnty, cnty,
							     pcc, misc,vtecinfo,
							     template_info, 
							     outfile_ptr, 
							     fpindex);
		      } 
		   }  /* end of loop on number of forecast point */
	        }     /* end of if point specific section */
	    }         /* end of loop on num_sections */
   	    
	    
	    /* add the test line if needed */
	    
	    if (get_test_setting(misc->workstation_mode, pcc->product.vtec_flag, pcc->product.vtec_cat))
	    {
	       write_line_text(TEST_LINE_END, outfile_ptr);
	    }
	    
	    
	    write_phrase_text(pcc->product.tcase, SEGMENT_END, outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    
	 } /* end of if misc->cnty_included */
      }    /* end of loop on number of groups */   	               
   }       /* end of county mode */
   
   
   /* free memory */
   
   if (fporder)      free(fporder);
   if (grporder)     free(grporder);
   if (cntyorder)    free(cntyorder);
   if (temp_fps_inc) free(temp_fps_inc);
   
   
   return;
}


/*********************************************************************
   create_summary_body()
   
   PURPOSE
   Create the body portion summary section of the product. 
   
   NOTES
   
   *******************************************************************/
void create_summary_body(fp_struct		*fp,
			 int			numgrps,
			 grp_struct		*grp,
			 int                    numcnty,
			 county_struct          *cnty,
			 pcc_struct		*pcc,
			 misc_struct		*misc,
			 vtecinfo_struct        *vtecinfo,
			 template_info_struct	*template_info,
			 FILE 			*outfile_ptr,
			 int             	grpindex_used)
{
   char 	template_file[MAXLEN_FILENAME];
   char 	template_name[MAXLEN_TEMPLATENAME];
   int 		template_type;
   int 		phrasenum;
   int 		line_written;
   char 	*newphrase;
   int 		i, grpindex;
   int 		*grporder = NULL;
   int		include_in_product;
   int          num_grps_to_use;
   int          bullet_1st = TRUE, indent_1st = TRUE;
   
   /* set the template file and type */
   
   sprintf(template_file, "%s/%s.%s",
	   paramdir, SUMMARY_TEMPLATEFILE, misc->selected_office); 
   template_type = PHRASE_WITH_CONDITION;
   
   line_written = FALSE;
   
   
   /* establish the order of the forecast groups. the grpindex_used
      indicates whether to product for one group or all groups. it 
      is used for segmented products */
   
   if (grpindex_used == MISSINGVAL)
   {
      grporder = (int *)malloc(sizeof(int) * numgrps);
      if (grporder == NULL) 
         log_msg(FAILED_MALLOC, "of grporder in create_summary_body");            
	 
      order_grps(numgrps, grp, vtecinfo, pcc->product.grpfp_order, grporder);
      num_grps_to_use = numgrps;
   }
   
   else
   { 
      grporder = (int *)malloc(sizeof(int) * 2);
      if (grporder == NULL) 
         log_msg(FAILED_MALLOC, "of grporder in create_summary_body");	         
      
      grporder[0] = grpindex_used;
      num_grps_to_use = 1;
   }
      
   
   /* loop on the groups and create the body of the summary section */
   
   for (i = 0; i < num_grps_to_use; i++)
   {
      grpindex = grporder[i]; 
      
      if (grpindex == MISSINGVAL)
         break;
	 
      /* now check if this forecast group should be included
	 in the product */
      
      if (pcc->product.nwr_flag)
      {	 
	 /* see if the group has included points in the tower area. if so, 
	    then include the summary statements for the group. this  
	    results in the product always having summary information
	    for the group containing the points that are in the tower's
	    product.  however, if the point is outside the tower's area
	    but a part of the point's group is inside, still no summary
	    information is given for the forecast point for those
	    group variables that list the forecast point */
	 
	 include_in_product = check_grp_in_tower(grp, grpindex, fp, misc);
      }
      else
	 include_in_product = TRUE;
      
      
      /* only process for this group if the group is included */
      
      if (misc->grps_included[grpindex] == TRUE && include_in_product == TRUE)
      {
	 /* set the template name for the group */
	 
         strcpy(template_name, pcc->summary.template[grpindex]);
	 
	 
	 /* get and load all the information for the template;
	    this call must be repeated within the loop on the groups
	    since the template condition stack and phrase string variables
	    are overwritten with data each time */
	 
	 buf_template(template_file, template_name, template_type,
		      misc->system_time, template_info);
	 
	 
	 /* check the result of the condition */
	 
	 check_conditions(grpindex, fp, grp, numcnty, cnty, misc, vtecinfo,pcc, 
	                  SUMMARY,template_info);
	 
	 
	 /* loop on each of the phrases and if the associated condition is
	    true, then load the data into the variables contained in the phrase
	    itself */
	 
	 for (phrasenum = 0; phrasenum < template_info->num_phrases; ++phrasenum)
	 {
	    if (template_info->include_phrase[phrasenum] == TRUE)
	    {
	       load_phrase_data(grpindex, SUMMARY, fp, grp, numcnty, cnty, 
	                        misc, vtecinfo,phrasenum, template_info, pcc, 
				&newphrase);
                
	       /*add using of "bulletstr" keyword to format bullet text*/
	 
	       if (template_info->bulletstr_flag[phrasenum] == 1)
	       {
	          if (bullet_1st == TRUE)
		  {
		     write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
		     bullet_1st = FALSE;
		  }   
	          write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
	       }
	       
	        /*add using of "indentstr" keyword to format indent text*/
		
	       else if (template_info->indentstr_flag[phrasenum] == 1)
	       {
	           if (indent_1st == TRUE)
		   {
		       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
		       indent_1st = FALSE;
		   }
		   write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
	       }	      
	       else
	       {
	          bullet_1st = TRUE;  
		  indent_1st = TRUE; 					
	          write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);
	       }
	        
	       line_written = TRUE;
	    }
	 }
      }  /* end of if on whether group is included */
   }     /* end of loop on the forecast groups */
   
   
   /* if at least one line was written, then end the current
      line and add a trailing blank line automatically */
   
   if (line_written == TRUE)
   {
      write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
      write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
   }
   
   
   /* free the memory */
   
   if (grporder) free(grporder);
   
   return;
   
}


/**************************************************************
   create_summary_body_county()
   
   PURPOSE
   Create the body portion summary section of the product
   for county mode.
   
   ***************************************************************/
void create_summary_body_county(fp_struct             *fp,
                                int                   numgrps,
				grp_struct            *grp,
				int                   numcnty,
				county_struct         *cnty,
				pcc_struct            *pcc,
				misc_struct           *misc,
				vtecinfo_struct       *vtecinfo,
				template_info_struct  *template_info,
				FILE                  *outfile_ptr,
				int                   cntyindex)   
{
   char    template_file[MAXLEN_FILENAME];
   char    template_name[MAXLEN_TEMPLATENAME];
   int     template_type;
   int     phrasenum;
   char    *newphrase;
   int     line_written;
   
   
   /* set the template file and type */
   
   sprintf(template_file, "%s/%s.%s", paramdir, SUMMARY_TEMPLATEFILE, 
	   misc->selected_office);
   template_type = PHRASE_WITH_CONDITION;
   
   line_written = FALSE;
   
   /* only process for this county if it is included */
   
   if (misc->cnty_included[cntyindex] == TRUE)
   {
      /* set the template name for the county group 
         note when in county segment mode, the feature 
	 for specifying unique templates per group (i.e. county
	 in this case) is NOT available.  in other words, when
	 in county mode, one and only one summary body template
	 is used.  in the future, possibly the pcc operations
	 will be modified to allow the "special template" to 
	 be specified for counties, and this could then be used
	 to allow unique templates for individual county segments  */
      
      
      strcpy(template_name, pcc->summary.default_template);
      
      
      /* get and load all the info for the template*/
      
      buf_template(template_file, template_name, template_type,
                   misc->system_time, template_info);
      
      
      /* check the result of condition */
      
      check_conditions(cntyindex, fp, grp, numcnty, cnty, misc, vtecinfo,pcc, SUMMARY,
                       template_info);
      
      
      /* loop on each of the phrases and if the associated condition is
	 true, then load the data into the variable contained in the phrase
	 itself */
      
      for (phrasenum = 0; phrasenum < template_info->num_phrases; ++phrasenum)
      {
         if (template_info->include_phrase[phrasenum] == TRUE)
	 {
	    load_phrase_data(cntyindex, SUMMARY, fp, grp, numcnty, cnty,
			     misc, vtecinfo, phrasenum, template_info, pcc, &newphrase);
	    
	    write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);
	    line_written = TRUE;
	 }
      }
   }  /* end of if on whether the county is included */
   
   
   /* if at least one line was written, then end the current line and
      add a trailing blank line automatically */
   
   if (line_written == TRUE)
   {
      write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
      write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
   }
   
   return;
   
}    	    	      		      


/*********************************************************************
   create_basis_section()
   
   PURPOSE
   Create the basis section of the product. 
   
   NOTES
   
   *******************************************************************/
void create_basis_section(fp_struct		*fp,
			  pcc_struct		*pcc,
			  misc_struct		*misc,
			  template_info_struct 	*template_info,
			  FILE			*outfile_ptr) 
{
   
   int 	template_type;
   char template_file[MAXLEN_FILENAME];
   char template_name[MAXLEN_TEMPLATENAME];
   int	phrasenum;
   int  bullet_1st = TRUE, indent_1st = TRUE;
   
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s", 
	   paramdir, BASIS_TEMPLATEFILE, misc->selected_office); 
   strcpy(template_name, pcc->basis.template);
   template_type = PHRASE_FIXED;
   
   
   /* get and load all the information for the template */
   
   buf_template(template_file, template_name, template_type,
		misc->system_time, template_info);
   
   
   /* loop on each of the phrases and load all the data into
      the phrases for this template;  
      note that the id index is set to 0 since no substitution supported
      for the basis section; then output the phrase itself;
      convert to upper case if specified */
   
   for (phrasenum = 0; phrasenum < template_info->num_phrases; ++phrasenum)
   {  
      /*add using of "bulletstr" keyword to format bullet text*/
	 
      if (template_info->bulletstr_flag[phrasenum] == 1)
      {
         if (bullet_1st == TRUE)
	 {
	   write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	   bullet_1st = FALSE;
	 }  
         write_bullet_text(pcc->product.tcase, 
	                template_info->phrase[phrasenum], outfile_ptr);
      }	
      
      /*add using of "indentstr" keyword to format indent text*/
	 
      if (template_info->indentstr_flag[phrasenum] == 1)
      {
         if (indent_1st == TRUE)
	 {
	   write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	   indent_1st = FALSE;
	 }  
         write_indent_text(pcc->product.tcase, 
	                template_info->phrase[phrasenum], outfile_ptr);
      }			
      else       
      {
         bullet_1st = TRUE;
	 indent_1st = TRUE;
	 
	 /*start with "." for the first line in the basis section - general
	 synopsis*/
	 
	/* if (phrasenum == 0 && template_info->num_phrases != 0)	 
	    write_phrase_text(pcc->product.tcase, ".", outfile_ptr);
     */
     	    
         write_phrase_text(pcc->product.tcase, 
			template_info->phrase[phrasenum], outfile_ptr);
      }			
   }
   
   
   /* put a trailing blank line automatically */
   
   write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
   write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
   
   return;
}


/*********************************************************************
   create_pointspecific_section()
   
   PURPOSE
   Create the pointspecific section of the product for
   regular and NWR products.  For segmented products,
   a separate function is used.
      
   *******************************************************************/
void create_pointspecific_section(int			numfps,
				  fp_struct		*fp,
				  int			numgrps,
				  grp_struct		*grp,
				  int                   numcnty,
				  county_struct         *cnty,
				  pcc_struct		*pcc,
				  misc_struct		*misc,
				  vtecinfo_struct       *vtecinfo,
				  template_info_struct	*template_info,
				  FILE 			*outfile_ptr)
{
   int 	fpindex, i, j;
   int 	line_written;
   char	msgstr[MAXLEN_STRING];     
   int	*fporder = NULL;
   int	include_in_product;	
   
   
   /* establish the order of the forecast points based
      on the user selection */
   
   fporder = (int *)malloc(sizeof(int) * numfps);
   if (fporder == NULL) 
      log_msg(FAILED_MALLOC, "of fporder in create_pointspecific_section");
   
   order_fps(numfps, fp, numgrps, grp, vtecinfo,pcc->product.grpfp_order, fporder);      
   
   
   /* loop on the number of forecast points */ 
   
   for (i = 0; i < numfps; ++i)
   {       
      /* set the forecast point being processed based
	 on the previously determined order */
      
      fpindex      = fporder[i];
      
      if (fpindex == MISSINGVAL)
         break;
	 
      line_written = FALSE;
      
      
      /* now check if this forecast point should be included
	 in the product */
      
      if (pcc->product.nwr_flag)
      {
	 include_in_product = check_loc_in_tower(fp[fpindex].id, 
						 misc->nwrtransPtr,
						 misc->loctransPtr);
      }
      else
	 include_in_product = TRUE;
      
      
      /* only process if the forecast point is to be included */
      
      if (misc->fps_included[fpindex] == TRUE && include_in_product == TRUE)
      {
	 /* log informational message */
	 
	 sprintf(msgstr, "Creating point specific section for %s...",
		 fp[fpindex].id);
	 log_msg("", msgstr);
	 
	 
	 /* loop on the number of subsections specified 
	    and create the appropriate subsection */
	 
	 for (j = 0; j < pcc->product.num_ps_subsections; ++j)
	 {	    
	    /* build the data roundup section, provided it is to
	       be included for this forecast point */
	    
	    if (pcc->product.ps_subsection_index[j] == DATA_ROUNDUP &&
		pcc->roundup.includeflag == TRUE)
	    {
	       create_roundup_subsection(fpindex, fp, grp, numcnty, cnty,
	                                 pcc, misc, vtecinfo,&line_written, 
					 template_info, outfile_ptr);
	    }
	    
	    
	    /* build the impact statement section, provided it is to
	       be included for this forecast point */
	    
	    else if (pcc->product.ps_subsection_index[j] == IMPACT_STATEMENT &&
		     pcc->impact.includeflag == TRUE)
	    {
	       if (pcc->impact.range_set[fpindex] == TRUE)
		  create_impact_subsection(fpindex, fp, grp, numcnty, cnty, 
		                           pcc, misc, vtecinfo, &line_written, 
					   template_info, outfile_ptr);
	    }
	    
	    
	    /* build the historical comparison section, provided it is
	       to be included for this forecast point */
	    
	    else if (pcc->product.ps_subsection_index[j] == HISTORICAL_COMPARISON &&
		     pcc->comparison.includeflag == TRUE)
	    {
	       if (pcc->comparison.compare_set[fpindex] == TRUE)
		  create_comparison_subsection(fpindex, fp, grp, numcnty, cnty, 
		                               pcc, misc, vtecinfo, &line_written, 
					       template_info, outfile_ptr);
	    }
	 }  /* end of loop on number of subsections */
	 
	 
	 /* end the current line and add a blank line automatically */
	 
	 if (line_written == TRUE)
	 {	    
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	 }
	 
      }  /* if forecast point included */      
   }     /* end of loop on forecast points */
   
   
   /* free the memory */
   
   free(fporder);
   
   return;
}


/*********************************************************************
   create_pointspecific_segment_section()
   
   PURPOSE
   Create the pointspecific section of the product, for 
   segmented products. 
   
   NOTES
   This functions only creates the point-specific section's
   for one forecast point, whether all one, two, or three
   subsections are included.
   
   *******************************************************************/
void create_pointspecific_segment_section(fp_struct		*fp,
				          int			numgrps,
				          grp_struct		*grp,
					  int                   numcnty,
					  county_struct         *cnty,
					  pcc_struct		*pcc,
					  misc_struct		*misc,
					  vtecinfo_struct       *vtecinfo,
					  template_info_struct	*template_info,
					  FILE 			*outfile_ptr,
					  int            	fp_to_proc)
{
   int 	j;
   int 	line_written;
   char	msgstr[MAXLEN_STRING];
   int  fpindex;
   
   
   /* set the forecast point being processed based
      on the previously determined order */
   
   line_written = FALSE;
   
   fpindex = fp_to_proc;
   
   
   /* only process if the forecast point is to be included */
   
   if (misc->fps_included[fpindex] == TRUE)
   {
      /* log informational message */
      
      sprintf(msgstr, "Creating point specific segment section for %s...",
	      fp[fpindex].id);
      log_msg("", msgstr);
      
      
      /* loop on the number of subsections specified 
         and create the appropriate subsection */
      
      for (j = 0; j < pcc->product.num_ps_subsections; ++j)
      {	    
         /* build the data roundup section, provided it is to
            be included for this forecast point */
	 
         if (pcc->product.ps_subsection_index[j] == DATA_ROUNDUP &&
             pcc->roundup.includeflag == TRUE)
         {
            create_roundup_subsection(fpindex, fp, grp, numcnty, cnty, 
	                              pcc, misc, vtecinfo, &line_written, 
                                      template_info, outfile_ptr);
         }
	 
	 
         /* build the impact statement section, provided it is to
            be included for this forecast point */
	 
         else if (pcc->product.ps_subsection_index[j] == IMPACT_STATEMENT &&
                  pcc->impact.includeflag == TRUE)
         {
            if (pcc->impact.range_set[fpindex] == TRUE)
               create_impact_subsection(fpindex, fp, grp, numcnty, cnty,
	                                pcc, misc, vtecinfo, &line_written, 
                                        template_info, outfile_ptr);
         }
	 
	 
         /* build the historical comparison section, provided it is
            to be included for this forecast point */
	 
         else if (pcc->product.ps_subsection_index[j] == HISTORICAL_COMPARISON &&
                  pcc->comparison.includeflag == TRUE)
         {
            if (pcc->comparison.compare_set[fpindex] == TRUE)
               create_comparison_subsection(fpindex, fp, grp, numcnty, cnty, 
	                                    pcc, misc, vtecinfo, &line_written, 
                                            template_info, outfile_ptr);
         }
      }  /* end of loop on number of subsections */
      
      
      /* end the current line and add a blank line automatically */
      
      if (line_written == TRUE)
      {
         if (last_is_bulletorindent == FALSE)
             write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
         write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
      }
   }  /* end of if on fps_included */
   
   return;
}



/*********************************************************************
   create_roundup_subsection()
   
   PURPOSE
   Creates the subsection of the point-specific section of the product. 
   
   NOTES
   
   *******************************************************************/
void create_roundup_subsection(int 			fpindex,
			       fp_struct		*fp,
			       grp_struct		*grp,
			       int                      numcnty,
			       county_struct            *cnty,
			       pcc_struct		*pcc,
			       misc_struct		*misc,
			       vtecinfo_struct          *vtecinfo,
			       int			*line_written,
			       template_info_struct	*template_info,
			       FILE 			*outfile_ptr)
			       
{
   char template_file[MAXLEN_FILENAME];
   char template_name[MAXLEN_TEMPLATENAME];
   int template_type;
   int phrasenum, num_include_phrase;
   char *newphrase;
 
   
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s",
	   paramdir, ROUNDUP_TEMPLATEFILE, misc->selected_office); 
   strcpy(template_name, pcc->roundup.template[fpindex]);
   template_type = PHRASE_WITH_CONDITION;
   
   
   /* get and load all the information for the template */
   
   buf_template(template_file, template_name, template_type,
		misc->system_time, template_info);
   
   
   /* check the result of the condition */
   
   check_conditions(fpindex, fp, grp, numcnty, cnty, misc, vtecinfo, pcc, 
                    DATA_ROUNDUP,template_info);
   
   
   /* loop on each of the phrases and if the associated condition is
      true, then load the data into the variables contained in the phrase
      itself */
   
   num_include_phrase = 0;
   
   for (phrasenum = 0; phrasenum < template_info->num_phrases; ++phrasenum)
   {
      if (template_info->include_phrase[phrasenum] == TRUE)
      {
         num_include_phrase++;
	 
	 load_phrase_data(fpindex, DATA_ROUNDUP, fp, grp, numcnty, cnty,
	                  misc, vtecinfo, phrasenum, template_info, pcc, 
			  &newphrase);
			  
	 if (fp[fpindex].fcstpoint_type == TIDAL)
	 {
	    convert_str_to_upcase(newphrase);
	    fprintf(outfile_ptr, newphrase); 
	 }
	 else
	 {
	    /*handle the output for indentstr or bulletstr if they are the first
	    phrase in the template*/
	    
	    if (phrasenum == 0 )
	    {	      	       	     
	      if (template_info->bulletstr_flag[phrasenum] == 1)
	         write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
	      else if (template_info->indentstr_flag[phrasenum] == 1)
	         write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
	      else	      	    
	         write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr); 
		 		
            } 	    	    
            else
	    {		       
	      if (template_info->bulletstr_flag[phrasenum] == 1)
	      {
		 if ((template_info->bulletstr_flag[phrasenum - 1] != 1 )&&
		     (template_info->indentstr_flag[phrasenum - 1] != 1))
		 {
	           write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);		 
		 }	 
		 write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
	      } 

	      /*add using of "indentstr" keyword to format indent text*/

              else if (template_info->indentstr_flag[phrasenum] == 1)
              {
        	 if ((template_info->bulletstr_flag[phrasenum - 1] != 1 )&&
		    (template_info->indentstr_flag[phrasenum - 1] != 1))
		 {
	           write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);		 
		 }	
		 
        	 write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
              }	  
	      else
	      {		 
		 write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);
	      }
	    
	    }
	    *line_written = TRUE;
	 }
      }
   }
   
   /*If bulletstr or indentstr is the first or last phrase to be written from the template, there might be 
   an extr blank line between this roundup template with others*/
     
   if (template_info->bulletstr_flag[num_include_phrase - 1] == 1 ||
      template_info->indentstr_flag[num_include_phrase - 1] == 1)

      last_is_bulletorindent = TRUE;  
   else
      last_is_bulletorindent = FALSE;  

   	     
   
   return;
}


/*********************************************************************
   create_impact_subsection()
   
   PURPOSE
   Creates the impact subsection of the point-specific section of
   the product. 
   
   NOTES
   
   *******************************************************************/
void create_impact_subsection(int 			fpindex,
			      fp_struct			*fp,
			      grp_struct		*grp,
			      int                       numcnty,
			      county_struct             *cnty,
			      pcc_struct		*pcc,
			      misc_struct		*misc,
			      vtecinfo_struct           *vtecinfo,
			      int			*line_written,
			      template_info_struct	*template_info,
			      FILE 			*outfile_ptr)
			     
{
   int template_type;
   char template_file[MAXLEN_FILENAME];
   char template_name[MAXLEN_TEMPLATENAME];
   int phrasenum, num_include_phrase;
   char *newphrase;
   int impact_found;
   
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s",
	   paramdir, IMPACT_TEMPLATEFILE, misc->selected_office); 
   strcpy(template_name, pcc->impact.template);
   template_type = PHRASE_WITH_SUBSTITUTION;
   
   
   /* get and load all the information for the template */
   
   buf_template(template_file, template_name, template_type,
		misc->system_time, template_info);
   
   
   /* initialize, note that only one phrase can be used */
   
   phrasenum = 0;
   if (template_info->num_phrases > 1)
      log_msg(EXCEED_MAXNUM_PHRASES, template_info->name);
   impact_found = FALSE;
   
   
   /* loop until no more impact statements are found that 
      match the given criteria */
   
   num_include_phrase = 0;
   
   do 
   {
      /* get the impact statement to be included and load into 
	 the temporary storage in misc structure */
      
      get_impact_chosen(fpindex, fp, pcc, misc, &impact_found);
      
      
      /* if an impact statements is found, then create the phrase */
      
      if (impact_found)
      {
         num_include_phrase++;
	 load_phrase_data(fpindex, IMPACT_STATEMENT, fp, grp, numcnty, cnty, 
	                  misc, vtecinfo, phrasenum, template_info, pcc, 
			  &newphrase);      
	 
	 /*add using of "bulletstr" keyword to format bullet text*/
	 
	 /*handle the output for indentstr or bulletstr if they are the first
	    phrase in the template*/
	    
	 if (phrasenum == 0 )
	 {	   	   
	   if (template_info->bulletstr_flag[phrasenum] == 1)
	      write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
	   else if (template_info->indentstr_flag[phrasenum] == 1)
	      write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
	   else	      	    
	      write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr); 		
         } 		 
         else
	 {		     	 	      		
	     if (template_info->bulletstr_flag[phrasenum] == 1)
	     {
		if ((template_info->bulletstr_flag[phrasenum - 1] != 1 )&&
		    (template_info->indentstr_flag[phrasenum - 1] != 1))
		{
	          write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);		 
		}	 
		write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
	     } 

	     /*add using of "indentstr" keyword to format indent text*/

             else if (template_info->indentstr_flag[phrasenum] == 1)
             {
        	if ((template_info->bulletstr_flag[phrasenum - 1] != 1 )&&
		   (template_info->indentstr_flag[phrasenum - 1] != 1))
		{
	          write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);		 
		}	

        	write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
             }	  
	     else
	     {		 
		write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);
	     }
	 }

	 *line_written = TRUE;
      }      
      
   } while (impact_found); 
   
   /*If bulletstr or indentstr is the first or last phrase to be written from the template, there might be 
   an extr blank line between this impact template with others*/
     
   if (template_info->bulletstr_flag[num_include_phrase - 1] == 1 ||
      template_info->indentstr_flag[num_include_phrase - 1] == 1)

      last_is_bulletorindent = TRUE;  
   else
      last_is_bulletorindent = FALSE;   
   
   return;
}


/*********************************************************************
   create_comparison_subsection()
   
   PURPOSE
   Creates the historical comparison subsection of the point-specific
   section of the product. 
   
   NOTES
   
   *******************************************************************/
void create_comparison_subsection(int 			fpindex,
				  fp_struct		*fp,
				  grp_struct		*grp,
				  int                   numcnty,
				  county_struct         *cnty,
				  pcc_struct		*pcc,
				  misc_struct		*misc,
				  vtecinfo_struct       *vtecinfo,
				  int			*line_written,
				  template_info_struct	*template_info,
				  FILE 			*outfile_ptr)
				  
{
   int template_type;
   char template_file[MAXLEN_FILENAME];
   char template_name[MAXLEN_TEMPLATENAME];
   int phrasenum;
   char *newphrase;
  
   
   /* set the template file, name, and type */
   
   sprintf(template_file, "%s/%s.%s",
	   paramdir, COMPARISON_TEMPLATEFILE, misc->selected_office); 
   strcpy(template_name, pcc->comparison.template);
   template_type = PHRASE_WITH_SUBSTITUTION;
   
   
   /* get and load all the information for the template */
   
   buf_template(template_file, template_name, template_type,
		misc->system_time, template_info);
   
   
   /* create the phrase and write it; note that only the first
      phrase is processed, if more were defined, then log msg */
   
   phrasenum = 0;
   if (template_info->num_phrases > 1)
      log_msg(EXCEED_MAXNUM_PHRASES, template_info->name);
   
   load_phrase_data(fpindex, HISTORICAL_COMPARISON, fp, grp,
		    numcnty, cnty, misc, vtecinfo, 
		    phrasenum, template_info, pcc, &newphrase); 
   
   if (phrasenum == 0 )
   {
     /*only one phrase*/
	      
     if (template_info->num_phrases == 1)
     {
	if (template_info->bulletstr_flag[phrasenum] == 1 ||
	    template_info->indentstr_flag[phrasenum] == 1)

	    last_is_bulletorindent = TRUE;  
	else
	    last_is_bulletorindent = FALSE;  
     }
     if (template_info->bulletstr_flag[phrasenum] == 1)
	write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
     else if (template_info->indentstr_flag[phrasenum] == 1)
	write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
     else	      	    
	write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr); 		
   } 		 
   else
   {	
       /*handle the last phrase in the template if it is indentsr or bulletstr*/
	    
       if (phrasenum == template_info->num_phrases - 1)
       {

	  if (template_info->bulletstr_flag[phrasenum] == 1 ||
	      template_info->indentstr_flag[phrasenum] == 1)

	      last_is_bulletorindent = TRUE;  
	  else
	      last_is_bulletorindent = FALSE;  	  	    	    
       }		 		
       if (template_info->bulletstr_flag[phrasenum] == 1)
       {
	  if ((template_info->bulletstr_flag[phrasenum - 1] != 1 )&&
	      (template_info->indentstr_flag[phrasenum - 1] != 1))
	  {
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);		 
	  }	 
	  write_bullet_text(pcc->product.tcase, newphrase, outfile_ptr);
       } 

       /*add using of "indentstr" keyword to format indent text*/

       else if (template_info->indentstr_flag[phrasenum] == 1)
       {
          if ((template_info->bulletstr_flag[phrasenum - 1] != 1 )&&
	     (template_info->indentstr_flag[phrasenum - 1] != 1))
	  {
	    write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);		 
	  }	

          write_indent_text(pcc->product.tcase, newphrase, outfile_ptr);
       }	  
       else
       {		 
	  write_phrase_text(pcc->product.tcase, newphrase, outfile_ptr);
       }
   }		    
   
   *line_written = TRUE;
   
   return;   
}


/*********************************************************************
   create_cta_section()
   
   PURPOSE
   Creates the call-to-action section of the product. 
   
   NOTES
   
   *******************************************************************/
void create_cta_section(fp_struct		*fp,
			pcc_struct		*pcc,
			misc_struct		*misc,
			template_info_struct	*template_info,
			FILE 			*outfile_ptr) 
{
   int	template_type;
   char	template_file[MAXLEN_FILENAME];
   char	template_name[MAXLEN_TEMPLATENAME];
   int	i, j;
   int  bullet_1st = TRUE, indent_1st = TRUE;
   
   /* if no templates specified, then issue message and do nothing */
   
   if (pcc->cta.num_ctas <= 0)
   {
      log_msg(NO_CTA_TEMPLATES, "");
      return;
   }
   
   
   /* set the template file and type */
   
   sprintf(template_file, "%s/%s.%s",
	   paramdir, CTA_TEMPLATEFILE, misc->selected_office); 
   template_type = PHRASE_FIXED;
   
   
   /* loop on the number of call-to-action statements */
   
   for (i = 0; i < pcc->cta.num_ctas; ++i)
   {
      /* load the template name */
      
      strcpy(template_name, pcc->cta.template[i]);
      
      
      /* get and load all the information for the template */
      
      buf_template(template_file, template_name, template_type,
		   misc->system_time, template_info);
      
      
      /* loop on each of the phrases and output the text to
	 the product; typically, one phrase will be given for
	 each call-to-action */
      
      for (j = 0; j < template_info->num_phrases; ++j)
      {	 
         /* add using of "bulletstr" keyword to format bullet text */
	 
	 if (template_info->bulletstr_flag[j] == 1)
	 {
	    if (bullet_1st == TRUE)
	    {
	       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	       bullet_1st = FALSE;
	    }   
	    write_bullet_text(pcc->product.tcase,
	                   template_info->phrase[j], outfile_ptr); 
         }
	 
	 /*add using of "indentstr" keyword to format indent text*/
	 
         if (template_info->indentstr_flag[j] == 1)
         {
            if (indent_1st == TRUE)
	    {
	       write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
	       indent_1st = FALSE;
	    }  
           write_indent_text(pcc->product.tcase, 
	                template_info->phrase[j], outfile_ptr);
         }				   
	 else
	 {
	    bullet_1st = TRUE; 
	    indent_1st = TRUE;  
	    write_phrase_text(pcc->product.tcase,
			   template_info->phrase[j], outfile_ptr);
         }			   
      }   
      
      
      /* support option to have blank line after each call-to-action */
      
      if (pcc->cta.skipline_flag == TRUE)
      {
         write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
         write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
      }
   } 
   
   
   /* assuming we haven't been adding newlines already, then
      end the current line and insert a trailing blank line */
   
   if (pcc->cta.skipline_flag == FALSE)
   {
      write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
      write_phrase_text(pcc->product.tcase, "\n", outfile_ptr);
   }
   
   return;   
}


/*********************************************************************
   get_impact_chosen()
   
   PURPOSE
   Utilty function used in helping generate the historical impact
   subsection. Given the range of impact stages to include, it
   searches through the impacts and extracts the appropriate
   statement and stores the statement in the holding area in
   the misc structure and flags its presence by setting the 
   impact_found variable.
   
   *******************************************************************/
void get_impact_chosen(int		fpindex,
		       fp_struct	*fp,
		       pcc_struct	*pcc,
		       misc_struct	*misc,
		       int		*impact_found)
{
   static int 		first_read_for_fp = TRUE;
   static Floodstmt 	*floodstmtHead = NULL;
   static Floodstmt	*stmtPtr = NULL;
   
   static char 		systemdate[DATE_LEN+1];
   
   char 		startdate[DATE_LEN+1], enddate[DATE_LEN+1];
   char 		where[300];
   struct tm 		*system_time;
   int 			within_range;

   
   /* initialize */
   
   *impact_found = FALSE;  
   
   
   /* if this is the first time that this forecast point is being
      accessed, get the impact statements and set the pointer to
      the first one */
   
   if (first_read_for_fp == TRUE)
   { 
      first_read_for_fp = FALSE;
      
      
      /* set the where clause to get only those stages in the stage range.
	 order them so highest ones are first. add a little fluff
	 to the value range to account for precision issue where
	 numbers like 32.1 are represented as 32.099998.  a study of the
	 precision shows that below 16.2, numbers are represented okay,
	 below 32.1, the numbers ending in .2,.3,.7,.8 are not okay,
	 and > 32.1, all but the .5 is bad... */	 
      
      sprintf (where,
	       " WHERE lid = '%s' AND impact_value >= %f AND impact_value <= %f"
	       " ORDER BY impact_value DESC ", fp[fpindex].id,
	       pcc->impact.lower_stage[fpindex] - 0.001, 
	       pcc->impact.upper_stage[fpindex] + 0.001);
      floodstmtHead = GetFloodstmt(where);
      
      if (floodstmtHead != NULL)
	 stmtPtr = (Floodstmt *) ListFirst(&floodstmtHead->list);
      else
      {
	 stmtPtr = NULL;
	 log_msg(MISSING_IMPACT, fp[fpindex].id);
      }
		    
      
      /* convert the format of the current time, each first time thru... */
      
      system_time = gmtime(&misc->system_time);   
      strftime(systemdate, DATE_LEN+1, "%m/%d/%Y", system_time);
   }
   
   
   /* if not first time, then advance the pointer to the next impact statement */
   
   else
   {
      if (stmtPtr != NULL)
         stmtPtr = (Floodstmt *)ListNext(&stmtPtr->node);
      else
      {
	 log_msg(FREE_NULL_MEMORY, "in get_impact_chosen(), stmtPtr");
      }
   }
   
   
   /* loop on the impact statements until find a match or no match
      is found after looking at all statements */
   
   while (stmtPtr != NULL)
   {      
      /* convert the date format and check the impact time of year */
      
      sprintf(startdate, "%s/1970", stmtPtr->datestart);
      sprintf(enddate,   "%s/1970", stmtPtr->dateend);
      within_range = check_impact_daterange(systemdate, startdate, enddate);
      
      
      /* load the impact text if it is within the time-of-year range  */
      
      if (within_range)
      {	 
	 strcpy(misc->longstring, stmtPtr->statement); 
	 misc->flt = stmtPtr->impact_value;
	 *impact_found = TRUE;
	 
	 break;
      }
      
      
      /* get the next impact */
      
      else
	 stmtPtr = (Floodstmt *)ListNext(&stmtPtr->node);
   }
   
   
   /* if at the end of the impacts, free the memory */
   
   if (stmtPtr == NULL)
   {
      if (floodstmtHead)
	 FreeFloodstmt(floodstmtHead);
      else
      {
	 log_msg(FREE_NULL_MEMORY, "in get_impact_chosen(), Head");
      }
      first_read_for_fp = TRUE;
   }
      
   return;
}


/*********************************************************************
   
   get_impact_chosen()
   
   PURPOSE
   This function returns a boolean indicating whether the special test
   message output should be written into the generated product.
   
   *******************************************************************/
int get_test_setting(int workstation_mode,
                     int vtec_flag,
		     int vtec_mode)
{
   int	write_test_flag;
   
   
   if (workstation_mode == TEST_MODE     ||
       workstation_mode == PRACTICE_MODE ||
       (vtec_flag == TRUE && 
        vtec_mode == VTEC_CAT_TEST))
      write_test_flag = TRUE;
   
   else 
      write_test_flag = FALSE;
   
   
   return(write_test_flag);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

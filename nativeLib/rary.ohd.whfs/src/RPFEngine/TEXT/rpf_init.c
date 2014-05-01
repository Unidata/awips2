#include <unistd.h>

#include <stdio.h>

#include "rpf_protos.h"
#include "read_pccnames.h"
#include "GetSuffix.h"

void rpf_init (fp_struct *fp, int *numfps,
			   grp_struct *grp, int *numgrps,
			   county_struct *cnty, int *numcnty,
			   vtecinfo_struct *vtecinfo, misc_struct *misc, pcc_struct *pcc,
			   pccnames_struct *pccnames, templatenames_struct *templatenames,
			   fcsttrend_info_struct *fcsttrend_info, int mode);

void init_pccdef(fp_struct *fp, int *numfps,
		         grp_struct *grp, int *numgrps,
			     county_struct *cnty, int *numcnty,
				 vtecinfo_struct *vtecinfo, misc_struct *misc,
				 pcc_struct *pcc, templatenames_struct *templatenames);

void load_pcclist(misc_struct *misc, pccnames_struct *pccnames);

void rpf_init (fp_struct *fp, int *numfps,
		       grp_struct *grp, int *numgrps,
		       county_struct *cnty, int *numcnty,
		       vtecinfo_struct *vtecinfo, misc_struct *misc, pcc_struct *pcc,
		       pccnames_struct *pccnames, templatenames_struct *templatenames,
		       fcsttrend_info_struct *fcsttrend_info, int mode)
{

   time_t 	checktime;

   /* get the environment variables that point to the directories,
      including the log directories.  pass a null file_suffix to
      force usage of the pid value. */
   get_envdirs("");

   /* allocate the memory that is not dependent upon the number of
      forecast points and groups, therefore this need only be done once. */
   malloc_templatenames(&templatenames);

   /* allocate and get the portion of the misc structure not dependent
      upon the number of forecast points, groups and counties. this is done
      once only. */

   malloc_miscmain(&misc);
   get_misc(misc);
   strcpy(misc->selected_office, misc->hsa);

   misc->issuance_set = FALSE;
   misc->expire_set   = MISSINGVAL;
   misc->workstation_mode = mode;
   misc->batch_mode = FALSE;


   /* always start the program as the host office.
      get its forecast point, grp, and county info first. */

   log_msg("", "Retrieving forecast point, group and county data...");
   time(&checktime);
   printf("Loading fp, grp, county info for %s; %s",
	  misc->selected_office, asctime(gmtime(&checktime)));

   get_fp_grp_county(misc->selected_office, numfps, fp,
					 numgrps, grp, numcnty, cnty);

   printf("Loaded fp[%d], grp[%d], county[%d]\n", *numfps, *numgrps, *numcnty);

   time(&checktime);
   printf("Loading assorted program support data; %s",
	  asctime(gmtime(&checktime)));


   // allocate the memory dependent upon the number of
   // forecast points, groups and counties
   malloc_vtecinfo(*numfps, &vtecinfo);
   //malloc_miscsubs(numfps, numgrps, numcnty, misc);
   //malloc_pcc(numfps, numgrps, &pcc);

   // initialize the default pcc vtec info to true so that the obs stage
   // filter is applied before computing the vtec-based recommendations.
   // then the recommended pcc info is loaded, and the filter is applied
   // once again.
   pcc->product.vtec_flag = TRUE;
   strcmp(pcc->product.vtec_default_signif, SIGNIF_WARNING);

   // read file FcstDetailTrend_data.xxx which is related to the
   // trend phrases for forecast data
   log_msg("", "Reading trend phrase file...");
   //printf("debug1\n");
   read_trendphrase(misc->selected_office);
   //printf("debug2\n");

   /*
   // get the template names for the selected office
   log_msg("", "Reading template names...");
   read_names(misc->selected_office, templatenames);
   log_templatenames(templatenames);


   // load the river, previous vtec, and previous product info.
   load_fresh_data();

   // determine/recommend and select which product to generate and which
   // forecase points to include in the product; this info is based on
   // vtec characteristics.
   log_msg("", "Computing recommended product and forecast pts and set to them...");
   select_pid_and_fps(numfps, fp, numgrps, grp, pcc, misc, vtecinfo);

   // run code from show_main_window
   //init_pccdef();  ->  whfs/rpf_actions.c
   //load_pcclist(); -> whfs/rpf_actions.c
   //set_recprod_label(); -> whfs/rpf_actions.c handle directly by GUI
   //load_ids(); -> whfs/setlocs_actions.c handle directly by GUI
   //set_canned_pts(); -> whfs/setlocs_actiosn.c handle directly by GUI
   init_pccdef(fp, numfps, grp, numgrps, cnty, numcnty,
		       vtecinfo, misc, pcc, templatenames);
   load_pcclist(misc, pccnames);
   */
}

// FROM WHFS/rpf_actions.c
void init_pccdef(fp_struct *fp, int *numfps,
		         grp_struct *grp, int *numgrps,
			     county_struct *cnty, int *numcnty,
				 vtecinfo_struct *vtecinfo, misc_struct *misc,
				 pcc_struct *pcc, templatenames_struct *templatenames)
{
	char pcc_file[MAXLEN_FILENAME];
	int prod_index = convert_prodid_to_index(pcc->product.prod_categ);

	/* set the name of the pcc file to be used. */
	set_pccfilename(misc, prod_index, pcc_file);

	strcpy(misc->pcc_file, pcc_file);

	/* since new pcc ettings are about to be loaded,
	   desensitize the issuance options */
	// TODO: will need to be handled by the GUI
	//sensitize_issuance(FALSE);

	/* read and load the pcc information.
	   this function also loads the impact and comparison info. */
	load_product_content(numfps, fp, numgrps, grp, numcnty, cnty, pcc_file,
			templatenames, misc, pcc, vtecinfo);
}

/************************************************************************
   load_pcclist()

   PURPOSE
   Loads the scrolled list for the product content definition sets
   shown in the main Rpf window and defines the initial definitions.

   NOTES
   This function retrieves the information each time it is invoked.
   It is designed to be able to be called more than once, although
   some mods may be needed to ensure that the previously selected item
   is still selected after a new item is added.
   The first time this function is called, the default item in the
   list is selected.

   ALL WINDOWING CODE HAS BEEN STRIPPED, only loads data into lists.
   *********************************************************************/

void load_pcclist(misc_struct *misc, pccnames_struct *pccnames)
{
   /* get the list of available pcc file names for the
      selected office and load into a linked list */

   log_msg("", "Reading pcc file name information...");
   pccnames = read_pccnames(misc);

   /* if there is no pcc information available, then
      abort the program */
   if (pccnames == NULL)
      log_msg(NO_PCCINFO, misc->selected_office);
   return;
}

/*********************************************************************
   rpf_main.c

   main()
   get_startup_options()
   abort_rpf()

   ********************************************************************/

#include <time.h>               /* time library */
#include <stdlib.h>             /* standard library */
#include <stdio.h>              /* standard io library */
#include <string.h>             /* string library */
#include <unistd.h>             /* Provides the prototype getopt ( ) */
#include <ctype.h>

#include "rpf_protos.h"         /* protypes for functions */

#define USER_SUFFIX_LEN 9

/*********************************************************************
   main()

   PURPOSE
   Main module for the River Product Formatter program.

   NON-INTERACTIVE VERSION. Require to specify the database name,
   product definition set (pcc file). The forecast points included
   in the generated product needed to be specified after keyword
   "INCLUDE_POINTS" in the pcc file.

   ********************************************************************/

int rpf_batch_main(int argc, const char ** argv)
{
   char 	msgstr[MAXLEN_STRING];
   char 	startup_pcc_file[MAXLEN_FILENAME];
   char		user_office[HYD_SERV_LEN + 1] = "";
   char		user_suffix[USER_SUFFIX_LEN + 1];
   int		num_tpl_files, num_default_pcc_files, num_total_pcc_files;
   int 		fpindex, i, status;
   char         *pcc_str;

   int 			numfps, numgrps, numcnty;
   fp_struct		*fp;
   grp_struct 		*grp;
   county_struct        *cnty;
   vtecinfo_struct	*vtecinfo;
   misc_struct 		*misc;
   pcc_struct		*pcc;
   templatenames_struct *templatenames;

   /* get the command line options specified with the program */

   rpf_batch_get_startup_options(argc, argv, startup_pcc_file, user_suffix);


   /* define the directories and other items;
      issue startup message - this needs to be after the get_envdirs
      call since the directories must be defined.
      the user_suffix allows the log files to be explicitly named. */

   get_envdirs(user_suffix);

   log_msg("", "Starting RIVERPRO batch program...\n");


   /* allocate and get the misc structure data that is not dependent
      on the numnber of forecast points, groups and counties.  */

   log_msg("", "Retrieving miscellaneous information...");
   malloc_miscmain(&misc);
   get_misc(misc);

   misc->issuance_set = misc->expire_set = FALSE;
   misc->batch_mode = TRUE;

   /* extract the office identifier from the pcc name.
      this step must be done after the hsa value is loaded
      into the misc structure.  */

   if (startup_pcc_file != NULL)
   {
      pcc_str = strstr(startup_pcc_file, ".pcc.");
      if (pcc_str != NULL)
         strncpy(user_office, (pcc_str+strlen(".pcc.")), HYD_SERV_LEN);
   }

   if (strlen(user_office) > 0)
      strcpy(misc->selected_office, user_office);
   else
      strcpy(misc->selected_office, misc->hsa);


   /* check that the office specified is valid.  if the files are
      not available, the program issues an error message and aborts.
      if the required office files are not there, abort.
      since this program run is a one-office run, don't botther
      loading up the fields of the misc->office info.
      this needs to be done, despite the requirement for a pcc file,
      because the template files must also be present. */

   status = check_office_files(misc->selected_office,
			       &num_tpl_files,
			       &num_default_pcc_files,
			       &num_total_pcc_files);

   sprintf(msgstr, "For %s, number of reqd pcc file, template files=%d %d\n",
	   misc->selected_office, num_default_pcc_files, num_tpl_files);
   log_msg("", msgstr);

   /* only if status is 0 and missing template files, then abort */

   if ((!status) && (num_tpl_files < NUM_TEMPLATE_FILES))
      log_msg(MISSING_OFFICE_FILES, misc->selected_office);


   /* load the fp and misc struct data stored in the database. */

   log_msg("", "Retrieving static forecast point, group and county data...");
   get_fp_grp_county(misc->selected_office, &numfps, &fp, &numgrps, &grp,
                     &numcnty, &cnty);


   /* allocate memory that is dependent on the number of fps, grps, county */

   malloc_vtecinfo(numfps, &vtecinfo);
   malloc_miscsubs(numfps, numgrps, numcnty, misc);
   malloc_pcc(numfps, numgrps, &pcc);


   /* load the previous event info for the forecast points.
      this needs to be done after the numfps have been determined
      so that the vtecinfo is allocated.  use the event info to
      obtain the product info for the event.
      this info is used for the recommendations, which are not used
      for the batch version of RiverPro; but some of the info is also
      used for the riverpro variables, so load it in. */

   log_msg("", "Loading previous event info and computing previous-based info...");
   load_prevevent_info(numfps, fp, misc, vtecinfo);
   load_fpprev_data(numfps, fp, vtecinfo, misc);


   /* this memory is not dependent on the number of fps or grps or counties.
      allocate, then get the template names. */

   log_msg("", "Reading template names...");
   malloc_templatenames(&templatenames);
   read_names(misc->selected_office, templatenames);

   /* get the curobs/maxfcst stage or discharge data.
      also compute the group level info. */

   log_msg("", "Retrieving dynamic max ob/fcst data...");

   for (fpindex = 0; fpindex < numfps; fpindex++)
      get_fpmofo_and_ts_ifneeded(fpindex, fp, vtecinfo, pcc);

   compute_grp_county_mofo(fp, numgrps, grp, numcnty, cnty);


   /* because the batch mode of RiverPro expects the pcc file to be specified,
      there is no need to compute the recommendations.  therefore, the
      select_pid_and_fps() function is not called.  in the user-specified
      pcc file, the user should specify which forecast points to include
      because no points are included by default */

   for (i = 0; i < numfps; i++)
      misc->fps_included[i] = FALSE;

   for (i = 0; i < numgrps; i++)
      misc->grps_included[i] = FALSE;

   for (i =0; i < numcnty; i++)
      misc->cnty_included[i] = FALSE;

   misc->numgrps_included = 0;
   misc->numcnty_included = 0;


   /* read and load in the pcc file.  then define the crest and impact
      recommendations.  this also sets the points to include if specified
      in the pcc file. */

   log_msg("", "Defining the product content; reading the pcc file...");

   if (strlen(startup_pcc_file) > 0)
      strcpy(misc->startup_pcc_file, startup_pcc_file);

   define_product_content(numfps, fp, numgrps, grp, numcnty, cnty,
			  templatenames, misc,
			  pcc, vtecinfo);


   /* generate the product using the instructions in the product
      content control file; set the issuance and expire flags
      to indicate that they are not set yet */

   log_msg("", "Creating product...");
   create_product(numfps, fp, numgrps, grp, numcnty, cnty,
		  vtecinfo, pcc, misc, WORK_FILE);


   /* write status messages */

   log_msg("warncnt", "");
   sprintf(msgstr, "\nCreated a %s (%s) product.",
	   pcc->product.prod_categ, pcc->product.product_cnx);
   log_msg("", "\nRIVERPRO Completed.\n");


   /* free allocated memory */

   free_memory(numfps, fp, numgrps, grp, numcnty, cnty,
               vtecinfo, misc, pcc, templatenames);


   /* exit program */

   CloseDbms();
   printf("RiverPro batch program exited normally\n");
   exit(0);
}


/*********************************************************************
   get_startup_options()

   PURPOSE
   Get the default/startup pcc filename.

   ********************************************************************/

void rpf_batch_get_startup_options(const	int		argc,
			 	char		**argv,
			 	char		startup_pcc_file[],
				char		user_suffix[])

{
   FILE 	*file_ptr;
   extern char  *optarg;
   char 	*dbms;
   int 		c, status;
   int		slen;
   char		msgstr[140];
   int		db_given, pccfile_given, suffix_given;


   /* initialize */

   db_given = pccfile_given = suffix_given = FALSE;

   memset(startup_pcc_file, 0, MAXLEN_FILENAME);
   memset(user_suffix, 0, 10);


   /* check args */

   if (argc <= 1)
   {
      fprintf(stderr,
	      "usage: rpf_batch -d<dbname> -p<pccfilename> [-s<filesuffix>]\n");
      exit(0);
   }


   while ((c = getopt(argc, argv, "d:p:s:")) != -1)
   {
      switch (c)
      {
         case 'd':
	    /* open the database */

            dbms = optarg;

	    status = OpenDbms(dbms);
	    if (status != 0)
	    {
	       sprintf(msgstr, "%s %s", DB_OPENFAIL, dbms);
	       fprintf(stderr, "%s\n", &msgstr[1]);
	       exit(0);
	    }
	    db_given = TRUE;
            break;

	 case 'p':
	    strcpy(startup_pcc_file, optarg);

	    file_ptr = fopen(startup_pcc_file, "r");
	    if(file_ptr == NULL)
	    {
	       sprintf(msgstr, "%s %s", FILE_OPENERR, startup_pcc_file);
	       fprintf(stderr, "%s\n", &msgstr[1]);
	       exit(0);
	    }
	    fclose(file_ptr);
	    pccfile_given = TRUE;
	    break;

         case 's':
	    /* get the file suffix */

            strcpy(user_suffix, optarg);

	    slen = strlen(user_suffix);
	    if (slen == 0 || slen > USER_SUFFIX_LEN)
	    {
	       sprintf(msgstr, "%s %s", INVALID_SUFFIX, user_suffix);
	       fprintf(stderr, "%s\n", &msgstr[1]);
	       exit(0);
	    }
	    suffix_given = TRUE;
            break;
      }
   }

   if (!db_given)
   {
      fprintf(stderr, "Specify database with -d argument\n");
      exit(0);
   }

   if (!pccfile_given)
   {
      fprintf(stderr,
        "Specify product content control file (path/xxx.pcc.XXX) with -p argument\n");
      exit(0);
   }
   return;
}


/********************************************************************

   abort_rpf()

   PURPOSE
   Aborts RPF program.  An identically named function
   exists for the interactive version.  That is why this function
   is located in the file that is unique to the version and also
   why no prototype is given for the function.

   ********************************************************************/

void abort_rpf(char *outmsg)
{
   printf("%s\n", outmsg);
   printf("Check error and message log files for more information...\n");

   exit(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "gen_areal_qpe.h"
#include "GeneralUtil.h"        /* for get_apps_defaults */

 /***********************************************************************

    gen_areal_qpe

    Generates a mosaic of multiple RFC QPE products as needed and crops the
    resulting mosaic into a local-sized grid defined by the MPE coord file.
    Reads the QPE products from an RFC that are placed in a temporary
    directory to hold the just-decoded GRIB products received from the SBN.


  ***********************************************************************/

/*********************************************************************
   get_gaq_tokens()

   PURPOSE
   Get the token values needed for the application.

 ********************************************************************/

static void get_gaq_tokens(gaq_options_struct	*options)
{
   static char * gaq_input_dir_token = "gaq_input_dir";
   static char * gaq_input_xmrg_1hr_dir_token = "gaq_xmrg_1hr_dir";
   static char * gaq_input_xmrg_6hr_dir_token = "gaq_xmrg_6hr_dir";
   static char * gaq_input_xmrg_24hr_dir_token = "gaq_xmrg_24hr_dir";
   static char * gaq_output_xmrg_dir_token = "gaq_temp_xmrg_dir";
   static char * geo_data_token = "geo_data";
   static char * st3_rfc_token = "st3_rfc";

   char site_id[SITE_ID_LEN + 1];
   int tokenlen, valuelen;
   char	path_string[MAX_PATH_LEN + 1];


   /* use geo_data for coord file location */

   tokenlen = strlen(geo_data_token);
   get_apps_defaults(geo_data_token, &tokenlen, path_string, &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             geo_data_token);
      exit(0);
   }
   else
   {
      strcpy(options->geo_coord_path, path_string);
      printf("geo_data token= %s\n",path_string);
   }


   /* using st3_rfc for site name */

   tokenlen = strlen(st3_rfc_token);
   get_apps_defaults(st3_rfc_token, &tokenlen, site_id, &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             st3_rfc_token);
      exit(0);
   }
   else if (valuelen > SITE_ID_LEN)
   {
      printf("ERROR: %s token too long -- program stopping.\n",
             st3_rfc_token);
      exit(0);
   }
   else
   {
      strcpy(options->site_id, site_id);
      printf("%s token= %s\n",st3_rfc_token, site_id);
   }


   /* get path for input netcdf files */

   tokenlen = strlen(gaq_input_dir_token);
   get_apps_defaults(gaq_input_dir_token, &tokenlen, path_string, &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             gaq_input_dir_token);
      exit(0);
   }
   else
   {
      strcpy(options->input_data_path, path_string);
      printf("%s token= %s\n",gaq_input_dir_token,path_string);
   }


   /* get path for output xmrg files */

   tokenlen = strlen(gaq_output_xmrg_dir_token);
   get_apps_defaults(gaq_output_xmrg_dir_token, &tokenlen, path_string,
                     &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             gaq_output_xmrg_dir_token);
      exit(0);
   }
   else
   {
      strcpy(options->output_xmrg_path, path_string);
      printf("%s token= %s\n",gaq_output_xmrg_dir_token, path_string);
   }


   /* get path for 1hr input qpe files */
   tokenlen = strlen(gaq_input_xmrg_1hr_dir_token);
   get_apps_defaults(gaq_input_xmrg_1hr_dir_token, &tokenlen, path_string,
                     &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             gaq_input_xmrg_1hr_dir_token);
      exit(0);
   }
   else
   {
      strcpy(options->input_xmrg_path_1hr, path_string);
      printf("%s token= %s\n",gaq_input_xmrg_1hr_dir_token, path_string);
   }

   /* get path for 6hr input qpe files */
   tokenlen = strlen(gaq_input_xmrg_6hr_dir_token);
   get_apps_defaults(gaq_input_xmrg_6hr_dir_token, &tokenlen, path_string,
                     &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             gaq_input_xmrg_6hr_dir_token);
      exit(0);
   }
   else
   {
      strcpy(options->input_xmrg_path_6hr, path_string);
      printf("%s token= %s\n",gaq_input_xmrg_6hr_dir_token, path_string);
   }

   /* get path for 24hr input qpe files */
   tokenlen = strlen(gaq_input_xmrg_24hr_dir_token);
   get_apps_defaults(gaq_input_xmrg_24hr_dir_token, &tokenlen, path_string,
                     &valuelen);
   if(valuelen == 0)
   {
      printf("ERROR: %s token not found -- program stopping.\n",
             gaq_input_xmrg_24hr_dir_token);
      exit(0);
   }
   else
   {
      strcpy(options->input_xmrg_path_24hr, path_string);
      printf("%s token= %s\n",gaq_input_xmrg_24hr_dir_token, path_string);
   }

   return;
}

/*********************************************************************
   get_gaq_options()

   PURPOSE
   Get the options specified for the application.
   This function opens the Informix database.

 ********************************************************************/

static void get_gaq_options(int	argc,
		            char **argv,
		            gaq_options_struct	*options)
{
   extern char 	*optarg;
   extern int	optind;
   extern int   optopt;

   int	c, i;
   int  slen;
   int	num;

   char	*token;
   char	token_delimiter[] = ",";


   /* initialize non-mandatory options */

   options->num_rfcs      = 0;
   options->num_durations = 0;


   /* check the number of arguments */

   if (argc != 3)
   {
       printf("Too few arguments.\n");
       printf("Usage: gen_areal_qpe -r<RFClist> -h<hour_list>\n");
       exit(0);
   }

   /* process and load the number of command line arguments */

   while ((c = getopt(argc, argv, "r:h:")) != -1)

   {
      switch (c)
      {

	 case 'h':
	    /* get the hour duration list */

	    token = strtok(optarg, token_delimiter);
	    while(token != NULL)
	    {
	       num = atoi(token);

	       if (options->num_durations == MAX_DURATIONS)
	          printf("Too many durations (>%d) specified.\n",
		         MAX_DURATIONS);

	       else if (num >= 0 && num <= 24)
	       {
	          options->durations[options->num_durations] = num;
		  options->num_durations++;
	       }

	       else
		  printf("Invalid duration given: %d\n", num);

	       token = strtok(NULL, token_delimiter);
	    }

	    break;


	 case 'r':
	    /* get the rfc list */

	    token = strtok(optarg, token_delimiter);
	    while(token != NULL)
	    {
	       slen = strlen(token);

	       if (options->num_rfcs == MAX_RFCS)
	          printf("Too many RFCs (>%d) specified.\n",
		          MAX_RFCS);

	       else if (slen == 5)
	       {
	          strcpy(options->rfcid[options->num_rfcs], token);;
		  options->num_rfcs++;
	       }
	       else
		  printf("Invalid rfc given: %s\n",token);

	       token = strtok(NULL, token_delimiter);
	    }

	    break;


         default:

	    break;
      }
   }


   /* ensure the mandatory command line arguments were given */

   if (options->num_durations == 0 || options->num_rfcs == 0)
   {
      printf("Invalid arguments specified.\n");
      printf("Usage: gen_areal_qpe -r<RFClist> -h<hour_list>\n");
      exit(0);
   }

   /* log the command line option info */

   printf("RFCs specified: ");
   for (i = 0; i < options->num_rfcs; i++)
      printf("%s ", options->rfcid[i]);
   printf("\n");

   printf("Durations specified: ");
   for (i = 0; i < options->num_durations; i++)
      printf("%d ", options->durations[i]);
   printf("\n");


   return;

}

int gen_areal_qpe_main(int argc, const char ** argv)

{
   gaq_options_struct	gaq_options;
   int			num_files;


   /* log a message stating the version number */

   printf("gen_areal_qpe: Version OB8.3, 03/25/2008 \n");


   /* get the user options from the command line */
   get_gaq_options(argc, argv, &gaq_options);

   /* get the directory paths */
   get_gaq_tokens(&gaq_options);


   /* perform the operations */
   num_files = process_qpe_files(&gaq_options);

   /* return with the count of the number of files actually reported */
   return(num_files);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc/src/gen_areal_qpe/RCS/gen_areal_qpe.c,v $";
 static char rcs_id2[] = "$Id: gen_areal_qpe.c,v 1.5 2007/03/26 15:22:10 gsood Exp $";}
/*  ===================================================  */

}

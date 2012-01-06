/*******************************************************

   Reads a file of areas defined in lat-lon pairs and
   loads them into the GeoArea table.

********************************************************/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "DbmsAccess.h"
#include "DbmsDefs.h"


#include "geo_dbutil.h"
#include "geo_header_file.h"

int load_geoarea_main(int argc, const char ** argv)
{

   extern char     *optarg;
   int	c, i;
   int	status;
   char *infilename = NULL ;
   char *dbms  = NULL ;
   FILE *infile = NULL ;
   int	db_open, file_open, type_given;
   char	geotype[LONG_CODE_LEN + 1];
   int	dummy_rank = 0;

   /* verify the proper number of input args */

   if (argc < 4)
   {
      fprintf(stderr, "usage: load_geoarea -i<infile> -t<type> -d<dbms>\n");
      return(-1);
   }

   /* initialize flags */

   db_open = file_open = type_given = 0;


   /* iterate through arguments */

   while ((c = getopt(argc, argv, "d:i:t:")) != -1)
   {
      switch (c)
      {
	 case 'd':
            /* open the database */

            dbms = optarg;

            status = OpenDbms(dbms);
            if (status != 0)
	    {
	       fprintf(stderr, "Error (%d) opening database %s.\n",
		       status, optarg);
	       exit (-1);
	    }
	    else
	       db_open = 1;

	    break;


	 case 'i':
	    infilename = optarg;
	    infile  = fopen(infilename, "r");

	    if  (! (infile) )
	    {
	       fprintf(stderr, "Error opening input file %s.\n", optarg);
	       exit(-1);
	    }
	    else
	       file_open = 1;

	    break;

	 case 't':
	    strcpy(geotype, optarg);

	    for (i = 0; i < strlen(geotype); i++)
	       geotype[i] = toupper(geotype[i]);

	    if ((strlen(geotype) == 0 || strlen(geotype) > LONG_CODE_LEN) ||
		(strcmp(geotype, "COUNTY") != 0 &&
		 strcmp(geotype, "STATE")  != 0 &&
		 strcmp(geotype, "RESRVR") != 0 &&
		 strcmp(geotype, "ZONE")   != 0 &&
		 strcmp(geotype, "BASIN")  != 0))
		{
		   fprintf(stderr, "Invalid type field "
			   "(use COUNTY, STATE, RESRVR, ZONE, BASIN): %s\n",
			   optarg);
		}

	    else
	       type_given = 1;
	    break;

	 default:
	    break;
      }
   }


   /* check that args are specified */

   if (!db_open  || !file_open || !type_given)
   {
      fprintf(stderr, "usage: load_geoarea -i<file> -t<type> -d<dbms>\n");
      exit(-1);
   }



   /* process the data, the rank field is only used for
      reading vector data such as rivers and roads */

   load_geodata(infile, geotype, dummy_rank, GEOAREA);


   /* close the file and database */

   exit_geoutil(infile);

   return 0 ;
}



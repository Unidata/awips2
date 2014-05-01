#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


#include "time_convert.h"
#include "set_stnclass.h"
#include "set_stnall.h"

#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "DbmsAccess.h"

/************************************************************************
   main()

   PURPOSE
   To define the station class attributes.

   ************************************************************************/

int set_stnclass_main (int argc, const char ** argv)
{
   int status;

   extern char	*optarg;
   char		*dbms;
   char 	lid[LOC_ID_LEN + 1];
   int		lid_given = 0;
   int		c;


   /* process the command line arguments that specify the database. */

   if (argc < 2)
   {
      fprintf(stderr, "usage: %s -d<database> [-l<lid>]\n", argv[0]);
      exit(-1);
   }

   status = 0;
   while ((c = getopt(argc, argv, "d:l:")) != -1)
   {
      switch (c)
      {
	 case 'd':
	    dbms = optarg;
	    if (OpenDbms(dbms) != OK)
	    {
	       fprintf(stderr, "Unable to open dbms.\n");
	       exit(-2);
	    }
	    else
	       status = 1;
	    break;

	 case 'l':
	    strcpy(lid, optarg);
	    lid_given = 1;
	    break;

	 default:
	    exit(-3);
      }
   }
   if (status == 0)
   {
      fprintf(stderr, "usage: %s -d<database> [-l<lid>]\n", argv[0]);
      exit(-1);
   }


   /* process all stations */

   if (!lid_given)
      set_stnclass_all();

   else
   {
      printf("Setting stn_class for %s\n", lid);
      set_stnclass(lid);
   }


   exit (0);
}




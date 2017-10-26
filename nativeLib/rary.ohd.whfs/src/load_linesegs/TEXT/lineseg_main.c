/* Modification History
   Date         Name              Description
   11/18/01     Bryon Lawrence    Fixed a problem that was preventing
                                  this program from processing any basins
                                  when the "-a" option was not specified.
   11/17/03     Bryon Lawrence    Added the "-t" option which allows
                                  the user to specify the boundary type.
   10/8/04      Moria Shebsovich  Added the "-i" option with allows
                                  to read from the ascii file. */
#include "DbmsDefs.h"
#include "lineseg_main.h"
/**********************************************************************/


static void get_lineseg_options(const	int argc,
			        char	**argv,
			        FILE ** infile,
                                char 	*area_id,
			        char   *boundary_type,
			        int    *verbose,
			        int    *printOnly)
{
     extern char  *optarg;
     extern int   optind, optopt;
     char *infilename = NULL ;
     char *dbms;
     int 	c ;
     int        compare_status ;
     int	db_open , file_open ;
     int        ret ;

     /*
     initialize
     */
     db_open = 0;
     file_open = 0;
     *verbose = 0;
     *printOnly = 0;

     if (argc < 3)
     {
	  fprintf(stderr,
		  "\nusage: %s -i<infile> -d<dbms> [-a<area_id>] "
		  "[-t<boundary_type>] [-v] [-p] \n", argv[0]);

          fprintf(stderr, "-i<infile> specifies that the data in <infile>"
                          " will be loaded into the geoarea database table.\n");
          fprintf(stderr, "-d<dbms> specifies a database\n");
	  fprintf(stderr, "-a<aread_id> specifies a single area to process -"
		          " the default is to process ALL areas\n");
	  fprintf(stderr, "-t<boudary_type> specifies the boundary type to"
			  " process.  The default is to process all\n"
			  "boundary types. Acceptable boundary types are"
			  " BASIN, COUNTY, RESRVR, STATE, ZONE.\n" ) ;
	  fprintf(stderr, "-v specifies verbose mode - prints all"
		          " calculated values\n");
	  fprintf(stderr, "-p specifies print only mode - no database"
		          " writing is performed\n");
	  exit(-1);
     }

     while ((c = getopt(argc, argv, "i:d:a:t:vp")) != -1)
     {
	  switch (c)
	  {
	       case 'd':
		    /* open the database */

		    dbms = optarg;

		    ret = OpenDbms(dbms);
		    if (ret != 0)
		    {
			 fprintf(stderr, "lineseg_main: Unable to open database: %s\n",
				 dbms);
			 exit(-2);
		    }
		    else
			 db_open = 1;
		    break;

	       case 'a':

		    /* define the optional id for the area being considered */
		    if ( ( optarg == NULL ) ||
                         ( strlen ( optarg ) == 0 ) )
                    {
                        fprintf (stderr , "Empty area_id specified\n" ) ;
                        exit ( -1 ) ;
                    }
                    else if ( strlen ( optarg ) > LOC_ID_LEN )
		    {
		        fprintf(stderr, "Specified area_id too long: %s\n",
				optarg);
		        exit ( -1 ) ;
		    }

		    strcpy ( area_id , optarg ) ;

		    break;

               case 'i':

                    /* Open input file */

                    infilename = optarg;
                    *infile  = fopen(infilename, "r");

                    if  (! (*infile) )
                    {
                       fprintf(stderr, "Error opening input file %s.\n", optarg);
                          exit(-1);
                    }
                    else
                        file_open = 1;

                        break;

	       case 'p':

		    *printOnly = 1;
		    printf("printOnly enabled\n");
		    break;

               case 't':

                    if ( ( optarg == NULL ) ||
                         ( strlen ( optarg ) == 0 ) )
                    {
                       fprintf ( stderr ,
                                 "Empty boundary_type specified.\n" ) ;
                       exit ( -1 ) ;
                    }
                    else if ( strlen ( optarg ) > BOUNDARY_TYPE_LEN )
                    {
                       fprintf ( stderr , "Specified boundary_type too long: "
                                          "%s\n" , optarg ) ;
                       exit ( -1 ) ;
                    }
                    else
                    {
                       /* The RESRVR boundary_type is not supported. */
                       compare_status = strcmp ( optarg , "RESRVR" ) ;

                       if ( compare_status == 0 )
                       {
                          fprintf ( stderr , "RESRVR boundary_type is not "
                                             "supported.\n" ) ;
                          exit ( -1 ) ;
                       }
                    }

                    strcpy ( boundary_type , optarg ) ;
		    break ;

	       case 'v':

		    *verbose = 1;
		    printf("verbose enabled\n");
		    break;

	       default:
		    break;
	  }
     }

     return;
}

/*****************************************************************************/

int load_linesegs_main ( int argc , const char ** argv)
{
   char 	areaId [ LOC_ID_LEN + 1 ] ;
   char         boundaryType [ BOUNDARY_TYPE_LEN + 1 ] ;
   int		verbose = 0;
   int		printOnly = 0;
   FILE *infile = NULL ;
   char geotype[LONG_CODE_LEN + 1];
   int  dummy_rank = 0;

   /*
        clear areaId and boundaryType arrays.
   */
   memset ( areaId , '\0' , LOC_ID_LEN + 1 ) ;
   memset ( boundaryType , '\0' , BOUNDARY_TYPE_LEN + 1 ) ;

   /*
        get options from the command line
	and open database
   */
   get_lineseg_options(argc, argv,  & infile, areaId, boundaryType ,
                       & verbose, & printOnly);

   createLinesegsForAreas(infile, geotype, dummy_rank, GEOAREA, verbose, printOnly);

   /*
        close database
   */
   CloseDbms();

   return 0;
}

/*********************************************************************************
*                                                                                *
* Function:  main_create_bas_bound.c                                             *
* Purpose:   Takes the output from an NWSRFS run (@DUMP PUNCH BASIN)             *
*            processes one basin at a time and writes information to an ascii    *
*            file.  Later call to function create_bas_bound_bin reads            *
*            the ascii file and creates the binary file in a standard binary     *
*            format for RFC applications.                                        *
*                                                                                *
*            Allows command one line option:                                     *
*                  ascii:  that assumes existance of ascii file (in proper       *
*                          format) then only calls routine to create binary      *
*                          file from the ascii one.                              *
*               OR                                                               *
*                  filename:  filename for the raw punch from NWSRFS             *
*                             (can be full path name)                            *
*                                                                                *
*            Defaults:  If there are no command line arguments the program       *
*                       will look for map_basin.orig (the NWSRFS dump file)      *
*                       and call create_bas_bound_ascii to create the            *
*                       map_basin.dat file.  The program then calls              *
*                       create_bas_bound_bin to read map_basin.dat and           *
*                       create the map_basin.bin file.                           *
*                                                                                *
* Written by:  Donna Page - HRL - 9 Feb. 1994                                    *
*                                                                                *
*********************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG 1

extern void create_bas_bound_ascii(char *, char *);
extern void create_bas_bound_bin(char *, char *);
extern char *build_filename(char *, char *);
extern int  get_apps_defaults(char *, int *, char *, int *);

int create_bas_bound_main(int argc, const char ** argv)
{
   int      len, len2;
   char     *nwsrfs_basin_fname;
   char     *ascii_fname;
   char     *bin_fname;
   char     path_name[512];
   char     rfc_name[9];
   char     temp_str[512]="\0";

   if(argc > 3)
   {
      printf("Only 2 command line arguments allowed - Try again");
      exit(1);
   }

   len = strlen("geo_data");
   get_apps_defaults("geo_data", &len, path_name, &len2);
   strcat(path_name, "/");
   len = strlen("ifp_rfc");
   get_apps_defaults("ifp_rfc", &len, rfc_name, &len2);
   strcat(path_name, rfc_name);

   if (DEBUG)
   		printf("token valus is: %s\n", path_name);

   if (argc ==3){

      len = strlen(argv[1]) + 1;
      strcpy(temp_str,"/ascii/");
      strcat(temp_str,argv[1]);
      ascii_fname = build_filename(path_name, temp_str);
      printf(" ascii_file_name=%s\n",ascii_fname);


      strcpy(temp_str,"/binary/");
      strcat(temp_str,argv[2]);
      bin_fname = build_filename(path_name, temp_str);
      printf(" bin_file_name=%s\n",bin_fname);

      create_bas_bound_bin(ascii_fname, bin_fname);


   }
   else if(argc == 2)
   {
		ascii_fname = build_filename(path_name, "/ascii/map_basin.dat");
		bin_fname   = build_filename(path_name, "/binary/map_basin.bin");

		if(strcmp(argv[1], "ascii") != 0)
		{
			if(strncmp(argv[1], "/", 1) == 0)
			{
				len = strlen(argv[1]) + 1;
				nwsrfs_basin_fname = malloc(len * sizeof(char));
				memset(nwsrfs_basin_fname, '\0', len);
				strcpy(nwsrfs_basin_fname, argv[1]);
				printf(" 1  nwsrfs_basin_fname=%s\n",nwsrfs_basin_fname);
			}
			else
			{
				len = strlen(path_name) + strlen("/ascii") + strlen(argv[1]) + 1;
				nwsrfs_basin_fname = malloc(len * sizeof(char));
				memset(nwsrfs_basin_fname, '\0', len);
				strcpy(nwsrfs_basin_fname, path_name);
				strcat(nwsrfs_basin_fname, "/ascii/");
				strcat(nwsrfs_basin_fname, argv[1]);
				printf(" 2  nwsrfs_basin_fname=%s\n",nwsrfs_basin_fname);
			}

			printf("ascii file name=%s\n",ascii_fname);
			printf("bin file name=%s\n",bin_fname);

			create_bas_bound_ascii(nwsrfs_basin_fname, ascii_fname);
			create_bas_bound_bin(ascii_fname, bin_fname);
		}
		else  /* argv[1]==ascii */
		{
			printf("ascii file name=%s\n",ascii_fname);
			printf("bin file name=%s\n",bin_fname);

			create_bas_bound_bin(ascii_fname, bin_fname);
			exit(0);
		}
	}
	else
	{
		nwsrfs_basin_fname = build_filename(path_name, "/ascii/map_basin.orig");
		printf("nwsrfs basin fname=%s\n",nwsrfs_basin_fname);

		ascii_fname = build_filename(path_name, "/ascii/map_basin.dat");
		printf("ascii file name=%s\n",ascii_fname);

		bin_fname = build_filename(path_name, "/binary/map_basin.bin");
		printf("bin file name=%s\n",bin_fname);

		create_bas_bound_ascii(nwsrfs_basin_fname, ascii_fname);

		create_bas_bound_bin(ascii_fname, bin_fname);
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/create_bas_bound/RCS/create_bas_bound_main.c,v $";
 static char rcs_id2[] = "$Id: create_bas_bound_main.c,v 1.1 2003/09/25 12:01:11 dws Exp $";}
/*  ===================================================  */

	return 0;

}

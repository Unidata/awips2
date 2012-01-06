/*
 * main_nc2grib.c
 *
 *  Created on: Aug 26, 2011
 *      Author: snaples
 */

/************************************************************************************
*
* nc2grib - GFE NetCDF to GRIB1 translator
*
* Dave Miller Wyle/IS, OHD/HSEB Version 4.1 August 2009
*
* This routine reads a NetCDF file created by the GFE command ifpnetCDF and
* creates a GRIB1 file from the parameter information.  This is required for
* the CHPS/FEWS application as the NetCDF file is not presently CF compliant.
* However, GRIB1 is self-describing and can be translated by the CHPS/FEWS
* application.
*
* At present, only the following parameters are converted:
* - Temperature
* - Height (Freezing Level)
* - Evaporation (Potential Evaporative Transpiration)
* - Precipitation
*
* Part of the difficulty is in choosing an equivalent GRIB1 parameter to the
* GFE parameter.  In the case of PET, this doesn't exactly match and
* Evaporation was chosed as a close substitute.
*
* In addition, since GRIB is particular in several areas, decided to have a
* lookup table file which will provide some of the values needed to correctly
* encode into GRIB.  In addition, this wasn't done for gribit and one has to
* modify the code whenever a new process is created.  However, reading from
* a text file requires no code change as long as the parameters don't change.
* That logic could perhaps change as well.
*
* The routine first uses standard C calls to read the netcdf file.  The structure
* of that file can be reviewed by reading the GFE help reference section on the
* ifpnetCDF command.
*
* The GRIB1 encoder routines, packgrib_.c, are actually open source from Bob Dattore at NCAR/UCAR
* http://dss.ucar.edu/libraries/grib/c_routines/packgrib_.c.  Note that there was an
* error in the code in that the PDS_EXT was thought to be able to contain anything the user
* thought worthy.  However, this is not the case and that section of the GRIB message
* is actually used by NCEP for ensemble forecast information.
*
* The mapping routines, cmapf/dmapf, are open source and available from NOAA/ARL.  These are used
* to retrieve information about the map projection based on information contained in the NetCDF
* file.
*
* Lastly, the copygb or copy GRIB executable, is also open source from Wesley Ebisuzaki
* of the Climate Prediction Center.  Source and info available at
* http://www.cpc.noaa.gov/products/wesley/copygb.html
*
* This routine very nicely copies the resulting GRIB file and translates/interpolates to NCEP
* Grid 218 for NPVU processing.  Had to modify the source in order for it to use 10km grid
* instead of the standard 12 km grid, but that wasn't too difficult.
*
* The routine reads NetCDF files with multiple time steps and outputs individual GRIB1 files
* according to their valid times.  This can be done for either forecast or observed grids.
*
* Version 4 allows users to combine all GRIB messages into one file.  This becomes useful
* when dealing with a lot of files for a parameter such as 1 hour QPF or temperature that
* goes out to 240 hours.
*
* This is still a work in progress and code can always be improved to increase efficiency.
*
**********************************************************************************************/
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <limits.h>

#include "DbmsAccess.h"
#include "DbmsDefs.h"   /* for LOC_ID_LEN */
#include "GeneralUtil.h"
#include "sqlca.h"
#include "time_convert.h"
#include "netcdf.h"
#include "packgrib.h"
#include "getopt.h"


#include "cmapf.h"

#define SECINHR         3600.
#define PATH_LEN        500
#define FILE_LEN        300
#define BUFFSIZE        1024
#define CMDSIZE         1000
#define COPYSIZE        4200000
#define MISCHECK        -9        /* if all data missing return this */
#define ZEROCHECK       -5        /* if all data is zero return this */
#define APSDEFERR	-1        /* this is an Apps Defaults error */
#define OPENERR         -2        /* this is an open file error */
#define FILEERR         -3	  /* error in file determination */
#define SUBERR          -4        /* subroutine return error */
#define UNERR           -6        /* unexpected or unhandled input to the program */
#define CDFERR          -7        /* error with the NetCDF file */
#define FILEOPERR       -8        /* this is a file operations error */
#define USAGE	         1        /* return for usage */
#define MALERR		-10	  /* memory allocation error */

/* This structure reads lookup values from a file called gfe2grib.txt and compares these
   against the information in the NetCDF file.
*/

typedef struct {
   char process[11];
   char gfename[20];
   int processid;
   int gribnum;
   int decscale;
   int timerange;
   int timeunit;
} mygfe2grib;

int nc_getAppsDefaults(const char* strToken, char* strTokenValue);

/************************************************************************
 * This function loads token value.
 * If token is not available, return -1; otherwise return 0.
 ************************************************************************/

int nc_getAppsDefaults(const char* strToken, char* strTokenValue)
{
    int tokenLen, tokenValueLen;

    tokenLen = strlen(strToken);

    get_apps_defaults((char *)strToken, &tokenLen,
                      strTokenValue, &tokenValueLen);

    if (tokenValueLen == 0)
    {
        strcpy(strTokenValue, "");
        return -1;
    }

    return 0;
}

int display_usage(void);

int timet_to_userformat_ansi(time_t timet, char *ansi, char *userformat);

int basetime_ansi_to_timet(char *ansi, time_t *timet);

int nc2grib_main (int argc, char *argv[])
{

   extern char *optarg;                /* these are for the getopt C library function */
   extern int optind, optopt;



   char fn[PATH_LEN+FILE_LEN]={'\0'};     /* complete input NetCDF path filename */


   char *infn=NULL;                      /* input NetCDF filename */
   char *ofntemp=NULL;                   /* output filename template for tailored date/time format */
   char *ofn=NULL;                             /* output GRIB1 filename */
   char outfn[PATH_LEN+FILE_LEN]={'\0'};  /* complete output GRIB1 path and filename */
   char outfnqpf[PATH_LEN+FILE_LEN]={'\0'};        /* output GRIB1 NPVU filename */
   char onegrib[PATH_LEN+FILE_LEN]={'\0'};   /* output path and combine GRIB file if desired */

   char *onegfname=NULL;                  /* output filename for combined GRIB file if desired */

   char *inpath=NULL;                     /* input NetCDF path */

   char *gribdir=NULL;                    /* output GRIB path */

   char command[CMDSIZE]={'\0'};          /* command string called via system */
   char fileline[LINE_MAX]={'\0'};        /* holds an input line from gfe2grib.txt file */
   char tmpNPVUfn[PATH_LEN+FILE_LEN]={'\0'};  /* temporary holding file for part of GRIB1 message */

   char wmohdr1[7]={'\0'};              /* first part of WMO header */
   char wmohdr2[5]={'\0'};              /* second part of WMO header */

   char crcrlf[3]={'\r','\r','\n'};     /* needed to separate WMO header from first part of GRIB message */
   unsigned char aspace={' '};          /* contains a space character for the header */
   unsigned char header[18]={'\0'};            /* full WMO header string */
   unsigned char printhdr[19]={'\0'};   /* for debug printing purposes */
   /* char printhdr[19]={'\0'};   /* for debug printing purposes */
   struct tm *curgmtime;                /* for containing the current time GMT, used in WMO header */
   time_t curtime, basetime_t;          /* time_t variables */
   char adayhrmin[7]={'\0'};            /* day, hour, minute info attached to WMO header */




   int numgfeparms=0;



   int numgfiles=0;    /* number of grib files for combining files into one if desired */
   char *gfiles[240];  /* array of char pointers for holding grib filenames if combining files */



   /* for reading the NetCDF file */
   int cdfid;                /* Netcdf id */
   int ndims;                /* number of dimensions */
   int nvars;                /* number of variables */
   int ngatts;               /* number of attributes */
   int recdim;
   long start[] = {0, 0, 0}; /* start at first value */
   long start1r[] = {0, 0};  /* accounts for netcdf with only 1 record and 2 dimensions of y,x */

   /* flags for different purposes: creating the header, verbose debugging,
      only processing 1 valid time in the NetCDF file (debugging)
   */
   int headflag=0;           /* NPVU file flag */
   int debugflag=0;          /* verbose debugging flag */
   int time1flag=0;          /* only process first NetCDF record */
   int iflag=0;              /* input filename flag */
   int oflag=0;              /* output filename flag */
   int nflag=0;              /* input path flag */
   int fflag=0;              /* filename format flag */
   int tflag=0;              /* output path flag */
   int bflag=0;              /* basis time flag */
   int pflag=0;              /* process ID flag */
   int qflag=0;              /* QPE flag for NPVU processing or estimate grid requiring basis time */
   int rflag=0;              /* reverse formatting flag */
   int helpflag=0;           /* display help flag */
   int errflag=0;            /* error flag */
   int fflagcntr=0;          /* a counter used in conjunction with the format flag */
   int onegribflag=0;        /* flag for combining all GRIB messages into one file */

   int found=0;
   int Yflag=0;
   int Mflag=0;
   int Dflag=0;
   int Hflag=0;
   int Nflag=0;
   int datewarn=0;
   int qpewarn=0;
   int formwarn=0;
   int onegribwarn=0;

   /* flag used with setting temp grib file to beginning for NPVU processing */

   int firstch=0;

   /* For storing information retrieved from the NetCDF file */

   double stdParallelOne, stdParallelTwo, xlov;
   double *latlonLL, *latlonUR, lonOrigin,*domainOrigin, *domainExtent, *latLonOrigin;
   int *gridPointLL, *gridPointUR;
   double x1, y1, x2, y2, lat1, lon1, lat2, lon2;
   nc_type vt_type, dn_type, ll_type, d_type, g_type;
   nc_type cdfvar_type;
   int vt_len, ll_len, d_len, g_len;
   int cdfvar_id, *gridSize;
   int cdfvar_ndims;
   int cdfvar_dims[MAX_VAR_DIMS];
   int cdfvar_natts;
   char varname[MAX_NC_NAME]={'\0'};
   char dimname[MAX_NC_NAME]={'\0'};
   char siteID[MAX_NC_NAME]={'\0'};
   char cdfunits[MAX_NC_NAME]={'\0'};
   char projection[MAX_NC_NAME]={'\0'};
   long dim_size;
   float *cdfvargrid=NULL;   /* this is the main array holding the actual data values */
   float arraysize;

   long *validTimes;
   char descriptName[MAX_NC_NAME]={'\0'};


   /* based on the filename, these are used to determine several time strings which
      could be coded differently depending on the parameter and whether this is a
      forecast or observed (estimated) grid
   */

   char basetime[ANSI_TIME_LEN+1]={'\0'};
   char basistime[11]={'\0'};   /* length of this should not change */
/*   char *basistime=NULL;*/
   char validtime[ANSI_TIME_LEN+1]={'\0'};
   char reftime[ANSI_TIME_LEN+1]={'\0'};
   char dummy[FILE_LEN]={'\0'};
   float timediff;
   int timedif_hr, perflag;

   double dxdy;    /* holds the DX, DY at standard latitude from a given map projection */

   int dumint[4];  /* dummy int array */
   maparam stcprm;  /* mapping structure required to hold projection parameters after initialization */
                    /* part of dmapf-c/cmapf */

   /* several file string variables */


   char file_path[PATH_LEN+FILE_LEN]={'\0'};
   char pprocbin[PATH_LEN+FILE_LEN]={'\0'};
   char appsdir[PATH_LEN+FILE_LEN]={'\0'};
   char process[FILE_LEN]={'\0'};

   /*

      The fcsth hold the forecast hours determined by differencing the basis time from the
      valid time in the NetCDF file.  This is then used to determine the valid time in the GRIB
      message of the grid.
   */

   int i, j, m, x, y, status, yr, mon, day, hrmin, sec, fcsth, esth, c;

   /* holds a position value of date/time wildcards in the output filename */

   size_t psn=0;

   char * valptr=NULL;

   /* these are a couple of check flags: missing data and all zeros.  The missing data will
      cause the program to return with an error.  The zeros is a warning but this could be
      correct in the case of QPE or QPF.
   */
   int mischek=0;
   int zerochek=0;


   /* declare structure variable */

   mygfe2grib gfe2grib;

   /* file and directory status structure variable */

   struct stat st;

   FILE *fptrqpf, *fptr, *fp, *tmpfptr, *onegfptr;  /* file pointers */



/**************************GRIB PARAMETERS for packgrib**********************/

   int grib_lbl[43];           /* holds the values for the GRIB meta data */
   int firstLon;
   char pds_ext[256]={'\0'};   /* this is not to be used here but is declared empty string */
   size_t iplen=0;
   size_t odim=COPYSIZE;
   size_t *output_buffer=NULL;
   unsigned char temp[BUFFSIZE];
   int iwordsz=4;
   float xmissing=-9999.;
   size_t length;

   size_t idim;

   output_buffer = (size_t *) malloc (sizeof(size_t)*odim);  /* output buffer used when writing GRIB message */

/*   output_buffer = (int *) malloc (sizeof(int)*odim);  /* output buffer used when writing GRIB message */

   if(output_buffer==NULL)
   {
      printf(" ERROR: Something went wrong with memory allocation for the GRIB output buffer....exiting\n");
      return MALERR;
   }


/************** start main routine ************************************************/



/* parse command line arguments */

   while ((c = getopt(argc, argv, ":n:i:t:o::b:p:g:Nfrqhv1")) != -1) {


        switch (c) {

	case 'i':                  /* input filename option */


	   if (iflag)
	      errflag++;
	   else
	   {
	      iflag++;
	      if(optarg!=NULL)
	      {
	         if(*optarg!='-')    /* can't process correctly if we reach the next option */
		 {
		    if (infn!=NULL)
		    {
		       free(infn);
		       infn=NULL;
		    }
	            infn=(char *) malloc(sizeof(char)*(strlen(optarg)+1));
	            if(infn==NULL)
	            {
	              printf(" ERROR: Something went wrong with memory allocation for the input file name....exiting\n");
		      return MALERR;
	            }

	            strcpy(infn, optarg);

		    *(infn+strlen(optarg))='\0';
		 }
		 else
		 {
		    printf("\n Option -%c requires a value\n", c);
                    errflag++;
		    optind--;
	         }

	      }
	      else
	         errflag++;
	   }

	   break;
	case 'o':                           /* output filename option */
	   if (oflag)
	      errflag++;
	   else
	   {
	      oflag++;

	      if(argv[optind]!=NULL && *(argv[optind])!='-')  /* have to process a bit differently as this option
	                                                         has an option argument */
	      {

                 if(ofn!=NULL)
		 {
		    free(ofn);
		    ofn=NULL;
		 }
/*                 ofn=(char *) malloc(sizeof(char)*(strlen(argv[optind])+1));*/
		 ofn=(char *) malloc(sizeof(char)*FILE_LEN);
	         if(ofn==NULL)
	         {
	               printf(" ERROR: Something went wrong with memory allocation for the output file name....exiting\n");
		       return MALERR;
	         }
		 if(ofntemp!=NULL)
		 {
		    free(ofntemp);
		    ofntemp=NULL;
		 }
/*                 ofntemp=(char *) malloc(sizeof(char)*(strlen(argv[optind])+1));*/
	         ofntemp=(char *) malloc(sizeof(char)*FILE_LEN);
	         if(ofntemp==NULL)
	         {
	               printf(" ERROR: Something went wrong with memory allocation for the temp output file name....exiting\n");
		       return MALERR;
	         }

                 /* copy to both because will use in conjunction with -f format flag if specified */

	         strcpy(ofntemp,argv[optind]);
		 strcpy(ofn,argv[optind]);

              }
	      else
	      {

                 if(ofn!=NULL)
		 {
		    free(ofn);
		    ofn=NULL;
		 }
	         if(ofntemp!=NULL)
		 {
		    free(ofntemp);
		    ofntemp=NULL;
		 }
		 oflag=0;
              }

	   }
	   break;
	case 't':                      /* output path option */
	   if (tflag)
	      errflag++;
	   else
	   {
	      tflag++;
	      if(optarg!=NULL)
	      {
	         if(*optarg!='-')
		 {
		    if(gribdir!=NULL)
		    {
		       free(gribdir);
		       gribdir=NULL;
		    }
	            gribdir=(char *) malloc(sizeof(char)*(strlen(optarg)+1));
		    if(gribdir==NULL)
		    {
	              printf(" ERROR: Something went wrong with memory allocation for the grib directory name....exiting\n");
		      return MALERR;
	            }

	            strcpy(gribdir,optarg);
		    *(gribdir+strlen(optarg))='\0';
		 }
		 else
		 {
		    printf("\nOption -%c requires a value\n", c);
                    errflag++;
		    optind--;
	         }

	      }
	      else
	         errflag++;
	   }
	   break;
	case 'n':                          /* input path option */
	   if (nflag)
	      errflag++;
	   else
	   {
	      nflag++;
	      if(optarg!=NULL)
	      {

		 if(*optarg!='-')
	  	 {
		    if(inpath!=NULL)
		    {
		       free(inpath);
		       inpath=NULL;
		    }
	            inpath=(char *) malloc(sizeof(char)*(strlen(optarg)+1));
		    if(inpath==NULL)
		    {
	              printf(" ERROR: Something went wrong with memory allocation for the input directory name....exiting\n");
		      return MALERR;
	            }

	            strcpy(inpath,optarg);
		    *(inpath+strlen(optarg))='\0';
		 }
		 else
		 {
		    printf("\n Option -%c requires a value\n", c);
                    errflag++;
		    optind--;
	         }


	      }
	      else
	         errflag++;

	   }
	   break;
	case 'p':                    /* GFE process id option */

	   if (pflag)
	      errflag++;
	   else
	   {
	      pflag++;
	      if(optarg!=NULL)
	      {


	         if(*optarg!='-')
		 {

	            strcpy(process,optarg);

		 }
		 else
		 {
		    printf("\n Option -%c requires a value\n", c);

                    errflag++;
		    optind--;
	         }

	      }
	      else
	         errflag++;

	   }
	   break;
	case 'N':                    /* flag to process NPVU QPF files */
	   if (headflag)
	      errflag++;
	   else
	      headflag++;
	   break;
	case 'f':                    /* format flag option */
	   if (fflag)
	      errflag++;
	   else
	   {
	      fflag++;

	   }
	   break;
	case 'q':                    /* QPE flag option */
	   if (qflag)
	      errflag++;
	   else
	   {
	      qflag++;

	   }
	   break;
	case 'r':                    /* estimated flag option */
	   if (rflag)
	      errflag++;
	   else
	   {
	      rflag++;

	   }
	   break;


	case 'b':                  /* basis time flag option */
	   if (bflag)
	      errflag++;
	   else
	   {
	      bflag++;
	      if(optarg!=NULL)
	      {
	         if(*optarg!='-')
		 {


	            strcpy(basistime,optarg);

                 }
		 else
		 {
		    printf("\n Option -%c requires a value\n", c);
                    errflag++;
		    optind--;
	         }

	      }
	      else
	         errflag++;
	   }
	   break;

	case 'g':                 /* combined GRIB message file option */

	   if (onegribflag)
	      errflag++;
	   else
	   {
	      onegribflag++;
	      if(optarg!=NULL)
	      {


		 if(*optarg!='-')
	  	 {
		    if(onegfname!=NULL)
		    {
		       free(onegfname);
		       onegfname=NULL;
		    }
	            onegfname=(char *) malloc(sizeof(char)*(strlen(optarg)+1));
		    if(onegfname==NULL)
		    {
	              printf(" ERROR: Something went wrong with memory allocation for the input directory name....exiting\n");
		      return MALERR;
	            }

	            strcpy(onegfname,optarg);
		    *(onegfname+strlen(optarg))='\0';
		 }
		 else
		 {
		    printf("\n Option -%c requires a value\n", c);
                    errflag++;
		    optind--;
	         }


	      }
	      else
	         errflag++;

	   }
	   break;

	case 'h':            /* display help */
	   helpflag++;
	   break;
	case 'v':            /* turn on verbose debugging */
	   if (debugflag)
	      errflag++;
	   else
	      debugflag++;
	   break;
	case '1':            /* process only one record of NetCDF, useful for debugging */
	      time1flag++;
	   break;
        case ':':       /* for options that need an operand */
	   if(optopt != 'o')
	   {
              printf("\n Option -%c requires a value\n", optopt);
              errflag++;
	   }
	   else
	   {


	      if(ofn!=NULL)
	      {

		 free(ofn);
		 ofn=NULL;
	      }
	      if(ofntemp!=NULL)
	      {
	         free(ofntemp);
		 ofntemp=NULL;
	      }
	   }
           break;

        case '?':
           printf("Unrecognized program command line option: -%c\n", optopt);
           errflag++;



        }
    }


    if (errflag || helpflag || argc==1 || ( iflag==0 || pflag==0) )
    {
        if ( iflag==0 || pflag==0)
	{
	   printf("\nOne or both of the -i (input NetCDF file) or the -p (process ID) option/command line arguments \n" \
	          "was missing when running nc2grib.  These must be specified as inputs to nc2grib at a minimum \n" \
		  "in order for it to run.  Check usage of nc2grib below.\n");
	}
	status=display_usage();
	return USAGE;
    }


   if(nc_getAppsDefaults("nc2g_app_dir",appsdir) == -1)
   {
   	fprintf(stderr," ERROR: Invalid token value for token \"nc2g_app_dir\".\n\t Program exit.\n");
	status=display_usage();
	return APSDEFERR;
   }

   sprintf(file_path,"%s/%s",appsdir,"gfe2grib.txt");

   if((fp = fopen(file_path, "r")) == NULL)
   {
   	printf (" ERROR: cannot open GFE NetCDF parameter input file: %s\n\tProgram exit.", file_path) ;
	return OPENERR;
   }


   if(gribdir==NULL)
   {
      gribdir=(char *) malloc(sizeof(char)*(PATH_LEN+1));

      if (gribdir==NULL)
      {
	  printf(" ERROR: Something went wrong with memory allocation for the grib output directory....exiting\n");
          return MALERR;
      }

      *(gribdir+PATH_LEN)='\0';

      if(nc_getAppsDefaults("fewsgrib_dir",gribdir) == -1)
      {
   	   printf(" ERROR: Invalid token value for token \"fewsgrib_dir\".\n\t Program exit.");
	   status=display_usage();
	   return APSDEFERR;
      }
      else if (debugflag>0)
      {
         printf("\n Debug option on...GRIB directory not specified.  Will save output GRIB files to:\n" \
	        " %s \n",gribdir);
      }

   }
   else if (debugflag>0)
   {
      printf("\n Debug option on...GRIB directory specified as %s\n",gribdir);
   }



/**************************************************************************/
/* debugflag > 0; debug option is on */

   if(debugflag>0)
      printf("\n Debug option on...reading from GFE to GRIB configuation file:\n" \
             " %s\n\n",file_path);

/**************************************************************************/

   while (fgets(fileline, LINE_MAX, fp) != NULL)
   {

      if(fileline[0] != '#')   /* check for comments */
      {

         sscanf(fileline,"%s%s%d%d%d%d%d",gfe2grib.process, gfe2grib.gfename, &gfe2grib.processid,
                &gfe2grib.gribnum,&gfe2grib.decscale, &gfe2grib.timerange, &gfe2grib.timeunit);
         if(debugflag>0)
            printf(" DEBUG: Read in from gfe2grib.txt %s %s %d %d %d %d %d	\n",gfe2grib.process, gfe2grib.gfename, gfe2grib.processid,
                   gfe2grib.gribnum,gfe2grib.decscale, gfe2grib.timerange, gfe2grib.timeunit);


/*         if (strstr(gfe2grib.process, process)!=NULL) */ /* found a problem using this.  try next if instead */

         if (!(strcmp(gfe2grib.process, process)))
         {

            found = 1;
            break;
         }
      }
   }



   if (found==0)
   {
      printf(" Could not match input process ID with those in gfe2grib.txt file\n" \
             " Input process ID = %s.  Modify gfe2grib.txt file and rerun.\n",process);
      return SUBERR;
   }
   else if(debugflag)
   {
      printf(" DEBUG: Match found between input process ID and value stored in gfe2grib.txt file\n" \
             " Process ID = %s\n",process);
   }
   fclose(fp);


   /*   open the Netcdf file*/

   if(inpath==NULL)
   {
      inpath=(char *) malloc(sizeof(char)*(FILE_LEN+1));


      if(inpath==NULL)
      {
	  printf(" ERROR: Something went wrong with memory allocation for the NetCDF input directory....exiting\n");
          return MALERR;
      }

      *(inpath+FILE_LEN)='\0';

      if(nc_getAppsDefaults("netcdf_dir",inpath) == -1)
      {
   	   printf(" ERROR: Invalid token value for token \"netcdf_dir\".\n\t Program exit.");
	   return APSDEFERR;
      }
      else if (debugflag>0)
      {
           printf(" Default path for the input NetCDF file not specified...Will use the following:\n" \
	          " %s\n",inpath);
      }
   }
/***************************************************************************/
   else if(debugflag)
      printf(" Will attempt to read NetCDF file from this path:\n" \
             " %s\n\n",inpath);

/**************************************************************************/
   if (stat(inpath,&st) != 0)
   {
      printf(" ERROR:  The NetCDF input path does not exist.  Please correct this error and try again.\n");
      return FILEERR;
   }

   sprintf(fn,"%s/%s",inpath,infn);

   cdfid=ncopen(fn,NC_NOWRITE);

   if (cdfid==-1)
   {
      printf("\n ERROR: Could not open the netcdf file: %s\n", fn);
      return CDFERR;
   }
   else
   {
      printf ("\n Netcdf file %s was opened successfully.\n\n",fn);
   }

   /* Inquire about the Netcdf file: No.of dimensions, No.of variables,
                                   No. of global attributes etc.*/

   ncinquire (cdfid, &ndims, &nvars, &ngatts, &recdim);
/*************************************************************************/
/* debug */

if (debugflag >0)
{
      printf("\n Debug option on.  Debug info from reading the netcdf file follows:\n\n");
      printf (" Number of dimensions for this netcdf file is: %d\n",ndims);
      printf (" Number of variables for this netcdf file is: %d\n",nvars);
      printf (" Number of global attributes for this netcdf file is: %d\n",ngatts);
}
/*************************************************************************/


   cdfvar_id = 0;  /* this should not change for this application as the first variable will be the one
                         that contains the QPF, Temp, etc. */

   ncvarinq (cdfid, cdfvar_id, varname, &cdfvar_type, &cdfvar_ndims, cdfvar_dims, &cdfvar_natts);


   printf ("\n NetCDF variable name = %s\n",varname);
/***********************************************************************/
if (debugflag>0)
{
   printf (" Number of %s dimensions - %d\n",varname, cdfvar_ndims);
   printf (" Number of %s attributes - %d\n\n",varname, cdfvar_natts);
}
/**********************************************************************/
   if (strstr(varname,gfe2grib.gfename)==NULL)
   {
      printf("ERROR:  The parameter name in the GFE NetCDF file, %s, doe not match the one\n" \
             "associated with the process id in the gfe2grib.txt file.\n" \
	     "In gfe2grib.txt process ID %s is associated with GFE parameter name %s.\n" \
	     "Please specify the correct process ID and try again\n\n",varname,gfe2grib.process,gfe2grib.gfename);
      return CDFERR;
   }
   if(cdfvar_ndims==3)  /* in some cases, this may not be true if file is produced from MPE/DQC */
   {
     for (i=0; i<cdfvar_ndims; i++)
     {

        ncdiminq(cdfid,cdfvar_dims[i],dimname,&dim_size);

        if (i==1)
           y=dim_size;
        else if (i==2)
           x=dim_size;
        else if (i!=0)  /* not an error for 0 and need that for debug below */
        {
           printf("\n Number of dimensions is %d, which is too many dimensions for variable %s.\n" \
	          " Please ensure the NetCDF file is created properly for at most three dimensions, where\n" \
	          " the first dimension allows the NetCDF file to contain multiple records.\n",cdfvar_ndims,varname);

	   return CDFERR;
        }
/*************************************************************************/
if (debugflag >0)
{
    printf(" DEBUG: cdfvar dimension %d: name=%s size=%ld\n",i+1,dimname,dim_size);
}
/*************************************************************************/

      }
   }
   else if (cdfvar_ndims==2)
   {


     for (i=0; i<cdfvar_ndims; i++)
     {

        ncdiminq(cdfid,cdfvar_dims[i],dimname,&dim_size);
        if (i==0)
           y=dim_size;

        else if (i==1)
           x=dim_size;
/*************************************************************************/
if (debugflag >0)
{
    printf(" DEBUG: cdfvar dimension %d: name=%s size=%ld\n",i+1,dimname,dim_size);
}
/*************************************************************************/

     }
   }
   else
   {
      printf("\n nc2grib is not coded to handle %d number of dimensions for variable %s.\n" \
             " Please ensure the NetCDF file is created properly for two or three dimensions, where\n" \
	     " two dimensions indicates only 1 record of the variable and three dimensions allow\n" \
	     " the NetCDF file to contain multiple records.\n",cdfvar_ndims,varname);
      return CDFERR;
   }


   /* get variable attributes */

   arraysize = x * y;

   cdfvargrid = (float *) malloc (sizeof(float)*arraysize);

   long count[]={1,y,x};
   long count1r[]={y,x};

   ncattinq(cdfid,cdfvar_id,"validTimes",&vt_type,&vt_len);

   validTimes = (long *) malloc(vt_len * nctypelen(vt_type));

   ncattget(cdfid, cdfvar_id, "validTimes", validTimes);

   ncattget(cdfid, cdfvar_id, "descriptiveName", descriptName);

   ncattget(cdfid, cdfvar_id, "siteID", siteID);

   ncattget(cdfid, cdfvar_id, "units", cdfunits);

   ncattget(cdfid, cdfvar_id, "projectionType", projection);

   ncattinq(cdfid,cdfvar_id,"latLonLL",&ll_type,&ll_len);

   latlonLL = (double *) malloc(ll_len * nctypelen(ll_type));

   ncattget(cdfid, cdfvar_id, "latLonLL", (void *) latlonLL);

   latlonUR = (double *) malloc(ll_len * nctypelen(ll_type));

   ncattget(cdfid, cdfvar_id, "latLonUR", (void *) latlonUR);

   ncattinq(cdfid,cdfvar_id,"domainOrigin",&d_type,&d_len);

   domainOrigin = (double *) malloc(d_len * nctypelen(d_type));

   ncattget(cdfid, cdfvar_id, "domainOrigin", (void *) domainOrigin);

   ncattinq(cdfid,cdfvar_id,"domainExtent",&d_type,&d_len);

   domainExtent = (double *) malloc(d_len * nctypelen(d_type));

   ncattget(cdfid, cdfvar_id, "domainExtent", (void *) domainExtent);

   ncattinq(cdfid,cdfvar_id,"gridSize",&g_type,&g_len);

   gridSize = (int *) malloc(g_len * nctypelen(g_type));

   ncattget(cdfid, cdfvar_id, "gridSize", (void *) gridSize);

   ncattinq(cdfid,cdfvar_id,"gridPointLL",&g_type,&g_len);

   gridPointLL = (int *) malloc(g_len * nctypelen(g_type));

   ncattget(cdfid, cdfvar_id, "gridPointLL", (void *) gridPointLL);

   ncattinq(cdfid,cdfvar_id,"gridPointUR",&g_type,&g_len);

   gridPointUR = (int *) malloc(g_len * nctypelen(g_type));

   ncattget(cdfid, cdfvar_id, "gridPointUR", (void *) gridPointUR);

   /* initialize the array to missing value */

   for (i=0;i<arraysize;i++)
      (*(cdfvargrid+i)) = xmissing;


/*************************************************************************/
if (debugflag >0)
{

       printf(" DEBUG: siteID = %s\n",siteID);
       printf(" DEBUG: number of valid times = %d type = %d\n",vt_len, vt_type);
       printf(" DEBUG: descriptName = %s\n",descriptName);
       printf(" DEBUG: projection = %s\n",projection);

      for (i=0; i<d_len; i++)
      {
         printf(" DEBUG: domain origin %d = %6.0f\n",i,*(domainOrigin+i));
         printf(" DEBUG: domain extent %d = %6.0f\n",i,*(domainExtent+i));

      }
}
/*************************************************************************/

/**********************pack the grid into GRIB1********************************************/

   idim=(size_t) x;

/* GRIB Edition number */
   grib_lbl[0]=1;

/* grid ID */
   grib_lbl[1]=255;  /* non standard grid */

/* parameter table version */
   grib_lbl[2]=2;

/* center id */
   grib_lbl[3]=9;


   grib_lbl[4]=gfe2grib.processid;    /* 180 = QPF, 30 for forecaster generated, will use for T and PE  */

/* ignored */
   grib_lbl[5]=0;

/* include the GDS */
   grib_lbl[6]=1;



/* for averaged grids but not used here */
   grib_lbl[19]=0;
	grib_lbl[20]=0;

/* parameter code, time unit, time range, decimal scale vary per parameter
  and are configurable in the gfe2grib.txt file
*/

   grib_lbl[7]=gfe2grib.gribnum;

   grib_lbl[15]=gfe2grib.timeunit;

   grib_lbl[18]=gfe2grib.timerange;

   grib_lbl[22]=gfe2grib.decscale;

/* level type */
   grib_lbl[8]=1;  /* surface */
   if (grib_lbl[7]==7)
	grib_lbl[8]=4;  /* freezing level surface */

/* level */
   grib_lbl[9]=0;
   grib_lbl[10]=0;

/* sub-center id */

   if  ( strstr(siteID,"TUA")!=NULL )
   {
           grib_lbl[21] = 150;

           strcpy(wmohdr2,"KTUA");
   }
   else if ( strstr(siteID,"ACR")!=NULL )
   {
	   grib_lbl[21] = 151;

           strcpy(wmohdr2,"PACR");
   }
   else if ( strstr(siteID,"STR")!=NULL )
   {
	   grib_lbl[21] = 152;

	   strcpy(wmohdr2,"KSTR");
   }
   else if ( strstr(siteID,"RSA")!=NULL )
   {
	   grib_lbl[21] = 153;

	   strcpy(wmohdr2,"KRSA");
   }
   else if ( strstr(siteID,"ORN")!=NULL )
   {
	   grib_lbl[21] = 154;

	   strcpy(wmohdr2,"KORN");
   }
   else if ( strstr(siteID,"RHA")!=NULL )
   {
	   grib_lbl[21] = 155;
	   strcpy(wmohdr2,"KRHA");
   }
   else if ( strstr(siteID,"KRF")!=NULL )
   {
	   grib_lbl[21] = 156;
	   strcpy(wmohdr2,"KKRF");
   }
   else if ( strstr(siteID,"MSR")!=NULL )
   {
           grib_lbl[21] = 157;
	   strcpy(wmohdr2,"KMSR");
   }
   else if ( strstr(siteID,"TAR")!=NULL )
   {
	   grib_lbl[21] = 158;
	   strcpy(wmohdr2,"KTAR");
   }
   else if ( strstr(siteID,"PTR")!=NULL )
   {
	   grib_lbl[21] = 159;
	   strcpy(wmohdr2,"KPTR");
   }
   else if ( strstr(siteID,"TIR")!=NULL )
   {
	   grib_lbl[21] = 160;
	   strcpy(wmohdr2,"KTIR");
   }
   else if ( strstr(siteID,"ALR")!=NULL )
   {
	   grib_lbl[21] = 161;
	   strcpy(wmohdr2,"KALR");
   }
   else if ( strstr(siteID,"FWR")!=NULL )
   {
	   grib_lbl[21] = 162;

           strcpy(wmohdr2,"KFWR");
   }
   else
   {
	    printf(" Unknown site ID %s for this application...Exiting\n",siteID);
	    return UNERR;
   }



/* binary data section flag */
   grib_lbl[23]=0 ;

/* packing width of data points */
   grib_lbl[24]=16;      /* original was 16 in the example  4 in gribit */

/* initialized but ignored in grib message */

   grib_lbl[26]=0;
   grib_lbl[27]=0;

/* length of GDS */
   if (strstr(projection,"POLAR")!=NULL)
   {


	   grib_lbl[25]=32;   /* polar stereographic and lat/long, 42 for Lambert */


	   /* grid (data representation) type, polar stereographic */
	   grib_lbl[28]=5;

           grib_lbl[29]=(int) x;
	   grib_lbl[30]=(int) y;

	   /* next for initialized but not used */
	   grib_lbl[39]=0;
	   grib_lbl[40]=0;
	   grib_lbl[41]=0;
	   grib_lbl[42]=0;

           ncattget(cdfid, cdfvar_id, "lonOrigin", &lonOrigin);

	   grib_lbl[34]=lonOrigin*1000.;   /* longitude of grid point orientation */

   }
   else if (strstr(projection,"LAMBERT")!=NULL)
   {

	   grib_lbl[25]=42;   /* Lambert Conformal, 32 for polar */

	   /* grid (data representation) type, lambert conformal */

	   grib_lbl[28]=3;

           grib_lbl[29]=(int) x;
	   grib_lbl[30]=(int) y;

           ncattinq(cdfid,cdfvar_id,"latLonOrigin",&ll_type,&ll_len);

   	   latLonOrigin = (double *) malloc(ll_len * nctypelen(ll_type));

           ncattget(cdfid, cdfvar_id, "latLonOrigin", latLonOrigin);

	   grib_lbl[34]=(*latLonOrigin)*1000.;

           ncattget(cdfid, cdfvar_id, "stdParallelOne", &stdParallelOne);
           ncattget(cdfid, cdfvar_id, "stdParallelTwo", &stdParallelTwo);


	   grib_lbl[39]=stdParallelOne*1000;
	   grib_lbl[40]=stdParallelTwo*1000;

	   grib_lbl[41]=0;
	   grib_lbl[42]=0;


   }
   else
   {
	   printf(" Unknown projection read from netcdf...Exiting");
	   return CDFERR;

	   /* might account for this as this is a lat,lon grid */
	   /* comment out for this version */

           /* latitude/longitude grid
	   grib_lbl(30)=idim
	   grib_lbl(31)=jdim
	   grib_lbl(32)=0
	   grib_lbl(33)=0
	   grib_lbl(34)=0
	   grib_lbl(35)=90000
	   grib_lbl(36)=360000
	   grib_lbl(37)=2500
	   grib_lbl(38)=2500
	   grib_lbl(39)=64
           */
   }


  /* resolution component flags */

   grib_lbl[33]=8;

  /* must find the grid map parameters and then the dx, dy resolution */
  /* normally, these are the same for polar stereographic and even lambert conformal,
     but not necessarily
  */

   x1=y1=x2=y2=lat1=lon1=lat2=lon2=0.;  /* initialize the end points of the local grid */

   /* Lower left corner of the main projected grid */

   x1=(double) *gridPointLL;
   y1=(double) (*(gridPointLL+1));
   lon1= (*latlonLL);
   lat1= (*(latlonLL+1));

   /* upper right corner of the main projected grid */

   x2=(double) *gridPointUR;
   y2=(double) (*(gridPointUR+1));
   lon2= (*latlonUR);
   lat2= (*(latlonUR+1));

   /* check if polar stereographic or lambert conformal to set map parameters correctly */

   if(grib_lbl[25]==32)
	stlmbr(&stcprm,90.,lonOrigin);
   else if(grib_lbl[25]==42)
   {
	xlov=*latLonOrigin;
	stlmbr(&stcprm,eqvlat(stdParallelOne,stdParallelTwo),xlov);
   }

   /* set Earth radius */

   cstrad(&stcprm,6371.2);      /* radius of Earth used by NCEP */


   stcm2p(&stcprm,x1,y1,lat1,lon1,x2,y2,lat2,lon2);  /* find map parameters based on known lat/lons */


   /* find DX DY values, should be identical for the projections for this app */

   if(grib_lbl[25]==32)
	dxdy = cgszll(&stcprm, 60., lonOrigin);

   else if(grib_lbl[25]==42)
        dxdy = cgszll(&stcprm, eqvlat(stdParallelOne,stdParallelTwo), xlov);

 /*************************************************************************/
if (debugflag >0)
{

       /* debug only */

        printf(" DEBUG: dxdy is %9.3f\n",dxdy);

	printf(" DEBUG: Crosscheck grid lower left and upper right info\n");

	printf(" DEBUG: LL X=%6.0f, LL Y=%6.0f, UR X=%6.0f, UR Y=%6.0f\n" \
	       " DEBUG: LL Lat=%f, LL Lon=%f, UR Lat=%f, UR Lon=%f\n",
	        x1,y1,x2,y2,lat1,lon1,lat2,lon2);


	printf(" DEBUG: longitude at origin = %d\n",grib_lbl[34]/1000);


}
/*************************************************************************/


   dxdy=ceil(dxdy*1000);

   int dx = dxdy;
   int dy = dxdy;

   /* in GFE, the gridsize should equal the extents if using the standard grid resolutions.
      If not, the site has changed resolutions and this must be determined
   */

   if ((int) y != (int) (*(domainExtent+1)) || (int) x != (int) (*domainExtent))
   {
	    /* first calculate x */

	    /* this formula is in the GFE online help - Adjusting the Grid Resolution in localConfig.py */


	     dx = dxdy * ((*domainExtent) / ( x -1));

	     dy = dxdy * ((*(domainExtent+1)) / ( y -1));

   }

   /* note that this may cause problems for places where dx != dy but they are still using polar stereographic
      and it usually assumes these are the same
   */

   grib_lbl[35]=dx;
   grib_lbl[36]=dy;

   /* now for the local grid (i.e grid 255 in GRIB), will need to get the lower left lat, lon and
      will use the cxy2ll command here for the domain with origin values of x and y */

   x=*domainOrigin;
   y=*(domainOrigin+1);


   cxy2ll(&stcprm,x,y,&lat1,&lon1);   /* Find lat lon */


   grib_lbl[31]=(lat1)*1000;
   grib_lbl[32]=(lon1)*1000;

   firstLon=grib_lbl[32];  /* must preserve because the packer changes the sign */

/*****************debug*********************/

if (debugflag>0)
{
   printf(" DEBUG: dx = %d dy = %d x = %d extent x = %f y = %d extent y = %f \n",dx,dy,x,*domainExtent, y,(*(domainExtent+1)));
   printf(" DEBUG: for local domain x = %d and y = %d, the corresponding lat = %f lon = %f\n",(int) x, (int) y, lat1, lon1);
}
/******************************************/


   grib_lbl[37]=0;

   /* scanning mode flag */

   grib_lbl[38]=64;

   /* in the original packgrib_.c documentation, it was thought that this pds_ext could be anything
      the user wanted.  However, this area of the GRIB message actually is used by NCEP to include
      ensemble forecast information for GRIB1 messages.  Therefore this should be set to the NULL
      string unless one really means to include ensemble information here.
   */

   strcpy(pds_ext,"");

   iplen=strlen(pds_ext);

/*************************************************************************/
if (debugflag >0)
{

       /* debug only */

        printf(" DEBUG: dxdy is %6.0f\n",dxdy);

	printf(" DEBUG: LL local domain lat=%f lon=%f\n",lat1,lon1);




}
/************************************************************************/


/*  may need this again somewhere */

/*   if(strrchr(fn,'/') != NULL)
      slashpos=strrchr(fn,'/') - fn;
*/



/* If this is a NetCDF file containing forecast grids, the -b switch with a basis time
   has to be included.  Split the basis time so it can be converted to a time_t
   variable and forecast hours can be determined for GRIB P1 and P2 calculation.
*/
   if(bflag)
   {

      for (i=0;i<4;i++)
      {
          dummy[i]=basistime[i];
      }

      dummy[4]='\0';

      yr=atoi(dummy);

      dummy[0]=basistime[4];
      dummy[1]=basistime[5];
      dummy[2]='\0';

      mon=atoi(dummy);

      dummy[0]=basistime[6];
      dummy[1]=basistime[7];
      dummy[2]='\0';

      day=atoi(dummy);

      dummy[0]=basistime[8];
      dummy[1]=basistime[9];
      dummy[2]='0';
      dummy[3]='0';
      dummy[4]='\0';

      hrmin=atoi(dummy);

      grib_lbl[11]=yr;
      grib_lbl[12]=mon;
      grib_lbl[13]=day;
      grib_lbl[14]=hrmin;

      sprintf(basetime,"%4d-%02d-%02d %c%c:00:00",yr,mon,day,basistime[8],basistime[9]);


      status = yearsec_ansi_to_timet(basetime, &basetime_t);
/*************************************************************/
if (debugflag>0)
   printf("\n DEBUG: Determined basis time = %s basis time_t = %ld sec \n",basetime,basetime_t);
/*************************************************************/

      if (status != 0 || basetime_t <= 0)
      {
         printf(" The basis time could not be correctly calculated from the input NetCDF filename.\n" \
                " Determined basis time = %s basis time_t = %ld sec \n" \
                " Please rename the file according to guidance and try again.\n", basetime, basetime_t);
         return FILEERR;
      }
   }


/************************************************************************************************************
    /* main loop to go through each forecast data set and grib up the data */

    /* note that had the loop set up for multiple valid times first.  Then thought ABRFC way of 1 file per forecast was
       simpler.  However, that didn't work for other RFC operations, so went back to multiple forecast hours within 1 NetCDF
       file.
    */


   if (time1flag>0)   /* for testing only to do just the first valid time from the netcdf file */
      vt_len=2;
/****************************************************************************/
if (debugflag>0)
   printf("\n ***Entering main loop to process NetCDF records(s) into GRIB files*** \n\n");
/****************************************************************************/


   for (m=0; m<vt_len; m+=2)

   {

	status = timet_to_yearsec_ansi((time_t) *(validTimes+m+1), validtime);


        for (i=0;i<4;i++)
   	{
	   dummy[i]=validtime[i];
        }
	dummy[4]='\0';

	yr=atoi(dummy);

	dummy[0]=validtime[5];
	dummy[1]=validtime[6];
	dummy[2]='\0';

	mon=atoi(dummy);

	dummy[0]=validtime[8];
	dummy[1]=validtime[9];
	dummy[2]='\0';

	day=atoi(dummy);

        dummy[0]=validtime[11];
	dummy[1]=validtime[12];
        dummy[2]=validtime[14];
	dummy[3]=validtime[15];
	dummy[4]='\0';

        hrmin=atoi(dummy);

        if(bflag==0)   /* qflag is to indicate this is an estimate product like QPE that requires basis time */
        {
	   if(qflag)
	   {
	      printf("\n ERROR: The -q option was specified but a basis time was not.  Using the -q option \n" \
	             " indicates this is an estimate product that requires a basis time to correct determine the \n" \
		     " GRIB reference, period, and valid times. This is commonly used for QPE for NPVU. \n" \
		     " Please revise your command line options accordingly and rerun the program \n" \
		     " Exiting...\n");
	     return UNERR;
	   }


	/* this is an "estimate" product rather than a forecast that doesn't need basis time */

	/* As this is an estimate product, it is valid at the end time retrieved from the NetCDF file rather than
	   determined from a base time.  First, though, get the reference time which is the first valid time for this
	   grid.  Will reuse basistime variable here for this purpose.
	*/

	   status = timet_to_yearsec_ansi((time_t) *(validTimes+m), basistime);


           for (i=0;i<4;i++)
   	   {
	      dummy[i]=basistime[i];
           }
	   dummy[4]='\0';

	   grib_lbl[11]=atoi(dummy);

	   dummy[0]=basistime[5];
	   dummy[1]=basistime[6];
	   dummy[2]='\0';

	   grib_lbl[12]=atoi(dummy);

	   dummy[0]=basistime[8];
	   dummy[1]=basistime[9];
	   dummy[2]='\0';

	   grib_lbl[13]=atoi(dummy);

           dummy[0]=basistime[11];
	   dummy[1]=basistime[12];
           dummy[2]=basistime[14];
	   dummy[3]=basistime[15];
	   dummy[4]='\0';

           grib_lbl[14]=atoi(dummy);


	   fcsth=0;

	   /* In the case of multiple accumulation periods in the same netcdf file, will need to attach this to the
	      filename in both cases.  Can't reuse fcsth as it might be needed to determine the WMO header for any
	      future NPVU estimate/observed grids.
	   */


	   esth=(int) ((*(validTimes+m+1)) - (*(validTimes+m)))/ SECINHR;

/*************************************************************/
if (debugflag>0)
   printf(" DEBUG: esth = %d valid time = %ld initial time = %ld\n",esth, (*(validTimes+m+1)), (*(validTimes+m)));
/*************************************************************/



	   if (esth > 240 || esth < 0)
           {
              printf(" The estimated/observed time period is either less than 0 or greater than 10 days (240 hours).\n" \
                     " Therefore, valid times within the input NetCDF filename may not have been generated \n" \
		     " correctly.  Or this is actually a forecast grid and the -b option should be used so it \n" \
		     " will be processed correctly.  Check your options and ensure this is an estimate or observed grid\n" \
		     " You could also try to generate the file again.\n" \
		     " For debug esth = %d\n",esth);
              return FILEERR;
           }


  /* see the GRIB table on this for determining reference and valid times for different types of products */

	   if (gfe2grib.timerange==3 || gfe2grib.timerange==4)
	   {
	      /* average or accumulation */
	      /* This will be the time determined from grib_lbl 11-14 to that date/time adding the number hours of esth */

	       grib_lbl[16]=0;      /* P1 */
	       grib_lbl[17]=esth;   /* P2 */
	   }
	   else if (gfe2grib.timerange==0)
	   {
	       /* while this is really for a forecast product valid at reference time + P1, use
	          this to determine the estimate/observed grid valid time as the reference time is the first valid
		  time in the sequence for the grid.
	       */

	       grib_lbl[16]=esth;                  /* P1 */
	       grib_lbl[17]=0;                     /* P2 */

           }
	   else
	   {
	       printf(" Unknown time range.  Check the gfe2grib.txt file \n");
	       return UNERR;
	   }


	   printf("\n\n NetCDF record %d is an estimate/observed product\n", m/2+1);
/*************************************************************/
if (debugflag>0)
   	/* this is an estimate/observed product */

    printf(" DEBUG: valid time = %d %d %d %d validtime=%s\n" \
	   " DEBUG: validTimes = %ld\n",  yr, mon, day, hrmin, validtime, *(validTimes+m+1));
/*************************************************************/

	}
   	else
	{


           printf("\n\n NetCDF record %d is a forecast or estimate product needing basis time\n", m/2+1);


           fcsth = (int) ((*(validTimes+m+1)) - basetime_t);

           timediff= (*(validTimes+m+1)) - (*(validTimes+m));

	   timedif_hr = (int) timediff/SECINHR;

	   fcsth /= SECINHR;

/*************************************************************/
if (debugflag>0)
   printf(" DEBUG: fcsth = %d timediff=%f valid time = %ld basis time_t = %ld\n",fcsth, timediff,(*(validTimes+m+1)), basetime_t);
/*************************************************************/

	   if (fcsth > 240 || fcsth < 0)
           {
              printf(" The forecast time is either less than 0 or greater than 10 days (240 hours).\n" \
                     " Therefore, the basis time may not be specified correctly or may need to be specified \n" \
		     " on the command line according to guidance.  Please check your command options or \n" \
		     " or the NetCDF file creation and try again.\n" \
		     " for debug fcsth = %d\n",fcsth);
              return FILEERR;
           }

/*************************************************************************/
if (debugflag >0)
{

          /* debug only */

	   /*printf(" base time 1=%ld 2=%ld diff=%f\n",*(validTimes+m),*(validTimes+m+1),timediff);*/

           printf(" DEBUG: reference time = %d%02d%02d%02d \n",yr,mon,day,hrmin);



}
/*************************************************************************/

	   if (gfe2grib.timerange==3 || gfe2grib.timerange==4)
	   {
	      /* average or accumulation */

	       grib_lbl[16]=fcsth-(int)(timediff/SECINHR); /* P1 */
	       grib_lbl[17]=fcsth;                         /* P2 */
	   }
	   else if (gfe2grib.timerange==0)
	   {
	       /* this is for a forecast product valid at reference time + P1 and
	          at present using this for PETF
	       */

	       grib_lbl[16]=fcsth;                     /* P1 */
	       grib_lbl[17]=0;                         /* P2 */

           }
	   else
	   {
	       printf(" Unknown time range.  Check the gfe2grib.txt file \n");
	       return UNERR;
	   }


	}

      /* Get data for this time record */

        if(cdfvar_ndims==3)
	{

           start[0]=(long) (m/2);

           status = ncvarget(cdfid,cdfvar_id,start,count,cdfvargrid);
	}
	else if (cdfvar_ndims==2)
	{
	   start1r[0]=(long) (m/2);

           status = ncvarget(cdfid,cdfvar_id,start1r,count1r,cdfvargrid);
	}

        if (status != NC_NOERR)
        {
           printf(" An error occurred while getting the cdfvar array\n");
           return CDFERR;
        }

      /* all missing check */

        for (i=0;i<arraysize;i++)
        {

           if((*(cdfvargrid+i))> xmissing)
	   {
	      mischek=1;
	      break;
	   }
        }

        if(mischek==0)
        {
           printf(" All data retrieved from the NetCDF file was missing. Exiting program...\n");
	   return MISCHECK;
        }

      /*  all data zero check. since already checked for all missing, can see if all data not equal to 0 */

        for (i=0;i<arraysize;i++)
        {

           if((*(cdfvargrid+i))!= 0.)
	   {
	      zerochek=1;
	      break;
	   }
        }

        if(zerochek==0 && debugflag > 0)
        {
           printf(" DEBUG WARNING: All data retrieved from the NetCDF file was zero. \n" \
	          "This may be normal in the case of QPF\n");

        }

 /* depending on the prarameter, convert to GRIB units standards  */

        if (grib_lbl[7]==61)   /* precipitation */
        {

	   if(strstr(cdfunits,"in")!=NULL)
	   {

              for (i=0;i<arraysize;i++)
              {
	         if((*(cdfvargrid+i))> xmissing)

                	*(cdfvargrid+i) *= 25.4; /* convert inches to mm */

              }
	   }

        }
        else if (grib_lbl[7]==11)   /* temperature */
        {

	   if(strstr(cdfunits,"F")!=NULL)
	   {

              for (i=0;i<arraysize;i++)
              {
	         if((*(cdfvargrid+i))> xmissing)

	                *(cdfvargrid+i) =  ((*(cdfvargrid+i)-32) * 5/9) + 273.16; /* convert F to K */

              }

	   }
	   else if (strstr(cdfunits,"C")!=NULL)
	   {
              for (i=0;i<arraysize;i++)
              {
	         if((*(cdfvargrid+i))> xmissing)

                	*(cdfvargrid+i) +=  273.16; /* convert C to K */

              }
	   }
        }

        else if (grib_lbl[7]==57)  /* evaporation */
        {
	   /* no code yet */

        }
        else if (grib_lbl[7]==7)  /* height */
        {
      /* this section is for freezing level */

	   if(strstr(cdfunits,"ft")!=NULL)
	   {

              for (i=0;i<arraysize;i++)
              {
	         if((*(cdfvargrid+i))> xmissing)

                	*(cdfvargrid+i) *= 0.3048; /* convert feet to meters */

              }
	   }
        }
        else
        {
	   printf(" Unknown parameter found in nc2grib...Exiting\n");
	   return UNERR;
        }

/*************************************************************************/
if (debugflag >0)
{
        printf("\n DEBUG:  GRIB message information follows:\n");
        j=6;
        for (i=0; i<43; i++)
	{
	   printf(" grib_lbl %2d=%7d",i,grib_lbl[i]);
	   if (i==j || i==42)
	   {
	      printf("\n");
	      j+=7;
	   }
	}
}
/*************************************************************************/


        status = packgrib(grib_lbl,pds_ext,&iplen,cdfvargrid,&idim,&xmissing,
                          output_buffer,&odim,&length);

   	if (status !=0)
   	{
      	  printf(" DEBUG: The routine which packs the grid into GRIB, packgrib, returned with errors status = %d\n",status);
		return SUBERR;
   	}
	else
	{
	  if(cdfvar_ndims==3)
	     printf("\n Gribbing of data successful for record %ld\n",start[0]+1);
	  else
	     printf("\n Gribbing of data successful for record %ld\n",start1r[0]+1);
	}

        /* create the GRIB1 output filename based input options */



/***************************************************************************/
if(debugflag)
   printf("\n DEBUG: Creating output file name \n");
/***************************************************************************/
	if(ofn==NULL)
	{

/**************************************************************************/
if(debugflag)
   printf("\n DEBUG: Output filename not specified...building from input filename \n");
/**************************************************************************/

	   if(strstr(infn,".cdf")!=NULL || strstr(infn,".nc") !=NULL)
	   {
	      valptr=strstr(infn,".cdf");
	      if (valptr==NULL)
	        valptr=strstr(infn,".nc");

	      psn=valptr-infn;

	      ofn=(char *) malloc(sizeof(char) * (psn+1));

	      if (ofn==NULL)
	      {

	           printf(" ERROR: Something went wrong with memory allocation for the GRIB filename....exiting\n");
		   return MALERR;
	      }


	      strncpy(ofn,infn,psn);

	      *(ofn+psn)='\0';

	   }
	   else
	   {

	      ofn=(char *) malloc(sizeof(char)*(strlen(infn)+1));
	      if (ofn==NULL)
	      {

	           printf(" ERROR: Something went wrong with memory allocation for the GRIB filename...exiting\n");
		   return MALERR;
	      }

	      strcpy(ofn,infn);

	   }

	   if(ofntemp!=NULL)
	   {
	      free(ofntemp);
	      ofntemp=NULL;
	   }

	   ofntemp=(char *) malloc(sizeof(char) * (strlen(ofn)+1));
	   if (ofntemp==NULL)
	   {

	           printf(" ERROR: Something went wrong with memory allocation for the temp output filename...exiting\n");
		   return MALERR;
	   }
	   if(ofn!=NULL)
	      strcpy(ofntemp,ofn);  /* must do this so ofntemp isn't NULL in the comparison below.  Might not make a
	                            difference but better safe than sorry for coding purposes
		   		    */
	   else
	   {
	       printf("\n ERROR occurred as out filename is NULL and shouldn't be before copying to ofntemp variable \n");
	       return UNERR;
	   }


	}


	/* DTM - 08/18/09.  An excellent suggestion from OHRFC (Mark Fenbers) is to use the function strftime and the
	   automatic assignment of date/time strings within the input filename.  Took a bit of doing but will use modified
	   versions of our on Time Util library routines to do this and remove the previous way of doing business
	   substituting date/time strings in the filenames.  This will move this executable to version 4.1.  Need to also
	   generalize the valid time for NPVU rather than search on the "QPE" string as it done in this present fix.

	   If the -f flag is specified, then valid time will be used instead of basetime.

	*/

	fflagcntr=0;  /* initialize counter */

        if(strrchr(ofntemp,'%') != NULL)  /* this will indicate that a date format will be substituted */
	{

           for (i=0;i<strlen(ofntemp); i++)
	   {
	      if(*(ofntemp+i)=='%')
	         fflagcntr++;
           }

	   if (fflagcntr < 4)
	   {
	      if (formwarn==0)
	      {
	         printf("\n NOTE: there are less than 4 format conversion values (beginning with %%) \n" \
		        " in the output string prefix, \n" \
		        " While this may be okay using values that combine individual data and time values, \n" \
			" there may also not be enough to form unique filenames. \n" );
	         formwarn++;
	      }
	   }


           if(fflag)     /* use valid time for string substitution instead of basis time */
	   {

	      status = timet_to_userformat_ansi((*(validTimes+m+1)), ofn, ofntemp);
	   }
	   else
	   {

	      status = timet_to_userformat_ansi(basetime_t, ofn, ofntemp);
	   }

	   if(strstr(ofntemp,"%%") == NULL && bflag>0)   /* we only need to check this if basis time is used
	                                                    in the output filename format  */
	   {
	      fflagcntr = 0;

	      if(datewarn<=0)
	      {
	         datewarn++;     /* only want to print this warning message for the first record in the NetCDF file */

	         printf("\n WARNING: Basis time option was found but the formatted time interval and/or hours from\n" \
		        " basetime is missing from the input format string.  These should have the format of %%0d \n" \
			" or %%d in the input format string. This should be included so as not overwrite files. \n\n");


                 printf("\n A default date,time will be used instead.\n" \
	                " If you intended on a custom one, please check the pattern for the missing\n" \
		        " time interval/hours past basis time pattern in your command line and try again.\n\n");
               }
	       else
	       {
	          printf("\n Please see WARNING message from first GRIB record generated \n" \
		         " concerning missing custom date/time wildcards in output GRIB filename \n\n");
	       }
	   }
	   else
	   {
	      if(bflag)   /* basis time is included, use the number of hours
			     past basis time unless other conditions occur with qflag
			  */
	      {
   	          if (qflag)
/*		       sprintf(ofn,ofn,timedif_hr);
		  else if (qflag)   */                /* due to filename limitations with base time in the filename,
		                                       will need both fcst and timedif_hr so filenames are not the same
						       for an estimate product that is the same number of hours from
						       base time but different time interval.  This can occur in the
						       precipitation QPE file from daily QC which has a 24 hour 6-hour
						       product and a 24 hour 24-hour product.  So we need to count the times
						       that %% occur together as this will indicate where to put each
						    */
		 {
		       perflag=0;
		       for (i=0; i<strlen(ofntemp);i++)
		       {
		          if(*(ofntemp+i) == '%')
			     if(*(ofntemp+i+1) == '%')
			       perflag++;
			       if (perflag >=2)
			          break;
		       }
		       if (perflag>=2)
		       {
		          if(!rflag)                 /* normally put time interval difference before hours past basis */

		             sprintf(ofn,ofn,timedif_hr,fcsth);
			  else
			     sprintf(ofn,ofn,fcsth,timedif_hr); /* but reverse the order here */
                       }
		       else
		       {
		          if(qpewarn==0)
			  {
	                     printf("\n WARNING!: If the NetCDF was generated by MPE/Daily QC and is QPE, \n" \
			            " you need to specify two place holders with %% vs one for forecast grids. \n" \
				    " This is because the NetCDF MPE/DQC QPE file \n" \
			  	    " contains two products valid 24 hours from basis time, \n" \
				    " a 6-hour QPE and a 24-hour QPE. \n" \
				    " Please check to ensure you formatted your output string accordingly. \n\n");
			     qpewarn++;
			  }
		          sprintf(ofn,ofn,fcsth);   /* user is taking responsibility to ensure estimate using basis time
			                               doesn't include multiple time intervals in NetCDF
						    */
		       }

		  }
		  else

		       sprintf(ofn,ofn,fcsth);    /* standard forecast product using forecast hours past basis time */

              }
	      else       /* without a basis time, this has to be an estimated/observed product using the valid time in
	                    the output file.  Note that if "%%" is NULL and bflag == 0, specifying esth here is
			    ignored in the output filename.
			 */

	          sprintf(ofn,ofn,esth);




	      if(strstr(ofn,".grb")!=NULL)
                 sprintf(outfn,"%s/%s",gribdir,ofn);

	      else
	         sprintf(outfn,"%s/%s.grb",gribdir,ofn);
	   }

	}

	if(oflag==0 || fflagcntr == 0)
	{

	   if(strstr(ofn,".grb")!=NULL)
	   {
	     valptr=strstr(ofn,".grb");
	     psn=valptr-ofn;

	     strncpy(dummy,ofn,psn);

	     dummy[psn]='\0';

	     if (ofn!=NULL)
	     {
	        free(ofn);
		ofn=NULL;
	     }

	     ofn=(char *) malloc(sizeof(char)*(strlen(dummy)+1));
	     if(ofn==NULL)
             {
	         printf(" ERROR: Something went wrong with memory allocation for the output file name\n" \
		        " before the default filename was determined...exiting\n");
		 return MALERR;
	     }



	     strcpy(ofn,dummy);



           }

           if(bflag)   /* default filenames if output filename and/or format not specified */
	   {
	       if(qflag)

	          sprintf(outfn,"%s/%s_%4d%02d%02d%02dh%03d.grb",gribdir,ofn,yr,mon,day,hrmin/100,timedif_hr);

	       else

	          sprintf(outfn,"%s/%s_%4d%02d%02d%02df%03d.grb",gribdir,ofn,yr,mon,day,hrmin/100,fcsth);
           }
	   else
	       sprintf(outfn,"%s/%s_%4d%02d%02d%02df%03d.grb",gribdir,ofn,yr,mon,day,hrmin/100,esth);

        }


        fptr = fopen ( outfn, "w" );  /* open the output GRIB file */


        if ( fptr == NULL ) {
	   printf ( " ERROR: output GRIB file could not be opened.\n" );
      	   return OPENERR;
	}
	else
	{
	   printf(" Writing grib data to file %s...\n",outfn);
        }

	/* write out the GRIB data to the output buffer */

	status = fwrite ( (unsigned char *)output_buffer, sizeof(unsigned char), length, fptr );

	if (status == 0 || length < 100)
	       printf("\n WARNING: Possible problem writing grib file, number of elements written = %d\n",length);
	else if ( length == 0)
	{
	       printf("\n DEBUG: length of GRIB file is zero.  Program is exiting\n");
	       return ZEROCHECK;
	}
	else
	       printf("\n GRIB file written %s number of elements = %d\n\n",outfn,status);


        fclose(fptr);

/* If this is precip (APCP) and headerflag is on, write out to a file for NPVU.  */

        if (headflag >0 && grib_lbl[7]==61)
	{


	/* get current GMT date and time for header */

		time( &curtime);

		curgmtime = gmtime (&curtime);

		sprintf(adayhrmin,"%02d%02d%02d",curgmtime->tm_mday,curgmtime->tm_hour,curgmtime->tm_min);
/********************************************************************/
if(debugflag>0)
{
   printf("\n DEBUG: current day hour min GMT = %s\n",adayhrmin);
}
/********************************************************************/


		if(nc_getAppsDefaults("pproc_bin",pprocbin) == -1)
   	       	{
   			printf(" ERROR: Invalid token value for token \"pproc_bin\".\n\t Program exit.");
			return APSDEFERR;
   		}


		/* fortran routine copygb_main_ */
   		sprintf(file_path,"%s/copygb.LX",pprocbin);

		sprintf(tmpNPVUfn,"%s/%s",gribdir,"tmpNPVU.grb");



		if(bflag && qflag==0)     /* old - strstr(process,"QPE")==NULL && strstr(process,"qpe")==NULL) */
		{

		   if(debugflag>0)

		   /* the -X here causes copygb to print out expanded information about its operation */

/*   	   	      sprintf(command,"%s -xg218 -X %s %s",file_path, outfn, tmpNPVUfn); */
	   	   	  sprintf(command,"-xg218 -X %s %s", outfn, tmpNPVUfn);
		   else
/*		      sprintf(command,"%s -xg218 %s %s",file_path, outfn, tmpNPVUfn); */
		      sprintf(command,"-xg218 %s %s", outfn, tmpNPVUfn);


           /* first write out the main GRIB file using the copygb command without the header determined above
		   to a temporary holding file.  This file will now contain the QPF forecast on GRID218 at 10km
		   resolution */
		   copygb_main_(command);
		   /* status = system(command); */
		}
		else
		{
		/* for a QPE grid, keep at the HRAP grid resolution and don't copy to the 218 GRID */

		   sprintf(command,"cp %s %s",outfn, tmpNPVUfn);

		   status = system(command);
		}

/********************************************************************/
if(debugflag>0)
{
   printf(" DEBUG: command for temp NPVU grib file=%s \n DEBUG: status of command execution=%d\n",command,status);
}
/********************************************************************/

                /* create an appropriate filename for the NPVU file */

	       if(strstr(outfn,".grb")!=NULL)
	       {
	          valptr=strstr(outfn,".grb");
	          psn=valptr-outfn;

	          strncpy(outfnqpf,outfn,psn);

	          outfnqpf[psn]='\0';
                }
		else
		  strcpy(outfnqpf,outfn);



		sprintf(outfnqpf,"%s_NPVU.grb",outfnqpf);

		fptrqpf = fopen ( outfnqpf, "wb" );


	 	if ( fptrqpf == NULL ) {
	      		printf ( " ERROR: NPVU GRIB file could not be opened.\n" );
      	      		return OPENERR;
		}
		else
		{
	    		printf(" Writing NPVU QPF WMO header info to file %s...\n",outfnqpf);
		}


		/* apply appropriate header based on estimate or forecast and number of hours */

		if (fcsth==0)
		    strcpy(wmohdr1,"ZETA98");
		else if (strstr(process,"QPE")!=NULL || strstr(process,"qpe")!=NULL)
		    strcpy(wmohdr1,"ZETA98");
		else
		{

		   if (fcsth == 6)
	   		strcpy(wmohdr1,"YEIG98");
	           else if (fcsth == 12)
		        strcpy(wmohdr1,"YEIM98");
	           else if (fcsth == 18)
		   	strcpy(wmohdr1,"YEIN98");
		   else if (fcsth == 24)
	   		strcpy(wmohdr1,"YEIO98");
		   else if (fcsth == 30)
	   		strcpy(wmohdr1,"YEIP98");
		   else if (fcsth == 36)
	   		strcpy(wmohdr1,"YEIQ98");
		   else if (fcsth == 42)
	   		strcpy(wmohdr1,"YEIR98");
		   else if (fcsth == 48)
	   		strcpy(wmohdr1,"YEIS98");
		   else if (fcsth == 54)
	   		strcpy(wmohdr1,"YEIS88");
		   else if (fcsth == 60)
	   		strcpy(wmohdr1,"YEIT98");
		   else if (fcsth == 66)
	   		strcpy(wmohdr1,"YEIT88");
		   else if (fcsth == 72)
	   		strcpy(wmohdr1,"YEIU98");
		   else
		   {
		        printf(" WARNING: No matching number of forecast hours.  WMO header YEIZ98 applied.\n" \
			       " to indicate forecast hour is in the GRIB Product Definition Section\n");
			strcpy(wmohdr1,"YEIZ98");
		   }
		}

		/* get current GMT date and time for header */

		j=0;

		for (i=0;i<strlen(wmohdr1);i++)
		{
		   header[j]=(unsigned char) wmohdr1[i];
		   j++;

		}
		header[j]=aspace;
		j++;

		for (i=0;i<strlen(wmohdr2);i++)
		{
		   header[j]=(unsigned char) wmohdr2[i];
		   j++;
		}
		header[j]=aspace;
		j++;

		for (i=0;i<strlen(adayhrmin);i++)
		{
		   header[j]= (unsigned char) adayhrmin[i];
                   j++;
		}

		status = fwrite(header,sizeof(unsigned char),18,fptrqpf);
		   if (status == 0)
	              printf("\n WARNING: Possible problem writing header to grib file\n");


		status = fwrite(crcrlf,sizeof(unsigned char),3,fptrqpf);
		   if (status == 0)
	              printf("\n WARNING: Possible problem writing header to grib file\n");




/*************************************************************************/
		/* debug */

if (debugflag >0)
{
/*   printf("\n j=%d\n",j);*/
   for (i=0; i<j ; i++)
   {
	   printhdr[i]=header[i];

   }

   printhdr[18]='\0';  /* ensure end of string */


   printf(" DEBUG: Header for QPF files = %s\n",printhdr);
}
/*************************************************************************/

		/* must close this so that we can append to the file */

		if (fptrqpf !=NULL)
		  fclose(fptrqpf);

		fptrqpf = fopen ( outfnqpf, "a" );


	 	if ( fptrqpf == NULL ) {
	      		printf ( " ERROR: NPVU GRIB file could not be opened.\n" );
      	      		return OPENERR;
		}



		tmpfptr = fopen ( tmpNPVUfn, "r" );

		if ( tmpfptr == NULL ) {
	      		printf ( " ERROR: NPVU GRIB data file could not be opened.\n" );
      	      		return OPENERR;
		}
		else
		{
	    		printf(" Appending NPVU QPF grib data to file %s...\n",outfnqpf);
		}


                if (setvbuf(fptrqpf,NULL,_IOFBF,BUFFSIZE) !=0)
		{
		  	printf(" ERROR: Could not create output buffer for %s...\n",outfnqpf);
			return FILEOPERR;
		}

                if (setvbuf(tmpfptr,NULL,_IOFBF,BUFFSIZE) !=0)
		{
		  	printf(" ERROR: Could not create output buffer for temp NPVU grib file...\n");
			return FILEOPERR;
		}

		long countr;
		char ch;
		/* must find the beginning of the GRIB message indicate by "GRIB" */

		while ((ch = getc(tmpfptr)) != EOF)
		{
		/* later testing revealed that there was a difference between the temp QPE
		   and converted QPF GRIB files on the 218 grid where extra characters
		   were in the beginning of the QPF GRIB message but not in the QPE
		   which started with "G".  Therefore, the first getc above moves the
		   file position pointer by one and therefore it never sees the "G" in the
		   QPE file. So while the header was copied, there was no "GRIB" found and none
		   in the NPVU QPE file.  Therefore if this is the first time we get
		   a character from the file, reposition it to the beginning and
		   grab that character again.  Finds "G" for the QPE files now correctly.
		*/

                   if(firstch==0)
		   {
		      fseek(tmpfptr,0L,SEEK_SET);

		      firstch=1;
		   }

		   countr=ftell(tmpfptr);
		   ch = getc(tmpfptr);
/*		   printf(" ch=%c countr=%ld \n",ch, countr); */
		   if (ch == 'G')
		   {
		      ch = getc(tmpfptr);

		      if (ch == 'R')
		      {

		         ch = getc(tmpfptr);
		         if (ch == 'I')
		         {

		           ch = getc(tmpfptr);
		           if (ch == 'B')
		              break;
		         }
		      }
		    }



		    fseek(tmpfptr,countr,SEEK_SET);
		}

		firstch=0;  /* reset the flag for next file */

		fseek(tmpfptr,countr,SEEK_SET);

		while ((odim = fread (temp,sizeof(unsigned char),BUFFSIZE,tmpfptr)) > 0 )
		{

		   fwrite(temp,sizeof(unsigned char),odim,fptrqpf);
		}

		if (ferror(tmpfptr) !=0 )
		{
		   printf(" Error reading temp NPVU grib file \n");
		   return FILEOPERR;
		}

		if (ferror(fptrqpf) !=0 )
		{
		   printf(" Error writing to NPVU grib file %s \n",outfnqpf);
		   return FILEOPERR;
		}
		else
		{
		   printf(" Writing to NPVU grib file appears to be successful\n");
		}


		if (tmpfptr != NULL)
		   fclose(tmpfptr);

		if (fptrqpf !=NULL)
		  fclose(fptrqpf);

		sprintf(command,"rm -f %s",tmpNPVUfn);  /* remove the temporary NPVU file */

		system(command);

	}

        /* ensure these are the correct sign for the next data set */

        grib_lbl[32]=firstLon;

	if(grib_lbl[25]==32)                 /* polar stereo */
	   grib_lbl[34]=lonOrigin*1000;
	else if(grib_lbl[25]==42)            /* lambert conformal */
	   grib_lbl[34]=(*latLonOrigin)*1000;
        else
	{
	   printf(" Map projection number %d not supported at this time...Exiting\n",grib_lbl[25]);
	   return UNERR;
	}
	odim=COPYSIZE;  /* reinitialize for copygb */

 	grib_lbl[24]=16; /* reinitialize for next data set in the NetCDF file */

	if(oflag==0)
	{
	   if(ofn!=NULL)
	   {
	      free(ofn);
	      ofn=NULL;
	   }
	   if(ofntemp!=NULL)
	   {
	      free(ofntemp);
	      ofntemp=NULL;
	   }
	}

	if(onegribflag)
	{
	   gfiles[numgfiles]= (char *) malloc (strlen(outfn)+1);
	   if(gfiles[numgfiles]==NULL && onegribwarn==0)
	   {
	      printf("\n Warning: can't combine grib files as desired due to memory allocation error \n" \
	             "\n Individual GRIB files will still be produced but not combined. Continuing...\n");
              onegribwarn++;
	   }
	   else if (onegribwarn==0)
	   {
	      strcpy(gfiles[numgfiles],outfn);
	      numgfiles++;
	   }
	}


   }

   ncclose(cdfid);

   /* if user desires only 1 GRIB file, must combine all into one */

   if (onegribwarn==0 && onegribflag>0 && numgfiles>=2)
   {
      if(gfiles[0]!=NULL && gfiles[1]!=NULL)
      {
         sprintf(onegrib,"%s/%s",gribdir,onegfname);

         onegfptr = fopen ( onegrib, "wb");
	 if ( onegfptr == NULL )
	 {
	    printf ( " ERROR: combined GRIB file could not be opened.\n" );
      	    return OPENERR;
	 }
	 else
	 {
	    printf(" Writing individual GRIB files into combined GRIB file %s...\n",onegrib);
	 }
	 if (setvbuf(onegfptr,NULL,_IOFBF,BUFFSIZE) !=0)
         {
     	    printf(" ERROR: Could not create output buffer for combined GRIB file %s...\n",onegfname);
	    return FILEOPERR;
 	 }


         for (i=0;i<numgfiles;i++)
	 {
	    fptr = fopen ( gfiles[i], "r");

	    if(fptr==NULL)
	    {
	     	printf ( " ERROR: GRIB file %s could not be opened.  Aborting combined GRIB file operation...\n", gfiles[i] );
      	     	return OPENERR;
            }

            if (setvbuf(fptr,NULL,_IOFBF,BUFFSIZE) !=0)
            {
	  	printf(" ERROR: Could not create output buffer for reading of GRIB file %s...\n", gfiles[i]);
		return FILEOPERR;
            }

    	    while ((odim = fread (temp,sizeof(unsigned char),BUFFSIZE,fptr)) > 0 )
	    {
                fwrite(temp,sizeof(unsigned char),odim,onegfptr);
            }
	    if (fptr != NULL)
	       fclose(fptr);

 	    sprintf(command,"rm -f %s",gfiles[i]); /* remove the grib file */
	    status=system(command);

         }
	 printf("\n Successfully combined individual GRIB files into this file:\n %s\n",onegrib);

      }
      else if (onegribflag>0)
      {
         printf("\n While attempting to combine files, there was a problem accessing the first two GRIB filenames.\n" \
	        " Therefore cannot combine GRIB files into one as desired\n");
      }

   }
   else if (onegribflag>0)
      printf("\n There was a problem while attempting to combine the GRIB files into one. \n" \
             " If number of GRIB files below equals to 1, won't be done. \n" \
             " For DEBUG purposes, GRIB warn = %d and number of GRIB files = %d\n",onegribwarn,numgfiles);

/* clean up */

   printf("\n nc2grib has completed processing for this run.\n");

   if(onegribflag>0)

      if(onegfptr!=NULL)
        fclose(onegfptr);

   for (i=0;i<numgfiles;i++)
      if(gfiles[i]!=NULL)
         free(gfiles[i]);

   if(output_buffer!=NULL)
      free(output_buffer);
   if(cdfvargrid!=NULL)
      free(cdfvargrid);
   if(gribdir!=NULL)
      free(gribdir);

   if(ofn!=NULL)
      free(ofn);
   if(infn!=NULL)
      free(infn);
   if(ofntemp!=NULL)
      free(ofntemp);
   if(inpath!=NULL)
      free(inpath);


   if(validTimes!=NULL)
      free(validTimes);
   if(latlonLL!=NULL)
      free(latlonLL);
   if(latlonUR!=NULL)
      free(latlonUR);
   if(gridSize!=NULL)
      free(gridSize);
   if(domainOrigin!=NULL)
      free(domainOrigin);
   if(domainExtent!=NULL)
      free(domainExtent);

   return 0;
}

int basetime_ansi_to_timet(char *ansi, time_t *timet)
{

   struct tm    gm_struct;
   int          rv = 0,
                scan_rv = 0;



   memset(&gm_struct,0,sizeof(struct tm));
   scan_rv = sscanf(ansi, "%4d%2d%2d%2d",
                      &(gm_struct.tm_year),
                      &(gm_struct.tm_mon),
                      &(gm_struct.tm_mday),
                      &(gm_struct.tm_hour));

   gm_struct.tm_year = gm_struct.tm_year - 1900;
   gm_struct.tm_mon  = gm_struct.tm_mon - 1;
   gm_struct.tm_min = 0;
   gm_struct.tm_sec = 0;

   if (scan_rv < 4)
   {
        printf("\n scan_rv = %d\n",scan_rv);
        rv = -1;
   }

   else
   {
      /* convert the tm structure to a time_t value using
         the inhouse function gm_mktime; this is needed because
         there is no system library function to convert to time_t
         in gm time; one only exists for local time */

      *timet = gm_mktime(&gm_struct);
   }

   return rv;
}

int timet_to_userformat_ansi(time_t timet, char *ansi, char* userformat)
{   /*

   Convert from the C language time_t representation to
   an ANSI time string representation.

   */


   struct tm    *gm_struct;
   size_t rv  = 0;


   /* convert the time_t variable into a time structure for gm time */

   gm_struct = gmtime(&timet);


   /* convert the time_t variable to a time string */

   strcpy(ansi,"");
   rv = strftime(ansi, FILE_LEN, userformat, gm_struct);

   return((int) rv);
}

int display_usage(void)
{
        printf("\n\n nc2grib GFE NetCDF to GRIB1 translator, usage:\n\n" \
              "./nc2grib.LX -n (input netcdf path) -i (netcdf file) -t (output grib path) -o (output grib file) \n" \
	      "             -b (basis time) -p (process ID) -g (one GRIB filename) -f -N -v -h\n" \
	      "where:\n" \
	      "-n (input netcdf path)            Refers to the path containing the NetCDF file\n" \
	      "   Optional, requires argument    generated by the GFE routine ifpnetCDF.\n" \
	      "                                  If not used, the token netcdf_dir will be used \n" \
	      "                                  to retrieve this information\n\n" \
	      "-i (input netcdf file)            Refers to the NetCDF file generated in the format\n" \
	      "   Required, requires argument    used by the GFE routine ifpnetCDF.\n\n" \
	      "                                  NOTE that this command line option and its argument\n" \
	      "                                  must be specified in the call to nc2grib.\n\n" \
	      "-t (output grib path)             Refers to the path of the GRIB file(s) generated by nc2grib.\n" \
	      "   Optional, requires argument    If not used, the token fewsgrib_dir will be used \n" \
	      "                                  to retrieve this information.\n\n" \
	      "-o (output grib file)             Refers to the GRIB file generated by nc2grib.\n" \
	      "   Optional, optional arguement   If this option is not specified or specified without an \n" \
	      "                                  argument, then the input file name without the .cdf extension\n" \
	      "                                  will be used to form the output filename. \n" \
	      "                                  Example: -i QPE6hr.cdf will result in QPE6hr_YYYYMMDDHHhNNN.grb\n" \
	      "                                  being produced (with date/hour characters filled in).\n\n" \
	      "-b (basis time in YYYYMMDDHH      Refers to the basis time for forecast GRIB files.\n" \
	      "    format)                       Used primarily for forecast grids and QPE grids sent to NPVU. \n" \
	      "   Required for forecast          Example: -b 2009051412 \n" \
	      "   grids and QPE grids going to   \n" \
	      "   NPVU,requires argument         \n\n" \
	      "-p (process ID)                   Refers to the parameter process ID relating to a GFE parameter\n" \
	      "   Required, requires argument    such as QPF. Needs to match against a process in the gfe2grib.txt\n" \
	      "                                  configuration file.\n" \
	      "                                  NOTE that this command line option and its argument \n" \
	      "                                  must be specified in the call to nc2grib.\n\n" \
	      "-g (one GRIB file flag)           If one GRIB file for all grids is desired, specify this flag on the \n\n" \
	      "   Optional, requires argument    command line with an output file name that will be used to combine all \n" \
	      "                                  individual GRIB files into one.\n" \
	      "                                  NOTE that individual NPVU GRIB files will still be produced. \n\n" \
	      "-q (QPE flag)                     This indicates that the process specified by -p is an estimated/observed grid \n" \
	      "   Required for QPE grids being   where the time determinations of the grid correspond to those expected by \n" \
	      "   sent to NPVU                   NPVU. This option is most likely used for QPE grids and is required in that case \n" \
	      "                                  Otherwise, the degribbing scripts at NPVU can't read the grids properly. \n\n" \
	      "-f (format flag)                  Tells the routine to use the grid's valid time within the filename format specifiers \n" \
	      "   Optional                       instead of basis time information.  Note that this option or lack thereof has no \n" \
	      "                                  effect if the routine assigns a default filename format.\n\n" \
	      "-r (reverse flag)                 Used in conjunction with -b and -q options and formatted output (see example),\n" \
	      "   Optional, can be used in       this flag tells nc2grib to put the hours past basis time before the period interval \n" \
	      "   conjunction with -b and -q     within the formatted filename.  Otherwise, the default is that the period interval \n" \
	      "   options                        comes before the hours past basis time. No effect if -f option is used.  \n\n" \
	      "-N (NPVU file flag)               Tells nc2grib to also generate a QPF forecast GRIB file with an\n" \
	      "   Optional                       appropriate WMO header so that it can be read and used by NPVU.\n\n" \
	      "-v (verbose debugging flag)       Optional flag used to print out expanded debugging information for\n" \
	      "   Optional                       each GRIB1 file\n\n" \
	      "-h (display help flag)            Displays this usage help message\n" \
	      "   Optional\n\n" \
	      "Filename formatting (see nc2grib documentation for more detailed examples and usage: \n\n" \
	      " Most commonly used date and time indicators: \n" \
	      "   %%Y Four digit year \n" \
	      "   %%m Two digit month \n" \
	      "   %%d Two digit day of the month \n" \
	      "   %%H Two digit 24-hour clock value \n" \
	      " example:  -o qpe6hr_%%Y%%m%%d_%%H \n\n" \
	      "Pertinent tokens which should be set:\n" \
	      "      nc2g_app_dir - contains the directory where the gfe2grib.txt file is located.\n" \
	      "      fewsgrib_dir - contains a default directory location for generated GRIB files.  If not overridden by \n" \
	      "                     the -t option above, it MUST be specified for nc2grib to run. \n" \
	      "      netcdf_dir   - contains default location of the generated NetCDF files.  If not overridden by the \n" \
	      "                     -n option above, it MUST be specified for nc2grib to run.\n\n" );

	      return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9d/ohd/pproc/src/nc2grib/RCS/main_nc2grib.c,v $";
 static char rcs_id2[] = "$Id: main_nc2grib.c,v 1.2 2010/06/14 15:04:32 millerd Exp $";}
/*  ===================================================  */

}


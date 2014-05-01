#define _POSIX_SOURCE
#define MAX_LOOP 10000                                     /* dgb:11/28/97 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global.h"

/* --------------------------------------------------------------------

       FUNCTION
         metar_decode

       PURPOSE
         Driver for the METAR decoder.  This version will 'pluck' any
         file from a user specified directory, decode the data, and
         change it into shef format.  The shef encoded file is then
         written to a directory where it is picked up by the shef
         decoder.

         The control of the decoder is managed through a file called
         metar.cfg.  It can be read through a command line option,
         "-fcfg filename" or default to the current directory.


       VERSION and UPDATES (for this function only..see documentation )
         1.0    APR 95   David G. Brandon
                Original Version
         1.1	DEC 95   David G. Brandon
                Add -m option.  This assumes observations are in metric
                and will be converted into english units.
         1.2    JAN 96   DGB
                Make a check so that only files are processed and
                that any subdirectories are passed over.
         1.3    Add REVISION flag.
         1.4    FEB 22 96 DGB
                Add command line option, '-i filename' so that a specific
                filename can be read from the command line.
         1.5    MAR 11 96 DGB
                Add the command line options, P24 and P6.
         1.6    APR 1 96 DGB]
                Include full path name when checking for file or
                directory names using the stat function.
         1.7    APR 15 96 DGB
                Add the -a option which strips off the first character
                of the id in a metar ob.  For example, the id KPIT
                would be output at PIT.  The default is not to strip
                off the id.
         1.8    MAY 28 96 DGB
                Add switch, "-oh", which allows the user the feature to
                directly write the shef out files to the naming
                structure used by oh.  OH uses shef_prod#, where #
                is a number between 1 and 999.
         1.9    JUL 8 96 DGB
                Add the -nospeci switch, which allows users to only
                decode routine hourly obs, and disregard specials
                (however, METAR record hourly reports that meet special
                criteria will still be decoded since they are on the hour).
         2.0    JUL 18 96 DGB
                Add the -pall6 option which will produce a zero
                precip amount for every station on the 6 hour time
                step, if a 6 hourly precip group is not received.
         2.1    JUL 19 96 DGB
                Add the -pall24 and -round, and -p12z switches.
         2.2    JUL 22 96 DGB
                Add the -fcfg filename switch.
         2.3    JUL 28 96 DGB
                Add the -asosts switch.
         2.4    SEP 5 96 DGB
                Tighten up bounds for -p12z switch to 11:40 to 12:30z.
         2.5    OCT 23 96 DGB
                Add xw and xc capabilities.
         2.6    NOV 8 96 DGB
                Fix RADAT problem.
         2.7    NOV 24 96 DGB
         	    Fix problem with the pattern search algorith that
                looks for the token 'SPECI '. The algorith was tightened
                up.  Previously, a trailing blank was though to be
                the culprit, but was found out not to be the problem.
                See functions, metar_read, for further information.
         2.8    DEC 2  96 DGB
                Add SDO and SCD capabilities.
         2.9    DEC 7 96 DGB
                Add new switch, -b, which when present will turn
                decoder on to decoder collectives.
         3.0    MAR 27 97 DGB
                Change to look for AO1 as well as AO2 for detecting
                automated rain gages.
         3.1    OCT 10 97 DGB
                Add -p1 option...generate a zero report for automated
                rain gage sites if no value is reported and the
                there is no PNO { Precip Not Operating } token.
                This function was supposed to be in place but was
                apparently not working properly.

                Put a check to test for AO1 or AO2 and change them to
                A01 and A02 if present.

                Fix an error in dcdmtrmk.c { Carl McCalla provided
                the fix }.  When a comment such as FRQ LTGCG VC
                or LTGCCG SE occurred followed by an hourly precip
                group, e.g. P0001, the precip group was not decoded.
                Actually the token that followed the lighting group
                was not decoded.
          3.2   NOV 10 97 DGB
                Changed Sky/Cloud Cover conversions.  See documentation.
                Version number changed to 4.3.
          3.3   APR 4 98 DGB
                Changed strcmp to search for -p12 instead of -p12z.
                Some people put -p12Z {or a capitalZ}.  The switch
                would not work properly.  This fix will only look for
                the first 4 characters of the string, i.e., -p12.
          3.4   JUL 1 98 DGB
                Include switch -e.  This will tell the decoder
                to NOT convert temperatures that are in metric to
                english units.
          3.5   AUG 5 98 DGB
                Add -y2k switch (metar_ver 4.9 )
          3.6   AUG 13 98 DGB
                metar_ver 5.0
                Add -y2k option for ssm reports also (left off
                last version.)
                Add a new switch, '-strip'.  This will strip
                off and convert certain supsect values to
                blanks.  The rules are:
                if value > 0  and value < 9 conver to blank
                if value > 10 and value < 32 convert to blank
                if value == 61 (ASCII '=') convert to blank.

                Change switch '-s' to -'sw'.
          3.7   JAN 3 98 ( Version 5.2 )
                Add new switch, '-kt'.  When included on the command
                line, all wind speeds will be output in units on knots.
                The default (if switch is not included on the command
                line) will be units of miles per hour (MPH).
                Change shef code from 'UP' to 'UG' since the
                max speed decoded is a gust during the time of observation.
          3.8   OCT 26 99  ( Version 5.3)
                Add capability to output a 'T' if a zero amount
                is received in the 3, 6 or 24 hour group.
          3.9   JAN 10 2000 DGB ( Version 5.5)
	   	Change it so that if the T group exists, the
	        decoder will output the temp and dew point
	        using degrees and tenths of degrees in the
		shef value in lieu of the values in the body
		of the observation ( requested by Jeff Zimmerman,
		Office of Hydrology).

		Set file pointers to 0 after closing...needed by
		some systems.

		*** new switch ***
		Add new switch, -x #.  This switch will alow a
		user to enter a date/time that will override
		the system date/time.  This is used in testing
		date time stamps. This included adding test_.century_flag
		and test_.century_string.
                Allow for year, month, and day.  For example:
                -x 2000         override system year & set to 2000
                -x 200012       override year and month
                -x 20001231     override year, month and day
                -x 2000123100   override year,month, day, hour
                -x 200012311010 override year,month,day,hour,min

                 *** -b switch ***
                 Set the -b switch to on permanently.

		 *** observation time computation ***
		 Changed the way that observation times are
		 computed.  There are two cases based on:
	         (1)  the day is included in the observation
		 (2)  the day not included in the observation

		 If the day is not included, the system year,
		 month and day are used along with the observation
		 time.

		 If the day is included, the system year and month
		 are used.  If the day in the ob is > than the
		 system day, the month is decremented ( and the
		 year if necessary).  If the day in the ob is
		 equal to the system day, then if the ob time is
		 greater than the system time, a warning is
		 printed.

                 Remove the -oh switch and code.

                 Add new switch, -howold #dec.
		 The decoder will wait this many seconds before
		 processing a file.  The default is 15 seconds.

		 Use the revised function: is_file_close
		 This version makes checks in seconds instead of
		 minutes. The tolerance variable was also removed.

		 The function 'strpat' changed.  It has two 'new' additional
		 arguments.  The argument call list to the function strpat
		 was changed for all calls to this function.

                 New switch, -howold # was added.  The # is in
                 units of seconds.  It tells how old a file needs
                 to be before processing begins...the default is 15 sec.

          3.9    FEB 04 2000 DGB ( Version 5.6)
	         Check for global SPECI in collectives.  If
		 present, include SPECI indicator in observation.

                 Add capability to screen output by specific id
                 read in from configuration file.  This is turned
                 on with the -g switch.

	 4.0     FEB 18 2000 DGB ( Version 5.7 )
	         New switch, -salias.  When present, the decoder will
		 check an alias id table in the cfg file and alias
		 numerical 'sm' ids to three characters ids.

	 4.1     OCT 31 2000 DGB ( Version 5.8 )
		 Add code for -j switch for normal output in individual files.
		 j1 = output observation + decode list
		 j2 = output observation only
		 j3 = output decode list only

	 4.2     OCT 31 2000 DGB ( Version 5.9 )
	         Check to see if the three mandatory directories
		 exist ...  as a precaution.

	         Switch pe codes of PD and PE.  PD should be
		 pressure change/tendency and PE presure
		 characteristic.  This is in the shef_it_metar
		 function and the output is switched.

		 Change -asosts switch to -l .

		 Add new switch, -q1.  When set this will output
		 wind direction in hundreds and not tens

	  4.3    Dec 01 00 DGB ( Version 6.0 )
	         Return (0) at bottom of function...some compilers
		 such gcc caused a dump at runtime.

	  4.4    FEB 26 2002 DGB
                 Add a blank before the "SPECI" string, making it
		 " SPECI".  Before this change, NOSPECI was incorrectly
		 detected as SPECI.  In function, metardrive.

	  4.5    AUG 09 2002 DGB
	         Fix core dump problem when encountering an SDO/SCD
		 observation with no data.

	  4.6    AUG 09 2002 DGB (Version 6.9 )
	         Change shef code for new 6 hour snowfall to SFQ
		 Add capabilities to decode 24/931sss group which
		 is new 24 hour snow fall...mainly for the SCD reports.


 *--------------------------------------------------------------------- */
extern int suspend_it( char dirname[] );
extern void metar_test();
extern void metar_ifile();
extern int print_usage();
extern void  metarclose();
extern int metar_error_handler( char function[] );
extern void  metarcl();
extern int metardrive();
extern int is_file_closed(char filename[], int how_old);
extern int metarop();
extern void read_xref();
extern void metar_open();


FILE *fp_postlog;

int    num_records;
int    WMO, METRIC, VERBOSE, DEBUG, begin_msg_ptr, begin_remarks_ptr, begin_clds_ptr;
int    AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, METAR, REMARKS;
int    REVISION, I_FILE, SWITCH_SOURCE, P24, P6, ID, CONTINUOUS;
int    GLOBAL_EOF, GLOBAL_TYPE, NO_SPECI;                  /* dgb:07/08/96 */
int    HOW_OLD;                                            /* dgb:01/10/00 */
int    PALL6;                                              /* dgb:07/18/96 */
int    PALL24, ROUND, P12Z;                                /* dgb:07/19/96 */
int    FCFG;                                               /* dgb:07/22/96 */
int    ASOSTS;                                             /* dgb:07/26/96 */
int    PEDTSEP;
int    SDO, SCD;                                           /* dgb:12/02/96 */
int    COLLECTIVES;                                        /* dgb:12/07/96 */
int    ENGL;                                               /* dgb:07/01/98 */
int    P1;                                                 /* dgb:10/10/97 */
int    Y2K;                                                /* dgb:08/05/98 */
int    STRIP;                                              /* dgb:08/18/98 */
int    KNOTS;                                              /* dgb:01/03/98 */
int    i_total, ii_total;                                  /* dgb:12/13/99 */
int    GLOBAL_SPECI;                                       /* dgb:02/04/00 */
int    GET_MTR_NAMES;                                      /* dgb:02/04/00 */
int    GET_PC_NAMES;                                       /* dgb:11/20/04 */
int    SALIAS;                                             /* dgb:02/18/00 */
int    JUST_OUTPUT;                                        /* dgb:10/31/00 */
int    WIND_LEN;                                           /* dgb:10/31/00 */
int    PC_TOLERANCE;                                       /* dgb:11/20/04 */
int    LOGIT;                                              /* dgb:11/20/04 */
DIR    *dirp;

char   *directory;
char   *pname = "metar_decode";

struct dirent *de;
void usage(int argc,char *argv[]);

int metar_decode_main( int argc, const char ** argv)

/* int argc; char *argv[]; */
{
   char  sheftemp1[90], sheftemp2[90], fname[100];

   struct stat buf;
   int ii;                                                    /* dgb:11/28/97 */
   int LOOP, SNOOZE_TIME = 5;                                 /* dgb:05/02/96 */


   /* Check command line options */
   /* Swap location with metar_open */                        /* dgb:07/22/96 */
      usage(argc,argv);

   /* Open the metar.cfg and parameter files */
      metar_open();

   /* Clean and initialize file arrays  */
      memset(sheftemp1,0,sizeof(sheftemp1));
      strcpy(sheftemp1,files_.shef_in);
      strcpy(sheftemp2,files_.shef_in);

   /* check if task should be suspended */
      if ( suspend_it(sheftemp1) != 0 )
           exit(0);


   /* if JUST_OUTPUT read in xref list */
   if ( JUST_OUTPUT )
        read_xref();

   LOOP = 1;


   while ( LOOP )
   {

   /* open the directory */

      ii_total = 0;                                         /* dgb:12/13/99 */
      i_total  = 0;                                         /* dgb:12/13/99 */
      chdir(sheftemp1);
      dirp = opendir(sheftemp1);
      if ( dirp != NULL )
      {
         ii = 0;                                            /* dgb:11/28/97 */
         /* Process each file in directory  */
         while (  ( de=readdir(dirp))  != NULL )
         {

            memset(fname,sizeof(fname),0);
            strcpy(fname,sheftemp1);
            strcat(fname,"/");
            strcat(fname,de->d_name);

            /* process only files - not directories */
            stat(fname,&buf);                               /* dgb:04/01/96 */
            if  ( !S_ISDIR(buf.st_mode)  )
            {

               /* skip files that begin with a '.' */
               if ( de->d_name[0] != '.' )
               {

                  if ( VERBOSE )
                       fprintf(stdout,"\n*****\nbegin processing file-> %s",de->d_name);
                  error_.nerror = 0;
                  GLOBAL_EOF = 0;                           /* dgb:05/28/96 */


                  /* check if file is in use */
                  if ( !is_file_closed(fname,HOW_OLD) )     /* dgb:01/10/00 */
                  {
                     stat(fname,&buf);
                     if ( buf.st_size > 0 )
                     {
                        strcpy(files_.shef_in,fname);
                        strcpy(stats_.product_name,de->d_name);

                        if ( metarop() == 0 )
                        {
                          metardrive();

                          metarcl();
                        }
                     }
                     else
                     {
                        remove( fname );
                     }

                  }

               }

            }
            ii++;                                           /* dgb:11/28/97 */
            if ( ii > MAX_LOOP ) metar_error_handler("metar_decode"); /* dgb:11/28/97 */

         }   /* end of while readdir */

         if ( VERBOSE )  fprintf(stdout,"\n     closing directory");
              closedir(dirp);

      }   /* end of if dir */

      /* close files */
         metarclose();

         if ( !CONTINUOUS )
           LOOP = 0;
         else
           sleep(SNOOZE_TIME);

   }   /* end of master while loop */

   return(0);


}



void usage( int argc, char *argv[])
{
int ii;

   /* dgb:10/10/95 usage function added */
   /*

       -t             turn on test option
       -d             turn on debug option
       -v             turn on verbose option
       -a             strip off the first character in the id of a metar ob
       -m             assume SAO obs are in metric units - translate into english units
       -g             read list of metar ids to process from the cfg file
       -e             do not convert metric temps to english
       -w             list wmo and ZCZC line in output
       -p24           decode only 24 precip amounts for times 1140-1230Z
       -p6            generate a 0 value for PPQ for 6 hour periods if 6$$$/ group is missing
       -sw            switch source of PEDTSEP from 'Z' to 'V' for testing METAR
       -strip         convert bad ascii values to blanks
       -howold #      how old a file needs to be in seconds before processing
       -kt            output wind speeds in units of knots (default mph)
       -i filename    input filename from command line
       -j#            output each metar in it's own file - j1=ob + decode, j2=ob, j3=decode - no shef
       -nospeci       do not decode special observations
       -pall6         generate 0 values for all stations on the 6 hourly times if no value present
       -pall24        generate 0 values for alls station on the 24 hourly times if no value present
       -round         round observation time to the whole hour for non special obs
       -p12z          only decode 24 precipitation in the 12Z window
       -oct #         tolerance in minutes for pc reset times (default = 2)
       -q1            output wind direction in hundreds & not tens
       -fcfg filename input configuration path/filename from command line
       -salias        check alias id table for sm ids
       -l             TS for ASOS stations = RO, TS for all other sites = RV\n\n
       -log           turn on log of current product and ob beind processed
       -b             turn on decoder to accept collectives
       -p1            generate a 0 value for automated stations if no precip value is present
       -x #           override system ccyymmdyhrmn with value = #
       -y2k           output century in SHEF output
      */

      DEBUG           = 0;
      VERBOSE         = 0;
      METRIC          = 0;
      WMO             = 0;
      I_FILE          = 0;                                 /* dgb 02/23/96 */
      SWITCH_SOURCE   = 0;
      P24             = 0;                                 /* dgb 03/11/96 */
      P6              = 0;                                 /* dgb 03/11/96 */
      ID              = 0;                                 /* dgb 04/15/96 */
      CONTINUOUS      = 0;                                 /* dgb 05/02/96 */
      NO_SPECI        = 0;                                 /* dgb:07/08/96 */
      PALL6           = 0;                                 /* ddg:07/18/96 */
      PALL24          = 0;                                 /* dgb:07/19/96 */
      ROUND           = 0;                                 /* dgb:07/19/96 */
      P12Z            = 0;                                 /* dgb:07/19/96 */
      FCFG            = 0;                                 /* dgb:07/22/96 */
      ASOSTS          = 0;                                 /* dgb:07/26/96 */
      test_.test_flag = 0;
      PEDTSEP         = 0;
      COLLECTIVES     = 0;                                 /* dgb:12/07/96 */
      P1              = 0;                                 /* dgb:10/10/97 */
      ENGL            = 0;                                 /* dgb:07/01/98 */
      Y2K             = 0;                                 /* dgb:08/05/98 */
      STRIP           = 0;                                 /* dgb:08/18/98 */
      KNOTS           = 0;                                 /* dgb:01/04/98 */
      test_.century_flag = 0;                              /* dgb:01/10/99 */
      HOW_OLD         = 15;                                /* dgb:01/10/00 */
      GET_MTR_NAMES   = 0;                                 /* dgb:02/04/00 */
      GET_PC_NAMES    = 0;                                 /* dgb:11/20/04 */
      SALIAS          = 0;                                 /* dgb:02/18/00 */
      JUST_OUTPUT     = 0;                                 /* dgb:10/31/00 */
      WIND_LEN        = 0;                                 /* dgb:10/31/00 */
      PC_TOLERANCE    = 2;                                 /* dgb:11/20/04 */
      LOGIT           = 0;                                 /* dgb:11/20/04 */

      if ( argc == 2 && strncmp(argv[1],"-?",2) == 0 )
           print_usage();

      for ( ii = 0; ii < argc; ii++ )
      {
         if ( strncmp(argv[ii],"-d",2) == 0 )
              DEBUG = 1;
         if ( strncmp(argv[ii],"-v",2) == 0 )
              VERBOSE = 1;
         if ( strncmp(argv[ii],"-g",2) == 0 )               /* dgb:02/04/00 */
              GET_MTR_NAMES = 1;                            /* dgb:02/04/00 */
         if ( strncmp(argv[ii],"-t",2) == 0 )
              test_.test_flag = 1;
         if ( strncmp(argv[ii],"-m",2) == 0 )
              METRIC = 1;
         if ( strncmp(argv[ii],"-sw",3) == 0 )
              SWITCH_SOURCE = 1;
         if ( strncmp(argv[ii],"-strip",6) == 0 )
              STRIP = 1;
         if ( strncmp(argv[ii],"-w",2) == 0 )
              WMO    = 1;
         if ( strncmp(argv[ii],"-a",2) == 0 )
              ID     = 1;
         if ( strncmp(argv[ii],"-q1",3) == 0 )              /* dgb:10/31/00 */
              WIND_LEN     = 1;                             /* dgb:10/31/00 */
         if ( strncmp(argv[ii],"-j1",3) == 0 )              /* dgb:10/31/00 */
              JUST_OUTPUT     = 1;                          /* dgb:10/31/00 */
         if ( strncmp(argv[ii],"-j2",3) == 0 )              /* dgb:10/31/00 */
              JUST_OUTPUT     = 2;                          /* dgb:10/31/00 */
         if ( strncmp(argv[ii],"-j3",3) == 0 )              /* dgb:10/31/00 */
              JUST_OUTPUT     = 3;                          /* dgb:10/31/00 */
         if ( strncmp(argv[ii],"-kt",3) == 0 )
              KNOTS     = 1;                                /* dgb:01/03/98 */
         if ( strncmp(argv[ii],"-e",2) == 0 )
              ENGL     = 1;                                 /* dgb:07/01/98 */
         if ( strncmp(argv[ii],"-p24",4) == 0 )             /* dgb 03/11/96 */
              P24    = 1;
         if ( strncmp(argv[ii],"-p1",3) == 0 )              /* dgb 10/10/97 */
              P1    = 1;
         if ( strncmp(argv[ii],"-p6",3) == 0 )              /* dgb 03/11/96 */
              P6    = 1;
         if ( strncmp(argv[ii],"-c",2) == 0 )               /* dgb 05/02/96 */
              CONTINUOUS  = 1;
         if ( strncmp(argv[ii],"-nospeci",8) == 0 )         /* dgb 07/08/96 */
              NO_SPECI = 1;
         if ( strncmp(argv[ii],"-pall6",6) == 0 )           /* dgb 07/18/96 */
              PALL6 = 1;
         if ( strncmp(argv[ii],"-pall24",7) == 0 )          /* dgb 07/19/96 */
              PALL24 = 1;
         if ( strncmp(argv[ii],"-round",6) == 0 )           /* dgb 07/19/96 */
              ROUND  = 1;
         if ( strncmp(argv[ii],"-p12z",4) == 0 )            /* dgb 04/04/98 */
              P12Z = 1;
         if ( strncmp(argv[ii],"-pedtsep",8) == 0 )
              PEDTSEP = 1;
         if ( strncmp(argv[ii],"-salias",7) == 0 )          /* dgb 02/18/00 */
              SALIAS= 1;                                    /* dgb 02/18/00 */
         if ( strncmp(argv[ii],"-l",7) == 0 )               /* dgb 10/31/00 */
              ASOSTS= 1;
         if ( strncmp(argv[ii],"-b",2) == 0 )               /* dgb:12/07/96 */
              COLLECTIVES = 1;
         if ( strncmp(argv[ii],"-log",4) == 0 )             /* dgb:11/20/04 */
              LOGIT = 1;                                    /* dgb:11/20/04 */

         COLLECTIVES = 1;                                   /* dgb:01/10/00 */
         if ( strncmp(argv[ii],"-y2k",4) == 0 )             /* dgb:08/05/98 */
              Y2K = 1;
         if ( strncmp(argv[ii],"-howold",7) == 0 )          /* dgb:01/10/00 */
         {                                                  /* dgb:01/10/00 */
              if ( ii+1 < argc )                            /* dgb:01/10/00 */
              {                                             /* dgb:01/10/00 */
                 if ( strchr(argv[ii+1],'-') != NULL )      /* dgb:01/10/00 */
                 {                                          /* dgb:01/10/00 */
                    fprintf(stdout,"\nmetar_decode:ERROR-no value specified for -howold option\n");
                    metarclose();                           /* dgb:01/10/00 */
                    exit(0);                                /* dgb:01/10/00 */
                 }                                          /* dgb:01/10/00 */
                 HOW_OLD = atoi(argv[ii+1]);                /* dgb:01/10/00 */
              }                                             /* dgb:01/10/00 */
              else                                          /* dgb:01/10/00 */
              {                                             /* dgb:01/10/00 */
                 if ( VERBOSE )                             /* dgb:01/10/00 */
                    fprintf(stdout,"\nmetar_decode:ERROR-no value specified for -howold option\n");
                 metarclose();                              /* dgb:01/10/00 */
                 exit(0);                                   /* dgb:01/10/00 */
              }                                             /* dgb:01/10/00 */
          }                                                 /* dgb:01/10/00 */

         if ( strncmp(argv[ii],"-pct",4) == 0 )             /* dgb:11/20/04 */
         {                                                  /* dgb:11/20/04 */
              GET_PC_NAMES = 1;                             /* dgb:11/20/04 */

              if ( ii+1 < argc )                            /* dgb:11/20/04 */
              {                                             /* dgb:11/20/04 */
                 if ( strchr(argv[ii+1],'-') != NULL )      /* dgb:11/20/04 */
                 {                                          /* dgb:11/20/04 */
                    fprintf(stdout,"\nmetar_decode:ERROR-no value specified for -pct option\n");
                    metarclose();                           /* dgb:11/20/04 */
                    exit(0);                                /* dgb:11/20/04 */
                 }                                          /* dgb:11/20/04 */
                 PC_TOLERANCE = atoi(argv[ii+1]);           /* dgb:11/20/04 */
              }                                             /* dgb:11/20/04 */
              else                                          /* dgb:11/20/04 */
              {                                             /* dgb:11/20/04 */
                 if ( VERBOSE )                             /* dgb:11/20/04 */
                    fprintf(stdout,"\nmetar_decode:ERROR-no value specified for -pct option\n");
                 metarclose();                              /* dgb:11/20/04 */
                 exit(0);                                   /* dgb:11/20/04 */
              }                                             /* dgb:11/20/04 */
          }                                                 /* dgb:11/20/04 */

         if ( strncmp(argv[ii],"-x",2) == 0 )               /* dgb:01/10/00 */
         {                                                  /* dgb:01/10/00 */
            test_.century_flag = atoi(argv[ii+1]);          /* dgb:01/10/00 */
            strcpy(test_.century_string,argv[ii+1]);        /* dgb:01/10/00 */
         }                                                  /* dgb:01/10/00 */
         if ( strncmp(argv[ii],"-i",2) == 0 )               /* dgb 02/23/96 */
         {
              I_FILE    = 1;
              if ( ii+1 < argc )
              {
                 if ( strchr(argv[ii+1],'-') != NULL )
                 {
                    fprintf(stdout,"\nmetar_decode:ERROR-no filename specified for -i option\n");
                    metarclose();
                    exit(0);
                 }
                 strcpy(tempfiles_.ifile,argv[ii+1]);
              }
              else
              {
                 if ( VERBOSE )
                    fprintf(stdout,"\nmetar_decode:ERROR-no filename specified for -i option\n");
                 metarclose();
                 exit(0);
              }
         }

         if ( strncmp(argv[ii],"-fcfg",5) == 0 )            /* dgb 07/21/96 */
         {
              FCFG    = 1;
              if ( ii+1 < argc )
              {
                 if ( strchr(argv[ii+1],'-') != NULL )
                 {
                    fprintf(stdout,"\nmetar_decode:ERROR-no filename specified for -fcfg option\n");
                    metarclose();
                    exit(0);
                 }
                 strcpy(tempfiles_.cfg,argv[ii+1]);
              }
              else
              {
                 if ( VERBOSE )
                    fprintf(stdout,"\nmetar_decode:ERROR-no filename specified for -fcfg option\n");
                 metarclose();
                 exit(0);
              }
         }

      }

      if ( test_.test_flag )
      {
          metar_open();                                     /* dgb:07/22/96 */
          metar_test();
          metarclose();
	  exit(1);
          exit(0);
      }

      if ( I_FILE )                                         /* dgb 02/22/96 */
      {
          metar_ifile();
          metarclose();
          exit(0);
      }


}


int print_usage()
{

   /* dgb:10/10/95 - print_usage function added */
   fprintf(stdout,
      "\n\nmetar2shef V7.0 - SEP 15, 2005 - NOAA/NWS/OHD \n");
   fprintf(stdout,
      "metar2shef.LX -a -l -b -c seconds -d -e -fcfg filename -g -j -kt -howold #sec \n              -i filename -m -nospeci -p1 -p6 -p24 -pall6 -pall24 -p12 -pct # -log -round\n              -salias -strip -sw -t -u -v -w -x # -y2k ");
   fprintf(stdout,"%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s",
	  "\nwhere:\n",
      " -a             strip off the first character in the id of a metar ob\n",
      " -b             turn on decoder to accept collectives\n",
      " -c seconds     run decoder in continuous mode every # of seconds\n",
      " -d             turn on debug information\n",
      " -e             do not convert temperatures from metric to english\n",
      " -fcfg filename input configuration path/filename from command line\n",
      " -g             read list of metar ids to process from the cfg file\n",
      " -kt            output wind speeds in units of knots (default mph)\n",
      " -howold #sec   how old a file needs to be in seconds before processing(default=15)\n",
      " -i filename    provide filename from command line\n",
      " -j#            output each metar in it's own file - j1=ob + decode, j2=ob, j3=decode - no shef\n",
      " -l             SHEF TS for ASOS stations = RO, TS for all other sites = RV\n",
      " -log           turn on log for current product and observation\n",
      " -m             assume SAO (Canadian) obs are in metric and convert to english\n",
      " -nospeci       do not decode special (SPECI) obs\n",
      " -p1            generate a 0 value for PPH for 1 hour period if precip\n                 group is missing for automatic stations only\n",
      " -p6            generate a 0 value for PPQ for 6 hour periods if precip\n                 group is missing for automatic stations only\n",
      " -p24           generate a 0 value for PPD for 24 hour period if precip\n                 group is missing for automatic stations only in window 1140-1230Z\n",
      " -pall6         generate a 0 value for PPQ for 6 hour periods if precip\n                 group is missing for all stations\n",
      " -pall24        generate a 0 value for PPD for 24 hour periods if precip\n                 group is missing for all stations in window 1140-1230Z\n",
      " -p12z          decode 24 precipitation amounts in the window 1140-1230\n                 default is to decode 24 amounts for all ob times\n",
      " -pct #         turn on for lookup in reset table and set tolerance in minutes for reset times (default = 2)\n",
      " -q1            output wind direction in hundreds & not tens\n",
      " -round         round time to nearest whole hour for non-special obs\n",
      " -salias        turn on check of alias id table for sm observations\n",
      " -sw            switch source of PEDTSEP form 'Z' to 'V' for testing METAR\n",
      " -strip         convert bad ascii values to blanks\n",
      " -t             execute test version\n",
      " -v             turn on verbose option\n",
      " -w             output wmo line and ZCZC CCCNNNXXX with output\n",
      " -x #           override system ccyymmdyhrmn to the value = #\n",
      " -y2k           output century in SHEF product\n");
 	exit(0) ;
}


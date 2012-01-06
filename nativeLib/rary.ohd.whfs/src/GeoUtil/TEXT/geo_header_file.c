/*******************************************************************************
* FILENAME:             geo_header.c
* NUMBER OF MODULES:    2
* GENERAL INFORMATION:
*   MODULE 1:           check_for_int_latlon 
* DESCRIPTION:          Checks to determine if the latitude / longitude 
*                       of the geoarea centroid point has been appended
*                       to the header of the geoarea data block. 
*
*   MODULE 2:           parse_geo_header
* DESCRIPTION:          Parses the header of the geoarea or geoline data 
*                       block.
*
* ORIGINAL AUTHOR:      Mark Glaudemans
*                       (Converted into a subroutine by Bryon lawrence 
*                        October 20, 2002.)
* CREATION DATE:        Unknown
* ORGANIZATION:         HSEB / OHD
* MACHINE:              HP-UX / Dell / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        Unknown      Mark Glaudemans   Original Coding  
*          2        Unknown      Mark Glaudemans   Original Coding
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "geo_header_file.h"
#include "geo_log.h"
#include "geoutil.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   check_for_int_latlon
*
* PURPOSE: Reads the interior lat/lon from the file.
*          If the pair of values is there they are read, and
*          the string is adjusted so the string terminates just
*          prior to the location of the interior value, so that
*          additional processing can be performed whether the
*          interior values are found or not.
*
*          Note that this routine requires that all excess leading and
*          trailing spaces be removed from the string containing the
*          header information ("str") before being passed into this
*          routine.  Also, any consecutive spaces within the header string
*          must be reduced to the occurence of one space. 
*
* ARGUMENTS:
*
*   TYPE   DATA TYPE   NAME             DESCRIPTION/UNITS
*   Input  FILE *      log_file         The name of the log file to which
*                                       error messages are written.  This may
*                                       be a user-defined file, "stdout" or
*                                       "stderr" for standard output and 
*                                       standard error output respectively. 
*   Input  FILE *      infile           The name of the geoarea text file.
*   Input  char *      str              The string containing the header
*                                       portion of the geoarea text block.
*                                       Must contain at least MAX_RECORD_LEN
*                                       characters.
*   Input  int         linenum          The line number of the geodata text
*                                       file which is currently being
*                                       processed.
*   Output double *    intlat           Contains the latitude of the centroid
*                                       point if it exists.
*   Output double *    intlon           Contains the longitude of the
*                                       centroid point if it exists.
*   Output  int *      save_data_block  A flag indicating if there was
*                                       an error encountered in the search
*                                       for the centroid lat/lon information.
*                                       The user is reponsible for initializing
*                                       this variable to "1" before calling
*                                       this routine.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME             HEADER FILE        DESCRIPTION
*   exit_geoutil     geoutil.h          Closes the input geoarea text file, 
*                                       the database, and returns control
*                                       to the operating system.
*   log_geomsg       geo_log.h          Logs a message to the user-spefied
*                                       log source.
*
* LOCAL DATA ELEMENTS:
*
*   DATA TYPE  NAME          DESCRIPTION
*   char *     s1            Used in pointing to string portions found
*                            using searches. 
*   char       str_test [ ]  This array contains a temporary copy of the
*                            "str" message string.
*   char *     decimal       Points to the locations of a "." in the header
*                            string.  This is assumed to be a portion
*                            of a latitude and longitude value.
*   char       msgstr [ ]    Log messages are written to this array before
*                            being sent to the log destination.
*
* DATA FILES AND/OR DATABASE:
*
* This routine requires a input file contain geoarea text data.  It also 
* requires a file or pointer to a file stream to which log messages 
* can be written.  The logfile may be explicity specified as 
* "stdout" or "stderr", or it may be a user-created and opened file.
*
* Upon the detection of an unrecoverable error, this routine will attempt
* to close any open connections to the IHFS database via a call to
* "exit_geoutil".
*
* Under normal operating circumstances, it is the responsibility of the
* calling routine to close the input file and the logfile. 
*
* ERROR HANDLING:
*
* Error handling is performed via the "save_data_block" variable.
* A value of "0" means that an error which compromises the integrity
* of the text data block for the geoarea has occurred.  A value of "1"
* means that no errors were detected. 
*
* Upon the detection of an unrecoverable error, this routine will close the
* input file, attempt to close any open connections to the IHFS database, 
* and then exit the from the program, returning control to the operating
* system.
*
* CONSTANTS / MACROS          HEADER_FILE
* MAX_RECORD_LEN              geo_header.h
*
********************************************************************************
*/

void check_for_int_latlon ( FILE * log_file ,
			    FILE * infile ,
			    char * str ,
			    int linenum ,
			    double * intlat ,
			    double * intlon ,
			    int * save_data_block )
{
   char * s1 = NULL ;
   char str_test [ MAX_RECORD_LEN ] ;
   char * decimal = NULL ;
   char msgstr [ MAX_RECORD_LEN ] ;
   
   /* make a copy of the string */
   strcpy ( str_test , str ) ;
   
   /* get the last item in the line. if nothing found then abort */
   s1 = strrchr ( str_test, ' ');

   if (s1 == NULL)
   {
      sprintf(msgstr,
	      "FATAL condition finding item in block header for line %d: %s\n",
	      linenum, str);
      log_geomsg(msgstr, log_file);
      exit_geoutil_file(infile);
   }

   s1++;
   
   
   /* now check if the item had a decimal point in it.
      if so, then assume that this line has two lat lon values a
      the end of it */
   
   decimal = strstr(s1, ".");
   
   if (decimal != NULL)
   {
      /* load the longitude value */
      s1 = strrchr(str, ' ');
      s1++;
      *intlon = atof(s1);
      
      /* verify the longitude value */
      /* if (*intlon < 0. || *intlon > 180.) */
      if (*intlon < -180. || *intlon > 180.)
      {
	 sprintf(msgstr,
		 "ERROR invalid interior lon %f in line %d: %s\n",
		 *intlon, linenum, str);
	 log_geomsg(msgstr, log_file);
	 
	 *save_data_block = 0;
      }
      
      
      /* now get the latitude value */
      s1--;  
      memset(s1, 0, 1);
      s1 = strrchr(str, ' ');
      
      if (s1 == NULL)
      {
	 sprintf(msgstr,
		 "FATAL condition finding interior lat in line %d: %s\n",
		 linenum, str);
	 log_geomsg(msgstr, log_file);
	 exit_geoutil_file(infile);
      }
      
      /* check for decimal point in lat value */
      decimal = strstr(s1, ".");
      
      
      /* get the lat value */
      s1++;
      *intlat = atof(s1);
      
      
      /* verify the longitude value */
      if (decimal == NULL || *intlat < 0. || *intlat > 90.)
      {
	 sprintf(msgstr,
		 "ERROR invalid interior %f lat in line %d: %s\n",
		 *intlat, linenum, str);
	 log_geomsg(msgstr, log_file);
	 
	 *save_data_block = 0;
      }
      
      
      /* adjust the string so additional processing can
	 be performed on the rest of the string */
      s1-- ;  
      memset(s1, 0, 1);      
   }
   
   return;
}
      
/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   parse_geo_header
* PURPOSE:
*
*
*   It is the responsibility of the routine calling this routine to close 
*   the infile and logfiles when done with them.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME            DESCRIPTION/UNITS
*   Input  char           buf [ ]         The input header record.  This must
*                                         be dimensioned to at least
*                                         MAX_RECORD_LEN characters.
*   Input  FILE *         infile          The file from which the header
*                                         record has been read.
*   Input  FILE *         log_file        The file or file stream to
*                                         to send dianostic / error
*                                         information to.
*   Input  int            linenum         The current line in "infile" which
*                                         is being processed.
*   Input  int            geotable        The name of the geo table in the
*                                         IHFS database which the text data
*                                         in "infile" represents.  This may
*                                         be GEOAREA for geoarea data or
*                                         GEOLINE for geoline data.
*   Output int *          save_data_block A flag indicating whether or not
*                                         an error was encountered while 
*                                         processing the header.  This error
*                                         may compromise the integrity of the
*                                         data.
*   Output char           id [ ]          The geoarea or geoline identifier.
*                                         This must be dimensioned to at least
*                                         LOC_ID_LEN + 1 characters.
*   Output char           name [ ]        The geoarea or geoline name.
*                                         This must be dimensioned to at least
*                                         LOC_NAME_LEN + 1 characters.
*   Output int *          npts            The number of lat/lon pairs defining
*                                         the geoarea or geoline
*   Output int *          order           The order of the geoarea of geoline.
*                                         This allows certain geographic 
*                                         features to be displayed according
*                                         to relative importance.
*   Output double *       intlat          For geoareas, this is the 
*                                         latitude of the area's centroid
*                                         point. 
*   Output double *       intlon          For geolines, this is the longitude
*                                         of the area's centroid point.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE  DESCRIPTION
*   strip_lblanks        geoutil.h    Strips off leading blanks from a string.
*   strip_tblanks        geoutil.h    Strips off trailing blanks from a string.
*   compress_blanks      geoutil.h    Removes repeated strings from within
*                                     a string. 
*   check_for_int_latlon geoheader.h  Checks if the geoarea header has 
*                                     centroid latitude / longitude
*                                     information.  Only geoarea data may
*                                     have this type of information.
*   log_geomsg           geoutil.h    Logs diagnostic and error information
*                                     to a user-created and opened file,
*                                     or to a standard file stream such
*                                     as "stdout" or "stderr".
*   exit_geoutil         geoutil.h    When an unrecoverable error is 
*                                     encountered, this routine is called.  It
*                                     closes the input file and attempts
*                                     to close any open connections to the
*                                     IHFS database.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME           DESCRIPTION
*   char       msgstr [ ]     Dianostic / Error messages are written to this
*                             array before being passed on to the 
*                             log file or file stream.
*   char *     s1             Used for parsing the header into manageable,
*                             recognizeable parts.
*   char       str [ ]        A copy of the header string is placed into
*                             here so that it may be worked on without
*                             affecting the original copy.
*   int        namelen_limit  The limit of the length of the name depends
*                             on whether geoarea or geoline data are being
*                             processed.
*   int        slen           Used to keep track of the lengths of the
*                             individual substrings parsed from the header 
*                             string.
*
* DATA FILES AND/OR DATABASE:
*
*   This routine requires an input file (which has been opened) containing
*   the text data that represents the geoarea or geoline information.  The
*   format of this data is explained above, in the purpose section.
*
*   This routine requires a log file. This may be a file created and opened
*   by the user, or it may be a pointer to a file stream such as "stdout" or
*   "stderr".
*
*   While this routine does not require a database connection, when a fatal
*   error is encountered, it does attempt to close any open connections to
*   the IHFS database.
*
*   It is the responsibility of the calling routine to close the input and log
*   files under normal operating circumstances.
*
* ERROR HANDLING:
*
*   Errors are reported through the "save_data_block" flag.  A value of "0"
*   means that an error which could compromise the data in the text block 
*   defining the geoarea or geoline definition.
*
*   Unrecoverable errors cause this routine to log a message and terminate
*   the program.  In this process, the "infile" is closed and any open
*   connections to the IHFS database are closed as well.
*
* CONSTANTS / MACROS     HEADER_FILE
*  GEOAREA               geo_header.h 
*  LOC_AREANAME_LEN      DbmsDefs.h
*  LOC_ID_LEN            DbmsDefs.h
*  LOC_NAME_LEN          DbmsDefs.h
*  MAX_PTS               geo_header.h
*  UNASSIGNED            geo_header.h
*
********************************************************************************
*/

void parse_geo_header ( char buf [ MAX_RECORD_LEN ] ,
                        FILE * infile ,
                        FILE * log_file ,
                        int linenum ,
                        int geotable ,
                        int * save_data_block ,
                        char  id [ LOC_ID_LEN + 1 ] ,
                        char name [ LOC_NAME_LEN + 1 ] ,
                        int * npts ,
                        int * order ,
                        double * intlat , 
                        double * intlon )
{

   char msgstr [ MAX_RECORD_LEN ] ;
   char * s1 = NULL ;
   char str [ MAX_RECORD_LEN ] ;
   int namelen_limit ;
   int slen ;

   /* extract each of the attributes from the header block for
      the subsequent set of points */
      
   /* allow the line to have the interior lat-lon at the end. */
   strcpy ( str , buf ) ;
  
   /* Remove any excess whitespace. */
   strip_lblanks(str);
   strip_tblanks(str);
   compress_blanks(str);

   *intlat = *intlon = UNASSIGNED;
   check_for_int_latlon(log_file, infile, str, linenum, 
	                intlat, intlon, save_data_block);
      
   /* first get the number of lat-lon pairs that follow, from the end
      of the line */
   s1 = strrchr(str, ' ');

   if (s1 == NULL)
   {
      sprintf(msgstr, "FATAL condition finding number of pts in line %d: %s\n",
		 linenum, buf);
      log_geomsg(msgstr, log_file);
      exit_geoutil_file(infile);
   }
      
   s1++; 
   *npts = atoi(s1);
      
   if (*npts < 0)
   {
      sprintf(msgstr, "FATAL condition reading number of pts in line %d: %s\n",
	              linenum, buf);
      log_geomsg(msgstr, log_file);
      exit_geoutil_file(infile);
   }
   else if (*npts > MAX_PTS)
   {
      sprintf(msgstr,
	      "FATAL condition exceeded max number of points in line %d\n", 
	      linenum);
      log_geomsg(msgstr, log_file);
      exit_geoutil_file(infile);
   }
           
      
   /* get the stream order, which is not always specified, from
      the field preceding the num of lat-lon pairs */
      
   s1--;  
   memset(s1, 0, 1);
   s1 = strrchr(str, ' ');
      
   if (s1 == NULL)
   {
      sprintf(msgstr, "WARNING: Error finding stream order in line %d: %s\n",
              linenum, buf);
      log_geomsg(msgstr, log_file);
      *order = -1;
   }

   s1++;
   *order = atoi(s1);
      
   if (*order < -1 || *order > 50)
   {
      sprintf(msgstr, "WARNING: Error reading stream order in line %d: %s\n",
	      linenum, buf);
      log_geomsg(msgstr, log_file);
   }
    
   /* force the string to be terminated just before the order value
      so that the subsequent reads can be performed */
      
   s1--;
   memset(s1, 0, 1);
      
   /* now get the identifier at the beginning of the string */
   s1  = strtok(str, " \t");
   slen = strlen(s1);
      
   if (slen <= 0 || slen > LOC_ID_LEN)
   {
      if (geotable == GEOAREA)
      {
         sprintf(msgstr,
	         "ERROR reading id (between 1-%d chars) in line %d: %s\n",
		  LOC_ID_LEN, linenum, buf);
	          log_geomsg(msgstr, log_file);
	    
	 strcpy(id, "N/A");
	 save_data_block = 0;
      }
      else
      {
         sprintf(msgstr,
	         "WARNING: invalid id (between 1-%d chars) in line %d: %s\n",
		 LOC_ID_LEN, linenum, buf);
	 log_geomsg(msgstr, log_file);
	 strcpy(id, "UNKNOWN");
      }
   }
   else
      strcpy(id, s1);
      
      
   /* get the identifying name */
   s1 = &str[slen + 1];
   slen = strlen(s1);

   if (geotable == GEOAREA)
      namelen_limit = LOC_AREANAME_LEN;
   else
      namelen_limit = LOC_NAME_LEN;
      
      
   if (slen > namelen_limit)
   {
      sprintf(msgstr,
	      "WARNING: truncated name (use 1-%d chars) in line %d: %s\n",
	      namelen_limit, linenum, buf);
      log_geomsg(msgstr, log_file);
	 
      memset(name, 0, LOC_AREANAME_LEN + 1);
      strncpy(name, s1, LOC_AREANAME_LEN);
   }
   else if (slen <= 0)
   {
      sprintf(msgstr,
	      "WARNING: invalid name (use 1-%d chars) in line %d: %s\n",
	       namelen_limit, linenum, buf);
      log_geomsg(msgstr, log_file);
	 
      strcpy(name, "UNDEFINED");
   }
   else
      strcpy(name, s1);
}

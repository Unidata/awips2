/********************************************************************************
* FILENAME:		process_gage_pp.c
* NUMBER OF MODULES:         1
* GENERAL INFORMATION:
*   MODULE 1:	        process_gage_pp	
* DESCRIPTION:		This is the subroutine to process data from the 
*	                flat file created by ShefDecode.
*
*			Calling subroutine: gage_pp_main program.
*
* ORIGINAL AUTHOR:	Moria Shebsovich
* CREATION DATE:	July 19, 2004
* ORGANIZATION:		HSEB / OHD
* MACHINE:		HP9000 / Linux
* MODIFICATION HISTORY:
*   MODULE #	    DATE	 PROGRAMMER	   DESCRIPTION/REASON
*          1        7/19/04      Moria Shebsovich  Original Coding
*          1        8/19/04      Bryon Lawrence    Modified/Documented.
*          1        9/17/04      Bryon Lawrence    Added include of 
*                                                  gage_pp_write_rec.h for
*                                                  definition of
*                                                  GagePPoptions struct
*                                                  and GPP_OK, GPP_ERROR,
*                                                  and GPP_STOP constants.
*********************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gage_pp_check_input.h"
#include "gage_pp_check_stop.h"
#include "gage_pp_main.h"
#include "gage_pp_log.h"
#include "gage_pp_rm_file.h"
#include "gage_pp_process_file.h"
#include "gage_pp_write_rec.h"
#include "process_gage_pp.h"

/*******************************************************************************
* MODULE NUMBER:   1
* MODULE NAME:     process_gage_pp
* PURPOSE:         Checks for the existence of load files containing top
*                  of the hour PC and PP data in the gage_pp_input 
*                  directory. If one or more files exists, each file is
*                  processed one at a time by calling routine gage_pp_process
*                  file.
*
* ARGUMENTS:
*   TYPE   DATA TYPE        NAME         DESCRIPTION/UNITS
*   Input  char *           datadir      The directory in which the load files
*                                        are written by ShefDecoder.
*   Input  char *           stopfilename The name of the stopfile to search
*                                        for.  This will also be in the 
*                                        data directory.
*   Input  GagePPoptions *  pOptions     Contains the options which control
*                                        how gage_pp processes precip reports.
*
* RETURNS:
*   DATA TYPE              DESCRIPTION
*   int                    Return code.  This is either GPP_OK or GPP_ERROR.  
*
* APIs UTILIZED:
*   NAME                  HEADER FILE             DESCRIPTION
*   gage_pp_check_input   gage_pp_check_input.h   Checks the gage_pp data 
*                                                 directory for input files.
*   gage_pp_check_stop    gage_pp_check_stop.h    Checks the gage_pp data
*                                                 directory for a stop file.
*   gage_pp_process_file  gage_pp_process_file.h  Processes a load file.  It
*                                                 stores the data into the
*                                                 appropriate slotted table.
*   gage_pp_rm_file       gage_pp_rm_file.h       Removes the dynamic memory
*                                                 used to store the name of
*                                                 a load file.
*   writelog              gage_pp_log.h           Writes to the gage_pp log 
*                                                 file.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME             DESCRIPTION
*   char [ ]   filelist         Gage PP returns all of the files available
*                               to be processed in this array. 
*   char *     infile           Contains the current PC/PP data file
*                               being processed.
*   char *     pStopfilename    The name of the stopfile.  Gage_pp periodically
*                               checks for this file to see if it should
*                               shutdown.
*   char [ ]   message          Log messages are written to this array and
*                               then written to the gage_pp log file.
*   int        i                A loop index.
*   int        malloc_length    The number of bytes to dynamically allocate
*                               for a variable.
*   int        numfiles         The number of load files than need to be
*                               processed.  Their names are contained in the
*                               filelist array.
*   int        status           Contains the resulting status code of
*                               calls to various subroutines.
*
* DATA FILES AND/OR DATABASE:
* The IHFS database must be opened prior to calling this routine. The
* ShefDecoder needs to create top-of-hour PC/PP data files in the gage_pp
* data directory.
*
* ERROR HANDLING:
*    ERROR CODE                        DESCRIPTION
*    GPP_OK                            This routine worked.
*    GPP_ERROR                         This routine encountered an error.
********************************************************************************
*/
int process_gage_pp ( const char * datadir , const char * stopfilename ,
	              const GagePPoptions * pOptions )
{
    char * filelist [ MAXFILES ] ;
    char * infile = NULL ;
    const char * pStopfilename = NULL ;
    char message [ MAX_LOG_LENGTH ] ;
    int i ;
    int malloc_length ;
    int numfiles ;
    int status = GPP_OK ;
 
    /* The stopfilename variable has the complete path and filename of the
       the stopfile.  The gage_pp_check_input routine only needs the filename.
       Get the filename. */ 
    pStopfilename = strrchr ( stopfilename , '/' ) ; 

    if ( pStopfilename == NULL )
    {
       pStopfilename = stopfilename ;
    }
    else
    {
       ++ pStopfilename ;
    }

    /* Scan input data directory and see if any files exist, and build a
       list of the files; the output and stop file names are passed in
       to prevent those from being included in the list */
    status = gage_pp_check_input ( datadir , pStopfilename , filelist , 
		                   MAXFILES , & numfiles ) ;

    if ( status == GPP_ERROR )
    {
       return GPP_ERROR ;
    }
 
    /* Loop on the number of files found */
   for ( i = 0 ; i < numfiles ; ++ i )
   {
      /* Build the full input file name;
         need to free malloc'd memory later (i.e. infile) */
      malloc_length = ( strlen ( datadir ) + 1 + 
                        strlen ( filelist [ i ] ) + 1  ) * sizeof ( char ) ;
      infile = ( char * ) malloc ( malloc_length ) ;

      if ( infile == NULL )
      {
         sprintf ( message , "In routine 'process_gage_pp':\n"
                             "Could not allocate %d bytes of memory to"
                             "build path for %s." , malloc_length ,
                             filelist [ i ] ) ;
         writelog ( message ) ;
         return GPP_ERROR ;
      }
      else 
      {
         sprintf ( infile , "%s/%s" , datadir , filelist [ i ] ) ;  	
         status = gage_pp_process_file ( infile , pOptions ) ;

         /* Remove the file. */
         gage_pp_rm_file ( infile ) ;

         free ( infile ) ;
         infile = NULL ;

	 if ( status != GPP_OK )
         {
            /* An error was encountered processing the file. */
            return GPP_ERROR ;
         }

      }

      /* Check for the stopfile after each file processed. */
      status = gage_pp_check_stop ( stopfilename ) ;

      if ( status == GPP_STOP ) break ;
   }

   /* Free the memory used to build the array of filenames. */
   for ( i = 0 ; i < numfiles ; ++ i )
   {
      if ( filelist [ i ] != NULL )
      {
         free ( filelist [ i ] ) ;
         filelist [ i ] = NULL ;
      }
   }

   return status ;
}

/*******************************************************************************
* FILENAME:            ReadSPE.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          ReadSPE
* DESCRIPTION:         Reads a hourly NetCDF Satellite Precipitation
*                      Estimate (SPE) product and returns data to the caller
*                      of this routine.
*
* ORIGINAL AUTHOR:     Paul Tilles
* CREATION DATE:       Unknown
* ORGANIZATION:        OHD 11 - HSEB
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        3/22/2004    Bryon Lawrence    Modified to take
*                                                  arguments by reference so
*                                                  that this routine can be
*                                                  called from Fortran.
*                                                  Updated Documentation.
*          2        4/15/2008    Bryon Lawrence    Fixed a problem that was offsetting
*                                                  the satellite precipitation estimates
*                                                  one HRAP grid cell to the west.
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ffconv.h"
#include "mpe_log_utils.h"
#include "netcdf.h"
#include "ReadSPE.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   ReadSPE
* PURPOSE:
*          This function opens and reads the NetCDF file containing
*          gridded satellite precip estimates (SPE) for CONUS. It computes
*          a subsection of this product which matches up with a site's MPE
*          forecast area.  This is done using the xor, yor, xsize, and ysize
*          variables supplied by the caller of this routine.
*
*          The SPE product provides a one hour satellite-based precipitation
*          estimate.  It is returned to the user as an array of short integers.
*
* ARGUMENTS:
*
*   TYPE   DATA TYPE  NAME       DESCRIPTION/UNITS
* 
*   Input  char *     filename   Full pathname (without "00.multi") of the netCDF file containing
*                                the SPE CONUS grid   
*   Input  int *      xor        The originating national grid column of the
*                                site's MPE forecast grid.
*   Input  int *      yor        The originating national grid row of the
*                                site's MPE forecast grid
*   Input  int *      site_xsize The number of HRAP columns in the site's
*                                forecast grid.
*   Input  int *      site_ysize The number of HRAP rows in the site's
*                                forecast grid.
*   I/O    short *    site_spe   A user-supplied array into which the SPE
*                                data read from NetCDF are stored.  This
*                                array should be dimensioned to contain
*                                site_xsize * site_ysize elements.
*   O      short *    x_factor   This is the xmrg factor.  It is used in
*                                converting satellite precipitation estimates
*                                to a form displayable by Hydroview/MPE.
*   0      short *    u_thres    The upper limit on a satellite precipitation
*                                estimate value.  Any values at or above this
*                                threshold are considered to be missing.
*   O      float *    mm_factor  The multiplicative factor used in converting
*                                SPEs.
*   I/O    int *      spe_status Indicates if errors were encountered while
*                                retrieving the SPE data.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME             HEADER FILE    DESCRIPTION
*   ffconv           ffconv.h       Converts unsigned char to short value
*                                   according to netcdf file formula
*   nc_close         netcdf.h       Close a NetCDF file.
*   ncdimid          netcdf.h       Create a NetCDF variable.
*   ncdiminq         netcdf.h       Read a value into a NetCDF variable.
*   nc_get_var_uchar netcdf.h       Retrieve the NetCDF SPE data into the
*                                   SPE array.
*   nc_open          netcdf.h       Open a NetCDF file.
*   ncvarid          netcdf.h       Contains the identifier of a NetCDF
*                                   variable.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME            DESCRIPTION
*   char []         netcdf_fname    The name of the NetCDF file to open.
*   unsigned char * conus_spe_grid  gridded spe array read from netCDF file
*                                   for CONUS.
*   int             arrayid         The NetCDF id of the SPE data in the
*                                   NetCDF file.
*   int             conus_xsize     The number of HRAP columns in the
*                                   National HRAP grid.
*   int             conus_ysize     The number HRAP rows in the National
*                                   HRAP grid.
*   int             i               A loop index variable.
*   int             indconus        Used to index the conus_spe_grid when
*                                   copying and converting it contents for
*                                   storage in the site_spe array.
*   int             ix              A loop index.
*   int             iy              A loop index.
*   int             ncid            The NetCDF identifier associated with
*                                   an open NetCDF filename.
*   int             status          The status of a NetCDF function call.
*   int             xdim            Used to retrieve the conus_xsize from
*                                   the NetCDF file.
*   int             ydim            Used to retrieve the conus_ysize from
*                                   the NetCDF file.
*   int             yoffset         The offset of the origin of the site's
*                                   MPE grid from the national grid.
*   long            xs              The same as conus_xsize.
*   long            ys              The same as conus_ysize.
*
* DATA FILES AND/OR DATABASE:
*
* Requires the NetCDF SPE file.  This name of this file is provided by the
* caller of this routine.  For AWIPS build OB4 it has the following
* format:
* /data/fxa/img/SBN/netCDF/HRAP/SPE/AE/CONUS/YYYYMMDD_HHMM.multi
* where YYYY is the year, MM is the month, DD is the day, HH is the hour,
* and MM is the minute.
*
* ERROR HANDLING: (As reported by the spe_status parameter)
*
*    ERROR CODE      DESCRIPTION
*    0               This routine functioned correctly.
*    1               This routine encountered an error. A message indicating
*                    the problem will be sent to the standard error stream.
*
********************************************************************************
*/
void ReadSPE ( const char * filename , const int * xor , const int * yor ,
               const int * site_xsize , const int * site_ysize ,
               short int * site_spe , short int * x_factor ,
               short int * u_thres , float * mm_factor , int * spe_status )
{

   char netcdf_fname [ 256 ] ;
   unsigned char * conus_spe_grid = NULL ;
   int arrayid ;
   int conus_xsize ;
   int conus_ysize ;
   int i ;
   int indconus ;
   int indsite ;
   int ix ;
   int iy ;
   int ncid ;
   int status ;
   int xdim ;
   int xpos;
   int ydim ;
   int ypos;
   int yoffset ;
   long xs = 0 ;
   long ys = 0 ;

   /* Initialize the site status to 1. */
   * spe_status = 1 ;

   /* Initialize the xmrg_factor, mmfactor, and upper_thres parameters.
      These will be returned as is to the calling routine. */
   * x_factor = xmrg_factor ;
   * mm_factor = mmfactor ;
   * u_thres = upper_thres ;

   /* Check to make sure the site_spe pointer is not NULL. */
   if ( site_spe == NULL )
   {
      flogMessage ( stdout,  "\nIn routine 'ReadSPE':\n"
                         "The user must supply a one dimensional array of\n"
                         "short int containing site_ysize * site_xsize\n"
                         "elements.\n" ) ;
      return ;
   }

   /* Initialize the site_spe array to -999. */
   for ( i = 0 ; i < ( ( *site_ysize ) * ( *site_xsize ) ) ; ++ i )
   {
      site_spe [ i ] = -999 ;
   }

   /*--------------------------------*/
   /*  open netcdf file for reading  */
   /*--------------------------------*/
   sprintf(netcdf_fname, "%s00.multi", filename);

   /*strcpy(netcdf_fname,filename);*/
/*
   flogMessage( stdout , "\nIn routine 'ReadSPE':\n"
                     "Opening file = %s\n" , netcdf_fname ) ;
*/

   status = nc_open(netcdf_fname,NC_NOWRITE,&ncid);

   if(status != NC_NOERR)
   {
      flogMessage( stderr , "\nIn routine 'ReadSPE':\n"
    		              "Cannot open top-of-hour SPE netcdf file.\n");
      sprintf(netcdf_fname, "%s15.multi", filename);
      status = nc_open(netcdf_fname,NC_NOWRITE,&ncid);
      if(status != NC_NOERR)
      {
        flogMessage( stderr , "\nIn routine 'ReadSPE':\n"
                        "Cannot open HH15 SPE netcdf file.\n");
        return;
      }
      else
      {
        flogMessage( stdout , "\nIn routine 'ReadSPE':\n"
                        "Opened HH15 SPE netcdf file.\n");

      }


   }

   /*--------------------------------*/
   /*  get dimensions of CONUS area  */
   /*--------------------------------*/

   xdim = ncdimid(ncid, "x");
   ydim = ncdimid(ncid, "y");

   if(xdim == -1 || ydim ==  -1)
   {
      flogMessage( stdout , "\nIn routine 'ReadSPE':\n"
    		               "Error reading header portion of SPE netCDF file.\n");

      return;
   }

   ncdiminq(ncid, xdim, (char *)NULL, &xs);
   ncdiminq(ncid, ydim, (char *)NULL, &ys);

   conus_xsize = xs;
   conus_ysize = ys;

   if(conus_xsize <= 0 || conus_ysize <= 0)
   {
      flogMessage( stdout , "\nIn routine 'ReadSPE':\n"
                        "Error reading header portion of SPE netCDF file.\n");
      return;
   }

   /*-------------------------------------------*/
   /*  malloc space for the SPE gridded array   */
   /*-------------------------------------------*/

   conus_spe_grid = (unsigned char *) malloc ( ( conus_ysize * conus_xsize ) *
                                                 sizeof ( unsigned char ) ) ;

   if ( !conus_spe_grid )
   {
      flogMessage ( stdout , "\nIn routine 'ReadSPE':\n"
                         "Malloc of space for conus_spe_grid array\n"
                         "failed.\n");
      return ;
   }

   /*---------------------------------------------------------------*/
   /*  read the gridded SPE array from the netCDF file              */
   /*  data values are unsigned chars quantized from 0 - 253        */
   /*---------------------------------------------------------------*/

   arrayid = ncvarid(ncid, "image");
   status = nc_get_var_uchar(ncid,
                             arrayid,
                             conus_spe_grid);
   if ( status != NC_NOERR )
   {
      flogMessage( stdout , "\nIn routine 'ReadSPE':\n"
                        "Error reading SPE netcdf file."
                        "%s\n" , nc_strerror ( status ) ) ;
      return;
   }

   /*------------------------------------------------------------*/
   /*  transform gridded SPE values from unsigned char to short int  */
   /*    and store in site_spe array                             */
   /*------------------------------------------------------------*/

   yoffset = conus_ysize - ( *yor );
   indsite = 0;

   for(iy=0; iy<(*site_ysize); iy++)
   {
      for(ix=0; ix<(*site_xsize); ix++)
      {
         xpos = ix + *xor;
         ypos = iy + *yor;

         indconus = conus_xsize*(yoffset - iy) + xpos - 1;

         if ( ( ( ypos >= 1 ) && ( ypos <= conus_ysize ) ) &&
             ( ( xpos >= 1 ) && ( xpos <= conus_xsize) ) )
         {
            site_spe[indsite] = MPEUtil_ffconv(conus_spe_grid[indconus]);
         }
         else
         {
            site_spe [ indsite ] = -999;
         }

         indsite++;
      }
   }

   /*----------------------------------*/
   /*  free array space for CONUS array  */
   /*----------------------------------*/

   free ( conus_spe_grid ) ;
   conus_spe_grid = NULL ;

   /*--------------------------------*/
   /*  close the netcdf file         */
   /*--------------------------------*/

   status = nc_close ( ncid ) ;

   if ( status != NC_NOERR )
   {
      flogMessage ( stdout , "\nIn routine 'ReadSPE':\n"
                         "Error closing SPE netcdf file\n");
      return;
   }

   * spe_status = 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc_lib/src/MPEUtil/RCS/ReadSPE.c,v $";
 static char rcs_id2[] = "$Id: ReadSPE.c,v 1.5 2009/07/27 17:03:15 pst Exp $";}
/*  ===================================================  */

}

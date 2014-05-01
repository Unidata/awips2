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
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "empe_fieldgen.h"
#include "ffconv.h"
#include "netcdf.h"

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
*   Input  char *     filename   Full pathname of the netCDF file containing
*                                the SPE CONUS grid.  
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
void read_spe ( const char * satpre_filename , 
                const geo_data_struct * pGeoData,
                const int hrap_grid_factor,
                double ** pSatPre, 
                int * spe_status )
{

   unsigned char * conus_spe_grid = NULL ; 
   int arrayid ;
   int conus_xsize ; 
   int conus_ysize ; 
   int indconus ; 
   int ix ;
   int iy ; 
   int ncid ; 
   int short_value;
   int status ; 
   int xdim ; 
   int ydim ;
   int yoffset ;
   long xs = 0 ; 
   long ys = 0 ;
   
   
   /*
    * change the current scaled hrap(such as Quarter HRAP) grid
    * to HRAP grid.
    * added by gzhou 03-2007 
    * 
    */

   const int hrap_x = pGeoData->hrap_x / hrap_grid_factor;
   const int hrap_y = pGeoData->hrap_y / hrap_grid_factor;
   const int row_size = pGeoData->num_rows / hrap_grid_factor;
   const int col_size = pGeoData->num_cols / hrap_grid_factor;

   /* Initialize the site status to 1. */
   * spe_status = 1 ;

   /* Check to make sure the site_spe pointer is not NULL. */
   if ( pSatPre == NULL )
   {
      fprintf ( stdout,  "\nIn routine 'ReadSPE':\n"
                         "The user must supply a two dimensional array of\n"
                         "doubles containing site_ysize * site_xsize\n"
                         "elements.\n" ) ;
      return ;
   }

   /*--------------------------------*/
   /*  open netcdf file for reading  */
   /*--------------------------------*/

   status = nc_open ( satpre_filename,NC_NOWRITE,&ncid );

   if(status != NC_NOERR)
   {
      fprintf( stderr , "\nIn routine 'ReadSPE':\n"
                        "Error opening SPE netcdf file.\n");
      return;
   }

   /*--------------------------------*/
   /*  get dimensions of CONUS area  */
   /*--------------------------------*/

   xdim = ncdimid(ncid, "x");
   ydim = ncdimid(ncid, "y");

   if(xdim == -1 || ydim ==  -1)
   {
      fprintf( stdout , "\nIn routine 'ReadSPE':\n"
                        "Error reading header portion of SPE netCDF file.\n");
      return;
   }

   ncdiminq(ncid, xdim, (char *)NULL, &xs);
   ncdiminq(ncid, ydim, (char *)NULL, &ys);

   conus_xsize = xs;
   conus_ysize = ys;

   if(conus_xsize <= 0 || conus_ysize <= 0)
   {
      fprintf( stdout , "\nIn routine 'ReadSPE':\n"
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
      fprintf ( stdout , "\nIn routine 'ReadSPE':\n"
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
      fprintf( stdout , "\nIn routine 'ReadSPE':\n"
                        "Error reading SPE netcdf file."
                        "%s\n" , nc_strerror ( status ) ) ;
      return;
   }

   /*------------------------------------------------------------*/
   /*  transform gridded SPE values from unsigned char to short int  */
   /*    and store in site_spe array                             */
   /*------------------------------------------------------------*/

   yoffset = conus_ysize - hrap_y ;

   for(iy = 0; iy < row_size; iy++)
   {
      for(ix = 0; ix < col_size; ix++)
      {
         indconus = conus_xsize * (yoffset - iy) + (ix + hrap_x);

         if((hrap_y + iy) <= conus_ysize && 
            (hrap_x + ix) <= conus_xsize)
         {
            short_value = ffconv(conus_spe_grid[indconus]);
            pSatPre[iy][ix] = ( double ) short_value/(double)100.0;
         }
         else
         {
            pSatPre[iy][ix] = -999;
         }

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
      fprintf ( stdout , "\nIn routine 'ReadSPE':\n"
                         "Error closing SPE netcdf file\n");
      return;
   }

   * spe_status = 0;

}

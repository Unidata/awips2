#include <stdio.h>
#include <stdlib.h>

#include "ffconv.h"
#include "gen_areal_qpe.h"
#include "netcdf.h"

/*****************************************************************************
PURPPOSE
   Read the netCDF file containing the RFC QPE grid.  Crop the portion of it
   that lies within the WFO's MPE forecast area.  Store this portion in the
   WFO RFC QPE mosaic array.
******************************************************************************/
int process_rfc_qpe_grid ( const gaq_options_struct * options,
                           int rfc_mask[][RFC_MASK_COLS],
                           const geo_data_struct * coord_info,
                           const qpe_info_struct * qpe_info,
                           float ** pXmrgGrid )
{
   char netcdf_path [ MAX_PATH_LEN + 1 ];

   float * plat00 = NULL;
   float * plon00 = NULL;
   float lat00;
   float lon00;
   float * rfc_qpe_float = NULL;
   float xhrap;
   float yhrap;
   int arrayid;
   int iflg=1;
   int istat;
   int l_len;
   int mask_col;
   int mask_row;
   
   
   int ncid;
   int status;
   int xoffset, yoffset, yloc, xloc, yoff, indrfc;
   int xrl, yrl;
   int xdim, ydim, ixh = 0, iyh = 0, ix, iy, xru, yru; 
   int rfc_size;

   int xsize;
   int ysize;

   long xs = 0;
   long ys = 0;

   nc_type l_type;

   short mask_value;


   /* Build the path to the netCDF file. */
   memset ( netcdf_path, '\0', MAX_PATH_LEN + 1 );
   
   sprintf ( netcdf_path,  "%s/%s", options->input_data_path, 
                                    qpe_info->name );

   /* Open the netCDF file. */
   status = nc_open ( netcdf_path, NC_NOWRITE, &ncid );

   if ( status != NC_NOERR )
   {
      printf ( "error opening netcdf file %s.\n", netcdf_path );
      return FAIL;
   }

   /* Get the dimensions of the RFC area. */
   xdim = ncdimid ( ncid, "x" );
   ydim = ncdimid ( ncid, "y" );

   if ( xdim == -1 || ydim == -1 )
   {
      printf ( "error in ncdimid while processing file %s.\n",
               netcdf_path ); 
      nc_close ( ncid );
      return FAIL;
   }

   ncdiminq ( ncid, xdim, ( char * ) NULL, &xs );
   ncdiminq ( ncid, ydim, ( char * ) NULL, &ys );

   xsize = ( int ) xs;
   ysize = ( int ) ys; 

   if ( xsize <= 0 || ysize <= 0 )
   {
      printf ( "error in ncdiminq while processing file %s.\n",
               netcdf_path );
      nc_close ( ncid );
      return FAIL;
   }

   printf ( "For file %s:  xsize = %d ysize = %d\n",
            netcdf_path, xsize, ysize );
   rfc_size = xsize * ysize;

   /* Retrieve the latitude and longitude of the upper left corner of 
      the RFC area. */
   ncattinq(ncid, NC_GLOBAL, "lat00", &l_type, &l_len);
   ncattinq(ncid, NC_GLOBAL, "lon00", &l_type, &l_len);

   plat00 = (float *) malloc ( l_len * nctypelen ( l_type ) );
   plon00 = (float * ) malloc ( l_len * nctypelen ( l_type ) );

   ncattget ( ncid, NC_GLOBAL, "lat00", plat00 );
   ncattget ( ncid, NC_GLOBAL, "lon00", plon00 );

   lat00 = *plat00;
   lon00 = *plon00;
   lon00 = (-1) * lon00;

   printf ( "lat00 = %f  lon00 = %f\n", lat00, lon00 );

   LLGD ( &lon00, &lat00, &iflg, &xhrap, &yhrap, &iflg, &istat );

   if ( istat != 0 )
   {
      printf ( "error attempting to transform lat/lon to hrap - "
               "error code = %d\n", istat );
      nc_close ( ncid );
      return FAIL;
   }

   ixh = xhrap;
   iyh = yhrap;

   printf ( "upper left (NW) corner of RFC area: x = %d y = %d\n", ixh,
            iyh );

   /* Calculate the HRAP coord of upper right (NE) corner of RFC area
      (xru,yru) = HRAP coord of upper right (NE) corner of RFC area. */
   xru = ixh + xsize;
   yru = iyh;

   printf ( "upper right (NE) corner of RFC area: x = %d y = %d\n", xru,
            yru );

   /* Calculate the HRAP coord of lower left (SW) corner of RFC area
      (xrl, yrl) = HRAP coord of lower left (SW) corner of RFC area. */
   xrl = ixh;
   yrl = iyh - ysize;

   /* Allocate space for the array to hold the data read from
      the RFC QPE netcdf file. */
   rfc_qpe_float = ( float * ) malloc ( rfc_size * sizeof ( float ) );

   if ( rfc_qpe_float == NULL )
   {
      printf ( "Could not allocate memory for rfc_qpe_float array.\n" );
      nc_close ( ncid );
      return FAIL;
   }

   /* Need to get the variable name here.  This depends on the duration
      of the precipitation product being processed. */
   if ( qpe_info->dur == 1 )
   {
      arrayid = ncvarid ( ncid, GAQ_NETCDF_QPE1 );
   }
   else if ( qpe_info->dur == 6 )
   {
      arrayid = ncvarid ( ncid, GAQ_NETCDF_QPE6 );
   }
   else
   {
      arrayid = ncvarid ( ncid, GAQ_NETCDF_QPE24 );
   }

   status = nc_get_var_float ( ncid, arrayid, rfc_qpe_float );

   if ( status != NC_NOERR )
   {
      printf ( "Error reading array from file %s.\n", netcdf_path );
      free ( rfc_qpe_float );
      rfc_qpe_float = NULL;
      nc_close ( ncid );
      return FAIL;
   }

   /* Transform the gridded QPE values from unsigned char to float.
      Then map gridded QPE array elements into mosaic array. */

   /*-----------------------------------------------------------------*/
   /*  xoffset < 0 occurs when western edge of site's area is west    */
   /*     of western edge of RFC area                                 */
   /*  for example, site = PBZ and RFC = MARFC                        */
   /*                                                                 */
   /*  yoffset < 0 occurs when northern edge of site's area is north  */
   /*     of northern edge of RFC area                                */
   /*  for example, site = ICT and RFC = ABRFC                        */
   /*-----------------------------------------------------------------*/
   xoffset = coord_info->hrap_x - xrl;
   yoffset = yru - ( coord_info->hrap_y + coord_info->num_rows );

   for(iy=0; iy<coord_info->num_rows; iy++)
   {
      yloc = (coord_info->hrap_y + coord_info->num_rows) - iy;
      yoff = ysize - yoffset - iy - 1;

      for(ix=0; ix<coord_info->num_cols; ix++)
      {
         indrfc = (xsize * yoff) + (ix + xoffset);

         if(yoff >= 0 && (yloc <= yru && yloc > yrl))
         {

            xloc = coord_info->hrap_x + ix;

            if(xloc <= xru && xloc >= xrl)
            {
               indrfc = (xsize * yoff) + (ix + xoffset);

               if(indrfc > rfc_size)
               {
                  printf("array index (%d) exceeds rfc size (%d)\n",
                           indrfc, rfc_size);
                  printf("ix = %d  iy = %d  \n",ix,iy);
                  printf("yoff = %d  \n",yoff);
                  printf("xoffset = %d  yoffset = %d  \n",xoffset,yoffset);
                  nc_close (ncid );
                  free ( rfc_qpe_float );
                  rfc_qpe_float = NULL;
                  return FAIL;
               }

              /*-------------------------------------------------------------*/
              /*  need additional check for non-missing data because of      */
              /*  overlapping areas and use of bit-map to set all bins       */
              /*  outside of area to missing 0,0 point in mosaic_ffg_float   */
              /*  array corresponds to SW corner of grid 0 element of        */
              /*  mosaic_ffg_uchar array corresponds to NW corner of grid    */
              /*-------------------------------------------------------------*/

               if ( rfc_qpe_float [indrfc] < FILL_VALUE_FLOAT)
               {
                  /* Need to determine if the site is OCONUS or if this part
                     of the forecast area extends beyond the bounds of the
                     rfc mask array. If we are outside of the bounds of the
                     rfc mask array, just use RFC data as is. */
                  mask_row = coord_info->num_rows - iy - 1 + coord_info->hrap_y;
                  mask_col = ix + coord_info->hrap_x;

                  /* Adjust to read correct bin in rfc mask array. */
                  --mask_row;
                  --mask_col;

                  if ( ( mask_row >= 0 ) && ( mask_row < RFC_MASK_ROWS ) &&
                       ( mask_col >= 0 ) && ( mask_col < RFC_MASK_COLS ) )
                  {
                     /* Only fill the value in the XMRG grid if the grid bin
                        "belongs" to RFC currently being processed or if the
                        value in the NPVU mask is missing for the grid bin. */
                     mask_value = rfc_mask [ mask_row ] [ mask_col ];
                  }
                  else
                  {
                     mask_value = RFC_MASK_MISSING_VALUE;
                  }
                     
                  if ( ( mask_value == qpe_info->mask_value ) ||
                       ( mask_value == RFC_MASK_MISSING_VALUE ) )
                  {
                     pXmrgGrid[coord_info->num_rows - iy - 1][ix] = 
                                                 rfc_qpe_float[indrfc];
                  }
               }

            }

         }

      }
   }

   /* Deallocate the rfc array. */
   free ( rfc_qpe_float );
   rfc_qpe_float = NULL;

   /* Close the netCDF file. */
   nc_close ( ncid );

   if ( plat00 != NULL )
   {
      free ( plat00 );
      plat00 = NULL;
   }

   if ( plon00 != NULL )
   {
      free ( plon00 );
      plon00 = NULL;
   }

   return SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}



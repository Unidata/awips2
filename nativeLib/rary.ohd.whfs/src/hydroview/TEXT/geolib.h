
#ifndef _geolib_h
#define _geolib_h

#ifdef __cplusplus
extern "C" {
#endif

/* Except where noted, all routines are called exactly the same from either
   c or Fortran. */

typedef
struct proj_parm {
   float xmin;
   float xmax;
   float ymin;
   float ymax;
   int   proj_idx;
   float cenlat;
   float cenlon;
   float rot;
   int   the_rest[12];
} GeoInfo;

#define MAXPTS 500

struct latLonCoord {
   float lat;
   float lon;
};

struct bcdRecord {
   int npts;
   float lat1;
   float lat2;
   float lon1;
   float lon2;
   struct latLonCoord pts[MAXPTS];
};


/* this routine works exactly like the routine SUPMAP in the old version
   of NCAR graphics, except that it only calculates the parameters for
   the specified map, it does not create any graphical instructions. */
void setsup(int * iproj,  /* see setsup.c for detailed instructions */
            float * polat,
            float * polong,
            float * rot,
            float * pl1,
            float * pl2,
            float * pl3,
            float * pl4,
            int * jjlts,
            int * status);

/* this routine return the proj_parm data structure after a call to setsup. */
void get_proj_parm(struct proj_parm * proj_ptr);  /* projection info */

/* this routine saves map definition parameters to a file, these parameters
   are created within the program by calling setsup.
   Fortran routine is SaveGeoFile */
void save_geo_file(char * geofile, /* file to save parameters in */
                   int * status);  /* 1 if successful, 0 if not */

/* this routine sets the satellite height for the SATELLITE_VIEW projection.
   Default is geostationary height. Units are km. */
void set_view_hgt(float * hgt);

/* this routine recalls map definition parameters from a file, these
   parameters must have been previously saved by calling save_geo_file.
   Fortran routine is InitGeoFile */
void init_geo_file(char * geofile, /* file to get parameters from */
                   int * status,   /* 1 if successful, 0 if not */
                   struct proj_parm * proj_ptr);  /* projection info */

/* this routine returns the range of x an y values (old SupMap u/v's) in
   an existing map.  Map must have been defined by a call to init_geo_file. */ 
void get_area_limits
               (float * x1, /* minimum x (u) value of map area */
                float * x2, /* maximum x (u) value of map area */
                float * y1, /* minimum y (v) value of map area */
                float * y2,  /* maximum y (v) value of map area */
                struct proj_parm * proj_ptr);  /* projection info */

/* this routine outputs calculated latitude and longitude values from input
   x an y values (old SupMap u/v's) for an existing map.  Map must have been
   defined by a call to setsup or init_geo_file.  Like MapInv_GC */ 
void xy_to_ll(float * x,  /* input x */
              float * y,  /* input y */ 
              float * lat, /* output latitude */
              float * lon, /* output longitude */
              struct proj_parm * proj_ptr);  /* projection info */

/* this routine outputs calculated x an y values (old SupMap u/v's) from input
   latitude and longitude values for an existing map.  Map must have been
   defined by a call to setsup or init_geo_file. Like SupCon. */ 
void ll_to_xy(float * lat,  /* input latitude */
              float * lon, /* input longitude */
              float * x,   /* output x */
              float * y,    /* output y */
              struct proj_parm * proj_ptr);  /* projection info */

/****************************************************************************/

/* this routine initializes the grid remapping function do_grid_remap.
   Once called, any number of calls to do_grid_remap will perform that
   remapping function, until additional calls are made to init_grid_remap
   or read_grid_table. Fortran routine is InitGridRemap.  Positive nx's
   indicate inner grid indices increasing left to right, negative right
   to left.  Positive ny's indicate outer grid indices increasing bottom
   to top, negative top to bottom. */
void init_grid_remap(
           char *in_geofile,  /* map parameters file for input grids */
           int *input_nx,     /* inner dimension of input grids */
           int *input_ny,     /* outer dimension of input grids */
           char *out_geofile,   /* map parameters file for output grids */
           int *output_nx,      /* inner dimension of output grids */
           int *output_ny,     /* outer dimension of output grids */
           int *ret_status);/* number of output points successfully mapped */

/* this routine dumps an internal table made by init_grid_remap to the named
   file.  init_grid_remap must have been called previously.  */
void dump_grid_table(char * table_file_name, /* name of file */
                     int * status);   /* 1 if successful, 0 if not */

/* this routine recalls an internal table made by init_grid_remap from the
   named file.  Must have been written by dump_grid_table. */
void read_grid_table(char * table_file_name, /* name of file */
                     int * status);   /* 1 if successful, 0 if not */
                             
/* this routine performs a remap of a two dimensional grid of four byte
   real values.  init_grid_remap or read_grid_table must have been called
   previously. */
void do_grid_remap(float *ingrid, /* input grid */
                   float *outgrid,  /* output grid */
                   float *flagval);  /* "missing" value */

/****************************************************************************/

/* this routine initializes the grid remapping function do_grhi_remap.
   This works like init_grid_remap except it is meant for remapping
   grids with many points.
   Once called, any number of calls to do_grhi_remap will perform that
   remapping function, until additional calls are made to init_grhi_remap
   or read_grhi_table. Fortran routine is InitGrHiRemap.  Positive nx's
   indicate inner grid indices increasing left to right, negative right
   to left.  Positive ny's indicate outer grid indices increasing bottom
   to top, negative top to bottom. */
void init_grhi_remap(
           char *in_geofile,  /* map parameters file for input grids */
           int *input_nx,     /* inner dimension of input grids */
           int *input_ny,     /* outer dimension of input grids */
           char *out_geofile,   /* map parameters file for output grids */
           int *output_nx,      /* inner dimension of output grids */
           int *output_ny,     /* outer dimension of output grids */
           int *ret_status);/* number of output points successfully mapped */

/* this routine dumps an internal table made by init_grhi_remap to the named
   file.  init_grhi_remap must have been called previously.  */
void dump_grhi_table(char * table_file_name, /* name of file */
                     int * status);   /* 1 if successful, 0 if not */

/* this routine recalls an internal table made by init_grhi_remap from the
   named file.  Must have been written by dump_grhi_table. */
void read_grhi_table(char * table_file_name, /* name of file */
                     int * status);   /* 1 if successful, 0 if not */
                             
/* this routine performs a remap of a two dimensional grid of four byte
   real values.  init_grhi_remap or read_grhi_table must have been called
   previously. */
void do_grhi_remap(float *ingrid, /* input grid */
                   float *outgrid,  /* output grid */
                   float *flagval);  /* "missing" value */

/****************************************************************************/

/* this routine initializes the wind remapping function do_wind_remap.
   Once called, any number of calls to do_wind_remap will perform that
   remapping function, until additional calls are made to init_wind_remap
   or read_wind_table. Fortran routine is InitWindRemap.  Positive nx's
   indicate inner grid indices increasing left to right, negative right
   to left.  Positive ny's indicate outer grid indices increasing bottom
   to top, negative top to bottom. */
void init_wind_remap(
           char *in_geofile,  /* map parameters file for input winds */
           int *input_nx,     /* inner dimension of input winds */
           int *input_ny,     /* outer dimension of input winds */
           char *out_geofile,   /* map parameters file for output winds */
           int *output_nx,      /* inner dimension of output winds */
           int *output_ny,     /* outer dimension of output winds */
           int *ret_status);/* number of output points successfully mapped */

/* this routine dumps an internal table made by init_wind_remap to the named
   file.  init_wind_remap must have been called previously.  */
void dump_wind_table(char * table_file_name, /* name of file */
                     int * status);   /* 1 if successful, 0 if not */

/* this routine recalls an internal table made by init_wind_remap from the
   named file.  Must have been written by dump_wind_table. */
void read_wind_table(char * table_file_name, /* name of file */
                     int * status);   /* 1 if successful, 0 if not */
                             
/* this routine performs a remap of a two dimensional wind of four byte
   real values.  init_wind_remap or read_wind_table must have been called
   previously. */
void do_wind_remap(float *in_u, /* input u component */
                   float *in_v,  /* input v component  */
                   float *out_u,  /* input u component  */
                   float *out_v,  /* input v component  */
                   float *flagval);  /* "missing" value */

/****************************************************************************/

/* this routine initializes the grid to image remapping function do_g2i_remap.
   Once called, any number of calls to do_g2i_remap will perform that
   remapping function, until additional calls are made to init_g2i_remap
   or read_g2i_table. Fortran routine is InitG2iRemap.  Positive nx's
   indicate inner grid/image indices increasing left to right, negative right
   to left.  Positive ny's indicate outer grid/image indices increasing bottom
   to top, negative top to bottom. */
void init_g2i_remap(
           char *in_geofile,  /* map parameters file for input grids */
           int *input_nx,     /* inner dimension of input grids */
           int *input_ny,     /* outer dimension of input grids */
           char *out_geofile,   /* map parameters file for output grids */
           int *output_nx,      /* inner dimension of output grids */
           int *output_ny,     /* outer dimension of output grids */
           int *ret_status);/* number of output points successfully mapped */

/* this routine dumps an internal table made by init_g2i_remap to the named
   file.  init_g2i_remap must have been called previously.  */
void dump_g2i_table(char * table_file_name, /* name of file */
                    int * status);   /* 1 if successful, 0 if not */

/* this routine recalls an internal table made by init_g2i_remap from the
   named file.  Must have been written by dump_g2i_table. */
void read_g2i_table(char * table_file_name, /* name of file */
                    int * status);   /* 1 if successful, 0 if not */
                             
/* this routine performs a remap of a two dimensional grid of four byte
   real values to an image composed of byte values.  init_g2i_remap or
   read_g2i_table must have been called previously. */
void do_g2i_remap(float *ingrid, /* input grid */
                  unsigned char *outimage,  /* output image */
                  char *iflag,   /* value used for unmapped points */
                  char *limbval, /* value used for points not on the earth. */
                  float *base, /* grid value that is image value of 0 */
                  float *delta); /* change of grid value per count */

/* this routine performs a remap of a two dimensional grid of four byte
   real values to an image composed of byte values.  init_g2i_remap or
   read_g2i_table must have been called previously. */
void do_g2i_remap_flag(float *ingrid, /* input grid */
                  float *gflag, /* missing value in input grid. */
                  unsigned char *outimage,  /* output image */
                  char *iflag,   /* value used for unmapped points */
                  char *limbval, /* value used for points not on the earth. */
                  float *base, /* grid value that is image value of 0 */
                  float *delta); /* change of grid value per count */

/****************************************************************************/

/* this routine initializes the image remapping function do_image_remap.
   Once called, any number of calls to do_image_remap will perform that
   remapping function until additional calls are made to init_image_remap
   or read_image_table. Fortran routine is InitImageRemap. Positive nx's
   indicate inner image indices increasing left to right, negative right
   to left.  Positive ny's indicate outer image indices increasing bottom
   to top, negative top to bottom.*/
void init_image_remap(
           char *in_geofile,  /* map parameters file for input images */
           int *input_nx,     /* inner dimension of input images */
           int *input_ny,     /* outer dimension of input images */
           char *out_geofile,   /* map parameters file for output images */
           int *output_nx,      /* inner dimension of output images */
           int *output_ny,     /* outer dimension of output images */
           int *ret_status);/* number of output points successfully mapped */

/* this routine dumps an internal table made by init_image_remap to the named
   file.  init_image_remap must have been called previously.  */
void dump_image_table(char * table_file_name, /* name of file */
                             int * status);   /* 1 if successful, 0 if not */

/* this routine recalls an internal table made by init_image_remap from the
   named file.  Must have been written by dump_image_table. */
void read_image_table(char * table_file_name, /* name of file */
                             int * status);   /* 1 if successful, 0 if not */

/* this routine performs a remap of a two dimensional image of byte
   values.  init_image_remap or read_image_table must have been called
   previously.  Return value will test logically true if successful. */
int do_image_remap(char *inimage, /* input image */
                   char *outimage,  /* output image */
                   char *fillval);  /*fill value for unmapped pixel*/

/* this routine performs a remap of a two dimensional image of byte
   values.  init_image_remap or read_image_table must have been called
   previously.  Return value will test logically true if successful.
   this version remaps to the specified area of the output table. */
int do_image_sector(char *inimage, /* input image */
                    char *outimage,  /* output image */
                    char *fillval,  /*fill value for unmapped pixel*/
                    int *firsti,
                    int *lasti,
                    int *firstj,
                    int *lastj);

/* this routine disconnects the program from the current image table and
   releases the associated memory. */
void release_current_imgtbl();

/* this routine sets the maximum number of tables that can be held in memory
   at the same time.  Return value tests true if maxtbls is in allowable
   range. */
int set_max_img_tables(int * maxtbls);

/****************************************************************************/

/* this sets up for reading an ascii file containing a list of lat-lon
   point pairs.  Fortran call is InitPointsFile. */
void init_points_file(char * points_file_name, /* name of file */
                      int * status);   /* 1 if successful, 0 if not */

/* this returns one segment from a file initialized with a call to 
   init_points_file.  When an unsuccessful status is returned, it means
   that there are no more points and the file is closed. Returns true if
   segment is separated from last one. */
int get_one_segment(
               float *lat1, /* latitude of 1st endpoint of segment */
               float *lon1, /* longitude of 1st endpoint of segment */ 
               float *lat2,  /* latitude of 2nd endpoint of segment */
               float *lon2, /* longitude of 2nd endpoint of segment */
               int *status);  /* 1 if successful, 0 if not */

/****************************************************************************/

/* this sets up for reading an binary file containing a list of lat-lon
   point pairs.  Fortran call is InitBcdFile. */
void init_bcd_file(char * bcd_file_name, /* name of file */
                   int * status);   /* 1 if successful, 0 if not */

/* this returns one record from a file initialized with a call to 
   init_bcd_file.  When an unsuccessful status is returned, it means
   that there are no more points and the file is closed. */
void get_one_bcdrec(struct bcdRecord * bcd_rec,
                    int *status);  /* 1 if successful, 0 if not */

#ifdef __cplusplus
};
#endif

#endif

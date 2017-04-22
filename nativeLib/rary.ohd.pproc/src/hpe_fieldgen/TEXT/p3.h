#ifndef P3_H
#define P3_H

#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <strings.h>
#include <malloc.h>

#include "GeneralUtil.h"
#include "util.h"


#include "empe_fieldgen.h"
#include "empe_params.h"
#include "empe_db_tables.h"


/*
extern mpe_params_struct * ptrMPEParams ;
extern gage_table_struct ** ptrGageTable;
extern radarLoc_table_struct * ptrRadarLocTable;
extern geo_data_struct * ptrGeoData ;
extern run_date_struct * ptrRunDate ;
*/

//#include "mpe_db_tables.h"


/*
extern run_date_struct * ptrRunDate;
extern mpe_params_struct * pMPEParams;
extern radarLoc_table_struct * pRadarLocTable;
extern geo_data_struct * ptrGeoData;
*/



#define NORMAL_SEARCH            0x00
#define INCLUDE_DELETED_POINTS   0x01
#define CLOSEST_X_ONLY           0x02
#define MISSING_VALUE            -9999.0



typedef struct
                       {
                       double lon;
                       double lat;
                       }DPOINTL;		       


typedef struct         {
			int   month,day,year,hour,min,sec;
			char  cmo[3],cda[3],cyr[3],chr[3];
			char  cdate[11];
			char  idate[20];
			char  ldate[20];
			char  lldate[20];
		       } p3_date_struct;



typedef struct         {
			char id[4];
			float xlat;
			float xlong;
			int   on;
			P3HRAP  ctr;
			int   ngrd;
			int   ngage;
		       } p3_nex_struct;
		       
		       

  typedef struct 
           {                    /* data structure for map overlays */
            char   id[9];       /* name of data set */
            int    npts;        /* number of points in set */
            P3HRAP   *hrap;     /* hrap coordinate data array */
           } overlay_struct;    	

typedef struct
           {                    /* data structure for map overlays */
           char   id[9];        /* name of data set */
           int     npts;        /* number of points in set */
           P3HRAP   *hrap;      /* hrap coordinate data array */
           int  status[3];
           } basin_struct;
    
typedef struct 
           {                    /* data structure for counties */
           char name[21];       /* name of counties */
           char id[9];          /* identifier of counties */
           int  npts;           /* number of points in county outline */
           P3HRAP *hrap;        /* county outline in P3HRAP coordinates */
           int  status[3];
           } county_struct;
      
typedef struct
  {
  long    other_dataset_tri_idx;  /* index of the triangle in the other dataset*/
                                  /* in which this CONTOUR datapoint resides*/
  double  other_dataset_zval;     /* this is the z value of the other dataset
 calibrate_factor */      
  }P3_CALIBRATE_DATA;   
    	  
  
typedef struct process1_struct {
	int		status;
	int		basin_status[3];
	int		county_status[3];
	struct tm	*t_local;
	int		old_hour;
	int		old_day;
	int		old_mon;
	int		old_year;
	char            date[11];
	char            cyr[3];
	char            cmo[3];
	char            cda[3];
	char            chr[3];
	int		num_bas;
	basin_struct	basins[400];
	int		num_county;
	county_struct	counties[5000];
	overlay_struct  rfc;
	int		numcwa;
    overlay_struct  cwa[200];
    int             numstates;
    overlay_struct  state[200];
    int             num_rvr;
    overlay_struct  river[1500]; 
    overlay_struct  contours[4];
    int             num_contour;
  	int		show_rfc;
	int		show_state;
	int		show_county;
	int		show_cwa;
	int		show_town;
	int		show_basin;
	int             show_contour;
	int		show_grid;
	int             show_gages;
	int             show_rings;
	int             show_triangles;
	int		show_river;
	float		zoom;
	float		orig_x;
	float		orig_y;
	} process1_struct;

short compute_ratio_and_diff_at_point(double rain_z, double radar_z, double max_ratio, double *ratio, double *diff);
void displayit(void);
int calibrate_radar(void);
void triangulategage(void);
int mpe_values(const run_date_struct *,const empe_params_struct *,const radarLoc_table_struct *,const geo_data_struct *,int);
void  write_p3_xmrg(double**,double**);
void set_best_estimate_p3();
void unset_best_estimate_p3();
void date_string(void);
void write_gage_triangles(void);
short contour_data_sort_x();
TRIPNT *cross_reference_triangles();
void calc_z_value_with_known_triangle();
void free_gages();
int  readgages(const gage_table_struct * ptrGageTable,List * pPolyList );
void readstagei(double ** AvgMosaic);
void triangulategage();
int calibrate_radar();
void date_string(void);
void  write_p3_xmrg(double ** P3Mosaic, double ** QPEMosaic);
void write_gage_triangles(void);
void printouttrifile(char*);
void free_tr();
void free_gages();
int  init_tr_mem();
void  cleanupgages();

short compute_adjusted_zval(p3_gage_struct *c, P3_CALIBRATE_DATA *cal, 
                            TRIANGLE *t,
                            long triidx,
                            double *x, double *y, double *z,
                            double *adj_z);


long determine_dataset_triangle(DPOINTL *pnt,
                                p3_gage_struct *c, TRIPNT *tripnt,
                                long * sortx, long numpnt,
                                TRIANGLE *tri, long numtri);

#endif

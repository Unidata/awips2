#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <strings.h>
#include <signal.h>
#include <malloc.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#define PI 3.141593
#define PROJ_MERCATOR  1
#define PROJ_HRAP      2
//#define HRAP_X		335
//#define HRAP_Y	        159
//#define HRAP_XOR	367
//#define HRAP_YOR	263
#define RESET		3
#define LINE_A_B_SHORTER      -1
#define LINE_A_B_NOT_CROSSED   0
#define LINE_A_B_LONGER        1
#define DELETE_PAIRS	       0
#define QUERY_LIST	       2
#define ADD_PAIR	       1


void MPEFieldGen_readGeoData();

typedef struct P3HRAP {
                      float x, y;
                    } P3HRAP;
 
		       

typedef struct
      {
      double min;
      double max;
      }MINMAX;


typedef struct{
	 long	*delete_array;
	 long	delete_cnt;
} CONTIGUITY_DELETE_ARRAY;

typedef struct
  {
         long n_triangles;
         long *tri_pnt;
  }TRIPNT;

typedef struct
  {
       long a;
       long b;
       long c;
       long a_to_b;
       long b_to_c;
       long c_to_a;
  }TRIANGLE;
  
typedef struct
  {
         long value[6];
  }TRIANGLE2;


  
  
  typedef struct {                    /* data structure for map overlays */
              char   id[9];                   /* name of data set */
              double  value;                /* value of gage */
              double lat, lon;                   /* lat and lon */
              long    n_contig;
              long   * contig;
    } p3_gage_struct;  
  

 //p3_gage_struct  radarpts[54000];
 p3_gage_struct*  radarpts;

      
typedef struct
  {
  long    other_dataset_tri_idx;  /* index of the triangle in the other dataset*/
                                  /* in which this CONTOUR datapoint resides*/
  double  calibrate_factor;       /* this is the z value of the dataset divided*/
                                  /* by the z value of the other dataset at this point.*/
                                  /* for instance if c[i].z was 104 and the computed*/
                                  /* z from the other data set was 109, then this*/
                                  /* factor would be 104/109.*/
  }CALIBRATE_DATA;   
    	  
  
typedef struct triangles_process1_struct {
	int		status;
	int		basin_status[3];
	int		county_status[3];
	struct tm	*t_local;
	int		old_hour;
	int		old_day;
	int		old_mon;
	int		old_year;
	char            date[9];
	int		num_bas;
	int		num_county;
	int		numcwa;
        int             numstates;
        int             num_rvr;
        float		zoom;
	float		orig_x;
	float		orig_y;
	} triangles_process1_struct;

triangles_process1_struct process1_data;
long            numofradarpts;

void            ReadParameters();
void            writemosaic();
int             InOut();
int             get_apps_defaults();
void            unmanage();
int             get_pixel_by_name();
void            display_date_window();
void            popup_workingDialog();
void            select_callback();
void            ok_callback();
void            pop_down();
void            popdown_shell();
void            SearchOutFiles();
void            SearchInFiles();
void            restore();
void            close_display();
void            expose_canvas();
void            handle_start();
void            handle_drag();
void            handle_done();
void            locategage();
void            edit_gage();
void            settonew();
void            settomiss();
void            settozero();
void            close_gage();
void            setthevalue();
void            ll_to_mi();
void            mi_to_ll();
void            get_contig();
long            get_seed_point();
short           try_to_connect_a_to_b();
long            *get_deleted_pnts_not_contig();
short           try_to_connect_a_to_b_children();
short           chk_intersect();
short           ccwise();
short           contiguous();
short           internal_triangle();
short           add_tripnt();
short           add_contiguity_point();
short           del_contiguity_point();
TRIANGLE        *calculate_triangles();
void            validate_data_points();
void            radarfile();
int             MPEFieldGen_readradarcont();
int             MPEFieldGen_readradartriangles();
void            HRAP_values ( int * HRAP_X, int * HRAP_Y, 
                              int * HRAP_XOR, int * HRAP_YOR );
void            free_mem ( );
short del_list(short option, p3_gage_struct *c, long pnta, long pntb, CONTIGUITY_DELETE_ARRAY *cda);




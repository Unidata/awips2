#ifndef GAGEQC_TYPES_H
#define GAGEQC_TYPES_H

/*******************************************************************************
* FILENAME:             gageqc_types.h
* DESCRIPTION:          Contains user-defined types and function prototypes.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        January 9, 2006 
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*   DATE             PROGRAMMER       DESCRIPTION/REASON
*   January 9, 2006  Bryon Lawrence   Created.
********************************************************************************
*/

#include <time.h>
#include <Xm/Xm.h>

#include "DbmsDefs.h"
#include "stage3.h"
#include "mpe_topo.h"
#include "get_colorvalues.h"

typedef struct ShiftTable
{
  double Val[4];
  double Sh[4];
  int max;

} ShiftTable;


typedef struct RatingTable
{
	double RatStg[100];
	int RatQ[100];
        ShiftTable Shift;
	double HMin;
	double HMax;
	int QMin;
	int QMax;
	int AryMax;
        double wrn;
        double fld;
        int bdate;
        int edate;
	int tbl_ver;
	int ver5;
	double hist_stg;
	int hist_q;
	int hist_date;
	double datum_adj;

} RatingTable;



struct bavg {

     char id[6];
     char pe1[2];
     char pe2[2];
     char dur[2];
     char t[2];
     char s[2];
     char e[2];
     char p[2];
     double jan;
     double feb;
     double mar;
     double apr;
     double may;
     double jun;
     double jul;
     double aug;
     double sep;
     double oct;
     double nov;
     double dec;
     long int calcdate;
     };     
 
struct print {

         char name[100];
         char command[100];
         
            };


struct ts {

        char abr[5];
        char name[20];


};


struct input_stations {

     char hb5[10];
     int type;
     int index;

   };

struct valid_list {

            char wfo[10];
            int alias;
            int inuse;
            int alive;

	  };

struct tag {

        char wfo[10];

      };

struct forecast_basins {

         char hb5[10];
         int owner;
         
             };


 struct fstn {

            float *rain;
            float *frz;
            int *tx;
            int *tn;
            
            }; 
                
struct fdata {

              struct fstn fstn[3000];

            };
    

struct fstation {

          char hb5[10];
          int pindex;
	  int tindex;
	  int zindex;
          int qpf;
          int frz;
          int txn;
	  int selected;
          int owner;
          
          };


struct bad_values {

        int used;
        char hb5[10];
        char parm[10];
        int month;
        float fvalue;

      };

struct bad_daily_values {

        int used;
        char hb5[10];
        char parm[10];
        int day;
        int quart;
        float fvalue;

      };

struct max_min {

       float max;
       float min;
       float amax;
       float amin;
       struct tm tmx;
       struct tm tmn;

     };

struct rec{

       char hb5[10];
       char parm[10];
       int xpos,ypos,xsize,ysize,end_page;

     };


struct gp {

       int max_stations;
       int is_realized;
       int x_per_page;
       int y_per_page;
       int max_pages;
       char title[25][100];
       char name[100];
       struct rec rec[100];

     };


struct stim {

      int year;
      int month;
      int day;
      int hour;
      int minute;

    };

struct sens_ok {

                   char tid[6];
                   char tpe1[2];
                   char tpe2[2];
                   char tdur[2];
                   char tt[2];
                   char ts[2];
                   char te[2];
                   char tp[2];
                   int cal_yr;
                   int mon;
                   int zday;
                   int ztime;
                   char tok[2];
                   char reason[81];
                   char init[4];
                   char agcode[7];
                   char agloc[4];
                   char comment[41];

};

struct stn_values {

              int zdate;
              int ztime;
              int cdate;
              int ctime;
              float value;
              char qual;

	    };


struct stn_rec {

     char id[6];
     char plat[9];
     char des[51];
     char det[41];
     char init[9];
     char lat[7];
     char lng[8];
     int elev;
     char stat[3];
     char huc[9];
     char cou[4];
     char zon[5];
     char hsa[4];
     char cwa[4];
     int post;
     char dbsource[4];
};
struct sens_rec {

     char id[6];
     char pe1[2];
     char pe2[2];
     char dur[2];
     char t[2];
     char s[2];
     char e[2];
     char p[2];
     char type[2];
     char des[51];
     char det[41];
     int post;
     char new_report[2];
     char active[2];
     char ok[2];
     char rfs[2];
     int obstime;
     char prod[10];
     char prod2ary[10];
     char obsag[7];
     char obsloc[4];
     char prov_avail[2];
     char final_avail[8];
     char ownag[7];
     char ownloc[4];
     char maintag[7];
     char maintloc[4];
     char init[9];
     char dbsource[4];

  };
  
struct save_rec {

       struct stn_rec stn_rec;
       struct stn_rec ptn_rec;
       char parm[10];
       char pparm[10];
       RatingTable rate_table;
       struct stn_values stn_values[10000];
       struct stn_values ptn_values[10000];
       struct stim btim;
       struct stim etim;
       int inum;
       int pnum;
       int volume;
       int polume;
       int is_realized;
       int converted;

          }; 
	/*  
 struct save_rec2 {

       struct stn_rec stn_rec;
       struct stn_rec qtn_rec;
        
       char parm[10];
       char qarm[10];
       
       RatingTable rate_table;
       RatingTable qate_table;
       
       struct stn_values stn_values[10000];
       struct stn_values qtn_values[10000];
       
       struct stim btim;
       struct stim etim;
       int inum;
       int qnum;
       int volume;
       int qolume;
       
       int is_realized;
       int converted;

          }; 
 */
struct hrap_data {

                int x;
                int y;
                int zone[4];

	      };


struct map {

           char hb5[10];
           int basin_points;
           int zones[4];
           char bchar[5];
           struct lcoord * basin;
           int hrap_points;
           struct hrap_data *hrap_data;
           int owner;
           int *maps_done;
           int *zmaps_done;
	   int *tmaps_done;	   
           float *gz;
           float *uz;
           float *mz;
           float *lz;
           float *gzc;
           float *uzc;
           float *mzc;
           float *lzc;
           float *zgz;
           float *zuz;
           float *zmz;
           float *zlz;
	   float *tgz;
           float *tuz;
           float *tmz;
           float *tlz;

	 };
  

struct rain {

            float data;
            short int qual;
            float estimate;
            float stddev;

	  };
	  
struct zlevel {

            float data;
            short int qual;
            float estimate;
            float stddev;

	  };

/*struct stn {

            short int tcons;
	    short int scons[5];
            struct rain rain[5];
            struct rain frain[5];
            int frzlvl[5];
            short int snoflag[5];
            short int sflag[5];
            struct rain srain[5];

            }; */
	    
struct stn {

            short int tcons;
	    short int *scons;
            struct rain *rain;
            struct rain *frain;
            int *frzlvl;
            short int *snoflag;
            short int *sflag;
            struct rain *srain;

            }; 
	        

struct ztn {

            struct zlevel *zlevel1;
            struct zlevel *zlevel2;

            }; 

struct tlevel {

            short int data;
	    char  qual;
	    short int estimate;
	    float stddev;
	    float a;
	   	    
            };
	     
struct ttn {

            struct tlevel *tlevel1;
	    struct tlevel *tlevel2;
	    
            }; 
      
struct rtn {

            int status;
            int forecast;
            int oldstatus;
            time_t timeout[10];

            }; 
            
struct pdata {

              time_t data_time;
	      int ztime;
              int used[5];
              float stddev;
              int level;
              struct stn stn[6000];

	    };
             
struct zdata {

              time_t data_time;
              int ztime;
              int used[5];
              float stddev;
              int level[5];
              struct ztn stn[1000];

	    };
struct tdata {

              time_t data_time;
              int ztime;
              int used[6];
              float stddev;
              int level[6];
              struct ttn stn[2000];

	    };
         
struct rdata {

              struct rtn rtn[3000];

	    };

struct station{

            float *isoh;
	    float *max;
	    float *min;
            char  *hb5;
            char  *name;
            char  *parm;
            char  *cparm;
            char rawlevel[12];
            short int *index;
            short int *zindex;
            float lat;
            float lon;
            int elev;
            int tip;
	    int qpf;
            int x;
            int y;
            int xadd;
            int yadd;
            float hrap_x;
            float hrap_y;
          };


struct Cur {

           int x;
           int y;

	 };

struct save {

            short int inuse;
            short int x[3];
            short int y[3];

	  };

struct abogus {

            short int x;
            short int y;
            struct abogus *next;

	  };

struct bogus {

             short int max;
             short int inuse[500];
             short int x[500];
             short int y[500];
             float value[500];
             float isoh[500];
             
	   };

struct wfobogus {

         char wfo[10];
         int owner;
         int bogus_flag[50];
         struct abogus *bbogus[50];
         struct bogus bogus[50];
         
         };

struct area {

            short int x;
            short int y;
            struct area *area;

	  };

struct stream {

     float stage;
     long stdif;

              };

struct graph {

             int charsize;  /*0=regular 1=small */
             int color;     /*user specified 0-15*/
             int notitle;   /*0=print title 1=dont*/
             int nobox;     /*0=print box 1=dont*/
             int numbers;   /*0=numbers on right 1=left*/
             int gap;       /*maximum data gap*/
             int contour;   /*0=autoscale */
             int tgap;      /* time gap allowable in hours */
             int bar;       /*bar graph plot*/
             int flow;      /*linear flow*/
             float max;
             float min;
             int lines;    /*0=interval lines on 1=interval lines off*/
             int num;
             int linterval;
             int maxmin;

             };

struct flood {

                float max;
                float min;
                float ws;
                float fs;
                float hist;
                float shift;
                char  hdate[15];
                float rtable[500][2];
                int rmax;
                int type;

                };


struct wx {

          int temp[24];
          int dew[24];
          int wind[24];
          int peak[24];
          int rn[24];
          int st[24];
          int flow[24];
          int pcnt[24];

          };



struct fparms {

       int st;
       int dt;
       char qual[10];
       int jtver;
       int itver;
       float okdif[10];
       struct fparms  *gparms;

              };
struct mappoints {

               int x;
               int y;

	     };

 struct maplayer {

                 short int color;
                 short int zoom;
                 short int number;
                 short int xmax;
                 short int ymax;
                 short int xmin;
                 short int ymin;
                 int type;
                 char *name;
                 struct mappoints *points;
                 struct maplayer *nextseg;

	       };

struct MenuItem {

char        *label;
char        mnemonic;
XmString    accel_text;
char        *accel;
int         def;
void        (*callback)();
XtPointer   callback_data;

}; 

  struct dval {

               double a;
               double xo;
               double yo;
               double lo;

               };

  struct nexrad {

               int value[131][131];
               char date[100];
               int flag;

	     };

  struct coord {

               short int x;
               short int y;
	       float lat;
	       float lon;

             };
	     
  struct icoord {

               short int x;
               short int y;
	       
             };
	     
  struct fcoord {

               float x;
               float y;
	       int i;
	       int j;
	       
             };

  struct nexcoord{
  
  

               char name[10];
               struct coord boxes[131][131];

	     };

  struct top {
 
              short int **value;
              int maxi;
              int maxj;
              float max_lat;
              float max_lon;
              float total_lat;
              float total_lon;
              float delta_lat;
              float delta_lon;

	    };

  struct isoh {

              struct icoord **coord;
              short int ***value;
              int maxi;
              int maxj;
              float max_lat;
              float max_lon;
              float total_lat;
              float total_lon;
              float delta_lat;
              float delta_lon;

	    };
	    
  struct isohn {

              struct fcoord **coord;
              short int **value;
              int maxi;
              int maxj;
              float max_lat;
              float max_lon;
              float total_lat;
              float total_lon;
              float delta_lat;
              float delta_lon;

	    };
	    
struct maxmin {

              struct icoord **coord;
              short int ***maxvalue;
	      short int ***minvalue;
              int maxi;
              int maxj;
              float max_lat;
              float max_lon;
              float total_lat;
              float total_lon;
              float delta_lat;
              float delta_lon;

	    };


   struct h_grid {

              short int maxi;
              short int maxj;
              struct coord **box;
              struct HRAP **latlon;
              short int **elev;

	    };

   struct gdata {

                float pcpn[500][500];

	      };

   struct hdata {

                short int x;
                short int y;
                short int flag;

	      };

   struct hrap_grid {
        
              short int maxi;
              short int maxj;
              short int hrap_minx;
              short int hrap_miny;
              struct coord **coord;
              struct gage **gage;
              short int **owner;
              short int ***isoh;
	      short int ***max;
	      short int ***min;
              short int **elev;
              
              
	    };

  struct gage {

              short int index[30];
              short int zindex[30];
              short int tindex[30];
	      
	    };

  struct pcp {
 
             
             short int **value;

	     };

  struct display_set {

                 long xoff[4];
                 long yoff[4];
                 long xmin[4];
                 long ymin[4];
                 long xmax[4];
                 long ymax[4];
                 long xcen[4];
                 long ycen[4];
                 float dmult[4];

	       };

struct latlon_info
{
   char lid [ LOC_ID_LEN + 1];
   double lat;
   double lon;
};

char *pars_argv(char **, char *);
//char *pars_line(char * buf1, char * s, char * sbuf);
//char *pars_linen(char * buf, char * s , char * sbuf);

char *strno(char *, char);
int strdec(char *,int);
int draw_back(Pixmap,int,int );
int strfind(char *, char *);
void get_NEXRAD_pixels(int,int,float,float,int);
void plot_NEXRAD(Pixmap,struct nexrad *,int,int);
void redraw_topo_legend(Drawable);
void redraw_edit_legend(Drawable,int,int,int);

void clear_pix_map(Pixmap);
void get_area_menu();
void read_maps(char *,int,int);
Widget AttachToCascade(Widget,char *,Widget);
void get_topo_pixels(int,int,float,float);
void calculate_NEXRAD(int,int);
void calculate_zlevel ( );
float get_isohyet(int, int, int);
float get_lisohyet(float, float,int);
void berror(Widget ,char *);
void plot_rain(Drawable,int,int,int);
void change_display_mode(Widget,XtPointer,XmDrawingAreaCallbackStruct *);
void map_select(Widget,XtPointer,XtPointer);
void change_scale(Widget,XtPointer,XmDrawingAreaCallbackStruct *);
void area_select(Widget,XtPointer);

void change_method(Widget,XtPointer,XmDrawingAreaCallbackStruct *);
void get_zlevel ( int pcpn_day,
	          struct station * station,
		  struct station * zstation,
		  int max_stations,
		  int max_zstation);

void point_define(Widget,XtPointer,XmDrawingAreaCallbackStruct *);
void area_define(Widget,XtPointer,XmDrawingAreaCallbackStruct *);
void kill_widget(Widget,XtPointer,XtPointer);
void check_date(Widget,XtPointer,XtPointer);
void move_field(Widget,XtPointer,XtPointer);
void input_time(Widget,XtPointer,XtPointer);

void input_time_step(Widget,XtPointer,XtPointer);

void get_isohyet_coord(char *,int,int);
void get_maxmin_coord(char *,int,int,int);
void redraw_map(int,int,unsigned int);
void draw_map(Drawable,int,int);
void get_NEXRAD_coord(void);
void read_NEXRAD(char *,struct nexrad *);
void plot_NEXRAD(Pixmap,struct nexrad *,int h, int);

Widget BuildPulldownToggleMenu(Widget,char *,char,struct MenuItem *);
Widget BuildPulldownPushMenu(Widget,char *,char,struct MenuItem *);

void plot_qpf_points(Pixmap,int,int,int);
void plot_qpf_area(Pixmap pixm,int, int,int);
void plot_gridded_precip ( int h, int display_flag, char *file, int num,
                           int mnum);

void move_area(int,int,int);
void plot_isoh(Pixmap,int, int);
void contour_isoh(Pixmap,int, int);
void plot_isohyets(int,Pixmap,int, int);
void contour_isohyets(int,Pixmap,int, int);

int macmartintest( double[500][2], int, double *);
void change_pcpn_legend(int,int);

void contour_precip(int h, int display_flag, char * filename,int time_pos);
void contour_temperature(int h, int display_flag, char * filename,int time_pos);
void contour_freezing(int h, int display_flag, char * filename,int time_pos);
 
void change_topo_legend(int,int);
void topo_select(void);
void change_topo(void);
void change_isohyet(void);
void change_NEXRAD(void);
void change_qpf(void);
void send_expose(void);
void redraw_isohyet_legend(Drawable);
void change_isohyet_legend(int,int);
void isohyet_select(void);
void Flush_Xbuffer(void);
void raster_select(void);
void foreground_select(void);
void write_screen(void);
void clear_drawable(Drawable);
void bogus_points(int,int,unsigned int,int);
void bogus_area(int,int,unsigned int,int);


void get_hrap_coord ( double maximum_latitude,
                      double minimum_latitude,
                      double center_longitude,
                      int smonth,
                      int emonth);

int get_hrap_mask ( const char * fname );

void get_tgage_grid ( double maximum_latitude,
                      double minimum_latitude,
                      double center_longitude);

void get_zgage_grid ( double maximum_latitude,
                      double minimum_latitude,
                      double center_longitude);

void change_edit_legend(Widget, XtPointer, XtPointer);
void render_qpf(int,int,int,char *);
void change_qpf_edit_legend(Widget,XtPointer,XtPointer);
void redraw_pcpn_legend(Drawable);


void get_lat_lon(int,int,float *,float *);


void get_isoh_pixels(int,int,float,float);

void get_maxmin_pixels(int,int,float,float);

void Create_Colors(Display *);

void Create_Drawing_Colors(Display *);

void get_coord(double,double,double,double);
void get_topo(char *);
int read_qpf(time_t,int,int);
void create_area_menu(void);
void edit_stations(int,int,unsigned int);
void graph_file();
void tgraph_file();
void displayPrecipTimeSeriesLite(int isave);
void displayTempTimeSeriesLite(int isave);
void edit_maxminstations(int,int,unsigned int);
void edit_zstations(int,int,unsigned int);
void plot_stations(Pixmap,int,int, int);
void get_station_list(char * fname, int smonth, int emonth);
void get_tstation_list(char * fname, int smonth, int emonth);
void get_zstation_list(const char * fname);
void change_legend_display(int,int);
void redraw_legend_display(Drawable);
void get_NEXRAD_data(time_t,int,char *);
void get_precipitation_data(time_t,int);
int read_precip_a(const char *fname,
                  time_t tget,
                  int i,
                  struct pdata * pdata,
                  const struct station * station,
                  int max_stations );
int read_precip_b(const char *fname,
                  time_t tget,
                  int i,
                  struct pdata * pdata,
                  const struct station * station,
                  int max_stations );
int read_monthly_precip_a(char *,int);
int read_monthly_precip_b(char *,int);

void create_map(int num);
void make_mat(int num, int mnum);
void area_callback(Widget,XtPointer,XtPointer);
void change_edit_stations(void);
void check_consistency ( int pcpn_day,
	                 struct station * station,
	                 int max_stations );
void make_qpf_widget(void);
void change_rain(Widget,XtPointer,XmDrawingAreaCallbackStruct *);
void estimate_daily_stations(int pcpn_day,
                             struct station * station,
                             int max_stations);
void estimate_partial_stations ( int pcpn_day,
		                 struct station * station,
				 int max_stations);
void estimate_daily_tstations ( int pcpn_day,
	                        struct station * tstation,
				int max_tstations);
void quality_control_stations(int pcpn_day,
		              struct station * station,
			      int max_stations);
void quality_control_tstations (int pcpn_day,
		                struct station * tstation,
				int max_tstations);
void get_frzlvl_data(int);
void sum_qpf(void);
void render_pcp ( int pcpn_day, int pcpn_time, int pcpn_time_step,
                  int max_stations, const struct station * station,
                  const struct hrap_grid * hrap_grid,
                  const struct pdata * pdata,
                  int * pcp_in_use );
void render_t ( int pcpn_day, int pcpn_time, int pcpn_time_step,
                  int max_tstations, const struct station * tstation,
                  const struct hrap_grid * hrap_tgrid,
                  const struct tdata * tdata,
                  int * pcp_in_use );

void render_t6(int pcpn_day,int pcpn_time,int pcpn_time_step,
               int max_tstations, const struct station * tstation,
	       const struct hrap_grid * hrap_tgrid,
	       const struct tdata * tdata,
	       int * pcp_in_use);
void make_pmap(void);
void make_rsel(int num,int mnum);
void plot_map(Pixmap,int,int,int);
void read_file(char *,int,struct pcp *);
void write_file(char *,int,struct pcp *);
int get_basin_data ( const char *basin_file, 
                     const char *hrap_file,
                     struct map map [ ],
                     struct tag tag [] );
void make_fmap_file(void);
int hb5_search(int,char *,struct stn_rec *);
void make_sensor_list(struct stn_rec,struct sens_rec *,int);
struct stim get_new_date(struct stim );
void get_units(char *,char *);
void get_acronym(char *,char *);
void display_group_widgets(int);
void draw_area(int,int,int,int,int,int);
struct max_min get_max_min(struct stn_values *,struct stn_values *,int,int);
float get_delta_valu(int);
int get_delta_time(time_t,time_t);
void  display_group_graphs(int);
int values_search(char *,char *,struct stim,struct stim,struct stn_values *);
int climo_search(char *,char *,struct stim,struct stim,struct stn_values *);
int fvalues_search(char *,char *,struct stim,struct stim,struct stn_values *);
int name_search(int,char *,struct stn_rec *);
int get_sensor_list(char  *,struct sens_rec *);
void print_draw(int);
double get_best_stage(struct stn_values *,int);
double rh_to_dew(double,double);
double dew_to_rh(double,double);
double cel_to_far(double);
double far_to_cel(double);
void to_stage(char *, char,char,struct stn_values *,int);
long ouptime ( int year, int month, int day, int hour, int minute, int second);
void time_series(int,int,unsigned int);
void XmtWaitUntilMapped(Widget);
int sum_qpf_grids(int,int,int);
int read_map(char *,int);
int write_map(char *,int,time_t);
int get_bad_recs(char *, char *,struct sens_ok *);
char get_quality(struct sens_ok *,int,int,int);
void write_archived_data(char *,char *,char *);
void read_archived_data(char *,char *,char *,time_t,char *);
void write_archived_obs_data(char *,char *,char *,time_t,char *);
void render_points_qpf(int,int,int,char *);
float get_lmaxmin(float ,float ,int ,int );
struct tm *gmttime(time_t *);
void get_pos(char *, char *);
int read_qpf_grids ( int k, char * fname );
void restore_bad_values ( int pcpn_day,
	                  struct station * station,
	                  int max_stations );
void restore_bad_tvalues ( int pcpn_day,
	                   struct station * tstation,
	                   int max_tstations );

int GetFlow(char *, char, char, int, double, int *);

int GetAFlow(char *, char, char, int, double, int *);

int BelowStage2Flow(RatingTable , double);

int AboveStage2Flow(RatingTable, double);

int ExactStage2Flow(RatingTable, int);

int InterpolateStage2Flow(RatingTable, double, int);

int get_rate(char *, char, char, int);

double GetStage(char *, char, char, int, int, int *);

double BelowFlow2Stage(RatingTable, int);

double AboveFlow2Stage(RatingTable, int);

double ExactFlow2Stage(RatingTable, int);

double InterpolateFlow2Stage(RatingTable, int, int);

RatingTable getrc(char *, char, char, int, int*);

int rfs(int);
void write_qpf_grids ( char * fname );

double Astage(char *, char, char, int, double, int *);

int read_snow ( char * fname, 
                const struct station * station,
                int max_stations,
                int i );
void read_bad_values ( char * fname, int m );
void read_bad_tvalues ( char * fname, int m );
int read_zlevel_a(char *fname,time_t tget,int i,
                  struct zdata zdata[], struct station * zstation,
                  int max_zstations );
int read_zlevel_b(char *fname,time_t tget,int i,
                  struct zdata zdata[], struct station * zstation,
                  int max_zstations);
int read_t_a ( char * fname, time_t tget, int i , struct station * stations,
                int num_tstations);
int read_t_b ( char * fname, time_t tget, int i );
void get_bad_snotel ( const char * bad_snow_file);
int load_gage_data ( const char * area_id,
                     int master_file_flag,
                     int begin_year,
                     int begin_month,
                     int begin_day,
                     int num_days );
char* return_area_val();
void free_area_val_mem();

void post_bad_values (int iday );
void post_bad_tvalues (int iday );
int get_bad_values (int iday, int iquart );
int get_bad_tvalues (int iday, int iquart );
int is_bad ( int iday, int iquart, char *hb5, char *parm);
int is_tbad ( int iday, int iquart, char *hb5, char *parm);
int is_good(int mon,int smonth,int emonth);

struct station * read_precip_station_list ( int * num_gages,
                                            const char * area_name,
	                                    int master_area_flag );
struct station * read_temperature_station_list ( int * num_gages,
                                                 const char * area_name,
	                                         int master_area_flag );
struct station * read_freezing_station_list ( int * num_gages,
                                              const char * area_name,
	                                      int master_area_flag );
struct latlon_info * read_mpe_station_list ( int * num_gages );
const char * get_station_list_path ( const char * site_id );

char** get_rfc_areas(int* num_tokens);
void free_rfc_areas_token_memory();
int dqc_run_date_changed();
int dqc_run_area_changed();
void init_dqc_local_date_and_area_values();
int is_area_master();
void initialize_gageqc ( );
int init_precip_climo ( const char * station_climo_file,
		        struct station station [ ],
		        int max_stations );
int init_temperature_climo ( const char * station_climo_file ,
                             struct station tstation [ ],
                             int max_tstations );
int map_freeze_gages_to_grid (int smonth,
			      int emonth,
			      const struct hrap_grid *hrap_grid,
			      const char *hrap_zgage_file,
			      const char *area_name,
			      const struct station *zstation,
			      const struct station *station,
			      int num_zstations, 
                              int num_stations);

struct hrap_grid * map_precip_gages_to_grid (int smonth,
				         int emonth,
					     const char *hrap_gage_file,
					     const char * area_name,
					     struct station *station, 
					     int num_stations);
int map_temp_gages_to_grid (int smonth,
	 	            int emonth,
			    const char *hrap_tgage_file,
			    const char *area_name,
 			    const struct station *tstation, 
 			    int num_tstations);
int read_mean_monthly_temp (const char * prism_path,
                            const char * mpe_site_id, 
                            int smonth,                       
                            int emonth);
int read_mean_monthly_precip (const char * prism_path,
                              const char * mpe_site_id, 
                              int smonth, 
                              int emonth);
int get_hsa_to_grid_mask (  struct hrap_grid * hrap_grid,
                            struct tag tag [ ],
                            int * wfo_all,
                            const char * hsa_fname );
void update_bad_values(int );
void update_bad_tvalues(int );
struct hrap_grid * get_hrap_grid ( );
struct station * get_precip_station_list ( int * num_gages );
struct station * get_temperature_station_list ( int * num_tgages );
struct station * get_freezing_station_list ( int * num_zgages );
double get_rmesh ( );
void estimate_missing_stations ( int j,
                                 const struct station * station,
                                 int max_stations,
                                 const struct pdata * pdata
                                );
void estimate_missing_tstations ( int j,
                                 const struct station * tstation,
                                 int max_tstations,
                                 const struct tdata * tdata
                                );
void write_bad_values(char *fname,int iday);
void write_bad_tvalues(char *fname,int iday);
int wasGageQCperformed ( );
void turnGageQCflagOff ( );
void render_z (int pcpn_day, int pcpn_time, int pcpn_time_step,
               int max_zstations, const struct station *zstation,
               const struct hrap_grid * hrap_grid, const struct zdata *zdata,
               int *pcp_in_use);
const char * get_mpe_scratch_dir ( );
void free_freezing_station_list ( );
void free_temperature_station_list ( );
void free_precip_station_list ( );
void free_dqc_data ( );
int get_num_basins ( );
void get_prev_start_end_month ( int * start_month, int * end_month );
int get_num_days_to_qc ( );
const char * get_mon_name ( int month_number );

void set_dqc_colorvalues( char * cv_use_name,
                         int    cv_default_dur,
			 int  * cv_numcol,
			 const NamedColorSetGroup *dqc_default_pColors);
			 
void set_dqc_colordelimit();			 
int getEnding6HourObsTime();
void set_button_handles ( Widget precipitation_push_button,
                          Widget temperature_push_button,
                          Widget freezing_level_push_button );
int getDqcBasetime ( );
			 
#endif /* #ifndef GAGEQC_TYPES_H */

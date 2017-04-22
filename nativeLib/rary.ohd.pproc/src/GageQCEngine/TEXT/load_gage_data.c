/*******************************************************************************
* FILENAME:            load_gage_data.c
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the load gage data routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence 
* CREATION DATE:       January 2006 
* ORGANIZATION:        CBRFC
* MACHINE:
* MODIFICATION HISTORY:
*   DATE           PROGRAMMER        DESCRIPTION/REASON
*   January 2006   Bryon Lawrence    Created a version of this routine
*                                    from Craig Peterson's (CBRFC)
*                                    area_select routine. 
********************************************************************************
*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"

//this variable is set to 1 if the curr time is > 12z
//this is required by Disagg to write 1hr values to db
int hrgt12z = -1;

/* Define the tdata, pdata and zdata arrays here.
   The tdata array contains the station 6,24 temperature data.
   The pdata array contains the station 6,24 precipitation data.
   The zdata array contains the station 6,24 freezing level data. */ 
struct pdata pdata [ MAX_GAGEQC_DAYS ];  /* Precip data. */
struct tdata tdata [ MAX_GAGEQC_DAYS ];  /* Temp data. */
struct zdata zdata [ MAX_GAGEQC_DAYS ];  /* Freezing data. */

struct pcp *pcp = NULL;
struct pcp *spf = NULL;
struct pcp *tpf = NULL;


struct topo *topo=NULL;
struct isoh *isoh=NULL;

struct bad_daily_values bad_values[6000];
struct bad_daily_values bad_tvalues[6000];
struct map mean_areal_precip_global[MAX_GAGEQC_BASINS];

/* Define timefile file extensions. */
const char *timefile[4][5]={{"00_06","06_12","12_18","18_00","00_00"},
                            {"06_12","12_18","18_00","00_06","06_06"},
                            {"12_18","18_00","00_06","06_12","12_12"},
                            {"18_00","00_06","06_12","12_18","18_18"} };
const char *ttimefile[4][6]={{"00","06","12","18","max","min"},
                             {"06","12","18","00","max","min"},
                             {"12","18","00","06","max","min"},
                             {"18","00","06","12","max","min"} };
const char *ztimefile[4][4]={{"00","06","12","18"},
                             {"06","12","18","00"},
                             {"12","18","00","06"},
                             {"18","00","06","12"}};
int dqcBasetime;
int dqcBaseIndex;
int render_all=1;
int wfo_orig;
int wfo_all = 1;
int wfo_in_use [ MAX_GAGEQC_WFOS ];

static int gageqc_performed = 0;

char forecast_basin_file[GAGEQC_FILENAME_LEN];
char grid_file[GAGEQC_FILENAME_LEN];
char map_file[GAGEQC_FILENAME_LEN];
char mat_file[GAGEQC_FILENAME_LEN];
char mpe_rfc_name_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
char pcpn_bad_file[GAGEQC_FILENAME_LEN];
char pcpn_dev_file[GAGEQC_FILENAME_LEN];
char proc_pcpn_file[GAGEQC_FILENAME_LEN];
char rsel_file[GAGEQC_FILENAME_LEN];
char station_climo_file[GAGEQC_FILENAME_LEN];
char station_list_custom_file[GAGEQC_FILENAME_LEN];
char temp_bad_file[GAGEQC_FILENAME_LEN];
char temp_dev_file[GAGEQC_FILENAME_LEN];
char tgrid_file[GAGEQC_FILENAME_LEN];
char tpoint2_file[GAGEQC_FILENAME_LEN];
char tstation_list_custom_file[GAGEQC_FILENAME_LEN];
char zgrid_file[GAGEQC_FILENAME_LEN];
char zpoint2_file[GAGEQC_FILENAME_LEN];
char zstation_list_custom_file[GAGEQC_FILENAME_LEN];

/* Variables which control the operation of dqc algorithms and neighbor
   searches. */
int mpe_dqc_max_precip_neighbors = DEFAULT_MAX_PRECIP_NEIGHBORS;
int mpe_dqc_max_temp_neighbors = DEFAULT_MAX_TEMP_NEIGHBORS;
float mpe_dqc_precip_deviation = DEFAULT_PRECIP_DEVIATION;
float mpe_dqc_temperature_deviation = DEFAULT_TEMPERATURE_DEVIATION;
int mpe_dqc_min_good_stations = DEFAULT_MIN_GOOD_STATIONS;
int mpe_copy_level2_dqc_to_ihfs_shef = DEFAULT_DQC_COPY_TO_IHFS;
int mpe_copy_level2_dqc_to_archive_shef = DEFAULT_DQC_COPY_TO_ARCHIVE;

struct tag tag[20];
struct maxmin *maxmin;


/*******************************************************************************
* MODULE NAME: load_gage_data
* PURPOSE:     Retrieves the station data.  This includes observations and
*              climatology data.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME              DESCRIPTION/UNITS
*   input  char *      area_id           The name of the area to retrieve
*                                        dailyqc data for.   
*   input  int         master_file_flag  Indicates whether or not the
*                                        the dataset is the master dataset,
*                                        i.e., is this a subarea?
*   input  int         begin_year        The year of the ending date
*                                        of the dailyqc period.
*   input  int         begin_month       The month of the ending date of the
*                                        dailyqc period.
*   input  int         begin_day         The day of the ending date of the
*                                        dailyqc period.
*   input  int         num_days          The number of days in the dailqc
*                                        period.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

char type [ 5 ];
float cen_lon;
float max_lat;
float min_lat;
float pxtemp = 1.0;
int dmvalue=1.0*100*3.28/.55;
int isohyets_used = -1;
int maxmin_used = -1;
int init_maxmin = -1;
int isom = -1;
int method = 2;
int old_isom = -1;
int tsmax = 0;
struct ts ts [ 20 ];
time_t btim, currentime;

static int prev_smonth = -1;
static int prev_emonth = -1;

static int num_days_to_qc = -1;

int emonth;
int smonth;

int load_gage_data ( const char * area_id,
                     int master_file_flag,
                     int begin_year,
                     int begin_month,
                     int begin_day,
                     int num_days )
{
   struct station * precip_stations = NULL;
   struct station * temperature_stations = NULL; 
   struct station * freezing_stations = NULL;
   char message [ GAGEQC_MESSAGE_LEN ];
   FILE * fp = NULL;
   float giflat [ 5 ];
   float giflon [ 5 ];
   int dqcBasetime;
   static int first = 1;
   int num_precip_gages;
   int num_temp_gages;
   int num_freezing_gages;
   int return_value;
   
   int l;
   int ier;
   int k,kk,mk,num;
   int length;
   int m;
   int maxgif;
   int mer;
   int reply_len;
   int ter,zer;
   char *p = NULL;
   struct tm *gm = NULL;
   struct tm *gmtim = NULL;
   struct hrap_grid *hrap_grid = NULL;
   struct hrap_grid *hrap_tgrid = NULL;
   time_t sget;
   time_t tget;

   // The number of precipitation, temperature and freezing level stations
   //read from the station list. 
   extern int max_stations;
   extern int max_tstations;
   extern int max_zstations;
   extern int pcp_in_use [];

   // Define and initialize the character arrays used 
   // DailyQC. 
   static char ibuf[GAGEQC_MESSAGE_LEN] = {'\0'};
   static char dbuf[GAGEQC_MESSAGE_LEN] = { '\0'};
   static char preca[GAGEQC_FILENAME_LEN] = {'\0'};
   static char precb[GAGEQC_FILENAME_LEN] = {'\0'};
   static char precc[GAGEQC_FILENAME_LEN] = {'\0'};
   static char precd[GAGEQC_FILENAME_LEN] = {'\0'};
   static char prece[GAGEQC_FILENAME_LEN] = {'\0'};
   static char databuf[GAGEQC_FILENAME_LEN] = {'\0'};
   static char zpointa[GAGEQC_FILENAME_LEN] = {'\0'};
   static char zpointb[GAGEQC_FILENAME_LEN] = {'\0'};
   static char tpointa[GAGEQC_FILENAME_LEN] = {'\0'};
   static char tpointb[GAGEQC_FILENAME_LEN] = {'\0'};
   static char tpointc[GAGEQC_FILENAME_LEN] = {'\0'};
   static char tpointd[GAGEQC_FILENAME_LEN] = {'\0'};

   static const char * mpe_basin_file_tok = "mpe_basin_file";
   static const char * mpe_gridmasks_tok = "mpe_gridmask_dir";
   static const char * mpe_rfc_name_tok = "mpe_site_id";
  
   static const char * mpe_archive_dir_tok = "mpe_archive_dir";
   static const char * mpe_gif_dir_tok = "mpe_gif_dir";
   static const char * mpe_gif_location_tok = "mpe_gif_location";

   static const char * mpe_point_precip_dir_tok = "mpe_point_precip_dir";
   static const char * mpe_grid_precip_dir_tok = "mpe_grid_precip_dir";
   static const char * mpe_map_precip_dir_tok = "mpe_map_dir";
   static const char * mpe_bad_precip_dir_tok = "mpe_bad_precip_dir";
   static const char * mpe_dev_precip_dir_tok = "mpe_dev_precip_dir";

   static const char * mpe_point_freezing_dir_tok = "mpe_point_freezing_dir";
   static const char * mpe_grid_freezing_dir_tok = "mpe_grid_freezing_dir";
   static const char * mpe_maz_freezing_dir_tok = "mpe_maz_dir";

   static const char * mpe_point_temperature_dir_tok = 
                       "mpe_point_temperature_dir";
   static const char * mpe_grid_temperature_dir_tok = 
                       "mpe_grid_temperature_dir";
   static const char * mpe_mat_temperature_dir_tok = "mpe_mat_dir";
   static const char * mpe_bad_temperature_dir_tok = 
                       "mpe_bad_temperature_dir";
   static const char * mpe_dev_temperature_dir_tok = 
	               "mpe_dev_temperature_dir";
   static const char * mpe_station_list_dir_tok = 
	               "mpe_station_list_dir";
   static const char * mpe_climo_list_dir_tok =
                       "mpe_climo_dir";
   static const char * mpe_prism_dir_tok = "mpe_prism_dir";

   /* Read tokens which control the how the DailyQC algorithms 
      work. */
   static const char * mpe_dqc_max_precip_neighbors_tok = 
                       "mpe_dqc_max_precip_neighbors";
   static const char * mpe_dqc_max_temp_neighbors_tok =
                       "mpe_dqc_max_temp_neighbors";
   static const char * mpe_dqc_precip_deviation_tok = 
                       "mpe_dqc_precip_deviation";
   static const char * mpe_dqc_temperature_deviation_tok =
                       "mpe_dqc_temperature_deviation";
   static const char * mpe_dqc_min_good_stations_tok =
                       "mpe_dqc_min_good_stations";
   static const char * mpe_copy_level2_dqc_to_ihfs_shef_tok =
                       "mpe_copy_level2_dqc_to_ihfs_shef";
   static const char * mpe_copy_level2_dqc_to_archive_shef_tok =
                       "mpe_copy_level2_dqc_to_archive_shef";

   static char mpe_basin_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_gridmasks_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_archive_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_gif_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_gif_location_tok_val[GAGEQC_FILENAME_LEN]={'\0'};

   static char mpe_point_precip_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_grid_precip_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_map_precip_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_dev_precip_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_bad_precip_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};

   static char mpe_point_freezing_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_grid_freezing_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_maz_freezing_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};

   static char mpe_point_temperature_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_grid_temperature_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_mat_temperature_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_bad_temperature_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_dev_temperature_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};

   static char mpe_station_list_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_climo_list_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_prism_dir_tok_val[GAGEQC_FILENAME_LEN]={'\0'};

   static char mpe_dqc_max_precip_neighbors_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_dqc_max_temp_neighbors_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_dqc_precip_deviation_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_dqc_temperature_deviation_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_dqc_min_good_stations_tok_val[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_copy_level2_dqc_to_ihfs_shef_tok_val[GAGEQC_FILENAME_LEN]
                                                      ={'\0'};
   static char mpe_copy_level2_dqc_to_archive_shef_tok_val[GAGEQC_FILENAME_LEN]
                                                      ={'\0'};


   static char station_list_file[GAGEQC_FILENAME_LEN]={'\0'};


   static char basin_file[GAGEQC_FILENAME_LEN] = {'\0'};
   static char hrap_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char pcpn_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char zpoint1_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char tpoint1_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char hrap_gage_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char hrap_zgage_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char hrap_tgage_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char hrap_grid_mask_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char ngrid_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char station_climo_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char scratch_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char gif_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char gifbuf[GAGEQC_FILENAME_LEN]={'\0'};
   static char snow_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char bad_snow_file[GAGEQC_FILENAME_LEN]={'\0'};
   static char mpe_prism_dir[GAGEQC_FILENAME_LEN]={'\0'};

   
   /* get the current time */
   
   time(&currentime); 

   // Initialize the most recent datetime for which to retrieve data.
   /* In order to allow user access the 12~18Z point data for the most recent day,
      advance one more day from current day if the later than 12Z */
      
   btim=ouptime(begin_year, begin_month, begin_day, 12, 0, 0);
   
   if (currentime > btim)
   {
      btim=ouptime(begin_year, begin_month, begin_day+1, 12, 0, 0);
      hrgt12z = 1;
   }
   else
   {
      hrgt12z = 0;
   }
      
   gmtim=gmtime(&btim);
   emonth=gmtim->tm_mon;
   num_days_to_qc = num_days;
	    
   // Initialize the oldest time to retrieve data for. 
   tget=btim - 24L*3600L*(long)9;
	    
   gm=gmtime(&tget);
   smonth=gm->tm_mon;
   isom=emonth;
   old_isom=emonth;

   /* Only read token val_tokues once. */
   if ( first == 1 )
   {
      dqcBasetime = getDqcBasetime ( );
      dqcBaseIndex = dqcBasetime % 6;
	    
      length = strlen ( mpe_basin_file_tok );
      get_apps_defaults ( ( char * ) mpe_basin_file_tok,
                          &length,
                          mpe_basin_tok_val,
                          & reply_len );

      length = strlen ( mpe_gridmasks_tok );
      get_apps_defaults ( (char * ) mpe_gridmasks_tok, 
   		          & length,  
		          mpe_gridmasks_tok_val, 
		          & reply_len );	

      length = strlen ( mpe_rfc_name_tok );
      get_apps_defaults ( ( char * ) mpe_rfc_name_tok, 
   		          & length, 
		          mpe_rfc_name_tok_val,
		          & reply_len );	

      length = strlen ( mpe_archive_dir_tok );
      get_apps_defaults ( ( char * ) mpe_archive_dir_tok, 
   		          & length,  
		          mpe_archive_dir_tok_val,
		          & reply_len );	

      length = strlen ( mpe_gif_dir_tok );
      get_apps_defaults ( ( char * ) mpe_gif_dir_tok, 
		          & length,  
		          mpe_gif_dir_tok_val,
		          & reply_len );	

      length = strlen ( mpe_gif_location_tok );
      get_apps_defaults (  ( char * ) mpe_gif_location_tok, 
		           & length, 
		           mpe_gif_location_tok_val, 
			   & reply_len );

      length = strlen ( mpe_point_precip_dir_tok );
      get_apps_defaults ( ( char * ) mpe_point_precip_dir_tok, 
		          & length, 
		          mpe_point_precip_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_grid_precip_dir_tok );
      get_apps_defaults ( ( char * ) mpe_grid_precip_dir_tok, 
		          & length, 
		          mpe_grid_precip_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_map_precip_dir_tok );
      get_apps_defaults ( ( char * ) mpe_map_precip_dir_tok, 
		          & length, 
		          mpe_map_precip_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_dev_precip_dir_tok );
      get_apps_defaults ( ( char * ) mpe_dev_precip_dir_tok, 
   		          & length, 
   		          mpe_dev_precip_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_bad_precip_dir_tok );
      get_apps_defaults ( ( char * ) mpe_bad_precip_dir_tok, 
		          & length, 
		          mpe_bad_precip_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_point_freezing_dir_tok );
      get_apps_defaults ( ( char * ) mpe_point_freezing_dir_tok, 
		          & length, mpe_point_freezing_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_grid_freezing_dir_tok );
      get_apps_defaults ( ( char * ) mpe_grid_freezing_dir_tok, 
		          & length, mpe_grid_freezing_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_maz_freezing_dir_tok );
      get_apps_defaults ( ( char * ) mpe_maz_freezing_dir_tok, 
	   	          & length, mpe_maz_freezing_dir_tok_val,
		          & reply_len );

      length = strlen ( mpe_point_temperature_dir_tok );
      get_apps_defaults ( ( char * ) mpe_point_temperature_dir_tok, 
		          & length, 
		          mpe_point_temperature_dir_tok_val, 
		          & reply_len );

      length = strlen ( mpe_grid_temperature_dir_tok );
      get_apps_defaults ( ( char * ) mpe_grid_temperature_dir_tok, 
		          & length, 
		          mpe_grid_temperature_dir_tok_val, 
		          & reply_len );

      length = strlen ( mpe_mat_temperature_dir_tok );
      get_apps_defaults ( ( char * ) mpe_mat_temperature_dir_tok, 
   		          & length, 
		          mpe_mat_temperature_dir_tok_val, 
		          & reply_len );

      length = strlen ( mpe_dev_temperature_dir_tok );
      get_apps_defaults ( ( char * ) mpe_dev_temperature_dir_tok, 
   		          & length, 
		          mpe_dev_temperature_dir_tok_val, 
		          & reply_len );

      length = strlen ( mpe_bad_temperature_dir_tok );
      get_apps_defaults ( ( char * ) mpe_bad_temperature_dir_tok, 
		         & length, 
		          mpe_bad_temperature_dir_tok_val, 
		          & reply_len );

      length = strlen ( mpe_station_list_dir_tok );
      get_apps_defaults ( ( char * ) mpe_station_list_dir_tok,
                          & length,
                          mpe_station_list_dir_tok_val,
                          & reply_len );

      length = strlen ( mpe_climo_list_dir_tok ); 
      get_apps_defaults ( ( char * ) mpe_climo_list_dir_tok,
                          & length,
                         mpe_climo_list_dir_tok_val,
                         & reply_len ) ; 
                       
      length = strlen ( mpe_prism_dir_tok ); 
      get_apps_defaults ( ( char * ) mpe_prism_dir_tok,
                          & length,
                          mpe_prism_dir_tok_val,
                          & reply_len ) ; 

      length = strlen ( mpe_dqc_max_precip_neighbors_tok );
      get_apps_defaults ( ( char * )mpe_dqc_max_precip_neighbors_tok,
                          & length,
                           mpe_dqc_max_precip_neighbors_tok_val,
                          & reply_len );  

      if ( reply_len > 0 )
      {
         mpe_dqc_max_precip_neighbors = 
                            atoi(mpe_dqc_max_precip_neighbors_tok_val );
      }

      length = strlen ( mpe_dqc_max_temp_neighbors_tok );
      get_apps_defaults ( ( char * )mpe_dqc_max_temp_neighbors_tok,
                          & length,
                           mpe_dqc_max_temp_neighbors_tok_val,
                          & reply_len );

      if ( reply_len > 0 )
      {
         mpe_dqc_max_temp_neighbors = 
                        atoi ( mpe_dqc_max_temp_neighbors_tok_val );
      } 

      length = strlen ( mpe_dqc_precip_deviation_tok );
      get_apps_defaults ( ( char * ) mpe_dqc_precip_deviation_tok,
                          & length,
                          mpe_dqc_precip_deviation_tok_val,
                          & reply_len );

      if ( reply_len > 0 )
      {
         mpe_dqc_precip_deviation = 
               atof ( mpe_dqc_precip_deviation_tok_val );
      }

      length = strlen ( mpe_dqc_temperature_deviation_tok );
      get_apps_defaults ( ( char * ) mpe_dqc_temperature_deviation_tok,
                          & length,
                          mpe_dqc_temperature_deviation_tok_val,
                          & reply_len );  
   
      if ( length > 0 )
      {
         mpe_dqc_temperature_deviation = 
                             atof ( mpe_dqc_temperature_deviation_tok_val );
      }

      length = strlen ( mpe_dqc_min_good_stations_tok );
      get_apps_defaults ( ( char * )  mpe_dqc_min_good_stations_tok,
                          & length,
                          mpe_dqc_min_good_stations_tok_val,
                          & reply_len );

      if ( reply_len > 0 )
      {
         mpe_dqc_min_good_stations = atoi ( mpe_dqc_min_good_stations_tok_val );
      }

      length = strlen ( mpe_copy_level2_dqc_to_ihfs_shef_tok ); 
      get_apps_defaults ( ( char * ) mpe_copy_level2_dqc_to_ihfs_shef_tok,
                          & length,
                          mpe_copy_level2_dqc_to_ihfs_shef_tok_val,
                          & reply_len );

      if ( reply_len > 0 )
      {
         if ( mpe_copy_level2_dqc_to_ihfs_shef_tok_val [ 1 ] == 'n' ||
              mpe_copy_level2_dqc_to_ihfs_shef_tok_val [ 1 ] == 'N' )
         {
             mpe_copy_level2_dqc_to_ihfs_shef = 1;
         }
         else
         {
             mpe_copy_level2_dqc_to_ihfs_shef = 0;
         }
      }

      length = strlen ( mpe_copy_level2_dqc_to_archive_shef_tok );
      get_apps_defaults ( ( char * ) mpe_copy_level2_dqc_to_archive_shef_tok,
                          & length,
                          mpe_copy_level2_dqc_to_archive_shef_tok_val,
                          & reply_len );

      if ( reply_len > 0 )
      {
         if ( mpe_copy_level2_dqc_to_archive_shef_tok_val [ 1 ] == 'n' ||
              mpe_copy_level2_dqc_to_archive_shef_tok_val [ 1 ] == 'N' )
         {
            mpe_copy_level2_dqc_to_archive_shef = 1; 
         }
         else
         {
            mpe_copy_level2_dqc_to_archive_shef = 0;
         }
      }

      sprintf(gifbuf,"%s",mpe_gif_location_tok_val);
      sprintf(mpe_prism_dir,"%s", mpe_prism_dir_tok_val);

      /* Parse the gif buffer. */
      if(gifbuf[0]!=0) 
      {
         m=0;

         while(gifbuf[m]!=0)
         {
            if(gifbuf[m]==',')
	       gifbuf[m]=' ';
		  
            m++;    
		  
         }      
	       
         ier=sscanf(gifbuf,"%f %f %f %f %f %f",
   		         &giflat[1],&giflon[1],       
		         &giflat[2],&giflon[2], 
		         &giflat[3],&giflon[3]);
	       
         maxgif=ier/2;
	       
      }
	    
      if(gif_file[0]==0)
      {
         strcpy(gif_file,scratch_file);                  
      }
	    
      if(type[0]==0)
      {
         strcpy(type,"2");
      }
   }

   /* Initialize the base paths of the DQC files. */
   sprintf(station_climo_file,"%s/%s_station_climo_list", 
                              mpe_climo_list_dir_tok_val, 
                              mpe_rfc_name_tok_val);
   sprintf(station_list_file,"%s/%s_station_list", 
                             mpe_station_list_dir_tok_val,
                             area_id);

   sprintf(hrap_gage_file,"%s/precip_neighbor_list_%s",
	             mpe_gridmasks_tok_val,
                     area_id);
   sprintf(hrap_zgage_file,"%s/freezing_neighbor_list_%s",
              mpe_gridmasks_tok_val,
              area_id);
   sprintf(hrap_tgage_file,"%s/temperature_neighbor_list_%s",
  	              mpe_gridmasks_tok_val,
	              area_id);
   sprintf(basin_file,"%s",mpe_basin_tok_val);
   sprintf(hrap_file,"%s/basin_to_grid_%s",mpe_gridmasks_tok_val,
                      mpe_rfc_name_tok_val);
   sprintf(hrap_grid_mask_file,"%s/hsa_to_grid_mask_%s",
           mpe_gridmasks_tok_val,mpe_rfc_name_tok_val);
   sprintf(gif_file,"%s/%s",mpe_gif_dir_tok_val,"gifs");
	    
   /* Create the filenames for the precipitation, freezing level and
      and temperature data.  */
   sprintf(pcpn_file,"%s/precip_1_%s_point_",
                     mpe_point_precip_dir_tok_val, area_id );

   sprintf(proc_pcpn_file,"%s/precip_2_%s_point_",
                     mpe_point_precip_dir_tok_val, area_id );

   sprintf(snow_file,"%s/snow_%s_point_",mpe_point_precip_dir_tok_val, area_id);
   sprintf(bad_snow_file,"%s/bad_snow_%s_point",mpe_bad_precip_dir_tok_val,
           area_id);
   sprintf(pcpn_dev_file,"%s/precip_%s_stddev_",
                         mpe_dev_precip_dir_tok_val, area_id);
   sprintf(pcpn_bad_file,"%s/precip_%s_bad_",
                         mpe_bad_precip_dir_tok_val, area_id);
   sprintf(map_file,"%s/map_%s_",
                    mpe_map_precip_dir_tok_val,
                    area_id );
   sprintf(grid_file,"%s/precip_%s_grid_",mpe_grid_precip_dir_tok_val,
                    area_id );
   sprintf(ngrid_file,"%s/nexrad_grid_",mpe_grid_precip_dir_tok_val);

   // Define the paths to the Freezing Level directories.
   sprintf(zpoint1_file,"%s/freezing_1_%s_point_",
		        mpe_point_freezing_dir_tok_val,
                        area_id );
   sprintf(zpoint2_file,"%s/freezing_2_%s_point_",
		        mpe_point_freezing_dir_tok_val,
                        area_id );

   sprintf(zgrid_file,"%s/freezing_%s_grid_",
                      mpe_grid_freezing_dir_tok_val,
                      area_id );
   sprintf(rsel_file, "%s/maz_%s_",
                      mpe_maz_freezing_dir_tok_val,
                      area_id );

   // Define the paths to the Temperature directories. 
   sprintf(tpoint1_file,"%s/temperature_1_%s_point_",
                        mpe_point_temperature_dir_tok_val,
                        area_id);
   sprintf(tpoint2_file,"%s/temperature_2_%s_point_",
		        mpe_point_temperature_dir_tok_val,
                        area_id);
   sprintf(tgrid_file,  "%s/temperature_%s_grid_",
		        mpe_grid_temperature_dir_tok_val,
                        area_id);
   sprintf(mat_file,    "%s/mat_%s_",
		        mpe_mat_temperature_dir_tok_val,
                        area_id);
   sprintf(temp_dev_file,"%s/temperature_%s_stddev_",
		        mpe_dev_temperature_dir_tok_val,
                        area_id);
   sprintf(temp_bad_file,"%s/temperature_%s_bad_",
		        mpe_bad_temperature_dir_tok_val,
                        area_id);
	    
   // Read the station lists for precpitation, temperature,
   // and freezing level gages. 
   freezing_stations = read_freezing_station_list ( & num_freezing_gages, 
                                                    area_id, 
                                                    master_file_flag );

   if ( freezing_stations == NULL )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read freezing level station list file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }

   // Now, read the temperature gage list. 
   temperature_stations = read_temperature_station_list ( & num_temp_gages, 
                                                          area_id, 
                                                          master_file_flag );

   if ( temperature_stations == NULL )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read temperature station list file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }

   // Now, read the list of precipitation gages. 
   precip_stations = read_precip_station_list ( & num_precip_gages, 
                                                area_id, 
	 	                                master_file_flag );

   if ( precip_stations == NULL )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read precipitation station list file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }

   // Allocate memory for the precip, freezing level and
    //  temperature data in the pdata array. 
   for( m = 0; m < max_stations; ++m )
   {
      for( l = 0; l < MAX_GAGEQC_DAYS; ++l )
      {
         pdata[l].stn[m].scons=calloc(5,sizeof(short int));
         pdata[l].stn[m].rain=calloc(5,sizeof(struct rain));
         pdata[l].stn[m].frain=calloc(5,sizeof(struct rain));
         pdata[l].stn[m].frzlvl=calloc(5,sizeof(int));
         pdata[l].stn[m].snoflag=calloc(5,sizeof(short int));
         pdata[l].stn[m].sflag=calloc(5,sizeof(short int));
         pdata[l].stn[m].srain=calloc(5,sizeof(struct rain));
      }
   }
	    
   for ( m = 0; m < max_tstations; ++m )
   {
      for ( l = 0; l < MAX_GAGEQC_DAYS; ++l )
      {
         tdata[l].stn[m].tlevel1=calloc(6,sizeof(struct tlevel));
         tdata[l].stn[m].tlevel2=calloc(6,sizeof(struct tlevel));
      }
   }
	    
   for ( m=0; m<max_zstations; m++)
   {
      for(l=0;l<MAX_GAGEQC_DAYS;l++)
      {
         zdata[l].stn[m].zlevel1=calloc(5,sizeof(struct zlevel));
         zdata[l].stn[m].zlevel2=calloc(5,sizeof(struct zlevel));
      }
	       
   }

   // Read the climo data for the stations. 
   return_value = init_precip_climo ( station_climo_file, 
                                      precip_stations, 
                                      num_precip_gages );

   if ( return_value == DAILYQC_FAILED )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read precipitation climo file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }

   return_value = init_temperature_climo ( station_climo_file ,
                                          temperature_stations,
                                          num_temp_gages );

   if ( return_value == DAILYQC_FAILED )
   { 
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read temperature climo file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }

   // Read and buffer the monthly precipitation, max temperature and
   // min temperature data.
   return_value = read_mean_monthly_precip ( mpe_prism_dir,
                                             mpe_rfc_name_tok_val, 
                                             smonth, 
                                             emonth);

   if ( return_value == DAILYQC_FAILED )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read temperature precipitation PRISM file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }
    
                              
   return_value = read_mean_monthly_temp ( mpe_prism_dir,
                                           mpe_rfc_name_tok_val, 
                                           smonth, 
                                           emonth);   

   if ( return_value == DAILYQC_FAILED )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message,"Could not read temperature PRISM file.\n"
                      "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }
  
   // Create the predefined gage, freezing level and temperature
   // HRAP grids which will later be used when rendering grids. 
   hrap_grid = map_precip_gages_to_grid ( smonth,
		                          emonth,
			                  hrap_gage_file,
			                  area_id,
			                  precip_stations, 
			                  num_precip_gages);

   if ( hrap_grid == NULL )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf(message, "Could not map precipitation gages to grid.\n"
                       "DailyQC stopped.\n");
      logMessage(message);
      return DAILYQC_FAILED;
   }
   
   return_value = map_freeze_gages_to_grid ( smonth,
			                     emonth,
			                     hrap_grid,
			                     hrap_zgage_file,
			                     area_id,
			                     freezing_stations,
			                     precip_stations,
			                     num_freezing_gages,
			                     num_precip_gages);

   if ( return_value == DAILYQC_FAILED )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN );
      sprintf ( message, "Could not map freezing level points to the\n"
                         "HRAP grid.  DailyQC stopped.\n" );
      logMessage(message);
      return DAILYQC_FAILED;
   }
   							              
   return_value = map_temp_gages_to_grid ( smonth,
 		 	                   emonth,
 			                   hrap_tgage_file,
 			                   area_id,
  			                   temperature_stations, 
 			                   num_temp_gages); 

   if ( return_value == DAILYQC_FAILED )
   {
      memset ( message, '\0', GAGEQC_MESSAGE_LEN );
      sprintf ( message, "Could not map temperature station locations to the\n"
                         "HRAP grid.  DailyQC stopped.\n" );
      logMessage(message);
      return DAILYQC_FAILED;
   }
   
   // Create the HSA mask.  This maps each HRAP grid bin to an HSA.
   // If no file is supplied, then the default grid_mask is set to all 1's. 
   get_hsa_to_grid_mask (  hrap_grid,
                           tag,
                           & wfo_all,
                           hrap_grid_mask_file );

   /* Was the precipitation climatology available? */	    
   if ( isohyets_used==0 )
   {
      method = 1;
   }
	    
   // Read the basin data.  
   // I think this only needs to be done once.
   return_value = get_basin_data ( basin_file, 
                                   hrap_file,
                                   mean_areal_precip_global,
                                   tag );

   if ( return_value == DAILYQC_FAILED )
   {
       memset ( message, '\0', GAGEQC_MESSAGE_LEN  );
       sprintf ( message, "Error retrieving basin data.  DailyQC Stopped.\n");
       logMessage ( message );
       return DAILYQC_FAILED;
   }
                    
   /* Need to change this to read in only the number of days requested by the
      user. */
   for ( m = 0; m < num_days; ++m )
   {
      tget = btim - 24L * 3600L * (long) m ;
	       
      // Read the level 1 precipitation data. 
      pdata[m].stddev=mpe_dqc_precip_deviation;
      tdata[m].stddev=mpe_dqc_temperature_deviation;
	       
      tget = btim - 24L*3600L * (long)m;
	       
      gm=gmttime(&tget);

      // Complete the names of the files containing the precipitation,
      //  freezing level, and temperature data.  This amounts to adding
      //  the current datetime being processed to each file being
      //  processed. 
      sprintf(preca,"%s%04d%02d%02d",
                    pcpn_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(precb,"%s%04d%02d%02d",
                    proc_pcpn_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(precc,"%s%04d%02d%02d",
                    pcpn_dev_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(precd,"%s%04d%02d%02d",
		    pcpn_bad_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(prece,"%s%04d%02d%02d",
                    snow_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(zpointa,"%s%04d%02d%02d",
                    zpoint1_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(zpointb,"%s%04d%02d%02d",
                    zpoint2_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(tpointa,"%s%04d%02d%02d",
                    tpoint1_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(tpointb,"%s%04d%02d%02d",
                    tpoint2_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(tpointd,"%s%04d%02d%02d",
                    temp_bad_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(tpointc,"%s%04d%02d%02d",
                    temp_dev_file,
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);
      sprintf(databuf,"%04d%02d%02d",
                    gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);

      /* Read the snow reports. */
      read_snow(prece,precip_stations,num_precip_gages,m); 
	       
      //  read level 1 precip data  
      mer=read_precip_a(preca,tget,m,pdata,precip_stations,num_precip_gages); 
	       
      // Read the list of bad precipitation values. 
      read_bad_values(precd,m);
       
      //  read level 2 precip data  
      ier=read_precip_b(precb,tget,m,pdata,precip_stations,num_precip_gages); 
	       
      // Read the level 1 freezing level data. 
      read_zlevel_a(zpointa,tget,m,zdata,freezing_stations,
                    num_freezing_gages); 

      zer=read_zlevel_b(zpointb,tget,m,zdata,freezing_stations,
                        num_freezing_gages); 
	       
      read_t_a(tpointa,tget,m,temperature_stations,num_temp_gages); 
	       
      read_bad_tvalues(tpointd,m);
	       
      ter=read_t_b(tpointb,tget,m); 
	       
      //--------------------------------------------------------------
      //  open and read file containing std dev of point precip data  
      //--------------------------------------------------------------
      fp=fopen(precc,"r");
	       
      if ( fp != NULL )
      {
         memset ( message, '\0', GAGEQC_FILENAME_LEN);
         sprintf(message, "%s%s\n", "Opened file: ",precc);
         logMessage(message);
		  
         p=fgets(ibuf,80,fp);
		  
         pdata[m].stddev=atof(ibuf);
		  
         if(pdata[m].stddev != 1.0 && 
            pdata[m].stddev != 3.0 && 
            pdata[m].stddev != 5.0)
            pdata[m].stddev=3.0;
		  
         fclose(fp);
		  
      }
      else
      {
         memset ( message, '\0', GAGEQC_FILENAME_LEN );
         sprintf(message,"%s%s\n","Could not open file",precc);
         logMessage(message);	
      }
	       
       
      // open and read file containing std dev of point temperature data
	       
      fp=fopen(tpointc,"r");

      if ( fp != NULL )
      {
         memset ( message, '\0', GAGEQC_FILENAME_LEN );
         sprintf ( message,"%s%s\n","Opened file", tpointc );
         logMessage ( message );
		  
         p = fgets ( ibuf, 80, fp );
         tdata[m].stddev=atof(ibuf);
		  
         if(tdata[m].stddev != 5.0 && 
            tdata[m].stddev != 10.0 && 
            tdata[m].stddev != 15.0)
         {
            tdata[m].stddev=10.0;
         }
		  
         fclose ( fp );
		  
       }
       else
       {
	  memset ( message, '\0', GAGEQC_MESSAGE_LEN);
	  sprintf(message,"%s%s\n","Could not open file",tpointc);
	  logMessage(message);
       }
	       
       if(ier==1)
       {
          sprintf(dbuf,"%s - level 2 data\n",databuf);
       }
       else if(ier==-1 && mer==1)
       {
          sprintf(dbuf,"%s - level 1 data\n",databuf);
       }      
       else if(ier==-2 && mer==1)
       {
          sprintf(dbuf,"%s - level 1 data overwrite\n",databuf);
       }
       else
       {
          sprintf(dbuf,"%s - no data\n",databuf);
       }

       logMessage( dbuf );

       // Estimate the daily precipitation stations. 
       estimate_daily_stations(m, precip_stations, num_precip_gages);
       estimate_partial_stations(m, precip_stations, num_precip_gages);

       // Quality control the stations. 
       quality_control_stations(m, precip_stations, num_precip_gages);
       check_consistency(m, precip_stations, num_precip_gages);
       restore_bad_values(m, precip_stations, num_precip_gages);
	       
       // Estimate and QC the daily temperature stations. 
       estimate_daily_tstations(m, temperature_stations, num_temp_gages);
       quality_control_tstations(m, temperature_stations, num_temp_gages);
       restore_bad_tvalues(m, temperature_stations, num_temp_gages);
	       
       if ( ier == 1 )
       {
          for ( k=0;k<5;k++)
          {
             if ( k < 2 )
             {
                sget=tget-84600L;
             }
             else
             {
                sget=tget;
             }
		     
     	     gmtim=gmttime(&sget);
		     
             kk=3-k;
		     
             sprintf(dbuf,"%s%s_%04d%02d%02d",ngrid_file,timefile[2][k],
                     gm->tm_year+1900,gm->tm_mon+1,gm->tm_mday);

             if ( k < 4 )
             {
                num=50+m*4+kk;
             }
	     else
             {
                num=90+m;
             }
		     
             mer = read_qpf_grids ( num, dbuf );
		     
             sprintf ( dbuf, "%s%s_%04d%02d%02d", grid_file, timefile[2][k],
		       gm->tm_year+1900, gm->tm_mon+1,gm->tm_mday);
		     
	     if(k < 4)
	     {
                num = m * 4 + kk;
             }
	     else
	     {
	        num=40+m;
	     }
		     
             mer = read_qpf_grids ( num,dbuf );
		     
	     if ( mer==-1 )
             {
                if ( k==4 )
                {
                   mk=1;
                }
                else
                {
                   mk=0;
                }
			
		if ( pdata[m].used[k] != 0 )
                {
                   render_pcp(m,k,mk, num_precip_gages, 
                              precip_stations, hrap_grid, pdata, pcp_in_use);
                   write_qpf_grids(dbuf);
                }
             }
		     
             create_map(num);
          }
		  
       }
	       
       if ( zer == 1 )
       {     
          for(k=0;k<4;k++)
          {
             if ( zdata[m].level[k] != 2 )
             {
                continue;
             }
		     
             if ( k < 2 )
             {
                sget=tget-84600L;
             }
             else
             {
                sget=tget;
             }
		     
             gmtim=gmttime(&sget);
		     
             kk=3-k;
             sprintf(dbuf,"%s%s_%04d%02d%02d",zgrid_file,ztimefile[dqcBaseIndex][k],
		     gm->tm_year+1900, gm->tm_mon+1,gm->tm_mday);
             num=100+m*4+kk;
             mer=read_qpf_grids(num,dbuf);
		     
             if(mer==-1)
             {
                render_z(m,k,0,num_freezing_gages,freezing_stations,
                         hrap_grid,zdata,pcp_in_use);
             }
		     
             make_rsel(num,num-100);            
		     
          }
       }
	       
       if ( ter==1 )
       {     
          for ( k=0; k<6; k++ )
          {
		     
             if(tdata[m].level[k] != 2) continue;
		     
             if (k < 2)
             {
                sget=tget-84600L;
             }
             else
             {
                sget=tget;
             }
		     
             gmtim=gmttime(&sget);
             kk=3-k;
		     
             sprintf(dbuf,"%s%s_%04d%02d%02d",tgrid_file,ttimefile[dqcBaseIndex][k],
		          gm->tm_year+1900, gm->tm_mon+1,gm->tm_mday);
		     
             if (k < 4) 
             {     
                num=150+m*4+kk;
                mer=read_qpf_grids(num,dbuf);
			
		if(mer==-1)
                {
                   render_t6(m,k,0,max_tstations, temperature_stations,
                             hrap_tgrid,tdata,pcp_in_use);
                }
			
		make_mat(num,num-150); 
			
             }
             else if ( k == 4 )
             {
                num=190+m;
                mer=read_qpf_grids(num,dbuf);
			
                if ( mer == -1 )
                {
                   render_t(m,k,1,max_tstations, temperature_stations, 
                   hrap_tgrid, tdata, pcp_in_use);
                }
			
             } 
             else if ( k == 5 )
             {
                num=200+m;
                mer=read_qpf_grids(num,dbuf);
			
                if ( mer == -1 )
                {
                   render_t(m,k,2,max_tstations, temperature_stations, 
                            hrap_tgrid, tdata, pcp_in_use);
                }
             }           
		     
          }
       }	
   }
	    
   get_bad_snotel(bad_snow_file); 
   calculate_zlevel();
	    
   for ( m=0; m < num_days; ++m )
   {
      get_zlevel(m,
                 precip_stations,
                 freezing_stations,
                 num_precip_gages,
                 num_freezing_gages
                 );
   }
	    
   strcpy(dbuf,"Done!!\n");
   first = 0;

   /* Store this information for when the DQC data structures need to be
      deallocated. */
   prev_smonth = smonth;
   prev_emonth = emonth;
   gageqc_performed = 1;

   return DAILYQC_OK;
}

/*******************************************************************************
* MODULE NAME: wasGageQCperformed
* PURPOSE:     Indicates whether or not gage QC data have been loaded
*              MPE Editor needs to know this when shutting down.  It
*              needs to check to determine if QC'd gage data have been
*              saved.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         gageqc_performed            1 = GageQC data have not been
*                                               loaded.
*                                           0 = GageQC data have not been
*                                               loaded.
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None   
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None 
*
********************************************************************************
*/
int wasGageQCperformed ( )
{
   return gageqc_performed;
}

/*******************************************************************************
* MODULE NAME:    turnGageQCflagOff
* PURPOSE:        This turns off the flag which indicates that GageQC
*                 data are displayed. 
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*  None 
*
* DATA FILES AND/OR DATABASE:
*  None  
*
* ERROR HANDLING:
*  None
*
********************************************************************************
*/
void turnGageQCflagOff ( )
{
   gageqc_performed = 0;
}

/*******************************************************************************
* MODULE NAME:    get_prev_start_end_month
* PURPOSE:        Returns the previous starting and ending month
*                 of dailyqc data selected by the user.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   output int *       start_month          The starting month.
*   output int *       end_month            The ending month.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   None 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None 
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
void get_prev_start_end_month ( int * start_month, int * end_month )
{
   * start_month = prev_smonth;
   * end_month = prev_emonth;
}

/*******************************************************************************
* MODULE NAME:  get_num_days_to_qc
* PURPOSE:      Returns the number of days of DailyQC data 
*               the user selected to QC.
*
* ARGUMENTS:
*    None 
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   integer     num_qc_days                 The number of days to
*                                           qc dailyqc data for.
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
int get_num_days_to_qc ( )
{
   return num_days_to_qc;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/load_gage_data.c,v $";
 static char rcs_id2[] = "$Id: load_gage_data.c,v 1.24 2007/07/12 16:10:57 lawrence Exp lawrence $";}
/*  ===================================================  */

}

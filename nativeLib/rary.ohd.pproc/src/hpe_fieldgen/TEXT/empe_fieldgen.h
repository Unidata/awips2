/*******************************************************************************
* FILENAME:              empe_fieldgen.h
*
* DESCRIPTION:         This file contains parameters and
*                      user-defined types for the empe_fieldgen main function.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         January 2007
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   07/2013      JingtaoD          add prototypes for dual pol proudcts DSA/DPR
********************************************************************************
*/

#ifndef EMPE_FIELDGEN_H
#define EMPE_FIELDGEN_H

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "BinarySearch.h"
#include "DbmsAccess.h"
#include "DbmsDefs.h"   /* for LOC_ID_LEN */
#include "GeneralUtil.h"
#include "HourlyPC.h"
#include "convert_hrap.h"    /* Hrap to LatLon Conversion utilities */
#include "empe_constants.h"
#include "empe_data_structs.h"
#include "empe_field_names.h"
#include "empe_params.h"
#include "empe_util.h"
#include "get_os_system.h"
#include "hpn.h"
#include "local_bias_params.h"
#include "read_stage1_decoded.h"
#include "sqlca.h"
#include "time_convert.h"

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

/* Ordering of the elements in this list must be the same as the beginning
 * of the enum DisplayFieldData type in the mpe_field_names.h header file. */

typedef enum {dhrmosaic = 0, bdhrmosaic, ermosaic, avgermosaic, maxermosaic,
              gaugeonly, ebmosaic, lmosaic, mmosaic, mlmosaic, lsatpre,
              p3lmosaic, num_mosaics}mosaicType ;

extern char    currTime[HHMMSS_LEN + 1];
extern char    message[MESSAGE_LEN] ;

extern FILE * logFile ;


extern run_date_struct * ptrRunDate ;

extern geo_data_struct * ptrGeoData ;

extern empe_params_struct * ptrEMPEParams ;

extern gage_table_struct ** ptrGageTable;
extern gage_table_struct ** ptrGageTableP3;

extern gage_table_struct ** ptrQCGageTable;

extern radarLoc_table_struct * ptrRadarLocTable;

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

void apply_mpe_polygons ( double ** real_mosaic,
                          const char * dateYMD,
                          int year,
                          int month,
                          int day,
                          int hour,
                          enum DisplayFieldData field,
                          const geo_data_struct * pGeoData,
                          double factor,
                          int add_flag,
                          int draw_only_persistent );

void    hpe_fieldgen_constructor() ;
void    hpe_fieldgen_constructorByRunTime() ;
void    hpe_fieldgen_constructorByGeodata(int blnMosaic[]) ;
void    constructorForMeanBias(int radarLocNum) ;

void    hpe_fieldgen_destructor() ;

void    editPolygonConstructor ( const geo_data_struct * pGeoData );
void    editPolygonDestructor ( const geo_data_struct * pGeoData );

void    readGeoData(const int hrap_grid_factor,
                 geo_data_struct* pGeoData);

void    readParams(empe_params_struct *) ;

void    readDBParams(empe_params_struct *) ;

void    readRWParams ( RWParams * , long int * ) ;

void    readRWBiasStat ( RWBiasStat * pRWBiasStat,
                      const char * office_id,
                      long int * ircbia ) ;

void    writeParams(const empe_params_struct *) ;

void    hpe_fieldgen_parseArgs(const int , char **, run_date_struct *);

void    readGageData(const run_date_struct * pRunDate ,
                 const empe_params_struct * pMPEParams ,
                 const geo_data_struct *pGeoData ,
                 gage_table_struct ** pGageTable ,
                 gage_table_struct ** pGageTableP3 ,
                 gage_table_struct ** pQCGageTable );

void    checkMultiple( gage_table_struct * pGageTable);

void readGagePrecip ( const int runHours ,
                      char ** datetimes ,
                      const int hrap_grid_factor ,
                      const geo_data_struct *pGeoData,
                      const int  gage_qc,
                      gage_table_struct ** pGageTable,
                      gage_table_struct ** pGageTableP3,
                      int * gageSize,
                      int * gageSizeP3 ) ;

void readPseudoPrecip( const int runHours ,
                       char ** datetimes ,
                       const int hrap_grid_factor ,
                       const geo_data_struct * pGeoData,
                       gage_table_struct ** pGageTable,
                       gage_table_struct ** pGageTableP3,
                       int * gageSize,
                       int * gageSizeP3 ) ;

void read_lightning(char dt[19], int *ihrap, int *jhrap,
                    int *num_strike, long int *irc) ;

double readPrecipLimit( const char * gageID ,
                        const time_t datetime) ;

void checkSpatialConsistency(const empe_params_struct * pMPEParams,
                            const geo_data_struct *pGeoData,
                            const gage_table_struct * pGageTable,
                            int * gageqc) ;

void writeGageQC ( const char * lid,
                   const double value,
                   const char * ts,
                   const int dur,
                   const char * dt,
                   const int qctype,
                   long int *irc,
                   int *messageid ) ;

void writeRWResult ( const char *rfc,
                    const char * dt,
                    const int ngag,
                    const int isat,
                    const int nrad,
                    const char *field,
                    int * overwrt,
                    long int *irc ) ;

void writeArrayConstructor ( const geo_data_struct * pGeoData );
void writeArrayDestructor ( const geo_data_struct * pGeoData );

void writeArray( const geo_data_struct * pGeoData ,
                 const char *    filedir ,
                 const char *    filename ,
                 const double    factor ,
                 const int    replace_missing ,
                 const char *    user ,
                 const time_t    tRunTime ,
                 const char *    proc_flag ,
                 double **    real_mosaic ,
                 long int *    irc ) ;

void writeQPE(const run_date_struct * pRunDate ,
            const empe_params_struct * pMPEParams ,
            const geo_data_struct * pGeoData ,
            const int gageNum ,
            const int gageNumP3 ,
            double ** pQPEMosaic ) ;

void writeRadarResult(const char * rad,
                    const char * dt,
                    const int ngag,
                    const int irad,
                    const double bias_used,
                    const double mem_span_bias,
                    long int * irc) ;

void saveGif( const geo_data_struct * pGeoData ,
                    const char * datestring,
                    const char * filen,
                    double ** mosaic,
                    long int * irc) ;

void saveNetCDF( const int max_x ,
                 const int  max_y,
                 const char * strDateTime ,
                 const char * fname,
                 double ** pMosaic ,
                 const char * proc_flag ,
                 const char * external_dir ,
                 long int *irc) ;

void checkMultisensorQC(const char * datetime ,
                        double ** mosaic ,
                        const geo_data_struct * pGeoData,
                        const gage_table_struct * pGageArray ) ;

void runERMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                empe_params_struct * pMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                const gage_table_struct * pGageTable,
                const gage_table_struct * pGageTableP3,
                const gage_table_struct * pQCGageTable,
                double * meanFieldBias,
                double ** RadarBeamHeight,
                int     ** ID,
                double ** RMosaic,
                double ** QPEMosaic,
                int * blnMosaic) ;

void readRadarLoc ( radarLoc_table_struct * pRadarLocTable ) ;

void readRadarResult (const char * datetime,
                      radar_result_struct * pRadarResult,
                      short * count ,
                      int * dual_pol_flag,
                      long int * irc) ;


void readDAARadarResult(const char * datetime,
                        radar_result_struct * pRadarResult,
                        short * count,
                        int * dual_pol_flag,
                        long int * irc);

void readRadarData(const char * radarID,
                const char * datetime,
                const int    dpa_wind,
                const int    ignoreRadarFlag,
                float radar [ ] [ NUM_DPA_COLS ] ,
                int *    radarAvailFlag) ;

void readDPARadar(const char * rad,
                  const char * datetime,
                  const int idpawind,
                  double * maxvald,
                  double * bias,
                  char * fname,
                  int * itim,
                  long int * irc) ;

void readMisc(const radarLoc_table_struct * pRadarLocTable,
              short int ** radarMiscBins ) ;

void getMeanBias(const radarLoc_record_struct * pRadarLocRecord,
                 const char * datetime ,
                 const int grid_rows,
                 const int grid_cols,
                 short ** radarMiscBins ,
                 float ** radar,
                 const geo_data_struct * pGeoData ,
                 const gage_table_struct * pGageArray ,
                 const empe_params_struct * pMPEParams ,
                 int dualpol_data_avail,
                 double * meanBias,
                 double * memSpanBias,
                 int *  gageRadarPairNum) ;

void retrieveMeanBias(const char * radarID,
                      const char * datetime ,
                      const empe_params_struct * pMPEParams ,
                      int dualpol_data_avail,
                      double * meanBias,
                      double * memSpanBias );

void readMeanBias(const run_date_struct * pRunDate,
                  const radarLoc_table_struct * pRadarLocTable ,
                  const empe_params_struct * pMPEParams ,
                  double * meanFieldBias,
                  int    dualpol_data_avail);

void pairGageRadar(const radarLoc_record_struct * pRadarLocRecord,
                   const int grid_rows,
                   const int grid_cols,
                   short ** radarMiscBins,
                   float ** radar,
                   const geo_data_struct * pGeoData ,
                   const gage_table_struct * pGageArray ,
                   const empe_params_struct * pMPEParams ,
                   gage_radar_pair_table_struct * pGageRadarPairTable) ;

void calculatePixelHeight(const double lon_coord,
                          const double lat_coord,
                          const double hrap_coord_x,
                          const double hrap_coord_y,
                          const double hrap_x,
                          const double hrap_y,
                          double * pixelHeight  ) ;

void calculateMeanBias(const char * radarID,
                    const char * datetime ,
                    const empe_params_struct * pMPEParams ,
                    const gage_table_struct * pGageArray,
                    gage_radar_pair_table_struct * pGageRadarPairTable ,
                    int dualpol_data_avail,
                    double * meanBias,
                    double * memSpanBias ) ;

void gageRadarPairsQC(const empe_params_struct * pMPEParams ,
                const double bias_long ,
                gage_radar_pair_table_struct * pGageRadarPairTable ,
                int * flag) ;

void updateStateVariable(const empe_params_struct * pMPEParams ,
                const float mem_span[] ,
                const int lag ,
                const gage_radar_pair_table_struct * pGageRadarPairTable ,
                double sumGage[] ,
                double sumRadar[] ,
                double num_pairs[] ) ;

void write_rwbiasdyn(const char *rad,
                    const char * dt,
                    const int num_span,
                    double * num_pairs,
                    double * sumgag,
                    double * sumrad,
                    double * bb,
                    long int *irc) ;

void readRWBiasDyn(const char *radar_id,
                     const char *office_id,
                     const char * str,
                     const int lag_cut,
                     double *num_pairs,
                     double *sumgag,
                     double *sumrad,
                     double *bias,
                     int *lag,
                     char sstr1[19],
                     int dualpol_data_avail,
                     long int *irc) ;

void readDAABiasDyn(const char *radar_id,
                    const char *office_id,
                    const char * str,
		    const int lag_cut,
		    double *num_pairs,
		    double *sumgag,
                    double *sumrad,
		    double *bias,
		    int *lag,
		    char sstr1[19],
		    long int *irc);	


void read_spe ( const char * satpre_filename ,
                const geo_data_struct * pGeoData,
                const int hrap_grid_factor,
                double ** pSatPre,
                int * spe_status );

double ** read_satellite ( const run_date_struct * pRunDate,
                           const geo_data_struct * pGeoData,
                           const int hrap_grid_factor,
                           int run_hour,
                           int * is_sat_avail );

void free_spe_memory ( const geo_data_struct * pGeoData,
                       const int hrap_grid_factor );

void free_locbias_memory(const geo_data_struct * pGeoData,
                         const int hrap_grid_factor);

void deleteZeros( int * gageSize,
                          short  * iug, short * ivg, float * zg,
                          double ** mosaic) ;

void get_climate(const char * os, const    int rowSize, const int colSize,
    const char* cem, double ** umeang) ;

void readxmrg(const char * os, const int rowSize, const int colSize,
    const char * fname, const int lenf, const double factor,
    double ** xmrg , int * irc);

void runGageonly(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                empe_params_struct * pMPEParams,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** umeang,
                double ** QPEMosaic) ;


void check_autosave ( const char * rfcname, const int * rfclen,
                    const char dt[ANSI_YEARSEC_TIME_LEN],
                    const int * datelen,
                    int * ioverwrt ) ;

void apply_mfb(const double * mfbias ,
               const int rowSize ,
               const int colSize ,
               int ** ID ,
               double ** RMosaic ,
               double ** BMosaic) ;

int applyLocalBias(const time_t tRunTime ,
                   const geo_data_struct * pGeoData ,
                   const empe_params_struct * pEMPEParams ,
                   double ** ERMosaic ,
                   double ** EBMosaic);

void runEBMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const empe_params_struct * pMPEParams ,
                double * meanFieldBias ,
                int ** ID ,
                double ** RMosaic ,
                double ** BMosaic,
                double ** QPEMosaic) ;

void runLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                empe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** RMosaic ,
                double ** LMosaic,
                double ** QPEMosaic) ;

void lb_gr_pairs ( float gr_min_value ,
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   double ** mosaic ,
                   gage_radar_pair_table_struct * pGageRadarPairTable) ;

void runMMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                empe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RMosaic ,
                double ** BMosaic ,
                double ** umeang ,
                double ** QPEMosaic) ;

void runMLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                empe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RMosaic ,
                double ** LMosaic ,
                double ** umeang ,
                double ** QPEMosaic) ;

void runLSatpre ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  empe_params_struct * pMPEParams,
                  const int gageSize,
                  short * iug ,
                  short * ivg ,
                  float * zg ,
                  double ** RMosaic,
                  double ** LSatpre,
                  double ** QPEMosaic ) ;

int runP3LMosaic ( const run_date_struct * pRunDate,
                   const empe_params_struct * pMPEParams,
                   const radarLoc_table_struct * pRadarLocTable,
                   const gage_table_struct * pGageTableP3,
                   const geo_data_struct * pGeoData,
                   enum DisplayFieldData radar_display_type,
                   double ** P3Mosaic,
                   double ** AvgMosaic,
                   double ** QPEMosaic);

/* Routines for computing local bias fields. */

const local_bias_params * getLocalBiasParams ( );
void local_bias ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  const int gageSize,
                  short * iug ,
                  short * ivg ,
                  float * zg ,
                  empe_params_struct * pMPEParams,
                  const local_bias_params * pLocalBiasParams,
                  float si_cut,
                  gage_radar_pair_table_struct * pGageRadarPair,
                  double ** RMosaic,
                  short int ** local_span,
                  double ** local_bias,
                  const char * dirname,
                  double ** lmosaic,
                  int * ierr ) ;


/* Quick sort routines. */

void qksort4 ( gage_radar_pair_table_struct * pGageRadarTable );
void qksort22 ( int n, float * dd, short * ii );
void qksort32 ( int n, double * dd, short * ii, short * jj );
void qksorti22 ( int n, short * dd, short * ii );

/* heap sort functionss. */

void heapSortForGeoIndex(float heapArray[], short hrap_x[],
                    short hrap_y[], int arraySize) ;
void heapSortForDoubleAndGeoIndex(double heapArray[], int index_x[],
                    int index_y[], int arraySize) ;


/* Routines for handling binary search for station
   latitude and longitude data. */

void free_mpe_latlon_info ( );
void freeRadarLocMemory();

int get_mpe_loc_latlon ( char * lid, double * dlat, double * dlon );
int get_mpe_loc_latlon_list ( int    * arraySize ,
                              double * dlat ,
                              double * dlon ) ;

void  buildNeighborList (const geo_data_struct * pGeoData ,
                         empe_params_struct * pMPEParams,
                         const int gageSize, short * iug,
                         short * ivg, float * zg ) ;

void  findNeighborList (
                        const int radius,
                        const int index_x,
                        const int index_y,
                        short * arrIndex,
                        float * arrDist,
                        int * listNum ) ;

void  findLocalBiasNeighborList (
                        const gage_radar_pair_table_struct * pGageRadarPair,
                        const short * iug,
                        const short * ivg,
                        const int radius,
                        const int index_x,
                        const int index_y,
                        short * arrIndex,
                        float * arrDist,
                        int * listNum );

void find_nbrsX(const int size ,
                short * iu ,
                short * iv ,
                const int iu0 ,
                const int iv0 ,
                const int iradi ,
                int * k ,
                short * ilist ,
                float * rlist ,
                int * iu0_prev ,
                int * m ,
                short * ivv ,
                short * in ) ;

void freeNeighborList(const geo_data_struct * pGeoData) ;

void createMosaic(const radarLoc_record_struct * pRadarLocRecord,
                  const int grid_rows,
                  const int grid_cols,
                  float ** radar ,
                  short ** radarMiscBins,
                  const geo_data_struct * pGeoData ,
                  const int index ,
                  double ** RadarBeamHeight,
                  double ** RMosaic ,
                  double ** MHeight,
                  int ** ID,
                  double  ** MaxMosaic,
                  double  ** AvgMosaic,
                  int     ** AvgMosaicNumRadars,
                  int     *  blnMosaic);

void rfcw_load_static (const int hrap_grid_factor, int * status );

void runDHRMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                empe_params_struct * pMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                double * meanFieldBias,
                double ** radar_bean_height,
                int    ** ID,
                double ** DHRMosaic,
                double ** QPEMosaic);

void runBDHRMosaic(const run_date_struct   * pRunDate ,
                   const geo_data_struct   * pGeoData ,
                   const empe_params_struct * pMPEParams ,
                   double * meanFieldBias ,
                   int    ** ID ,
                   double ** RMosaic ,
                   double ** QPEMosaic);

void createDHRMosaic(const radarLoc_record_struct * pRadarLocRecord,
                     const int grid_rows,
                     const int grid_cols,
                     float ** radar ,
                     short ** radarMiscBins,
                     const geo_data_struct * pGeoData ,
                     const int index ,
                     double ** RadarBeamHeight,
                     double ** height,
                        int ** ID,
                     double ** mosaic);

void readDHRData(const char * radarID,
                 const char * datetime,
                 const int    dhr_wind,
                 const int    ignoreRadarFlag,
                 float ** radar,
                 int *    radarAvailFlag);

void readDPRData(const char * radarID,
                 const char * datetime,
                 const int dhr_wind,
                 const int ignoreRadarFlag,
                 float ** radar,
                 int * radarAvailFlag);


void readDHRRadar(const char * radid,
                  const char * datetime,
                  const int dhr_window,
                  double * prev_bias,
                  double * post_bias,
                  char * prev_filename,
                  char * post_filename,
                  int * prev_offset,
                  int * post_offset,
                  int * status);

void readDPRRadar(const char * radid,
                  const char * datetime,
                  const int dhr_window,
                  double * prev_bias,
                  double * post_bias,
                  char * prev_filename,
                  char * post_filename,
                  int * prev_offset,
                  int * post_offset,
                  int * status);

void readDecodedDHR(const char * filename,
                    float ** radar ,
                    int * status);

void readDecodedDPR(const char * filename,
                   float ** radar,
                   int * status);

void readDecodedDSP(const char * filename,
                    float ** radar,
                    int * status);

void readDecodedDSA(const char * filename,
                   float ** radar,
                   int * status);

void readDSPRadar(const char * radid,
                  const char * datetime,
                  const int dsp_window,
                  const int dsp_duration,
                  const int ignoreRadarFlag,
                  float ** radar,
                  int * radarAvailFlag);
	
void readDSARadar(const char * radid,
                  const char * datetime,
                  const int dsp_window,
                  const int dsp_duration,
                  const int ignoreRadarFlag,
                  float ** radar,
                  int * radarAvailFlag);	

void loadRadarBeamHeight(double ** radar_bean_height,
                         const int grid_rows,
                         const int grid_cols);

void writeFormattedXMRG(const empe_params_struct * pEMPEParams,
                        const geo_data_struct * pGeoData,
                        const char * mosaic_dir,
                        const char * fname_mosaic,
                        const char * proc_flag ,
                        const char * save_grib_token,
                        const char * save_gif_token,
                        const char * gif_dir_token,
                        const char * gif_id_token,
                        const char * save_netcdf_token,
                        const char * netcdf_dir_token,
                        const char * netcdf_id_token,
                        const char * save_jpeg_token,
                        double ** pMosaic);

void read_daabiasdyn(const char *radar_id, 
                     const char *office_id,
                     const char * str,
                     const int lag_cut,
                     double *num_pairs,
                     double *sumgag,
                     double *sumrad,
                     double *bias, 
                     int *lag,
                     char sstr1[19],
                     long int *irc);	
		     
		     
void wrtodb_HPERadarResult(const char   * hpe_productname,
		           const char   * producttime,
                           const empe_params_struct * pEMPEParams,
			   const int    radar_data_source);

#endif /* #ifndef MPE_FIELDGEN_H */

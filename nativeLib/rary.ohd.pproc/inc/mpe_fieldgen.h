/*******************************************************************************
* FILENAME:              mpe_fieldgen.h
*
* DESCRIPTION:         This file contains parameters and
*                      user-defined types for the mpe_fieldgen main function.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         January 5, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*  9/15/2006     P Tilles          added lid to structure gage_radar_pair_struct
*  August 2008   P Tilles          added arrays for Q2
*
********************************************************************************
*/

#ifndef MPE_FIELDGEN_H
#define MPE_FIELDGEN_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "DbmsDefs.h"   /* for LOC_ID_LEN */
#include "DbmsAccess.h"
#include "sqlca.h"
#include "GeneralUtil.h"
#include "read_stage1_decoded.h"
#include "mpe_params.h"
#include "mpe_constants.h"
#include "mpe_field_names.h"
#include "local_bias_params.h"
#include "get_os_system.h"
#include "convert_hrap.h"    /* Hrap to LatLon Conversion utilities */
#include "time_convert.h"
#include "HourlyPC.h"
#include "mpe_write_xmrg.h"

/*--------------------------------*/
/*  definition of constants       */
/*--------------------------------*/

#define MPE_DIRNAME_LEN 128

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

/* Ordering of the elements in this list must be the same as the beginning
 * of the enum DisplayFieldData type in the mpe_field_names.h header file. */
typedef enum {rmosaic = 0, avgrmosaic, maxrmosaic, gaugeonly, bmosaic, lmosaic,
              mmosaic, mlmosaic, satpre, lsatpre, p3lmosaic, srmosaic, sgmosaic, srgmosaic,
              qmosaic, lqmosaic, mlqmosaic, rdmosaic, avgrdmosaic, maxrdmosaic,
              bdmosaic, ldmosaic, mdmosaic, mldmosaic, srdmosaic, srdgmosaic,
              rfcmosaic, rfcbmosaic, rfcmmosaic, num_mosaics
             }mosaicType ;

extern char    currTime[HHMMSS_LEN + 1];
extern char    message[MESSAGE_LEN] ;

extern FILE * logFile ;



typedef struct
{
   int hourNum;            /* Number of hours to be run. */

   time_t tRunTime;        /* ending date & time of runs. */

} run_date_struct;

typedef struct _gage_radar_pair_struct
{

    char lid[ LOC_ID_LEN + 1]; /* The id of the gage */

    int hrap_x ;        /* HRAP x-coordinates of positive radar-gage pair */

    int hrap_y ;        /* HRAP y-coordinates of positive radar-gage pair */

    double gageValue ;    /* positive gage data */

    double radarValue ;    /* positive radar data */

} gage_radar_pair_struct;

typedef struct _gage_radar_pair_table_struct
{
    gage_radar_pair_struct * ptrGageRadarPair;

    int    pairNum ;        /* number of positive radar-gage pairs */

} gage_radar_pair_table_struct;

typedef struct _radar_result_struct
{
    char  radID[ RADAR_ID_LEN + 1];
    short edit_bias;
    short ignore_dpa_radar;
    short ignore_daa_radar;
    float bias ;

} radar_result_struct;

typedef struct _neighbor_list_struct
{
  short listSize ;

  short * pIndex;
  float * pDistance;

} neighbor_list_struct;

extern run_date_struct * ptrRunDate ;

extern geo_data_struct * ptrGeoData ;

extern mpe_params_struct * ptrMPEParams ;

extern gage_table_struct ** ptrGageTable;
extern gage_table_struct ** ptrGageTableP3;

extern gage_table_struct ** ptrQCGageTable;

extern radarLoc_table_struct * ptrRadarLocTable;

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/
void MPEFieldGen_apply_mpe_polygons ( double ** real_mosaic,
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

void    printUsage();
int     isDigits(const char* );
char*   toLowerCase(char* );
int     getAppsDefaults(const char*, char*);
void    printMessage(const char*, FILE * ) ;
void    getCurrentTime(char* strTime) ;

void    constructor() ;
void    constructorByRunTime() ;
void    constructorByGeodata(int blnMosaic[]) ;
void    constructorByRadarLoc(int radarLocNum) ;

void    destructor() ;

void    MPEFieldGen_editPolygonConstructor ( const geo_data_struct * pGeoData );
void    MPEFieldGen_editPolygonDestructor ( const geo_data_struct * pGeoData );

void    mpe_fg_openLogFile(const time_t tDate, const int i) ;
void    deleteLogFiles() ;

void    MPEFieldGen_readGeoData(geo_data_struct *);

void    MPEFieldGen_readParams(mpe_params_struct *) ;

void    MPEFieldGen_readDBParams(mpe_params_struct *) ;

void    MPEFieldGen_readRWParams ( RWParams * , long int * ) ;

void    MPEFieldGen_readRWBiasStat ( RWBiasStat * ,const char *, long int * ) ;

void    MPEFieldGen_writeParams(const mpe_params_struct *) ;

void    parseArgs(const int , char **, run_date_struct *);

void    shutDownMPE(const char *, FILE * logFile);

void    MPEFieldGen_readGageData(const run_date_struct * pRunDate ,
                 const mpe_params_struct * pMPEParams ,
                 const geo_data_struct *pGeoData ,
                 gage_table_struct ** pGageTable ,
                 gage_table_struct ** pGageTableP3 ,
                 gage_table_struct ** pQCGageTable );

void    MPEFieldGen_checkMultiple( gage_table_struct * pGageTable);

void MPEFieldGen_readGagePrecip ( const int runHours ,
                      char ** datetimes ,
                      const geo_data_struct *pGeoData,
                      const int  gage_qc,
                      gage_table_struct ** pGageTable,
                      gage_table_struct ** pGageTableP3,
                      int * gageSize,
                      int * gageSizeP3,
                      const run_date_struct * pRunDate ) ;

void readGagePrecipFromMpeEditor ( const int runHours ,
                      char ** datetimes ,
                      const geo_data_struct *pGeoData,
                      const int  gage_qc,
                      gage_table_struct ** pGageTable,
                      gage_table_struct ** pGageTableP3,
                      int * gageSize,
                      int * gageSizeP3,
                      const run_date_struct * pRunDate ) ;

void MPEFieldGen_readPseudoPrecip( const int runHours ,
                       char ** datetimes ,
                       const geo_data_struct * pGeoData,
                       gage_table_struct ** pGageTable,
                       gage_table_struct ** pGageTableP3,
                       int * gageSize,
                       int * gageSizeP3 ) ;

void MPEFieldGen_read_lightning(char dt[19], int *ihrap, int *jhrap,
                    int *num_strike, long int *irc) ;

double MPEFieldGen_readPrecipLimit( const char * gageID ,
                        const time_t datetime) ;

void MPEFieldGen_checkSpatialConsistency(const mpe_params_struct * pMPEParams,
                            const geo_data_struct *pGeoData,
                            const gage_table_struct * pGageTable,
                            int * gageqc) ;

void MPEFieldGen_writeGageQC ( const char * lid,
                   const double value,
                   const char * ts,
                   const int dur,
                   const char * dt,
                   const int qctype,
                   long int *irc,
                   int *messageid ) ;

void MPEFieldGen_writeRWResult ( const char *rfc,
                    const char * dt,
                    const int ngag,
                    const int isat,
                    const int nrad,
                    const char *field,
                    int * overwrt,
                    long int *irc ) ;

void MPEFieldGen_writeArrayConstructor ( const geo_data_struct * pGeoData );
void MPEFieldGen_writeArrayDestructor ( const geo_data_struct * pGeoData );

void MPEFieldGen_writeArray( const geo_data_struct * pGeoData ,
                 const char *    filedir ,
                 const char *    filename ,
                 const double    factor ,
                 const int    replace_missing ,
                 const char *    user ,
                 const time_t    tRunTime ,
                 const char *    proc_flag ,
                 double **    real_mosaic ,
                 long int *    irc ) ;

void MPEFieldGen_writeQPE(const run_date_struct * pRunDate ,
            const mpe_params_struct * pMPEParams ,
            const geo_data_struct * pGeoData ,
            const int gageNum ,
            const int gageNumP3 ,
            double ** pQPEMosaic ) ;

void MPEFieldGen_writeRadarResult(const char * rad,
                    const char * dt,
                    const int ngag,
                    const int irad,
                    const double bias_used,
                    const double mem_span_bias,
                    long int * irc) ;

void MPEFieldGen_saveGif( const geo_data_struct * pGeoData ,
                    const char * datestring,
                    const char * filen,
                    double ** mosaic,
                    long int * irc) ;

void MPEFieldGen_saveNetCDF( const int max_x ,
         const int max_y,
         const char * fname,
         const char * filename_xmrg ,
         long int *irc) ;

void MPEFieldGen_checkMultisensorQC(const char * datetime ,
                        double ** mosaic ,
                        const geo_data_struct * pGeoData,
                        const gage_table_struct * pGageArray ) ;

void MPEFieldGen_runRMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                mpe_params_struct * pMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                const gage_table_struct * pGageTable,
                const gage_table_struct * pGageTableP3,
                const gage_table_struct * pQCGageTable,
                double * meanFieldBias,
                int     ** ID,
                double ** RMosaic,
                double ** QPEMosaic,
                int * blnMosaic) ;

void MPEFieldGen_runRDMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                mpe_params_struct * pMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                const gage_table_struct * pGageTable,
                const gage_table_struct * pGageTableP3,
                const gage_table_struct * pQCGageTable,
                double * meanFieldBias,
                int ** IDDP,
                double ** RDMosaic,
                double ** QPEMosaic,
                int * blnMosaic) ;

void MPEFieldGen_readRadarLoc ( radarLoc_table_struct * pRadarLocTable ) ;

void MPEFieldGen_readRadarResult (const char * datetime,
                      radar_result_struct * pRadarResult,
                      short * count ,
                      long int * irc) ;

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

void MPEFieldGen_readDAARadar(const char * rad, 
                  const char * datetime, 
                  const int idpawind,
                  double * maxvald,
                  double * bias,
                  char * fname,
                  int * itim,
                  int * coverageDur,
                  int * nullProductFlag,
                  long int * irc) ;

void MPEFieldGen_readDAARadarResult (const char * datetime,
                      radar_result_struct * pRadarResult,
                      short * count ,
                      long int * irc);

void MPEFieldGen_readMisc(const radarLoc_table_struct * pRadarLocTable, 
              const char * os,
              short int ** radarMiscBins ) ;

void MPEFieldGen_getMeanBias(const radarLoc_record_struct * pRadarLocRecord,
                const char * datetime ,
                short int radarMiscBins [ ] [ NUM_DPA_COLS ] ,
                float radar [ ] [ NUM_DPA_COLS ] ,
                const geo_data_struct * pGeoData ,
                const gage_table_struct * pGageArray ,
                const mpe_params_struct * pMPEParams ,
                double * meanBias,
                double * memSpanBias,
                int *  gageRadarPairNum) ;

void MPEFieldGen_getMeanBiasDP(const radarLoc_record_struct * pRadarLocRecord,
                const char * datetime ,
                short int radarMiscBins [][NUM_DPA_COLS] ,
                float radar [][NUM_DPA_COLS] ,
                const geo_data_struct * pGeoData ,
                const gage_table_struct * pGageArray ,
                const mpe_params_struct * pMPEParams ,
                double * meanBias,
                double * memSpanBias,
                int *  gageRadarPairNum) ;
                
void MPEFieldGen_pairGageRadar(const radarLoc_record_struct * pRadarLocRecord,
                short int radarMiscBins [][NUM_DPA_COLS] , 
                float radar [][NUM_DPA_COLS] ,
                const geo_data_struct * pGeoData ,
                const gage_table_struct * pGageArray ,
                const mpe_params_struct * pMPEParams ,
                gage_radar_pair_table_struct * pGageRadarPairTable) ;

void MPEFieldGen_calculatePixelHeight(const double lon_coord,
                          const double lat_coord,
                          const double hrap_coord_x,
                          const double hrap_coord_y,
                          const double hrap_x,
                          const double hrap_y,
                          double * pixelHeight  ) ;

void MPEFieldGen_calculateMeanBias(const char * radarID,
                    const char * datetime ,
                    const mpe_params_struct * pMPEParams ,
                    const gage_table_struct * pGageArray,
                    gage_radar_pair_table_struct * pGageRadarPairTable ,
                    double * meanBias,
                    double * memSpanBias ) ;

void MPEFieldGen_calculateMeanBiasDP(const char * radarID,
                    const char * datetime ,
                    const mpe_params_struct * pMPEParams ,
                    const gage_table_struct * pGageArray,
                    gage_radar_pair_table_struct * pGageRadarPairTable ,
                    double * meanBias,
                    double * memSpanBias ) ;

void MPEFieldGen_gageRadarPairsQC(const mpe_params_struct * pMPEParams ,
                const double bias_long ,
                gage_radar_pair_table_struct * pGageRadarPairTable ,
                int * flag) ;

void MPEFieldGen_updateStateVariable(const mpe_params_struct * pMPEParams ,
                const float mem_span[] ,
                const int lag ,
                const gage_radar_pair_table_struct * pGageRadarPairTable ,
                double sumGage[] ,
                double sumRadar[] ,
                double num_pairs[] ) ;

void MPEFieldGen_write_rwbiasdyn(const char *rad,
                    const char * site_id,
                    const char * dt,
                    const int num_span,
                    double * num_pairs,
                    double * sumgag,
                    double * sumrad,
                    double * bb,
                    long int *irc) ;

void MPEFieldGen_write_daabiasdyn(const char *rad,
                    const char * site_id,
                    const char * dt,
                    const int num_span,
                    double * num_pairs,
                    double * sumgag, 
                    double * sumrad,
                    double * bb,
                    long int *irc) ;

void MPEFieldGen_read_rwbiasdyn2(const char *rad, 
                    const char * site_id,
                    const char * str,
                    const int lag_cut,
                    double *num_pairs,
                    double *sumgag,
                    double *sumrad,
                    double *bias,
                    int *lag,
                    char sstr1[19],
                    long int *irc) ;

void MPEFieldGen_read_daabiasdyn(const char *rad, 
                    const char * site_id, 
                    const char * str,
                    const int lag_cut,
                    double *num_pairs,
                    double *sumgag,
                    double *sumrad,
                    double *bias, 
                    int *lag,
                    char sstr1[19],
                    long int *irc) ;

void MPEFieldGen_read_spe ( const char * satpre_filename ,
                const geo_data_struct * pGeoData,
                double ** pSatPre,
                int * spe_status );

double ** MPEFieldGen_read_satellite ( const run_date_struct * pRunDate,
                           const geo_data_struct * pGeoData,
                           int run_hour,
                           int * is_sat_avail );

void MPEFieldGen_free_spe_memory ( const geo_data_struct * pGeoData );

void MPEFieldGen_deleteZeros( int * gageSize,
                  short  * iug, short * ivg, float * zg,
                  double ** mosaic) ;

void MPEFieldGen_get_climate(const char * os,
                 const int rowSize, const int colSize,
                 const int mon, double ** umeang) ;

void MPEFieldGen_readxmrg(const char * os, const int rowSize, const int colSize,
    const char * fname, const int lenf, const double factor,
    double ** xmrg , int * irc);

void MPEFieldGen_runGageonly(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                mpe_params_struct * pMPEParams,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** umeang,
                double ** QPEMosaic) ;


void MPEFieldGen_check_autosave ( const char * rfcname, const int * rfclen,
                    const char dt[ANSI_YEARSEC_TIME_LEN],
                    const int * datelen,
                    int * ioverwrt ) ;

void MPEFieldGen_apply_mfb(const double * mfbias ,
               const int rowSize ,
               const int colSize ,
               int ** ID ,
               double ** RMosaic ,
               double ** BMosaic) ;

void runBMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                double * meanFieldBias ,
                int ** ID ,
                double ** RMosaic ,
                double ** BMosaic,
                double ** QPEMosaic) ;

void runBDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                double * meanFieldBias ,
                int ** ID ,
                double ** RDMosaic ,
                double ** BDMosaic,
                double ** QPEMosaic) ;

void runMLDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RDMosaic ,
                double ** LDMosaic ,
                double ** umeang ,
                double ** QPEMosaic) ;

void runMDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RDMosaic ,
                double ** BDMosaic ,
                double ** umeang ,
                double ** QPEMosaic) ;

void MPEFieldGen_runLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** RMosaic ,
                double ** LMosaic,
                double ** QPEMosaic) ;

void runLDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** RDMosaic ,
                double ** LDMosaic,
                double ** QPEMosaic) ;

void runQMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                double ** QMosaic,
                double ** MPEFieldGen_QPEMosaic);

void runLQMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** QMosaic ,
                double ** LQMosaic,
                double ** MPEFieldGen_QPEMosaic) ;

void MPEFieldGen_lb_gr_pairs ( float gr_min_value ,
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   double ** mosaic ,
                   gage_radar_pair_table_struct * pGageRadarPairTable) ;

void MPEFieldGen_runMMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RMosaic ,
                double ** BMosaic ,
                double ** umeang ,
                double ** QPEMosaic) ;

void MPEFieldGen_runMLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RMosaic ,
                double ** LMosaic ,
                double ** umeang ,
                double ** QPEMosaic) ;

void runMLQMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** Q2ID ,
                double ** QMosaic ,
                double ** LQMosaic ,
                double ** umeang ,
                double ** MPEFieldGen_QPEMosaic) ;

void runSatpre ( const run_date_struct * pRunDate,
                 const geo_data_struct * pGeoData,
                 mpe_params_struct * pMPEParams,
                 double ** QPEMosaic ) ;

void MPEFieldGen_runLSatpre ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  mpe_params_struct * pMPEParams,
                  const int gageSize,
                  short * iug ,
                  short * ivg ,
                  float * zg ,
                  double ** RMosaic,
                  double ** LSatpre,
                  double ** QPEMosaic ) ;

void runSRMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                int    ** ID, 
                double ** LSatpre ,
                double ** LMosaic ,
                double ** QPEMosaic) ;

void runSGMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** LSatpre ,
                double ** umeang ,
                double ** QPEMosaic) ;

void runSRGMosaic(const run_date_struct * pRunDate ,
                 const geo_data_struct * pGeoData ,
                 mpe_params_struct * pMPEParams ,
                 const int gageSize,
                 short * iug ,
                 short * ivg ,
                 float * zg ,
                 double ** umeang ,
                 double ** QPEMosaic) ;

void runSRDGMosaic(const run_date_struct * pRunDate ,
                 const geo_data_struct * pGeoData ,
                 mpe_params_struct * pMPEParams ,
                 const int gageSize,
                 short * iug ,
                 short * ivg ,
                 float * zg ,
                 double ** umeang ,
                 double ** QPEMosaic) ;

int MPEFieldGen_runP3LMosaic ( const run_date_struct * pRunDate,
                   const mpe_params_struct * pMPEParams,
                   const radarLoc_table_struct * pRadarLocTable,
                   const gage_table_struct * pGageTableP3,
                   const geo_data_struct * pGeoData,
                   enum DisplayFieldData radar_display_type,
                   double ** P3Mosaic,
                   double ** AvgMosaic,
                   double ** QPEMosaic);

void runRfcMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                double ** QPEMosaic);

void runRfcBMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                const radarLoc_table_struct * ptrRadarLocTable,
                double * meanFieldBias ,
                int ** ID ,
                double ** RMosaic ,
                double ** RfcBMosaic,
                double ** QPEMosaic);

void runRfcMMosaic(const run_date_struct * pRunDate ,
                   const geo_data_struct * pGeoData ,
                   mpe_params_struct * pMPEParams ,
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   int ** ID ,
                   double ** RMosaic ,
                   double ** RfcBMosaic ,
                   double ** umeang ,
                   double ** QPEMosaic);

/* Routines for computing local bias fields. */
const local_bias_params * MPEFieldGen_getLocalBiasParams ( );
void MPEFieldGen_local_bias ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  const int gageSize,
                  short * iug ,
                  short * ivg ,
                  float * zg ,
                  mpe_params_struct * pMPEParams,
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
void sort_gage_radar_table ( gage_radar_pair_table_struct * pGageRadarTable );
void MPEFieldGen_qksort22 ( int n, float * dd, short * ii );
void sort_float_short ( int n, float * dd, short * ii );
void MPEFieldGen_qksort32 ( int n, double * dd, short * ii, short * jj );
void sort_double_short_short ( int n, double * dd, short * ii, short * jj );
void MPEFieldGen_qksorti22 ( int n, short * dd, short * ii );
void sort_short_short ( int n, short * dd, short * ii );

/* heap sort functionss. */
void MPEFieldGen_heapSortForGeoIndex(float heapArray[], short hrap_x[],
                    short hrap_y[], int arraySize) ;
void MPEFieldGen_heapSortForDoubleAndGeoIndex(double heapArray[], int index_x[],
                    int index_y[], int arraySize) ;


void hrap_to_latlon(const double hrap_x ,
                const double hrap_y ,
                double * flon ,
                double * flat );

void hrapsize(const double flat , double * rmesh);

/* Routines for handling binary search for station
   latitude and longitude data. */
void free_mpe_latlon_info ( );
int get_mpe_loc_latlon ( char * lid, double * dlat, double * dlon );
int get_mpe_loc_latlon_list ( int    * arraySize ,
                              double * dlat ,
                              double * dlon ) ;

void  MPEFieldGen_buildNeighborList (const geo_data_struct * pGeoData ,
                         mpe_params_struct * pMPEParams,
                         const int gageSize, short * iug,  short * ivg, float * zg ) ;

void  MPEFieldGen_findNeighborList (
                        const int radius,
                        const int index_x,
                        const int index_y,
                        short * arrIndex,
                        float * arrDist,
                        int * listNum ) ;

void  MPEFieldGen_findLocalBiasNeighborList (
                        const gage_radar_pair_table_struct * pGageRadarPair,
                        const short * iug,
                        const short * ivg,
                        const int radius,
                        const int index_x,
                        const int index_y,
                        short * arrIndex,
                        float * arrDist,
                        int * listNum );

void MPEFieldGen_find_nbrsX(const int size ,
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

void MPEFieldGen_freeNeighborList(const geo_data_struct * pGeoData) ;

void MPEFieldGen_createMosaic(const radarLoc_record_struct * pRadarLocRecord,
                  float radar [ ][ NUM_DPA_COLS ] ,
                  short int radarMiscBins [ ][ NUM_DPA_COLS],
                  const int index ,
                  const geo_data_struct * pGeoData ,
                  double ** RMosaic ,
                  double ** MHeight,
                  int ** ID,
                  double  ** MaxMosaic,
                  double  ** AvgMosaic,
                  int     ** AvgMosaicNumRadars,
                  int     *  blnMosaic);

void createRDMosaic(const radarLoc_record_struct * pRadarLocRecord,
                  float radar [ ][ NUM_DPA_COLS ] ,
                  short int radarMiscBins [ ][ NUM_DPA_COLS],
                  const int index ,
                  const geo_data_struct * pGeoData ,
                  double ** RDMosaic ,
                  double ** MHeight,
                  int     ** radarIDDP,
                  double  ** MaxRDMosaic,
                  double  ** AvgRDMosaic,
                  int     ** AvgRDMosaicNumRadars,
                  int     *  blnMosaic);

void MPEFieldGen_rfcw_load_static ( int * status );
void get_rgb(const char* color_name, int *r, int *g, int *b);
void read_daa_decoded ( const char * filename ,
                        const int * fname_length ,
                        float radar [ ] [ NUM_DPA_COLS ] , 
                        int * ierr ) ; 

#endif /* #ifndef MPE_FIELDGEN_H */

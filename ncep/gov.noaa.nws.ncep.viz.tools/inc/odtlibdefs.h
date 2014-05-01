/* various constants */
int kstart_v64=24;     /* inner cloud region analysis radius (km) */
int kend_v64=136;      /* outer cloud region analysis radius (km) */
int kenda_v64=190;     /* automated cursor position analysis radius (km) */
int keyerM_v64=24;     /* outer eye region search radius (km) - Manual position */
int keyerA_v64=75;     /* outer eye region search radius (km) - Auto position */
int kres_v64=4;        /* width of the cloud region analysis rings */
int arfd_v64;          /* the FILE id for the image in question */

/* global variables */
char   eyetype_v64[7][20]={ "CLEAR","PINHOLE","LARGE CLEAR","LARGE RAGGED","RAGGED","OBSCURED","NONE" };
char cloudtype_v64[6][20]={ "UNIFORM CDO","EMBEDDED CENTER","IRREGULAR CDO","CURVED BAND","SHEAR","EYE" };
char     cbasin_v64[2][9]={ "ATLANTIC","PACIFIC " };
char      cmon_v64[12][4]={ "JAN","FEB","MAR","APR","MAY","JUN",
                        "JUL","AUG","SEP","OCT","NOV","DEC" };
/* T#-Pressure/Wind relationships (Atlantic and Pacific) */
float tno_v64[73]={-9999.,-8888.,
   1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,
   2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,
   3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,
   4.0,4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,4.9,
   5.0,5.1,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9,
   6.0,6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9,
   7.0,7.1,7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.9,8.0};

float pres_v64[2][73]= {
/* Atlantic pressure relationship values */
{-9999.0,-8888.0,
  1014.0,1013.6,1013.2,1012.8,1012.4,1012.0,1011.4,1010.8,1010.2,1009.6,
  1009.0,1008.2,1007.4,1006.6,1005.8,1005.0,1004.0,1003.0,1002.0,1001.0,
  1000.0,998.8,997.6,996.4,995.2,994.0,992.6,991.2,989.8,988.4,
   987.0,985.4,983.8,982.2,980.6,979.0,977.2,975.4,973.6,971.8,
   970.0,968.0,966.0,964.0,962.0,960.0,957.6,955.2,952.8,950.4,
   948.0,945.4,942.8,940.2,937.6,935.0,932.2,929.4,926.6,923.8,
   921.0,918.0,915.0,912.0,909.0,906.0,902.8,899.6,896.4,893.2,890.0},
/* Pacific pressure relationship values */
{-9999.0,-8888.0,
  1005.0,1004.6,1004.2,1003.8,1003.4,1003.0,1002.4,1001.8,1001.2,1000.6,
  1000.0,999.4,998.8,998.2,997.6,997.0,995.8,994.6,993.4,992.2,
   991.0,989.6,988.2,986.8,985.4,984.0,982.4,980.8,979.2,977.6,
   976.0,974.0,972.0,970.0,968.0,966.0,963.6,961.2,958.8,956.4,
   954.0,951.4,948.8,946.2,943.6,941.0,938.2,935.4,932.6,929.8,
   927.0,924.4,921.8,919.2,916.6,914.0,910.8,907.6,904.4,901.2,
   898.0,894.2,890.4,886.6,882.8,879.0,874.8,870.6,866.4,862.2,858.0} };

/* Atlantic/Pacific pressure relationship values */
float wind_v64[73]={-9999.0,-8888.0,
    25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 26.0, 27.0, 28.0, 29.0,
    30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 37.0, 39.0, 41.0, 43.0,
    45.0, 47.0, 49.0, 51.0, 53.0, 55.0, 57.0, 59.0, 61.0, 63.0,
    65.0, 67.4, 69.8, 72.2, 74.6, 77.0, 79.6, 82.2, 84.8, 87.4,
    90.0, 92.4, 94.8, 97.2, 99.6,102.0,104.6,107.2,109.8,112.4,
   115.0,117.4,119.8,122.2,124.6,127.0,129.6,132.2,134.8,137.4,
   140.0,143.0,146.0,149.0,152.0,155.0,158.0,161.0,164.0,167.0,170.0};

/* BD curve break points */
float ebd_v64[10]={ 30.0, -0.0,-30.0,-42.0,-54.0,
               -64.0,-70.0,-76.0,-80.0,-100.0};

/* AODT library global variables */
logical odt_v64,olist_v64,oautomode_v64,override_v64;
logical ostartstr_v64,oland_v64,osearch_v64;
int     idomain_v64,ixdomain_v64,ifixtype_v64;
float   osstr_v64;
float   spiralband_v64[2][37];
float   fcstlat_v64[5],fcstlon_v64[5];
double  fcsttime_v64[5],starttime_v64,endtime_v64;

/* XXX char    hfile_v64[200]="\0",fixfile_v64[200]="\0",iout[500]="\0"; */
/* XXX char    cursat[7]="\0",atcftype_v64[5]="\0"; */
/* XXX char    diagnostics_v64[5000]="\0"; */
char    *hfile_v64,*fixfile_v64; /* XXX */
char    *atcftype_v64; /* XXX */
char    *diagnostics_v64; /* XXX */ 

struct  odtdata *odthistoryfirst_v64;
struct  odtdata *odtcurrent_v64;
struct  ringdata *tcircfirst_v64;
struct  datagrid *areadata_v64;

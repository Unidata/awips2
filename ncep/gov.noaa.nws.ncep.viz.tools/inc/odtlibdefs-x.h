extern int     kstart_v64,kend_v64,kenda_v64,keyerM_v64,keyerA_v64,kres_v64,arfd_v64;
extern char    eyetype_v64[7][20],cloudtype_v64[6][20],cbasin_v64[2][9],cmon_v64[12][4];
extern float   tno_v64[73],pres_v64[2][73],wind_v64[73],ebd_v64[10];

extern logical odt_v64,olist_v64,oautomode_v64,override_v64;
extern logical ostartstr_v64,oland_v64,osearch_v64;
extern int     idomain_v64,ixdomain_v64,ifixtype_v64;
extern float   osstr_v64;
extern float   spiralband_v64[2][37];
extern float   fcstlat_v64[5],fcstlon_v64[5];
extern double  fcsttime_v64[5],starttime_v64,endtime_v64;

/* XXX extern char    hfile_v64[200]="\0",fixfile_v64[200]="\0",iout[500]="\0"; */
/* XXX extern char    cursat[7]="\0",atcftype_v64[5]="\0"; */
/* XXX extern char    diagnostics_v64[5000]="\0"; */
extern char    *hfile_v64,*fixfile_v64; /* XXX */
extern char    *atcftype_v64; /* XXX */
extern char    *diagnostics_v64; /* XXX */

extern struct  odtdata *odthistoryfirst_v64;
extern struct  odtdata *odtcurrent_v64;
extern struct  ringdata *tcircfirst_v64;
extern struct  datagrid *areadata_v64;

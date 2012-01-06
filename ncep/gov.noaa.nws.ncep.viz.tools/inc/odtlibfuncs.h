/* these are functions that are called/shared within the AODT library */
/* defined in odtfuncs.c */
extern double aodtv64_calctime( int, int );
extern float aodtv64_slopecal( double,int );
extern float aodtv64_getpwval( int,float );
extern float aodtv64_PWlandsea( float );
extern float aodtv64_ptovmax( float );
extern float aodtv64_ptotno( float );
extern int aodtv64_cmonth2julian( char * );
extern void aodtv64_julian2cmonth( int, char * );
extern void aodtv64_distance( float,float,float,float,int,float *,float * );
extern void aodtv64_distance2( float, float, float, float, float *, float * );
extern float aodtv64_atoif( char *, int, int );
extern void aodtv64_calcskew( float *, int, float *, float *, float * );
extern int aodtv64_idmyyd( int,int,int );
extern void aodtv64_yddmy( int,int *,int *,int * );
extern int  aodtv64_oceanbasin( float,float );
extern int  aodtv64_sattypes( int,char * ); /* XXX */
extern int aodtv64_initcurrent(int);

/* defined in odtintensity.c */
extern int aodtv64_calcintensity(void);

/* defined in odtscene.c */
extern void aodtv64_classifyredo(void);

/* defined in odtfft.c */
extern int aodtv64_fft( float *, float *, int * );

/* defined in odtauto.c */
extern void aodtv64_logspiral( float, float, float, int, int *, float *, float * );

/* defined in odthistory.c */
extern int aodtv64_datetime(int,int,char *,char *,int); /* XXX */
extern int aodtv64_deletehistoryrec(int *);
extern int aodtv64_listhistory(struct odtdata *,int,char *,char *); /* XXX */

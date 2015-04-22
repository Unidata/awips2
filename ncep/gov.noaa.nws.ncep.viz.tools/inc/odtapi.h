struct irdata {
  int date;
  int time;
  float TrawO;
  float Traw;
  float Tfinal;
  float Tfinal3;
  float CI;
  float eyet;
  float warmt;
  float cloudt;
  float cloudt2;
  float cwcloudt;
  float latitude;
  float longitude;
  float warmlatitude;
  float warmlongitude;
  float eyesize;
  float eyestdv;
  float cloudsymave;
  int sattype;
  int eyescene;
  int cloudscene;
  int eyesceneold;
  int cloudsceneold;
  int rule9;
  int rule8;
  int land;
  int eyefft;
  int cloudfft;
  int cwring;
  int ringcb;
  int ringcbval;
  int ringcbvalmax;
  float ringcblatmax;
  float ringcblonmax;
  float CIadjp;
  float sst;
  float TIEraw;
  float TIEavg;
  int   TIEflag;
  int autopos;
  int LBflag;
  float rmw;
  char comment[50];
  /* char *comment; */
};

struct odtdata {
  struct irdata IR;
  struct odtdata *nextrec; };

/* library functions defined in odtapi */
extern int aodtv64_sethistoryfile(char *); /* XXX */
extern int aodtv64_gethistoryfile(char *); /* XXX */
extern int aodtv64_setforecastfile(char *,int,char *); /* XXX */
extern int aodtv64_getforecastfile(char *,int *,char *); /* XXX */
extern int aodtv64_setdomain(int);
extern int aodtv64_getdomain(int *);
extern int aodtv64_setIRimageinfo(int,int,int);
extern int aodtv64_getIRimageinfo(int *,int *,int *,char *); /* XXX */
extern int aodtv64_setlocation( float, float, int );
extern int aodtv64_getlocation( float *, float *, int * );
extern int aodtv64_setmiscoptions(int,int);
extern int aodtv64_getmiscoptions(int *,int *);
extern int aodtv64_setstartstr(int,float);
extern int aodtv64_getstartstr(int *,float *);
extern int aodtv64_setsstvalue(float);
extern int aodtv64_getsstvalue(float *);
extern int aodtv64_settopovalue(float);
extern int aodtv64_gettopovalue(int *);
extern int aodtv64_getversion(char *); /* XXX */

extern int aodtv64_runautomode1(float *,float *,int *);
extern int aodtv64_runautomode2(float,float,float *,float *,int *);   /* changed 1-10-05 */
extern int aodtv64_getwarmeyetemplocation(float *,float *);

extern int aodtv64_readtopofile(char *,float,float,int *); /* XXX */
//extern int aodtv64_loadIRimage(float **,float **,float **,int,int);
extern int aodtv64_loadIRimage(float temps[],float lats[],float lons[],int numx,int numy );
//extern int aodtv64_loadIRimage(float *,float *,float *,int,int);


extern int aodtv64_seteyecloudtemp( void );
extern int aodtv64_scenetype( void );
extern int aodtv64_intensity( void );
extern int aodtv64_getscenetypes(int *,int *,int *,int *);
extern int aodtv64_setscenetypes(int,int,int,int);
extern int aodtv64_scenemap(int,int,char *);  /* XXX */

extern int aodtv64_setdatetime(int,int,char *,char *,int); /* XXX */
extern int aodtv64_historylistfmt(struct odtdata *,int,char *,char *); /* XXX */
extern int aodtv64_historybullfmt(struct odtdata *,char *); /* XXX */
extern int aodtv64_historygetnextrec(int,struct odtdata **);
extern int aodtv64_historydeleterec(int *,int *);
extern int aodtv64_historyrecordinsert(int *,int *);
extern int aodtv64_historywritefile(int *);
extern int aodtv64_bulletinoutput(char *);  /* XXX */
extern int aodtv64_historyaddcomment(char *,int *); /* XXX */

extern int aodtv64_atcfoutputfile(char *,int,char *); /* XXX */

extern int aodtv64_qmessage(int,int,char *,char *);  /* XXX */
extern int aodtv64_qdiagnostics(char *); /* XXX */

extern int aodtv64_initialize( void );
extern void aodtv64_freememory( void );

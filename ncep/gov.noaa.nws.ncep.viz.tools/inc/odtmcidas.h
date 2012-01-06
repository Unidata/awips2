/* external McIDAS application functions defined in odtmcidas directory */
extern int mcidas_initenv(int, char **);
extern int mcidas_initialize( void ); /* XXX */
extern int mcidas_freememory( void ); /* XXX */
extern int mcidas_getpaths(char *,char *,char *,char *,char *,char *); /* XXX */
extern int mcidas_setpaths(char *,char *,char *,char *,char *,char *); /* XXX */
extern int mcidas_setgraphoptions(int,int,int,int,int,int,int,int,int,int,int,int);
extern int mcidas_getinputs(int, char **,
                  int *,int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,int *,int *,int *,
                  int *,int *,
                  float *,
                  char *,char *,char *,char *,char *,char *,char *,char *,char *,
                  char *,char *,char *); /* XXX */
extern int mcidas_getcursorloc(char *,float *,float *); /* XXX */
extern int mcidas_getsatdatainfo(char *,int,char *,int *,int *,int *); /* XXX */
extern int mcidas_getsatimagedata(char *,float,float,int,float **,float **,float **,int *,int *); /* XXX */
extern int mcidas_overridescenetype(int *,int *,int *,int *);
extern int mcidas_graphhistoryrec(void);
extern void mcidas_qmessage(int,int,char *,char *); /* XXX */

/**** c call to f for fconvt *************************************************/
/*--added by AV --*/
#ifdef LINX
   #define FCONVT fconvt_
#else
   #define FCONVT fconvt
#endif

extern void FCONVT(char *, char *, char *, float *, float *, int *) ;

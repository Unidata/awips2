#ifdef LINX /*-- added by AV --*/
   #define UMOVEX umovex_
#else
   #define UMOVEX umovex
#endif

extern void UMOVEX(char[][4], int * , char *, int *, int *);

#ifdef LINX /* added by AV-- */
   #define MCOMND mcomnd_
#else
   #define MCOMND mcomnd
#endif

extern void MCOMND(int*, char[], int*, int*);

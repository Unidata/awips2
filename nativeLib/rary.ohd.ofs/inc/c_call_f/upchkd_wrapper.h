#ifdef LINX
   #define UPCHKD_WRAPPER upchkd_wrapper_
#else
   #define UPCHKD_WRAPPER upchkd_wrapper
#endif

extern void UPCHKD_WRAPPER(char[], int*, int*) ;

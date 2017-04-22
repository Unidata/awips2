/**** c call to f for mdyh1 *************************************************/

#ifdef LINX
   #define MDYH1 mdyh1_
#else
   #define MDYH1 mdyh1
#endif

extern void MDYH1(int *, int*, int*, int*, int*, int*, int*, int*, int*);


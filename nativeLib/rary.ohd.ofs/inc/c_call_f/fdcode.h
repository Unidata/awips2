/**** c call to f for fdcode *************************************************/
/*AV  */
#ifdef LINX
   #define FDCODE fdcode_
#else
   #define FDCODE fdcode
#endif

extern void FDCODE(char *, char *, char *, int *, int *, char *, int *, int *);

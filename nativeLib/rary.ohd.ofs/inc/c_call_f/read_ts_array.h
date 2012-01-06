#ifdef LINX /*-- added by AV -- */
   #define READ_TS_ARRAY read_ts_array_
#else
   #define READ_TS_ARRAY read_ts_array
#endif

extern void READ_TS_ARRAY(char[], int*, float[]);

#ifdef LINX /*added by AV -- */
   #define READ_SEG_NAMES read_seg_names_
#else
   #define READ_SEG_NAMES read_seg_names
#endif

extern void READ_SEG_NAMES(char[], int*, char[][8]);

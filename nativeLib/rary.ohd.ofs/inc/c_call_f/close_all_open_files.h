#ifdef LINX /*-- added by AV -- */
   #define CLOSE_ALL_OPEN_FILES close_all_open_files_ 
#else
   #define CLOSE_ALL_OPEN_FILES close_all_open_files    
#endif

extern void CLOSE_ALL_OPEN_FILES();

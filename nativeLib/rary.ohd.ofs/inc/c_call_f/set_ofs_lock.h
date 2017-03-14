#ifdef LINX
   #define SET_OFS_LOCK set_ofs_lock_
#else
   #define SET_OFS_LOCK set_ofs_lock
#endif 

extern void SET_OFS_LOCK(char[], int*) ;

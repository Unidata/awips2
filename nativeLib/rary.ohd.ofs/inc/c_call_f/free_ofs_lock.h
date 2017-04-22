#ifdef LINX
   #define FREE_OFS_LOCK free_ofs_lock_
#else
   #define FREE_OFS_LOCK free_ofs_lock
#endif 

extern void FREE_OFS_LOCK(int*) ;

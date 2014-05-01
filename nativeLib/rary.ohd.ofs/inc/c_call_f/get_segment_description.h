#ifdef LINX /*added by kwz*/
   #define GET_SEGMENT_DESCRIPTION get_segment_description_
#else
   #define GET_SEGMENT_DESCRIPTION get_segment_description
#endif

extern void GET_SEGMENT_DESCRIPTION(char[]) ;

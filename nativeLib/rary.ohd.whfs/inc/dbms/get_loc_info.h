#ifndef GET_LOC_INFO_H
#define GET_LOC_INFO_H

#include "DbmsDefs.h"

#define      LOCINFOFOUND      1
#define      LOCINFONOTFOUND   0
#define      ALLOWED_NAME_LEN  20
typedef struct
{
   char   id[LOC_ID_LEN + 1];
   char   name[ALLOWED_NAME_LEN + 1];
   double lat;
   double lon;
} loc_info_struct;


int get_loc_info(char     *lid,
                double    *dlat,
                double    *dlon,
		char      *name);
		
		

#endif

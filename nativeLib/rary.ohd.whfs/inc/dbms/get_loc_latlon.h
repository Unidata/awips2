#ifndef GET_LOC_LATLON_H
#define GET_LOC_LATLON_H

#include "DbmsDefs.h"

#define      LATLONFOUND      1
#define      LATLONNOTFOUND   0

typedef struct
{
   char id[LOC_ID_LEN + 1];
   double lat;
   double lon;
} latlon_list_struct;


int get_loc_latlon(char      *lid,
                   double    *dlat,
                   double    *dlon);

void get_loc_latlon_nrv(char      *lid,
                        double    *dlat,
                        double    *dlon,
                        int       *status);

#endif

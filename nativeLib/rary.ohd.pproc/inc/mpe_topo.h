#ifndef MPE_TOPO_H
#define MPE_TOPO_H

/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

struct lcoord
{
   float lon;   /* x */
   float lat;   /* y */
};

struct topo
{
     struct lcoord **coord;
     short int **value;
     int maxi;
     int maxj;
     float max_lat;
     float max_lon;
     float total_lat;
     float total_lon;
     float delta_lat;
     float delta_lon;
     char **color;
 };

const struct topo * get_topo_coord ( );

#endif /* #ifndef MPE_TOPO_H */

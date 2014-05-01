#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

/*** Define struct to locate info into database **/

typedef struct{
     int x_hgrid;
     int y_hgrid;
     char obstime[22];
     int no_strike;
}light_t;


struct point {
      double value;
      double lat;
      double lon;
      int time;
};


/*  Version variables */

static char lightproc_name [ ] =  "LightningProc" ;
static char lightproc_ver [ ] = "OB8.3" ;
static char lightproc_date [ ] ="Jan 17, 2008" ;

/*** declare object struct */

struct point light[100000];
struct point rain[100000];
float min_lat, min_lon, max_lat, max_lon;
int   llx, lly, nx, ny;

/*** Define function prototypes */
int ntcdfrd( char *fn);
void ggerd(FILE *inp_file);
void write_lightning(int);
void get_mmll_site();
void ddgsc3(int*,int*,int*,int*,int*,int*,int*);
void ddgdj3(int*,int*,int*);
void ddgjc(int*,int*,int*,int*);


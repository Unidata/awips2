#include "util.h"
#include <string.h>
#include"convert_hrap.h"
/*******************************************************************************/
int HRAP_X_local;
int HRAP_Y_local;
int HRAP_XOR_local;
int HRAP_YOR_local;

//void HRAP_values(int*,int*,int*,int*);
void triangulateradar();
void gethraps();
void readGeoData();
void gethraps()
{
int		i,j, counter=0, counter1=0;
double            tmp_x = 0.,tmp_y = 0., tmp2_x = 0.,tmp2_y = 0.;

    printf("inside of gethraps...\n");
   counter = 0;


   for(i = 0; i < HRAP_X_local; i++)
     for( j = 0; j < HRAP_Y_local; j++){

       tmp_x = i + HRAP_XOR_local;
       tmp_y = j + HRAP_YOR_local;
       HrapToLatLongByReference(tmp_y,tmp_x,&tmp2_y,&tmp2_x);

       radarpts[counter].lat = tmp2_y;
       radarpts[counter].lon = tmp2_x;


       counter++;
       if((counter1 - counter) == 1000)
       {
          counter1 = counter;
          printf("counter = %d\n",counter);
       }
     }

   numofradarpts = counter;
   //printf("numofradarpts = %ld\n", numofradarpts);
   //printf("number of radar grids = %d\n", numofradarpts);

}
/****************************************************************************/
int create_triangles_main(int argc, const char ** argv)
{
   int           i;
   char         *fname;
 int dbg = 0;

HRAP_values(&HRAP_X_local,&HRAP_Y_local,&HRAP_XOR_local,&HRAP_YOR_local);

 printf("Welcome to ProcessI Utility Program to create Static Files\n");


 /*-------------------------------------------------------------------------*/
 /*     check to see if debug is desired                                    */
 /*-------------------------------------------------------------------------*/

 dbg = 0;
 fname = (char *)malloc(80*sizeof(char));

 if (argc > 1)
    {
     for (i=1; i<argc; i++)
	{
	 if (strcmp(argv[i],"debug") == 0)
	    dbg = 1;

	}
    }
 if (dbg != 0) printf("dbg= %d\n",dbg);

//     HRAP_X_local = 500;
  //   HRAP_Y_local = 500;

     //readGeoData();
     printf("%d\n",HRAP_X_local);
     printf("%d\n",HRAP_Y_local);
     printf("%d\n",HRAP_XOR_local);
     printf("%d\n",HRAP_YOR_local);
     radarpts = (p3_gage_struct*) malloc(sizeof(p3_gage_struct)*HRAP_X_local*HRAP_Y_local);
     gethraps();
     triangulateradar(&radarpts);
        if(radarpts != NULL)
        {
           free(radarpts);
	   radarpts = NULL;
        }

     return 0;
}

/********************************************* END main ********************/


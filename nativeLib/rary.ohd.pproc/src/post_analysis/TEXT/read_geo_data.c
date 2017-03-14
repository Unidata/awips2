/*=========================================================================*/
/*                              NAME:   read_geo_data.c                    */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   read_geo_data()                    */
/*                                      read_overlay_data()                */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include "post_stage3.h"
#include "overlay.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   read_geo_data                                         */
/*       FUNCTION:   read geographic overlay data files                    */
/***************************************************************************

Function type:
   void

Called by function:
   main_rfcwide (mpe)
   ReadParam (mpe_post_analysis)

Functions called:
   read_overlay_data

Local variables:
   overlay_avail.xxxxx = overlay availability flags
                       = 0 -- overlay info not available
                       = 1 --    "     "   available

******************************** BEGIN read_geo_data *********/

void read_geo_data()
{
   FILE  *file;
   int    i, j, len;
   char   pdir[128],infile[128],infile1[128],infile2[128];

/*  create directory name for geographic data  */

     len = strlen("geo_data");
     get_apps_defaults("geo_data",&len,pdir,&len);

     len = strlen("st3_rfc");
     get_apps_defaults("st3_rfc",&len,RFC,&len);

     sprintf(infile1,"%s/%s/ascii/coord_%s.dat",pdir,RFC,RFC);
     sprintf(infile2,"%s/%s/binary",pdir,RFC);
 /*
    read coordinates of rectangle surrounding RFC area
    coordinates are on national HRAP grid
 */

 if((file = fopen(infile1,"r")) == NULL)
 {
   printf("ERROR: coord_%s.dat file not found -",RFC);
   printf(" program stopping \n");
   exit(0);
 }

 fscanf(file,"%d",&XOR);
 fscanf(file,"%d",&YOR);
 fscanf(file,"%d",&MAXX);
 fscanf(file,"%d",&MAXY);

 fclose(file);

/*----------------------------------------------------------------*/
/*  open and read overlay data files                              */
/*  if opened, files are closed within read_overlay_data function */
/*----------------------------------------------------------------*/

 sprintf(infile,"%s/rfc_boundary.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
 {
     overlay_avail.site_boundary=0;
     printf("warning: rfc_boundary.bin file not found \n");
 }
 else
 {
     overlay_avail.site_boundary=1;
     bound = read_overlay_data(&numrfc,file);
 }

/*---------------------------------------------------------*/

 sprintf(infile,"%s/state.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
 {
     overlay_avail.state=0;
     printf("warning: state.bin file not found \n");
 }
 else
 {
     overlay_avail.state=1;
     state = read_overlay_data(&numstates,file);
 }

/*---------------------------------------------------------*/

 sprintf(infile,"%s/county.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
 {
     overlay_avail.county=0;
     printf("warning: county.bin file not found \n");
 }
 else
 {
     overlay_avail.county=1;
     county = read_overlay_data(&numcounty,file);
 }

/*---------------------------------------------------------*/

 sprintf(infile,"%s/map_basin.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
 {
     overlay_avail.mapbasin=0;
     printf("warning: map_basin.bin file not found \n");
 }
 else
 {
     overlay_avail.mapbasin=1;
     mapbasin = read_overlay_data(&nummap,file);
 }

/*---------------------------------------------------------*/

 sprintf(infile,"%s/fg_basin.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
 {
     overlay_avail.fgbasin=0;
     printf("warning: fg_basin.bin file not found \n");
 }
 else
 {
     overlay_avail.fgbasin=1;
     fgbasin = read_overlay_data(&numfg,file);
 }

/*---------------------------------------------------------*/

 sprintf(infile,"%s/river.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
 {
     overlay_avail.river=0;
     printf("warning: river.bin file not found \n");
 }
 else
 {
     overlay_avail.river=1;
     river = read_overlay_data(&numrivers,file);
 }

 /*-------------------------------------------------------------------------*/
 /*     read locator data from files                                        */
 /*-------------------------------------------------------------------------*/

 sprintf(infile,"%s/grid_to_basin.bin",infile2);
 if((file = fopen(infile,"rb")) == NULL)
     {
       overlay_avail.gridtobasin=0;
       printf("warning: grid_to_basin.bin file not found \n");
     }
     else
     {

     overlay_avail.gridtobasin=1;
     loc_basin = (char ***)malloc(MAXY*sizeof(char **));
     for (i=0; i<MAXY; i++)
	{
	loc_basin[i] = (char **)malloc(MAXX*sizeof(char *));
	for (j=0; j<MAXX; j++)
	   {
	   loc_basin[i][j] = (char *)malloc(9*sizeof(char));
	   }
	}

     for (i=0; i<MAXX; i++)
     for (j=0; j<MAXY; j++)
	{
	fread(loc_basin[j][i],9*sizeof(char),1,file);
	}

     fclose(file);
 }

/*-----------------------------------------------------------------*/

     sprintf(infile,"%s/grid_to_county.bin",infile2);
     if((file = fopen(infile,"rb")) == NULL)
     {
       overlay_avail.gridtocounty=0;
       printf("warning: grid_to_county.bin not found \n");
     }
     else
     {

     overlay_avail.gridtocounty=1;
     loc_cty = (char ***)malloc(MAXY*sizeof(char **));
     for (i=0; i<MAXY; i++)
	{
	loc_cty[i] = (char **)malloc(MAXX*sizeof(char *));
	for (j=0; j<MAXX; j++)
	   {
	   loc_cty[i][j] = (char *)malloc(21*sizeof(char));
	   }
	}

     for (i=0; i<MAXX; i++)
     for (j=0; j<MAXY; j++)
	{
	fread(loc_cty[j][i],21*sizeof(char),1,file);
	}

     fclose(file);
 }
}

/************************************ END read_geo_data *********/

/**********************************************************************/
/*  FUNCTION NAME:  read_overlay_data()                               */
/*       FUNCTION:  read overlay data from geo_data directory files   */
/***********************************************************************

Function type:
   overlay_struct **

Called by function:
   read_geo_data

Functions called:
   none

******************************************** BEGIN read_overlay_data *******/

overlay_struct  **read_overlay_data(int *nc, FILE *file)
{
    int             n;
    int             i = 1;
    char            temp[9];
    overlay_struct  **data;

       data = (overlay_struct **) malloc(sizeof(overlay_struct *));
       memset(temp, '\0', 9);

       while( (n = fread(temp, sizeof(char), 9, file)) != 0)
       {
          data[i - 1] = (overlay_struct *) malloc(sizeof(overlay_struct));
	  strncpy(data[i - 1]->id, temp, 9);

          memset(data[i - 1]->name, '\0', 21);
	  n = fread(data[i - 1]->name, sizeof(char), 21, file);

	  n = fread(&data[i - 1]->order, sizeof(int), 1, file);
	  n = fread(&data[i - 1]->npts, sizeof(int), 1, file);

	  data[i - 1]->hrap = (HRAP *) malloc(data[i - 1]->npts * sizeof(HRAP));
	  n = fread(data[i - 1]->hrap, sizeof(HRAP), data[i - 1]->npts, file);

	  i++;
          data = (overlay_struct **) realloc(data, sizeof(overlay_struct *) * i);
	  memset(temp, '\0', 9);
       }

       *nc = i - 1;
       fclose(file);
       return (data);

}

/*********************************** END read_overlay_data****/

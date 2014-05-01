#include "gen_areal_qpe.h"

/*********************************************************   
  get_coord_info()
  
  PURPOSE
  Read HRAP coordinates of SW corner of site and dimensions
  of site grid.                                              
  This info is in .../geo_data/xxx/ascii/coord_xxx.dat  
                      where xxx = site id              
********************************************************** */

void get_coord_info(const gaq_options_struct * options,
                    geo_data_struct *coord_info)
{
   char  filename[256];
   FILE *file;
   int xor, yor, xsize, ysize;
   float xxr, yyr;

   /* create coord file filename */

   sprintf(filename,"%s/%s/ascii/coord_%s.dat",
                     options->geo_coord_path, 
		     options->site_id, 
		     options->site_id);
		     

   /* read coordinates of rectangle surrounding site area.
      read coordinates of SouthWest corner of site area. 
      coordinates are on national HRAP grid */

   if((file = fopen(filename,"r")) == NULL)
   {
      printf("ERROR: %s file not found -", filename);
      printf(" program stopping \n");
      exit(0);
   }

   fscanf(file, "%d", &xor);
   fscanf(file, "%d", &yor);
   fscanf(file, "%d", &xsize);
   fscanf(file, "%d", &ysize);
   
   fclose(file);
   file = NULL;

   /* generate lat/lon of NorthWest corner */
   xxr = xor;
   yyr = yor + ysize;

   printf("site info: x,y ord=%d %d x,y size=%d %d\n",
          xor, yor, xsize, ysize); 
	  
   /* load the information into the structure to be returned */
   coord_info->hrap_x = xor;
   coord_info->hrap_y = yor;
   coord_info->num_cols = xsize;
   coord_info->num_rows = ysize;
   
   return;
}

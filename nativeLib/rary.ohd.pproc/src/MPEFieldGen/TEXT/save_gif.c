#include "convert_hrap.h"
#include "gd1_inc/gd1.h"
#include "gd1_inc/gd1fonts.h"
#include "gd1_inc/gd1fontmb.h"
#include "gd1_inc/gd1fontl.h"
#include "save_gif.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpe_fieldgen.h"
#include "ColorValue.h"
#include "List.h"

typedef int Colors;
float  MPEFieldGen_dfltclvls[] = {0.00,0.01,0.10,0.20,0.30,0.40,0.50,0.75,1.00,1.25,1.50,1.75,2.0,2.5,3.0};


/*
   this function creates the gif images and writes them to the mpe_gif_dir directory

   calling subroutine: main_rfcwgen
*/
/*
void mpefield_save_gif(int *xor, int *yor, int *maxx, int *maxy, char *datestring,
                   char *filen, int *lenf, float *mmosaic, int *irc)
*/
void MPEFieldGen_saveGif( const geo_data_struct * pGeoData ,
					const char *datestring,
					const char *filen,
					double ** mosaic,
					long int *irc)
{
  
  gdImagePtr            gim;              /* Image Shell */
  gdPoint               *points,siiigrid;
  FILE                  *out;
  int                   c,i,j,k,nlvls,len,slope,maxpts;
  Colors                background,SlateGrey,grey,SlateGray1,LightSkyBlue,SteelBlue1;
  Colors                RoyalBlue1,LightSeaGreen,LawnGreen,GreenYellow,yellow, magenta;
  Colors                gold,DarkKhaki,DarkOrange,red,maroon,pink,honeydew,khaki,cyan1;
  Colors                LemonChiffon,white,ivory,wheat;
  Colors                tcolor;
  Colors                lvlcolors[32];
  short int             pcpnamt; 
  float                 ul,lr,cfactor = 0.255;
  float                 clvls[20]; 
  float                 scale_factor,units_factor, factor;
  char                  filename[128];
  char                  datestamp[128],timestamp[128];
  char                  legendlbl[6],buf[8],dates[11];
  
  



float dfltclvls_site[17];
int list_count = 0;
Colors temp_color;
List* clr_val_list = NULL;
ColorValue* clr_val_head = NULL;
ColorValue* clr_val_node = NULL;
char where[250];
char temp_str[50];
Colors colors_site[17];
int *my_red = NULL, *my_green = NULL, *my_blue = NULL;
int site_colors_set = 0;






my_red = (int*) malloc(sizeof(int));
my_green = (int*) malloc(sizeof(int));
my_blue = (int*) malloc(sizeof(int));
      
*my_red = 0;
*my_green = 0;
*my_blue = 0;

dates[10] = '\0';
  strncpy(dates, datestring, 10);

  /* Initialize the error return code to 0. */
  * irc = 0;

  /*--------------------------------------------*/
  /*  define graphics scale                     */
  /*--------------------------------------------*/
          
  len=strlen("st3_auto_graphic_scale");
  get_apps_defaults("st3_auto_graphic_scale",&len,buf,&len);
  if (len == 0)
     gui.scalex = 4.0;
  else
     gui.scalex = atof(buf);
    
  /* define geo data variables in structure */
  
   gui.XOR = pGeoData->hrap_x ;
   gui.YOR = pGeoData->hrap_y ;
   gui.HRAPX = pGeoData->num_cols ;
   gui.HRAPY = pGeoData->num_rows ;
      
   /* set graphic defaults */
  
   units_factor = 25.4;
   scale_factor = 100.;

   gui.zoom_factor = 1.0;
   gui.orig_x = 0.0;
   gui.orig_y = 0.0;
   gui.scaley = gui.scalex;
   gui.offset = 50;
   
   gui.WIDTH = (int)(gui.offset + gui.scalex *( 2 + gui.HRAPX ));
   gui.HEIGHT = (int)(gui.offset + gui.scaley * ( 2 + gui.HRAPY));
   
   slope = (float)(gui.HRAPX/gui.HRAPY);
   
  gim = gdImageCreate(gui.WIDTH,gui.HEIGHT);
       

if(ptrMPEParams->rfc_name == NULL)
{
   shutDownMPE("rfc site id not defined...exiting...", logFile); 
}
memset(where, '\0', 250);
memset(temp_str, '\0', 50);
strcpy(where, "where userid='");
sprintf(temp_str, "%s", ptrMPEParams->rfc_name);
strcat(where, temp_str);
strcat(where, "' and color_use_name='FIELDGEN-GIF' and application_name='hmapmpe' order by threshold_value");
SetColorValueErrorLogging(1);
clr_val_head = (ColorValue*) GetColorValue((const char*)where);



if(clr_val_head != NULL)
{
   clr_val_list = (List*) &clr_val_head->list;
   if(clr_val_list != NULL)
   {
      list_count = ListCount(clr_val_list);
   }

   if(list_count > 17)
   {
      list_count = 17;
   }
   memset(dfltclvls_site, 0, 17);
   memset(colors_site, 0, 17);
   clr_val_node = (ColorValue*) ListFirst(clr_val_list);
   i=0;
   gdImageColorAllocate(gim,0,0,0);
   while(i<list_count && clr_val_node!=NULL)
   {
      if(!strcmp(clr_val_node->userid,ptrMPEParams->rfc_name) && 
         !strcmp(clr_val_node->application_name,"mpe_fieldgen") &&
         !strcmp(clr_val_node->color_use_name,"COLOR-PREF") &&
         clr_val_node->duration == 3600)
      {
         dfltclvls_site[i] = clr_val_node->threshold_value;
	 get_rgb(clr_val_node->color_name, my_red, my_green, my_blue); 
	 temp_color = gdImageColorAllocate(gim,*my_red,*my_green,*my_blue);
	 if(*my_red >= 0 && *my_green >= 0 && *my_blue >= 0)
	 {
            site_colors_set = 1;
	    colors_site[i] = temp_color;
	 }
      }
      else
      {
         site_colors_set = 0;
      }
      
      clr_val_node = (ColorValue*) ListNext(&clr_val_node->node); 
      i++;
   }
}



  /* legend and background colors */
    
  background = gdImageColorAllocate(gim,(int)(cfactor*0),(int)(cfactor*0),(int)(cfactor*0)); /* black */
  SlateGrey=gdImageColorAllocate(gim,(int)(cfactor*396),(int)(cfactor*451),(int)(cfactor*514));
  grey=gdImageColorAllocate(gim,(int)(cfactor*600),(int)(cfactor*600),(int)(cfactor*600));
  SlateGray1=gdImageColorAllocate(gim,(int)(cfactor*776),(int)(cfactor*886),(int)(cfactor*1000));
  LightSkyBlue = gdImageColorAllocate(gim,(int)(cfactor*529),(int)(cfactor*808),(int)(cfactor*980));
  SteelBlue1=gdImageColorAllocate(gim,(int)(cfactor*361),(int)(cfactor*702),(int)(cfactor*1000));
  RoyalBlue1=gdImageColorAllocate(gim,(int)(cfactor*188),(int)(cfactor*431),(int)(cfactor*1000));
  LightSeaGreen=gdImageColorAllocate(gim,(int)(cfactor*243),(int)(cfactor*663),(int)(cfactor*624));
  LawnGreen=gdImageColorAllocate(gim,(int)(cfactor*529),(int)(cfactor*969),(int)(cfactor*90));
  GreenYellow=gdImageColorAllocate(gim,(int)(cfactor*694),(int)(cfactor*984),(int)(cfactor*90));
  yellow=gdImageColorAllocate(gim,(int)(cfactor*1000),(int)(cfactor*988),(int)(cfactor*90));
  gold=gdImageColorAllocate(gim,(int)(cfactor*831),(int)(cfactor*627),(int)(cfactor*90));
  DarkKhaki=gdImageColorAllocate(gim,(int)(cfactor*718),(int)(cfactor*678),(int)(cfactor*349));
  DarkOrange=gdImageColorAllocate(gim,(int)(cfactor*973),(int)(cfactor*502),(int)(cfactor*90));
  red=gdImageColorAllocate(gim,(int)(cfactor*965),(int)(cfactor*133),(int)(cfactor*90));
  maroon=gdImageColorAllocate(gim,(int)(cfactor*506),(int)(cfactor*20),(int)(cfactor*255));
  pink=gdImageColorAllocate(gim,(int)(cfactor*980),(int)(cfactor*686),(int)(cfactor*745));
  

/*--------------------------------------------*/
  /*  define colors/levels                      */
  /*--------------------------------------------*/
          
   for(i=0;i<17;i++)
   {
      if(site_colors_set == 1)
      {
         if(i >= list_count)
	 {
	    break;
	 }
	 else
	 {
	    clvls[i] = dfltclvls_site[i];
	 } 
      }
      else
      {
         clvls[i] = MPEFieldGen_dfltclvls[i];
      }
   }
   nlvls=i-1;
      
  
  /* Initialize color levels */
  
  if(site_colors_set == 1)
  {
     for(i=0;i<list_count;i++)
     {
        lvlcolors[i] = colors_site[i];
     }
     while(i<32)
     {
        lvlcolors[i] = background;
	i++;
     }
  }
  else
  {
     lvlcolors[0] = background; lvlcolors[1] = SlateGrey; lvlcolors[2] = grey;
     lvlcolors[3] = SlateGray1; lvlcolors[4] = LightSkyBlue; lvlcolors[5] = SteelBlue1;
     lvlcolors[6] = RoyalBlue1; lvlcolors[7] = LightSeaGreen; lvlcolors[8] = LawnGreen;
     lvlcolors[9] = GreenYellow; lvlcolors[10] = yellow; lvlcolors[11] = gold;
     lvlcolors[12] = DarkKhaki; lvlcolors[13] = DarkOrange; lvlcolors[14] = red;
     lvlcolors[15] = maroon; lvlcolors[16] = pink;
  }

  
  /* text and overlay colors */
  
  honeydew=gdImageColorAllocate(gim,(int)(cfactor*941),(int)(cfactor*996),(int)(cfactor*933));
  LemonChiffon=gdImageColorAllocate(gim,(int)(cfactor*1000),(int)(cfactor*973),(int)(cfactor*776));
  khaki=gdImageColorAllocate(gim,(int)(cfactor*678),(int)(cfactor*663),(int)(cfactor*431));
  cyan1=gdImageColorAllocate(gim,(int)(cfactor*341),(int)(cfactor*996),(int)(cfactor*1000));
  white = gdImageColorAllocate(gim,(int)(cfactor*1000),(int)(cfactor*1000),(int)(cfactor*1000));
  wheat = gdImageColorAllocate(gim,(int)(cfactor*961),(int)(cfactor*871),(int)(cfactor*702));
  ivory = gdImageColorAllocate(gim,(int)(cfactor*1000),(int)(cfactor*1000),(int)(cfactor*941));
  magenta = gdImageColorAllocate(gim,(int)(cfactor*1000),(int)(cfactor*0),(int)(cfactor*1000));

  
  /* fill rectangles with proper colors  */

  for(i=0; i<pGeoData->num_cols; i++) 
  {
    siiigrid.x = (int)((i * gui.scalex) - (int)(gui.orig_x)) * gui.zoom_factor;

    for(j=0; j<pGeoData->num_rows; j++)
    {
    
       siiigrid.y = (int)(((gui.HRAPY - (j - gui.orig_y)) * gui.scaley) -
                                          (int)(gui.orig_y)) * gui.zoom_factor;
                             
/*                                                          
       k = (j * (*maxx)) + i;

       pcpnamt = mosaic[k] * scale_factor;
*/

       pcpnamt = mosaic[j][i] * scale_factor;
                             
       tcolor = background;
       factor = units_factor * scale_factor;
                             
       for(c=1;c<nlvls-1;c++)
         if(pcpnamt > clvls[c-1]*factor && pcpnamt <= clvls[c]*factor) tcolor = lvlcolors[c-1]; 
                                                
       if (pcpnamt <= clvls[0]*factor) 
          tcolor = background;
       if( pcpnamt > clvls[nlvls-1]*factor) 
           tcolor = lvlcolors[nlvls-1];
                           
//       gdImageFilledRectangle(gim, siiigrid.x, siiigrid.y-2,siiigrid.x+2,siiigrid.y,tcolor);
                              
    
     }
  }                     
  
   /* Overlay RFC Basin Boundary */
   
      
   points = (gdPoint *)calloc(gdata.rfc.npts+1,sizeof(gdPoint));
   for (j = 0; j < gdata.rfc.npts; j++){
          points[j].x = (((gdata.rfc.hrap[j].x - gui.XOR) * gui.scalex)
                         - gui.orig_x) * gui.zoom_factor;

          if(points[j].x > 3000) points[j].x = 3000;
          if(points[j].x < -500) points[j].x = -500;

          points[j].y = (((gui.HRAPY - (gdata.rfc.hrap[j].y - gui.YOR))* gui.scaley)
                         - gui.orig_y) * gui.zoom_factor;
                                  
          if(points[j].y > 2000) points[j].y = 2000;
          if(points[j].y < -500) points[j].y = -500;
          
          
  }
  
  
  gdImagePolygon(gim,points,gdata.rfc.npts,wheat);
    
      
   /* Overlay towns */
   
    points = (gdPoint *)calloc(gdata.ntowns+1,sizeof(gdPoint));
  
    for (j=0; j< gdata.ntowns;j++) {
          points[j].x = (( gdata.town[j].location.x - gui.XOR ) * gui.scalex
			 - gui.orig_x * gui.scalex) * gui.zoom_factor;
	  
	  if(points[j].x > 3000) points[j].x = 3000;
	  if(points[j].x < -500) points[j].x = -500;
	 
	  points[j].y = (( gui.HRAPY - (gdata.town[j].location.y - gui.YOR) - gui.scaley) * gui.scaley
			 - (gui.orig_y) * gui.scaley) * gui.zoom_factor;
	  if(points[j].y > 2000) points[j].y = 2000;
	  if(points[j].y < -500) points[j].y = -500;
	  
	  
	  gdImageArc(gim,points[j].x,points[j].y,2,2,0,360,ivory);
	  gdImageString(gim,gdFontMediumBold,points[j].x+2,points[j].y+1,gdata.town[j].name,white);
  }
  
  free(points);
  
       
   for(j=0;j< gdata.nstates; j++) {
          
    maxpts = gdata.state[j].npts;
   
    points = (gdPoint *)calloc(maxpts+1,sizeof(gdPoint)); 
   
    for(k=0; k< maxpts; k++) {
      points[k].x = (((gdata.state[j].hrap[k].x - gui.XOR) * gui.scalex)
                    - gui.orig_x) * gui.zoom_factor;
                    
      if(points[k].x > 3000) points[k].x = 3000;
      if(points[k].x < -500) points[k].x = -500;
     
      points[k].y = (((gui.HRAPY - (gdata.state[j].hrap[k].y - gui.YOR))*gui.scaley)
                    - gui.orig_y) * gui.zoom_factor;
                    
      if(points[k].y > 2000) points[k].y = 2000;
      if(points[k].y < -500) points[k].y = -500;
     
    }
     
    for(k=1;k<maxpts;k++) 
     gdImageLine(gim,points[k-1].x,points[k-1].y,points[k].x,points[k].y,ivory);
     
    free(points);

  }
  
  
  /* create legend */
  
   if(site_colors_set == 1)
   {
      nlvls = list_count; 
   }
 
   if( slope >= 1.0)
   {
  
     gdImageFilledRectangle(gim,0,(int)(0.90*gui.HEIGHT),(int)(gui.WIDTH),(int)(gui.HEIGHT),background);
   
     for(c=0;c<nlvls;c++){
     
       memset(legendlbl,'\0',6);
       sprintf(legendlbl,"%.2f",clvls[c]);
       tcolor=lvlcolors[c];
     
       ul = 0.07 + (c * 0.04);
       lr = ul + 0.04;
     
          gdImageString(gim,gdFontSmall,(int)(ul*gui.WIDTH),(int)(0.91*gui.HEIGHT),legendlbl,white); 
          gdImageFilledRectangle(gim,(int)(ul*gui.WIDTH),(int)(0.95*gui.HEIGHT),(int)(lr*gui.WIDTH),(int)(0.995*gui.HEIGHT),tcolor);

    }      

   }
   
   if( slope < 1.0)
   {
       gdImageFilledRectangle(gim,(int)(0.90*gui.WIDTH),0,(int)(gui.WIDTH),(int)(0.90*gui.HEIGHT),background);
   
       for(c=0;c<nlvls;c++){
     
       memset(legendlbl,'\0',6);
       sprintf(legendlbl,"%.2f",clvls[c]);
       tcolor=lvlcolors[c];
     
       ul = 0.07 + (c * 0.04);
       lr = ul + 0.04;
       
          gdImageFilledRectangle(gim,(int)(0.91*gui.WIDTH),(int)(ul*gui.HEIGHT),(int)(0.95*gui.WIDTH),(int)(lr*gui.HEIGHT),tcolor);
          gdImageString(gim,gdFontSmall,(int)(0.96*gui.WIDTH),(int)(ul*gui.HEIGHT),legendlbl,white); 
       
    }      

   }

    

    /* place time-date in legend area */
  
     memset(datestamp,'\0',128);
     for(c=0;c<8;c++) datestamp[c] = dates[c];

     memset(timestamp,'\0',128);
     for(c=8;c<10;c++) timestamp[c-8] = dates[c];

     timestamp[2] = 'z';
   
     if(slope >= 1.0)
     {
    
      gdImageString(gim,gdFontMediumBold,(int)(0.77*gui.WIDTH),(int)(0.95*gui.HEIGHT),"RFCWide",LemonChiffon); 
      gdImageString(gim,gdFontMediumBold,(int)(0.84*gui.WIDTH),(int)(0.95*gui.HEIGHT),datestamp,LemonChiffon); 
      gdImageString(gim,gdFontMediumBold,(int)(0.91*gui.WIDTH),(int)(0.95*gui.HEIGHT),timestamp,LemonChiffon);
      
     }
     
     if(slope < 1.0)
     {
      gdImageString(gim,gdFontMediumBold,(int)(0.80*gui.WIDTH),(int)(0.91*gui.HEIGHT),"RFCWide",LemonChiffon); 
      gdImageString(gim,gdFontMediumBold,(int)(0.80*gui.WIDTH),(int)(0.95*gui.HEIGHT),datestamp,LemonChiffon); 
      gdImageString(gim,gdFontMediumBold,(int)(0.85*gui.WIDTH),(int)(0.95*gui.HEIGHT),timestamp,LemonChiffon);
      
     }
      
/*------------------------------------------*/
/*  open file and write GIF image           */
/*  file is closed in gd routine GIFEncode  */
/*------------------------------------------*/
     
     memset(filename,'\0',128);
     strcpy(filename,filen);
	 int lenf = strlen(filename);
     filename[lenf] = '\0';
                               
     out = fopen(filename,"wb");
     if(!out) 
     {
         *irc = -1;
         return;
     }

      gdImageGif(gim,out);

   gdImageDestroy(gim);
   
} /* end MPEFieldGen_saveGif */   

/********************************************************************/

void MPEFieldGen_rfcw_load_static(int *irc)

{
    
/*
   this function loads the geo_data information for use in generating the gif 
   images for mpe_fieldgen

   calling subroutine: main_rfcwgen
*/

  double                col;
  double                row; 
  FILE                  *in_file;
  char                  geobdir[100],geoadir[100], filename[128];
  char                  dummy_id[9],dummy_name[21];
  float                 r_lat,r_lon;
  int                   j,k,len,dummy_order;
  
  len = strlen("geo_st3_bin");
  get_apps_defaults("geo_st3_bin",&len,geobdir,&len);
  len = strlen("geo_st3_ascii");
  get_apps_defaults("geo_st3_ascii",&len,geoadir,&len);
  
   /* Read in site Basin Boundary */

    sprintf(filename, "%s/rfc_boundary.bin",geobdir);

    sprintf ( message, "Opening rfc bounday overlay file %s.\n", filename );
    printMessage ( message, logFile );

    if((in_file = fopen(filename,"rb")) == NULL)
    {
      sprintf ( message, "Could not open rfc boundaries file %s\n",
                          filename );
      printMessage ( message, logFile );
       
      *irc =  -1;
      return;
    }  

    fread(&gdata.rfc.id, sizeof(char), 9, in_file);

    fread(&dummy_name, sizeof(char), 21, in_file);

    fread(&dummy_order, sizeof(int), 1, in_file);

    fread(&gdata.rfc.npts, sizeof(int), 1, in_file);

    gdata.rfc.hrap = (HRAP *)calloc(gdata.rfc.npts, sizeof(HRAP));

    while(!feof(in_file)) {
         for(j=0; j<gdata.rfc.npts; j++)
        fread(&gdata.rfc.hrap[j], sizeof(HRAP), 1, in_file);

    }
    
    fclose(in_file);
  
                  
  
   /* read in town locations and names */

   sprintf(filename, "%s/town.dat", geoadir);
   sprintf ( message, "Opening rfc town overlay  file %s.\n", filename );
   printMessage ( message, logFile );

   if((in_file = fopen(filename,"r")) == NULL)
   {
     sprintf ( message, "Could not open town locations file %s.\n",
                        filename );
     printMessage ( message, logFile );
     *irc = -2;  
     return;
   }  
   j=0;

   while(!feof(in_file)) 
   {
     fscanf(in_file,"%s %f %f",gdata.town[j].name, &r_lat,&r_lon);
     LatLongToHrapByReference ( r_lat, r_lon, & row, & col);
     gdata.town[j].location.x = ( float ) col ;
     gdata.town[j].location.y = ( float ) row ;
     j++;
   }

   fclose(in_file);
   gdata.ntowns = j-1;
   
   
   /* read in state boundaries */

   sprintf(filename, "%s/state.bin", geobdir);
   sprintf ( message, "Opening state bounday overlay file %s.\n", filename );
   printMessage ( message, logFile );
   if((in_file = fopen(filename, "rb")) == NULL)
   {
      sprintf ( message, "Could not open state overlay file %s.\n",
                         filename );
      printMessage ( message, logFile );
      *irc = -3; 
      return;
   }  
    
   j = 0;

   while(!feof(in_file))
   {
   
      
      if((fread(&dummy_id, sizeof(char), 9, in_file)) == 0) break;
      
      fread(&dummy_name, sizeof(char), 21, in_file);

      fread(&dummy_order, sizeof(int), 1, in_file);

      fread(&gdata.state[j].npts, sizeof(int), 1, in_file);


      gdata.state[j].hrap = (HRAP *)calloc(gdata.state[j].npts,sizeof(HRAP));
      for (k=0; k<gdata.state[j].npts; k++)
          fread(&gdata.state[j].hrap[k], sizeof(HRAP), 1, in_file);

      j++;
    }

    fclose(in_file);
    gdata.nstates = j-1;
    *irc = 0;
    
} /* end MPEFieldGen_rfcw_load_static */     

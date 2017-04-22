/*======================================================================================*/
/*                                                                                      */
/*  FUNCTIONS CONTAINED IN THIS FILE:                                                   */
/*                                      void             initialize_data()              */
/*                                      void             ReadParameters()               */
/*                                      int              read_ForecastPoint_data()      */
/*                                      overlay_struct   **read_overlay_data()          */
/*                                      overlay_struct   **read_overlay_data2()         */
/*                                                                                      */
/*                                                                                      */
/*======================================================================================*/



#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"




int     read_ForecastPoint_data();






/***************************************************************************/
/* FILE PATH/NAME:   (ifp_source)/IFP_Map/read_write_data.c                */
/*  FUNCTION NAME:   initialize_data()                                     */
/*       FUNCTION:   main calling program for stage3                       */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   ReadRadarData
   Mosaic
   ReadGageData
   read_rfc_boundary
   read_overlay_data
   read_county_data
   Difference (COMMENTED OUT)

Local variables:
   mfile - deref FILE structure;
   bfile - deref FILE structure;
   cfile - deref FILE structure;
   first_run - integer;
   istat - integer;
   mx - integer;
   my - integer;
   i, j - integer;
   filename - stack deref character;
   temp_filename - stack deref (array) character; dimensioned 60;
   rfc_name - stack deref (array) character; dimensioned 9;

******************************************** BEGIN initialize_data *********/

void initialize_data(widget_struct)
	the_widget_struct       *widget_struct;
{

	FILE    *mfile, *bfile, *cfile;
	FILE    *cityfile;
	FILE    *zoom_cityfile;
	int     first_run, istat, mx, my, i, j;
	char    *filename;
	char    path_name[120];
	char    rfc_name[9];
	char    *cityfile_name;
	char    *zoom_cityfile_name;
        int     len, len2;



 /*-------------------------------------------------------------------------*/
 /*     Build filename then                                                 */
 /*     read Overlay Data from static data files                            */
 /*-------------------------------------------------------------------------*/

     memset(path_name, '\0', 120);
     memset(rfc_name, '\0', 9);

     /* call routine to get the overlays directory path and rfc name*/
     len = strlen("geo_data");
     get_apps_defaults("geo_data", &len, path_name, &len2);
     len = strlen("ifp_rfc");
     get_apps_defaults("ifp_rfc", &len, rfc_name, &len2);

     strcat(path_name, "/");
     strcat(path_name, rfc_name);

     /*-----------------------------------------*/
     /* RFC Boundary data...                    */
     /*-----------------------------------------*/
     
     filename = build_filename(path_name, "/binary/rfc_boundary.bin");
     if((bound = read_overlay_data(filename, &numrfc)) == (overlay_struct **) NULL)
     /*if((bound = read_rfc_boundary(&numrfc)) == (overlay_struct *) NULL)*/ 
        puts("RFC Boundary data file was not found!");
     
     /*-----------------------------------------*/
     /* Forecast Point data...                  */
     /*-----------------------------------------*/
     filename = build_filename(path_name, "/ascii/forecastpt.dat");
      
     if((NumForecastPoints = read_ForecastPoint_data(filename, &forecastpoints)) == 0)
		XtSetSensitive(widget_struct->forecastPoints_widget, FALSE);
     
     /*-----------------------------------------*/
     /* County Boundaries...                    */
     /*-----------------------------------------*/
     filename = build_filename(path_name, "/binary/county.bin");
     if((county = read_overlay_data(filename, &numcounty)) == NULL)
		XtSetSensitive(widget_struct->county_widget, FALSE);
      
     /*-----------------------------------------*/
     /* State Boundaries...                     */
     /*-----------------------------------------*/
     filename = build_filename(path_name, "/binary/state.bin");
     if((state = read_overlay_data(filename, &numstates)) == NULL)
		{
		XmToggleButtonSetState(widget_struct->states_widget, FALSE, FALSE);
		XtSetSensitive(widget_struct->states_widget, FALSE);
		}

     /*-----------------------------------------*/
     /* MAP Basin Boundaries...                 */
     /*-----------------------------------------*/
      filename = build_filename(path_name, "/binary/map_basin.bin");
      if((mapbasin = read_overlay_data(filename, &nummap)) == NULL)
               {
	        XtSetSensitive(widget_struct->basin_boundaries_widget, FALSE);
		XtSetSensitive(widget_struct->FcstGroup_widget, FALSE);
	       }

     /*-----------------------------------------*/
     /* Forecast Group Boundaries...            */
     /*-----------------------------------------*/
     filename = build_filename(path_name, "/binary/fg_basin.bin");
     if((fgbasin  = read_overlay_data(filename, &numfg)) == NULL)
		XtSetSensitive(widget_struct->FcstGroupBoundaries_widget, FALSE);


     /*-----------------------------------------*/
     /* River & Stream Boundary data...         */
     /*-----------------------------------------*/
     filename = build_filename(path_name, "/binary/river.bin");
     if((river = read_overlay_data(filename, &numrivers)) == NULL)
		XtSetSensitive(widget_struct->rivers_widget, FALSE);


     /*-----------------------------------------*/
     /* City & Town point data...               */
     /*-----------------------------------------*/

     zoom_cityfile_name = build_filename(path_name, "/ascii/town_zoom.dat");
     cityfile_name = build_filename(path_name, "/ascii/town.dat");
     
     if((cityfile = fopen(cityfile_name, "r")) == NULL)
		{
		XmToggleButtonSetState(widget_struct->cities_widget, FALSE, FALSE);
		XtSetSensitive(widget_struct->cities_widget, FALSE);
		}
     else       fclose(cityfile);

     if((zoom_cityfile = fopen(zoom_cityfile_name, "r")) == NULL)
		{
		XmToggleButtonSetState(widget_struct->cities_widget, FALSE, FALSE);
		XtSetSensitive(widget_struct->cities_widget, FALSE);
		}
     else       fclose(zoom_cityfile);

}


/***************************************************************************/
/* FILE PATH/NAME:   (ifp_source)/IFP_Map/read_write_data.c                */
/*  FUNCTION NAME:   ReadParameters()                                      */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:
 ? main

Functions called:

Local variables:
   pfile - deref FILE structure;
   sfile - deref FILE structure;
   coord_file - deref FILE structure;
   fname - stack deref (array) character; dimensioned 40;
   difftemp - float;
   stemp - stack deref (array) character; dimensioned 4;
   i - integer;

******************************************** BEGIN ReadParameters **********/

void ReadParameters()
{
   FILE        *pfile, *sfile, *coord_file;
   char         fname[120];
   float        difftemp;
   char         stemp[4];
   int          i;
   char         rfc_name[9];
   int          len, len2;

 memset(fname, '\0', 120);
 memset(rfc_name, '\0', 9);

 /* call routine to get the overlays directory path and rfc name*/
 len = strlen("ifp_options_dir"); 
 get_apps_defaults("ifp_options_dir", &len, fname, &len2);
 len = strlen("ifp_rfc");
 get_apps_defaults("ifp_rfc", &len, rfc_name, &len2);

 strcat(fname, "/param/ifp_param.dat");

 if( (pfile = fopen(fname, "r")) == NULL)
    puts("Problems opening ifp_param.dat file!");
 else
 {
    /*-------------------------------------------------------------------------*/
    /*     read in river orders                                                */
    /*-------------------------------------------------------------------------*/

    fscanf(pfile,"%d%d%d%d",&ORD_MOS,&ORD_SS,&ORD_ZM,&ORD_FG);

    /*-------------------------------------------------------------------------*/
    /*     read in maximum difference threshold                                */
    /*-------------------------------------------------------------------------*/

    fscanf(pfile,"%f",&difftemp);

    /*-------------------------------------------------------------------------*/
    /*     read in default overlay info                                        */
    /*-------------------------------------------------------------------------*/

    istate=0;
    icity=0;
    iriver=0;
    ibound=0;
    icounty=0;
    fscanf(pfile,"%s",stemp);
    if (strcmp(stemp,"ON")==0) istate = 1;
    fscanf(pfile,"%s",stemp);
    if (strcmp(stemp,"ON")==0) icity = 1;
    fscanf(pfile,"%s",stemp);
    if (strcmp(stemp,"ON")==0) iriver = 1;
    fscanf(pfile,"%s",stemp);
    if (strcmp(stemp,"ON")==0) ibound = 1;
    fscanf(pfile,"%s",stemp);
    if (strcmp(stemp,"ON")==0) icounty = 1;

    /*-------------------------------------------------------------------------*/
    /*     read rfc id                                                         */
    /*-------------------------------------------------------------------------*/

    fscanf(pfile,"%s",rfcid);

/* set the flag for saving gif files
   added 07/21/95 - dp 
*/    
    fscanf(pfile,"%s",stemp);
    save_gif = 0;
    if (strcmp(stemp,"ON")==0) 
       save_gif = 1;
     
    fclose(pfile);
 }  /* end of pfile processing */

 memset(fname, '\0', 120);

 /* call routine to get the overlays directory path*/
 len = strlen("geo_data");
 get_apps_defaults("geo_data", &len, fname, &len2);

 strcat(fname, "/");
 strcat(fname, rfc_name);
 strcat(fname, "/ascii/coord_");
 strcat(fname, rfc_name);
 strcat(fname, ".dat");

 if( (coord_file = fopen(fname,"r")) != NULL) 
 {
    fscanf(coord_file,"%d",&XOR);
    fscanf(coord_file,"%d",&YOR);
    fscanf(coord_file,"%d",&MAXX);
    fscanf(coord_file,"%d",&MAXY);

    fclose(coord_file);
 }

}

/* ****************************************************************************

	int read_ForecastPoint_data()

   **************************************************************************** */

int read_ForecastPoint_data(char *fileName, fcstpoint_struct **data_ptr)
{

	int     length;
	int     i = 1;

	char    *buffer;
	char    *nextLine;
	char    *endLine;
	char    *temp_ptr;
	char    *first_blank;
	char    *buffer_ptr;
	char    tempString[10];

	fcstpoint_struct *data;



 buffer = GetSource(fileName);
 if(buffer == (char *) NULL)
	{
	puts("No Forecast Points data read...");
	return(0);
	}
 buffer_ptr = buffer;

 nextLine = (char *) malloc(100 * sizeof(char));


 data = (fcstpoint_struct *) malloc(sizeof(fcstpoint_struct));

 while(strlen(buffer) > 0)
	{
	/* -------------------------------------*/
	/*      Get the next line & increment   */
	/*      'buffer' to the start of the    */
	/*      next line...                    */
	/* -------------------------------------*/

	if((endLine = strchr(buffer, '\n')) == (char *) NULL) length = strlen(buffer) + 1;
	else    {
		length = endLine - buffer + 1;

		}

		/*Beta bug for ob3: in case the input file has bad formatted 
		(without basin name and/or river name etc) 
		creteria:
		check the length of each record, it fails if the length is less than
		the sum of the lengths of the basin name(25) and the river name(16)-- gzhou (09/09/2003)*/
		if(length < 41)
		{
			puts("Bad Formatted Forecast Points data ...");
			return(0);
		}


	nextLine = (char *) realloc(nextLine, length * sizeof(char));

	memset(nextLine, '\0', length);
	strncpy(nextLine, buffer, length - 1);
	temp_ptr = nextLine;
	buffer += strlen(nextLine) + 1;


	/* -------------------------------------*/
	/*      Basin Name...                   */
	/* -------------------------------------*/
	memset(data[i - 1].name, '\0', 30);
	strncpy(data[i - 1].name, temp_ptr, 25);   /* Get the Basin Name...                        */
	
	temp_ptr += 25;                                         /* Advance to the River name...                 */

	/* -------------------------------------*/
	/*      River name...                   */
	/* -------------------------------------*/
	memset(data[i - 1].river, '\0', 30);
	strncpy(data[i - 1].river, temp_ptr, 16);               /* Get the name of the River...                 */

	temp_ptr += 17;                                         /* Advance to the Basin ID...                   */

	/* -------------------------------------*/
	/*      Basin ID...                     */
	/* -------------------------------------*/
	first_blank = strchr(temp_ptr, ' ');                    /* 1st blank space following the Basin ID...    */
	length = first_blank - temp_ptr;
	memset(data[i - 1].id, '\0', 9);
	strncpy(data[i - 1].id, temp_ptr, length);              /* Get the Basin ID...                          */
	temp_ptr = first_blank;                                 /* Point to the 1st blank char in temp_ptr...   */

	while(isspace(*temp_ptr)) temp_ptr++;                   /* Advance past any white space to Latitude...  */

	/* -------------------------------------*/
	/*      Forecast Point Latitude...      */
	/* -------------------------------------*/
	first_blank = strchr(temp_ptr, ' ');                    /* 1st blank space following the Latitude...    */
	length = first_blank - temp_ptr;
	memset(tempString, '\0', 10);
	strncpy(tempString, temp_ptr, length);                  /* Get Latitude...                              */
	data[i - 1].Lat = atof(tempString);

	temp_ptr = first_blank;                                 /* Point to the 1st blank char in temp_ptr...   */

	while(isspace(*temp_ptr)) temp_ptr++;                   /* Advance past any white space to Longitude... */
	/* -------------------------------------*/
	/*      Forecast Point Longitude...     */
	/* -------------------------------------*/
	data[i - 1].Long = atof(temp_ptr);

	i++;
	data = (fcstpoint_struct *) realloc(data, sizeof(fcstpoint_struct) * i);
	}


 free(nextLine);
 free(buffer_ptr);

 if(i > 1)
	{
	*data_ptr = data;
	return (i);  /* Number of Forecast Points...         */
	}
 else   return (0);

}


/***************************************************************************/
/* FILE PATH/NAME:   (ifp_source)/IFP_Map/read_write_data.c                */
/*  FUNCTION NAME:   read_overlay_data()                                   */
/*       FUNCTION:                                                         */
/***************************************************************************/

/******************************************** BEGIN read_overlay_data2 *******/
overlay_struct  **read_overlay_data(char *fname, int *nc)
{
    FILE            *file;
    int             n;
    int             i = 1;
    int             j;
    char            temp[9];
    overlay_struct  **data;

    if((file = fopen(fname,"rb")) != NULL)
    { 
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
    else   return (NULL);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/read_write_data.c,v $";
 static char rcs_id2[] = "$Id: read_write_data.c,v 1.4 2003/09/09 17:37:50 gzhou Exp $";}
/*  ===================================================  */

}

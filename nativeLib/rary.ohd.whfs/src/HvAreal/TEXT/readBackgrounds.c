
#include "GeneralUtil.h"
#include "mapBackgrounds.h"

/***************************************************************************/

void	readMapAreasFromFile(MapArea 	**areas_ptr,
			     long 	*num_ptr,
			     char 	*fileName)
{
   FILE	  *fp = NULL ;
   char	  pathname[MAXPATHLEN];
   char	  buf[MAX_RECORD_LEN]; 
   char	  *str;
   char	  *s1;
   int	  i;
   int	  j;
   MapArea		*areas = NULL ;
   int		num_areas = 0;
   char		geo_dir[128];
   int          gad_token_len=0, gad_value_len=0;
   
   
   gad_token_len = strlen("whfs_geodata_dir");
   get_apps_defaults("whfs_geodata_dir", &gad_token_len, geo_dir, &gad_value_len);
   if (strlen(geo_dir) > 0)
   {
      sprintf(pathname,"%s/%s", geo_dir, fileName);
   }	
   else
   {
      fprintf(stderr,"Environment Variable whfs_geodata_dir not set");
      return;
   }
   
   
   if ((fp = fopen(pathname, "r")) == (FILE *) NULL)
   {
      fprintf(stderr, "read_areas: unable to open input file.\n");
      return;
   }
   
   
   num_areas = 0;
   while (fgets(buf, MAX_RECORD_LEN, fp))
   {
      if (isalpha(buf[0]))
	 num_areas++;
   }
   
   
   /* allocate memory for all areas. */
   
   if ((areas = (MapArea *) malloc(num_areas * sizeof(MapArea))) == NULL)
   {
      fprintf(stderr, "read_areas: unable to allocate memory.\n");
      return;
   }
   
   
   rewind(fp);
   for (i = 0; i < num_areas; i++)
   {
      fgets(buf, MAX_RECORD_LEN, fp);
      str = buf;
      if (isalpha(buf[0]))
      {
	 areas[i].npts = atoi(strrchr(str, ' '));
	 areas[i].poly = (XPoint *) malloc(areas[i].npts * sizeof(XPoint));
	 if (areas[i].poly == NULL)
	 {
	    fprintf(stderr, "read_areas: malloc failure.\n");
	    break;
	 }
	 
	 areas[i].actual = (M_MapPoint *) malloc(areas[i].npts * sizeof(M_MapPoint));
	 if (areas[i].actual == NULL)
	 {
	    fprintf(stderr, "read_areas: malloc failure.\n");
	    break;
	 }
      }
      
      for (j = 0; j < areas[i].npts; j++)
      {
	 fgets(buf, MAX_RECORD_LEN, fp);
	 str = buf;
	 
	 s1  = strrchr(str, ' ');
	 
	 s1++;
	 areas[i].actual[j].lon = -1.0 * atof(s1);
	 
	 s1  = strtok(str, " \t");
	 areas[i].actual[j].lat = atof(s1);
      }
   }
   
   
   
   /* set the indirect variables to the altered values.
   Do this to avoid ugly indirection in the rest of the code. */
   
   *areas_ptr = areas;
   *num_ptr = num_areas;
   
   
   /* close file and return */
   
   fclose(fp);
   return;
}

/***************************************************************************/

void readMapLinesFromFile(MapLine 	**lines_ptr,
			  long 		*num_ptr, 
			  char 		*fileName)
{
   FILE	*fp = NULL ;
   char	pathname[MAXPATHLEN];
   char	buf[MAX_RECORD_LEN];
   char	*str = NULL ;
   char	*s1 = NULL ;
   int	i;
   int	j;
   
   MapLine	*lines;
   int	num_lines = 0;

   char	geo_dir[128];
   int  gad_token_len=0, gad_value_len=0;
   
   
   gad_token_len = strlen("whfs_geodata_dir");
   get_apps_defaults("whfs_geodata_dir", &gad_token_len, geo_dir, &gad_value_len);
   if (strlen(geo_dir) > 0)
      sprintf(pathname,"%s/%s",s1, fileName);
   else
   {
      fprintf(stderr,"Environment Variable whfs_geodata_dir not set");
      return;
   } 
   
   
   if ((fp = fopen(pathname, "r")) == (FILE *) NULL)
   {
      fprintf(stderr, "read_lines: unable to open %s\n", pathname);
      return;
   }
   
   
   num_lines = 0;
   while (fgets(buf, MAX_RECORD_LEN, fp))
   {
      if (isalpha(buf[0]))
	 num_lines++;
   }
   
   /* allocate memory for all lines. */
   
   if ((lines = (MapLine *) malloc(num_lines * sizeof(MapLine))) == NULL)
   {
      fprintf(stderr, "read_lines: unable to allocate memory.\n");
      return;
   }
   
   
   rewind(fp);
   for (i = 0; i < num_lines; i++)
   {
      fgets(buf, MAX_RECORD_LEN, fp);
      str = buf;
      if (isalpha(buf[0]))
      {
	 lines[i].npts = atoi(strrchr(str, ' '));
	 lines[i].poly = (XPoint *) malloc(lines[i].npts * sizeof(XPoint));
	 if (lines[i].poly == NULL)
	 {
	    fprintf(stderr, "read_lines: malloc failure.\n");
	    break;
	 }
	 
	 lines[i].actual = (M_MapPoint *) malloc(lines[i].npts * sizeof(M_MapPoint));
	 if (lines[i].actual == NULL)
	 {
	    fprintf(stderr, "read_lines: malloc failure.\n");
	    break;
	 }
      }
      
      for (j = 0; j < lines[i].npts; j++)
      {
	 fgets(buf, MAX_RECORD_LEN, fp);
	 str = buf;
	 
	 s1  = strrchr(str, ' ');
	 
	 s1++;
	 
	 lines[i].actual[j].lon = -1.0 * atof(s1);
	 
	 s1  = strtok(str, " \t");
	 lines[i].actual[j].lat = atof(s1);
      }
   }
   
   
   /* set the indirect variables to the altered values.
   Do this to avoid ugly indirection in the rest of the code. */
   
   *lines_ptr = lines;
   *num_ptr = num_lines;
   
   
   /*
   Close file and return
   */	
   fclose(fp);
   return;
}


/***************************************************************************/

void	read_radars(MapPoint 		**radarPts_ptr,
		    long 		*num_ptr)
{
   RadarLoc	*radars = NULL , *radarPtr = NULL ;   
   MapPoint	*radarPts = NULL ;
   long		num_radars = 0 ;
   double     	row;
   double     	col;
   double     	lat;
   double     	lon;   
   int		i;   
   char		*token;
   
   
   if ( ( radars = GetRadarLoc("") ) != NULL )
   {
      num_radars = ListCount(&radars->list);
      
      radarPts = (MapPoint *) malloc(num_radars * sizeof(MapPoint));
      if (radarPts == (MapPoint *) NULL)
      {
	 fprintf(stderr,"Error allocating radarPts.\n");   
	 return;
      }
      
      radarPtr = (RadarLoc *) ListFirst(&radars->list);
      
      for (i = 0; i < num_radars; i++)
      {	
	 
	 /* set name, it could be more than one word */
	 
	 /* for now, simply set name to the lid, not the
	 associated city name */
	 
	 token = strtok(radarPtr->radid, " \t\n");
	 strcpy(radarPts[i].name, token);
	 
	 lat = radarPtr->lat;
	 lon = radarPtr->lon;
	 
	 
	 /*  this section is new */
	 
	 /* adjust the lat and lon in order to fake the
	 radar location to the center of an hrap bin */
	 
	 printf("before lat = %f lon = %f\n", lat, lon);
	 
	 LatLongToHrapByReference(lat, lon, &row, &col);
	 
	 HrapToLatLongByReference(floor(row) + 0.5,
		       floor(col) + 0.5,
		       &lat, &lon);  
	 
	 radarPts[i].loc.lat = lat; 
	 radarPts[i].loc.lon = -1*lon;
	 
	 printf("after lat = %f lon = %f\n", lat, lon);
	 
	 radarPtr = (RadarLoc *) ListNext(&radarPtr->node);
      }
   }
   
   
   /* set the indirect variables to the altered values.
   Do this to avoid ugly indirection in the rest of the code. */
   
   *radarPts_ptr = radarPts;
   *num_ptr = num_radars;
   
   
   FreeRadarLoc(radars);
   return;
}


/***************************************************************************/


char * get_name(char *buf)
{
   
   static char name[FEATURE_NAME_LEN] ;
   char * token ;
   
   strcpy(name, "");
   token = strtok(buf," \t\n");
      
   while (strcmp(token,"XXX") != 0)
   {
      strcat(name, token);
      token = strtok(buf, " \t\n");		
   }
   
   return name;
   
}


/***************************************************************************/


void	read_cities(MapPoint 	**cityPts_ptr,
		    long 	*num_ptr, 
		    long 	disp_precedence)
{
   City		*cHead = NULL , *cPtr = NULL ;
   int		i;
   char		where[BUFSIZ];   
   
   MapPoint		*cityPts = NULL;
   long		num_cities = 0;
   
   
   sprintf(where," WHERE disp_precedence = %ld", disp_precedence);
   
   if ( ( cHead = GetCity(where) ) != NULL )
   {
      num_cities = ListCount(&cHead->list);
      
      cityPts = (MapPoint *) malloc(num_cities * sizeof(MapPoint));
      
      if (cityPts == (MapPoint *) NULL)
	 return;
      
      cPtr = (City *) ListFirst(&cHead->list);
      
      for (i = 0; i < num_cities; i++)
      {	
	 
	 /* set name, it could be more than one word */
	 
	 strcpy(cityPts[i].name, cPtr->name);
	 
	 
	 /*  to slow SWS down so that towns are not shifted to NW
	    
	    ????  don't really understand this */
	 
	 fflush(stdout);
	 
	 
	 /*
	 printf("TOWN: %d <<<%s>>>, <<<%s>>>\n",
	 i,cityPts[i].name, cPtr->name);
	 */
	 
	 
	 cityPts[i].loc.lat = cPtr->lat;
	 cityPts[i].loc.lon = - cPtr->lon;
	 
	 
	 cPtr = (City *) ListNext(&cPtr->node);
      }
   }
   
   /* set the indirect variables to the altered values.
      Do this to avoid ugly indirection in the rest of the code. */
   
   *cityPts_ptr = cityPts;
   *num_ptr = num_cities;
   
   
   /*  close file and return */	
   
   FreeCity(cHead);
   return;
}


/***************************************************************************/

void readMapAreasFromDb(MapArea 	**areas_ptr, 
			long 		*num_ptr, 
			char 		*boundaryType)
{
   
}


/***************************************************************************/

void readMapLinesFromDb(MapLine 	**lines_ptr,
			long 		*num_ptr, 
			char 		*vectorType,
			long 		feature_rank)
{   

}


/***************************************************************************/
/***************************************************************************/

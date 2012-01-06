#include <string.h>
//#include "rfcwide.h"
#include "HydroStatus.h"
#include "util.h"
void   triangulateradar();
TRIANGLE *calculate_triangles();
void validate_data_points();
void printoutconfile();
void printouttrifile(char*);
TRIANGLE  *t;
long    numoftri;
//const static int HRAP_X = 200;
//const static int HRAP_Y = 200;
//const static int HRAP_XOR = 470;
//const static int HRAP_YOR = 850;
//const static int HRAP_X = 335;
//const static int HRAP_Y = 159;
//const static int HRAP_XOR = 367;
//const static int HRAP_YOR = 263;
static int HRAP_X;
static int HRAP_Y;
static int HRAP_XOR;
static int HRAP_YOR;

static char rfcwide_utiltriangles_directory[150];
static int get_apps_defaults_utiltriangles = 0;

void free_mem();
HydroStatus read_mpe_coordinate_file (int*,int*,int*,int*);

/*************************************************************************************/
static short in_recursion_array(long **array, long pnt)
{
long	i;

  if (!(*array))  { return(0);  }
  
  for (i = 1; i <= (*array)[0]; i++){
  
  	if (pnt == (*array)[i]) return(1);
  }
  return(0);
}

/*************************************************************************************/
static long *add_point_to_recursion_array(long **array, long pnt)
{
  if (!(*array)){
  	if (!((*array) = malloc(sizeof(long) * 2))) { 
	//printf("at line 622\n");exit(-2);
	}
  	(*array)[0] = 1;
  }
  else {
  	if (!((*array) = realloc(*array, (size_t)((*array)[0] + 2) * sizeof(long)))){ 
	//printf("at line 626\n"); 
	exit(-2);}
  	(*array)[0] += 1;
  }
  (*array)[(*array)[0]] = pnt;
  
  return(*array);
}
  
			

/*************************************************************************************/
static short check_crossing(p3_gage_struct *c, long pnta, long pntb, long pntx, CONTIGUITY_DELETE_ARRAY *cda, long **recursion_array_pntb)
{
long	i,j;
short	retval;
short	chkval;

  if (in_recursion_array(recursion_array_pntb, pntx)) return(LINE_A_B_NOT_CROSSED);
  
  *recursion_array_pntb = add_point_to_recursion_array(recursion_array_pntb, pntx);
  
  //Added by Ram for debugging...Apr 11'th
  {
  	//fprintf(stderr, "c[pnta].n_contig = %ld, c[pntb].n_contig = %ld\n", c[pnta].n_contig, c[pntb].n_contig);
  }
  
  retval = LINE_A_B_NOT_CROSSED;
 /* printf("inside of check cross...\n");  */ 
  for (i = 0L; i < c[pntx].n_contig; i++){
  	if (del_list(QUERY_LIST, c, pntx, c[pntx].contig[i], cda)) continue;
  	/*printf("after dellist in check crossing...\n");  */
  	switch(chk_intersect(c, pnta, pntb, pntx, c[pntx].contig[i])){
  		case LINE_A_B_SHORTER: 
  			del_list(ADD_PAIR, c, pntx, c[pntx].contig[i], cda);
  			chkval = check_crossing(c, pnta, pntb, c[pntx].contig[i],
  					cda, recursion_array_pntb);
                     /*   printf("line a_b was shorter...pnta = %ld pntb = %ld  chkval = %ld c[pntx].contig[i] = %ld\n", pnta,  pntb, chkval, c[pntx].contig[i]);
  			*/
  			if (chkval == LINE_A_B_LONGER) return(chkval);
  			
  			retval = LINE_A_B_SHORTER;
  			break;
  		case LINE_A_B_NOT_CROSSED:
  			break;
  		case LINE_A_B_LONGER:
  			return(LINE_A_B_LONGER);
  	}
  }
  
  for (i = 0L; i < c[pntx].n_contig; i++){
  	if (c[pntx].contig[i] == pntb) continue;
 	for (j = i + 1L; j < c[pntx].n_contig; j++){
 		if (c[pntx].contig[j] == pntb) continue;
 		if (!contiguous(c, c[pntx].contig[i], c[pntx].contig[j])) continue;
 		if (del_list(QUERY_LIST, c, c[pntx].contig[i],  c[pntx].contig[j], cda)) continue;
 		switch(chk_intersect(c, pnta, pntb, c[pntx].contig[i], c[pntx].contig[j])){
 			case LINE_A_B_SHORTER:
 				del_list(ADD_PAIR, c, c[pntx].contig[i],c[pntx].contig[j], cda);
 				chkval = check_crossing(c, pnta, pntb, c[pntx].contig[i], cda, recursion_array_pntb);
 				/* printf("Second part...line a_b was shorter...pnta = %ld pntb = %ld  chkval = %ld c[pntx].contig[i] = %ld\n", pnta,  pntb, chkval, c[pntx].contig[i]);
  			*/
 				if (chkval == LINE_A_B_LONGER) return(chkval);
 				chkval = check_crossing(c, pnta, pntb, c[pntx].contig[j], cda, recursion_array_pntb); 
				if (chkval == LINE_A_B_LONGER) return(chkval);
				retval = LINE_A_B_SHORTER;
				
  			
				break;
			case LINE_A_B_NOT_CROSSED:
				break;
			case LINE_A_B_LONGER:
				return(LINE_A_B_LONGER);
		}
	}
  }
  //fprintf(stderr, "c[pnta].n_contig = %ld, c[pntb].n_contig = %ld\n", c[pnta].n_contig, c[pntb].n_contig);
  return(retval);
}


/*************************************************************************************/

void HRAP_values(int* a,int* b, int* c, int* d)
{
	read_mpe_coordinate_file(&HRAP_XOR,&HRAP_YOR,&HRAP_X,&HRAP_Y);

	*a = HRAP_X;
	*b = HRAP_Y;
	*c = HRAP_XOR;
	*d = HRAP_YOR;
	//*a = 500;
	//*b = 500;
}

short del_list(short option, p3_gage_struct *c, long pnta, long pntb, CONTIGUITY_DELETE_ARRAY *cda)
{
long	i;

  switch(option){
  	case ADD_PAIR:
  		if (pntb < pnta){
  			i = pntb;
  			pntb = pnta;
  			pnta = i;
  		}
  		(cda->delete_cnt)++;
  		
  		cda->delete_array = (long *)realloc(cda->delete_array, (size_t)(sizeof(long) * cda->delete_cnt * 2));
  		
  		if (cda->delete_array == NULL){
  			//printf("couldn't realloc delete_array in del_list\n");
  			exit(1);
  		}
  		
  		cda->delete_array[(cda->delete_cnt - 1) * 2]       = pnta;
  		cda->delete_array[(cda->delete_cnt - 1) * 2 + 1]   = pntb;
  		return(0);
  	case DELETE_PAIRS:
  	       
  		for (i = 0L; i < cda->delete_cnt; i++){
  		     /*   printf("value of i = %d\n", i);  */
  			del_contiguity_point(c, cda->delete_array[i * 2],cda->delete_array[i * 2 + 1]);
  		}
  		
  		if (cda->delete_array != NULL) free(cda->delete_array);
  		cda->delete_array = NULL;
  		cda->delete_cnt = 0L;
  		return(0);
  	case RESET:
  		if (cda->delete_array != NULL) free(cda->delete_array);
  		cda->delete_array = NULL;
  		cda->delete_cnt = 0L;
  		return(0);
	case QUERY_LIST:
		if (pntb < pnta){
			i = pntb;
			pntb = pnta;
			pnta = i;
		}
		for (i = 0L; i < cda->delete_cnt; i++){
			if (pnta == cda->delete_array[i * 2]){
				if (pntb == cda->delete_array[i * 2 + 1]) return(1);
			}
		}
		return(0);
  }
  return(0);
}

/***************************************************************************************/
void   triangulateradar()
{
double      reference_lat,  reference_lon;
reference_lon = radarpts[0].lon;
reference_lat = radarpts[0].lat;
//printf("starting to triangulate the radar data....\n");


ll_to_mi(radarpts, numofradarpts, reference_lon, reference_lat);
//printf("starting to get contig....\n");
get_contig(radarpts, numofradarpts);
//printf("done get contig....\n");
mi_to_ll(radarpts, numofradarpts, reference_lon, reference_lat);
printoutconfile();
t = calculate_triangles(radarpts, numofradarpts, &numoftri, t);
printouttrifile("grid_triangles");

free_mem();
}  
/***********************************************************************************/
void printoutconfile()
{

FILE   *outfile;
int    counter;
int    i, j, k, len=0;
char   fname[150], pdir[150];


bzero(fname,150);
counter = 0;
if(get_apps_defaults_utiltriangles == 0)
{
	len = strlen("rfcwide_utiltriangles_dir");
	get_apps_defaults("rfcwide_utiltriangles_dir",&len,pdir,&len);
	sprintf(fname, "%s/radarconfile", pdir);
	printf(fname, "%s/radarconfile\n", pdir);
	bzero(rfcwide_utiltriangles_directory,150);
        strcpy(rfcwide_utiltriangles_directory,pdir);
        get_apps_defaults_utiltriangles = 1;
}
else
{
	bzero(pdir,150);
	strcpy(pdir,rfcwide_utiltriangles_directory);
}
//printf ( "opening file: %s\n", fname );
outfile = fopen(fname, "wb"); 

if ( outfile == NULL )
{
   fprintf ( stdout, "Could not open radar config file:\n%s\n", fname ); 
   exit(-1);
}

for(i = 0; i < HRAP_X; i++)
     for( j = 0; j < HRAP_Y; j++){
       fwrite(&radarpts[counter].lat, sizeof(double),1, outfile);
       fwrite(&radarpts[counter].lon, sizeof(double),1, outfile);
       fwrite(&radarpts[counter].n_contig, sizeof(long),1, outfile);
//       fprintf(stderr,"lat = %lf, lon = %lf, n_contig = %ld", *&radarpts[counter].lat,*&radarpts[counter].lon,*&radarpts[counter].n_contig);

  //     printf("\n%lf %lf %lf: \n",radarpts[counter].lat, radarpts[counter].lat, radarpts[counter].lat);

       for(k=0; k < radarpts[counter].n_contig; k++)
       {
    //     printf("%ld - ", radarpts[counter].contig[k]);
         fwrite(&radarpts[counter].contig[k], sizeof(long),1, outfile);
//	 fprintf(stderr, "contig[%d] = %ld", k, *&radarpts[counter].contig[k]);
       }
  //     fprintf(stderr, "\n\n");

       counter++;
       }
}
/**********************************************************************************/
void printouttrifile(char* str)
{
char   fname[150], pdir[150];
FILE   *outfile;
int    i, len;

if(!strcmp(str,"grid_triangles"))
{
	if(get_apps_defaults_utiltriangles == 0)
	{
		len = strlen("rfcwide_utiltriangles_dir");
		get_apps_defaults("rfcwide_utiltriangles_dir",&len,pdir,&len);
		sprintf(fname, "%s/utiltriangles", pdir);
		printf("%s/utiltriangles\n", pdir);
		bzero(rfcwide_utiltriangles_directory,150);
    		strcpy(rfcwide_utiltriangles_directory,pdir);
       		get_apps_defaults_utiltriangles = 1;
	}
	else
	{
		sprintf(fname, "%s/utiltriangles", rfcwide_utiltriangles_directory);
		//bzero(pdir,150);
		//strcpy(pdir,rfcwide_utiltriangles_directory);
	}
}
else
{
	len = strlen("rfcwide_gagetriangles_dir");
	get_apps_defaults("rfcwide_gagetriangles_dir",&len,pdir,&len);
	sprintf(fname, "%s/gagetriangles", pdir);
}

//printf("printing out triangles..please wait.....");
outfile = fopen(fname, "wb");

if ( outfile == NULL )
{
   fprintf ( stdout, "Could not open triangles file:%s\n", fname );
   exit(-1);
}

for (i=0; i<numoftri; i++)
 {
   fwrite (&t[i].a, sizeof(long),1, outfile);
   fwrite (&t[i].b, sizeof(long),1, outfile);
   fwrite (&t[i].c, sizeof(long),1, outfile);
   fwrite (&t[i].a_to_b, sizeof(long),1, outfile);
   fwrite (&t[i].b_to_c, sizeof(long),1, outfile);
   fwrite (&t[i].c_to_a, sizeof(long),1, outfile);
//   fprintf(stderr, "a = %ld, b = %ld, c = %ld, a_to_b = %ld, b_tO_c = %ld, c_to_a = %ld\n\n",t[i].a,t[i].b,t[i].c,t[i].a_to_b,t[i].b_to_c,t[i].c_to_a);
 }
fclose(outfile);


}
/**********************************************************************************/  
void ll_to_mi(p3_gage_struct * c,
              long numpnt,
              double reference_lon,
              double reference_lat)
{
double factor;
long i;

for(i = 0L; i < numpnt; i++)
  {
  factor = (reference_lat + c[i].lat) * PI / 360.;
  
  c[i].lon = 69.055 * cos(factor) * (c[i].lon - reference_lon);
  
  c[i].lat = 69.055 * (c[i].lat - reference_lat);
  
/*  printf("%d %s   %lf  %lf\n", i, c[i].id, c[i].lon, c[i].lat);  */
  }
return;
}

/*************************************************************************************/

void mi_to_ll(p3_gage_struct * c,
              long numpnt,
              double reference_lon,
              double reference_lat)
{
double factor;
long i;

for(i = 0L; i < numpnt; i++)
  {
  c[i].lat = reference_lat + c[i].lat / 69.055;

  factor = (reference_lat + c[i].lat) * PI / 360.;

  c[i].lon = reference_lon + c[i].lon / (69.055 * cos(factor));
  }
return;
}

/*************************************************************************************/

short try_to_connect_a_to_b(p3_gage_struct *c, long pnta, long pntb, CONTIGUITY_DELETE_ARRAY *cda)
{
long	*recursion_array_pntb = NULL;
long	*possible_orphan_points = NULL;

 /* printf("inside of trytoconnect a to b \n");  */
  switch(check_crossing(c, pnta, pntb, pntb, cda, &recursion_array_pntb)){
  	case LINE_A_B_SHORTER:
  	     /*   printf("back from check cross...shorter***********************************\n"); 
  	   */
  		possible_orphan_points = get_deleted_pnts_not_contig(c, pntb, cda);
  	
  		if (possible_orphan_points){
  			free(possible_orphan_points);
  			possible_orphan_points = NULL;

                        if ( recursion_array_pntb != NULL )
                        {
                           free ( recursion_array_pntb );
                           recursion_array_pntb = NULL;
                        }

  			return(0);
  		}
  		
  		add_contiguity_point(c, pnta, pntb);
  		
  		del_list(DELETE_PAIRS, c, -1L, -1L, cda);
  		
  		try_to_connect_a_to_b_children(c, pnta, pntb, cda);
  		
  		break;
  	case LINE_A_B_NOT_CROSSED:
  	      
  		add_contiguity_point(c, pnta, pntb);
  		try_to_connect_a_to_b_children(c, pnta, pntb, cda);
  		break;
  }
  
  if (recursion_array_pntb){
  	free(recursion_array_pntb);
  	recursion_array_pntb = NULL;
  }
 
  return(0);
}	
  		
/***************************************************************************************/

void get_contig(c, numpnt)
p3_gage_struct  *c;
long numpnt;
{
CONTIGUITY_DELETE_ARRAY	cda;
long	i;
long	pnta, pntb;
long	*recursion_array = NULL;

  cda.delete_array = NULL;
  cda.delete_cnt = 0;

  for (i = 0; i < numpnt; i++) {
	c[i].n_contig = 0;
	c[i].contig = NULL;
  }


  for (pnta = 1; pnta < numpnt; pnta++){
 // printf("pnta = %ld\n",pnta);

  	pntb = pnta - 1;
	while (c[pntb].n_contig == -1) pntb--;
	pntb = get_seed_point(c, pnta, pntb, &recursion_array);
	//printf("have gotten seed point... value = %d\n", pntb);  
	 
	if (recursion_array){
		free(recursion_array);
		recursion_array = NULL;
	}
	
	if (pntb == -1){
		c[pnta].n_contig = -1;
		continue;
	}
	
	if (pntb == -2){
	     c[pnta].n_contig = -1;
	     continue;
	     //   printf("at line 122...exiting...
	//	exit (-1);  
	}
	
	//Added by Ram Apr 11'th for debugging....
	
	{
		//fprintf(stderr, "pntb = %ld\n", pntb);
		//fprintf(stderr, "pnta.n_contig = %ld\n", c[pnta].n_contig);
	}
	
	del_list(RESET, c, -1L, -1L, &cda);
	//printf("done deleting list....\n"); 
	try_to_connect_a_to_b(c, pnta, pntb, &cda);
	//for(j=0;j<c[pnta].n_contig;j++)
	//{
		//printf("%ld + ",c[pnta].contig[j]);
	//}
	//fprintf(stderr, "pnta = %ld    ", pnta);
	//fprintf(stderr, "pnta.n_contig = %ld\n", c[pnta].n_contig);
	//printf("done trying to connect....\n");  
	if (c[pnta].n_contig == 0){
	        //printf("at line 131...exiting...");
		//added by Ram - Apr 8'th.
		fprintf(stderr,"each point must connect atleast to one other point while forming the triangles, which was not possible for this point here...\n");
	}
  }
  
  /* Make sure CDA array is deallocated. */
  if ( cda.delete_array != NULL )
  {
     free ( cda.delete_array );
     cda.delete_array = NULL;
  }
}

/*************************************************************************************/

long get_seed_point(p3_gage_struct *c, long pnta, long pntb, long **recursion_array)
{
long	i, j, seed, retval;

  if (in_recursion_array(recursion_array, pntb)) return(-2);

  *recursion_array = add_point_to_recursion_array(recursion_array, pntb);

  if (c[pnta].lon == c[pntb].lon && c[pnta].lat == c[pntb].lat) return(-1);

  retval = pntb;
  if (c[pntb].n_contig == 0) return(retval);

  for (i = 0L; i < c[pntb].n_contig; i++){
   if (!chk_intersect(c, pnta, pntb, pntb, c[pntb].contig[i])) continue;
 
   seed = get_seed_point(c, pnta, c[pntb].contig[i], recursion_array);
   if (seed == -2) continue;
 
   return(seed);
  }

  for (i = 0L; i < c[pntb].n_contig; i++){
  	for (j = i + 1L; j < c[pntb].n_contig; j++){
  		if (!contiguous(c, c[pntb].contig[i], c[pntb].contig[j])) continue;
  		
  		if (!chk_intersect(c, pnta, pntb, c[pntb].contig[i],c[pntb].contig[j])) continue;
  		
  		seed = get_seed_point(c, pnta, c[pntb].contig[i], recursion_array);
  		
  		if (seed == -2){
  			seed = get_seed_point(c, pnta, c[pntb].contig[j], recursion_array); 
  			return(seed);
  		}
  		
  		return(seed);
  	}
  }
  
  return(retval);
}

/*************************************************************************************/
  		
long *get_deleted_pnts_not_contig(p3_gage_struct *c, long pntb, CONTIGUITY_DELETE_ARRAY *cda)
{
long	i;
long	*possible_orphan_points = NULL;

  if (cda->delete_cnt > 0){
  	for (i = 0; i < cda->delete_cnt * 2; i++){
  		if (pntb == cda->delete_array[i]) continue;
  		if (contiguous(c, pntb, cda->delete_array[i])) continue;
  		possible_orphan_points = add_point_to_recursion_array(&possible_orphan_points, cda->delete_array[i]);
  	}
  }
  return(possible_orphan_points);
}

/*************************************************************************************/

short try_to_connect_a_to_b_children(p3_gage_struct *c, long pnta, long pntb, CONTIGUITY_DELETE_ARRAY *cda)
{
long	i;
long     *contig_lst = NULL;
long	n_contig;

  n_contig = c[pntb].n_contig;
  if ((contig_lst = (long *)malloc((size_t)(sizeof(long) * n_contig))) == NULL){
  	//printf("error in malloc try_to_connect_a_to_b_children\n");
  	exit(1);
  }
  
  for (i = 0L; i < n_contig; i++) contig_lst[i] = c[pntb].contig[i];
  
  for (i = 0L; i < n_contig; i++){
  	if (pnta == contig_lst[i]) continue;
  	if (contiguous(c, pnta, contig_lst[i])) continue;
  	del_list(RESET, c, -1L, -1L, cda);
  	
  	try_to_connect_a_to_b(c, pnta, contig_lst[i], cda);
  }
  
  if ( contig_lst != NULL )
  {
     free(contig_lst);
     contig_lst = NULL;
  }

  return(0);
}     

/*************************************************************************************/
short chk_intersect(p3_gage_struct *c, long pnta, long pntb, long pntc, long pntd)
{
short t1, t2, t3, t4;

/*printf("inside chk_intersect...."); printf("a %ld    b %ld     c %ld    d %ld\n", pnta, pntb, pntc, pntd);
printf("inside chk_intersect....%s %s %s %s \n", c[pnta].id, c[pntb].id,c[pntc].id, c[pntd].id);
 */ t1 = ccwise(&c[pnta].lon, &c[pnta].lat,
  	      &c[pntb].lon, &c[pntb].lat,
  	      &c[pntc].lon, &c[pntc].lat);
  t2 = ccwise(&c[pnta].lon, &c[pnta].lat,
  	      &c[pntb].lon, &c[pntb].lat,
  	      &c[pntd].lon, &c[pntd].lat);
  t3 = ccwise(&c[pntc].lon, &c[pntc].lat,
  	      &c[pntd].lon, &c[pntd].lat,
  	      &c[pnta].lon, &c[pnta].lat);
  t4 = ccwise(&c[pntc].lon, &c[pntc].lat,
  	      &c[pntd].lon, &c[pntd].lat,
  	      &c[pntb].lon, &c[pntb].lat);
/*printf("inside chk_intersect...."); printf("t1 %ld    t2 %ld    t3 %ld    t4 %ld\n", t1, t2, t3, t4);
  */	      
  if (t1 * t2 <= 0 && t3 * t4 <= 0){
  	if (t1 == -1 || t1 == 1 ||
  	    t2 == -1 || t2 == 1 ||  	      
  	    t3 == -1 || t3 == 1 ||  
  	    t4 == -1 || t4 == 1){
  	    		if ((pnta == pntc || pnta == pntd) &&
  	    		    (pntb != pntc && pntb != pntd)) return(LINE_A_B_NOT_CROSSED);
  	    		if ((pntb == pntc || pntb == pntd) &&
  	    		    (pnta != pntc && pnta != pntd)) return(LINE_A_B_NOT_CROSSED);
  	    		
  	    		if ((c[pntb].lon - c[pnta].lon) *
  	    		    (c[pntb].lon - c[pnta].lon) +
  	    		    (c[pntb].lat - c[pnta].lat) *
   	    		    (c[pntb].lat - c[pnta].lat) <
			    (c[pntd].lon - c[pntc].lon) *
  	    		    (c[pntd].lon - c[pntc].lon) +
  	    		    (c[pntd].lat - c[pntc].lat) *
   	    		    (c[pntd].lat - c[pntc].lat)) return(LINE_A_B_SHORTER);
   	    		return(LINE_A_B_LONGER);
   	}
   	
   	if (t1 + t2 + t3 + t4 == 0) return(LINE_A_B_NOT_CROSSED);
   	
   	if (((t1 == 2 && t2 == 0) || (t1 == 0 && t2 == 2)) &&
   	    ((t3 == 2 && t4 == 0) || (t3 == 0 && t4 == 2))) return(LINE_A_B_NOT_CROSSED);     	    		   
 	    		         
	if ((c[pntb].lon - c[pnta].lon) *
	    (c[pntb].lon - c[pnta].lon) +
	    (c[pntb].lat - c[pnta].lat) *
	    (c[pntb].lat - c[pnta].lat) <
	    (c[pntd].lon - c[pntc].lon) *
	    (c[pntd].lon - c[pntc].lon) +
	    (c[pntd].lat - c[pntc].lat) *
	    (c[pntd].lat - c[pntc].lat)) return(LINE_A_B_SHORTER);
	return(LINE_A_B_LONGER);
  }
  return(LINE_A_B_NOT_CROSSED);
}	  	      
  	      
/*************************************************************************************/
short ccwise(double *pax, double *pay, double *pbx, double *pby, double *pcx, double *pcy)
{
double dx1, dx2, dy1, dy2;

  dx1 = *pbx - *pax;
  dy1 = *pby - *pay;
  dx2 = *pcx - *pax;
  dy2 = *pcy - *pay;
  

  if (dx1 * dy2 > dx2 * dy1) return(1);
  
  if (dx1 * dy2 < dx2 * dy1) return(-1);    	      
  	      
  if (dx1 * dx2 < 0.0 || dy1 * dy2 < 0.0) return(-2);
  
  if (dx1 * dx1 + dy1 * dy1 >= dx2 * dx2 + dy2 * dy2) return(0);
  
  return(2);
}

/*************************************************************************************/
short contiguous(p3_gage_struct *c, long pnta, long pntb)
{
long 	i;

  for (i = 0L; i < c[pnta].n_contig; i++){
      if (c[pnta].contig[i] == pntb) return(1);
  }
  
  return(0);
}


/*************************************************************************************/
short internal_triangle(p3_gage_struct *c, long pnta, long pntb, long pntc, long pntx)
{
short	i;
short	cross = 0;
double   slope, intercept, test;
long	pnt[4];

  pnt[0] = pnt[3] = pnta;
  pnt[1] = pntb;
  pnt[2] = pntc;
  
  cross = 0;
  
  for (i = 0; i < 3; i++){
  	if ((c[pntx].lon > c[pnt[i]].lon && c[pntx].lon <= c[pnt[i+1]].lon) ||
	    (c[pntx].lon < c[pnt[i]].lon && c[pntx].lon >= c[pnt[i+1]].lon)){
	    	if (c[pntx].lat > c[pnt[i]].lat && c[pntx].lat > c[pnt[i+1]].lat) continue;
	    	if (c[pntx].lat <= c[pnt[i]].lat && c[pntx].lat <= c[pnt[i+1]].lat) cross++;
	    	else{
	    		slope = (c[pnt[i+1]].lat - c[pnt[i]].lat) / (c[pnt[i+1]].lon - c[pnt[i]].lon);
	    		intercept = c[pnt[i]].lat - slope * c[pnt[i]].lon;
	    		test = slope * c[pntx].lon + intercept;
	    		if (test >= c[pntx].lat) cross++;
	    	}
	}
  }
  return(cross % 2);
}


/*************************************************************************************/
short add_tripnt(TRIPNT *tripnt, long pnta, long pntb, long pntc, long tri_idx)
{
  
  tripnt[pnta].n_triangles++;
  tripnt[pntb].n_triangles++;
  tripnt[pntc].n_triangles++;

  tripnt[pnta].tri_pnt = (long *)realloc(tripnt[pnta].tri_pnt,(size_t)(sizeof(long) * tripnt[pnta].n_triangles));
  tripnt[pntb].tri_pnt = (long *)realloc(tripnt[pntb].tri_pnt,(size_t)(sizeof(long) * tripnt[pntb].n_triangles));
  tripnt[pntc].tri_pnt = (long *)realloc(tripnt[pntc].tri_pnt,(size_t)(sizeof(long) * tripnt[pntc].n_triangles));

  if (!tripnt[pnta].tri_pnt || !tripnt[pntb].tri_pnt || !tripnt[pntc].tri_pnt){
     //printf("Couldn't realloc tripnts in add_tripnt\n");
     exit(1);
  }
  
  tripnt[pnta].tri_pnt[tripnt[pnta].n_triangles - 1] = tri_idx;
  tripnt[pntb].tri_pnt[tripnt[pntb].n_triangles - 1] = tri_idx;
  tripnt[pntc].tri_pnt[tripnt[pntc].n_triangles - 1] = tri_idx;
  
  return(0);
}


/*************************************************************************************/
short add_contiguity_point(p3_gage_struct *c, long pnta, long pntb)
{

  if (contiguous(c, pnta, pntb)) return(0);
  
  c[pnta].n_contig++;
  c[pnta].contig = (long *)realloc(c[pnta].contig, (size_t)(sizeof(long) * c[pnta].n_contig));
  if (c[pnta].contig == NULL){
  	//printf("Couldn't alloc contig in add_contiguity_point\n");
  	exit(1);
  }
  
  c[pntb].n_contig++;
  c[pntb].contig = (long *)realloc(c[pntb].contig, (size_t)(sizeof(long) * c[pntb].n_contig));
  if (c[pntb].contig == NULL){
  	//printf("Couldn't alloc contig in add_contiguity_point\n");
  	exit(1);
  }

  c[pnta].contig[c[pnta].n_contig - 1] = pntb;
  c[pntb].contig[c[pntb].n_contig - 1] = pnta;
  
  return(0);
}
        
    	    
/*************************************************************************************/
short del_contiguity_point(p3_gage_struct *c, long pnta, long pntb)
{
long i;
 /* printf("inside of delete cont point....\n");  */
  if (!contiguous(c, pnta, pntb)) return(0);
 
  for (i = 0L; i < c[pnta].n_contig; i++)
       {
  	if (c[pnta].contig[i] == pntb){
  		c[pnta].contig[i] = c[pnta].contig[c[pnta].n_contig - 1L];
  		c[pnta].n_contig--;
		if (c[pnta].n_contig > 0L){
			c[pnta].contig = (long *)realloc(c[pnta].contig, (size_t)(sizeof(long) * c[pnta].n_contig));
			if (c[pnta].contig == NULL){
			  	//printf("Couldn't alloc contig in del_contiguity_point\n");
			  	exit(1);
			}
		}
		else{
			free(c[pnta].contig);
			c[pnta].contig = NULL;
		}
		break;
	}
  }
  
  for (i = 0L; i < c[pntb].n_contig; i++){
  	if (c[pntb].contig[i] == pnta){
  		c[pntb].contig[i] = c[pntb].contig[c[pntb].n_contig - 1L];
  		c[pntb].n_contig--;
		if (c[pntb].n_contig > 0L){
			c[pntb].contig = (long *)realloc(c[pntb].contig, (size_t)(sizeof(long) * c[pntb].n_contig));
			if (c[pntb].contig == NULL){
			  	//printf("Couldn't alloc contig in del_contiguity_point\n");
			  	exit(1);
			}
		}
		else{
			free(c[pntb].contig);
			c[pntb].contig = NULL;
		}
		break;
	}
  }
  return(0);
}


/*************************************************************************************/
TRIANGLE *calculate_triangles(p3_gage_struct *c, long npnt, long *num_triangles, TRIANGLE *triangle)
{
long 	beg_time, end_time;
long	pnt, i, j, k;
short	iside;
short	internal = 0;
TRIANGLE2	*triangle2;
TRIPNT 		*tripnt = NULL;
short	found;

#define TRIANGLE_ALLOCATE_GRANULE	5000

  beg_time = time(NULL);
  
  if (!(tripnt = (TRIPNT *)malloc(npnt * sizeof(TRIPNT)))){
  	  //printf("Couldn't allocate triangle points\n");
  	  exit(1);
  }
  
  for (i = 0; i < npnt; i++){
  	tripnt[i].n_triangles = 0;
  	tripnt[i].tri_pnt = NULL;
  }
  
  *num_triangles = 0;

  if (triangle != NULL) 
  {
     free(triangle);
     triangle = NULL;
  }
  
  
  for (pnt = 0; pnt < npnt; pnt++){
  	for (i = 0; i < c[pnt].n_contig; i++){
  		if (c[pnt].contig[i] < pnt) continue;
  		for (j = i + 1; j < c[pnt].n_contig; j++){
  			if (c[pnt].contig[j] < pnt) continue;
  			
  			if (!contiguous(c, c[pnt].contig[i], c[pnt].contig[j])) continue;
  			
  			for (k = 0; k < c[pnt].n_contig; k++){
  				internal = 0;
  				if (c[pnt].contig[k] == c[pnt].contig[i]) continue;
  				if (c[pnt].contig[k] == c[pnt].contig[j]) continue;

				if (!contiguous(c, c[pnt].contig[i], c[pnt].contig[k])) continue;
				if (!contiguous(c, c[pnt].contig[j], c[pnt].contig[k])) continue;

				if ((internal = internal_triangle(c, pnt, c[pnt].contig[i],c[pnt].contig[j],c[pnt].contig[k]))) break;
				
			}
			
			if (internal) continue;
			
			if (!(*num_triangles % TRIANGLE_ALLOCATE_GRANULE)){
				triangle = (TRIANGLE *)realloc(triangle, (size_t)(sizeof(TRIANGLE) * (*num_triangles + TRIANGLE_ALLOCATE_GRANULE)));
			}
			(*num_triangles)++;
			
			if (triangle == NULL){
				//printf("Could not alloc triangle\n");
				exit(1);
			}
			
			triangle[*num_triangles - 1].a = pnt;

			
			if (c[pnt].contig[i] < c[pnt].contig[j]){
				triangle[*num_triangles - 1].b = c[pnt].contig[i];
				triangle[*num_triangles - 1].c = c[pnt].contig[j];
			}
			else {
				triangle[*num_triangles - 1].b = c[pnt].contig[j];
				triangle[*num_triangles - 1].c = c[pnt].contig[i];
			}
			
			add_tripnt(tripnt, pnt, c[pnt].contig[i], c[pnt].contig[j], *num_triangles - 1);
		}
	}
  }
  
  end_time = time(NULL);
  
  //printf("%ld triangles made in phase 1 time = %ld\n", *num_triangles, end_time - beg_time);
  
  beg_time = time(NULL);
  
  triangle = (TRIANGLE *)realloc(triangle, (size_t)(sizeof(TRIANGLE) * *num_triangles));
  
  if (triangle == NULL){
  	//printf("Couldn't alloc triangles in phase 2\n");
  	exit(1);
  }
  
  triangle2 = (TRIANGLE2 *)triangle; 
  
  for (i = 0; i < *num_triangles; i++){
  	for (j = 3; j < 6; j++){
  		triangle2[i].value[j] = -1;
  	}
  }
  
  #define PNTJ triangle2[i].value[iside % 3]
  #define PNTK triangle2[i].value[(iside + 1) % 3]  
			
  for (i = 0; i < *num_triangles; i++){
  	for (iside = 3; iside < 6; iside++){
  		found = 0;
  		for (j = 0; !found && j < tripnt[PNTJ].n_triangles; j++){
  			if (tripnt[PNTJ].tri_pnt[j] == i) continue;
  			for (k = 0; !found && k < tripnt[PNTK].n_triangles; k++){
  				if (tripnt[PNTJ].tri_pnt[j] == tripnt[PNTK].tri_pnt[k]){
  					found = 1;
  					triangle2[i].value[iside] = tripnt[PNTJ].tri_pnt[j];
  				}
  			}
  		}
  	}
  }
  
  #undef PNTJ
  #undef PNTK
  
  end_time = time(NULL);
  
  //printf("phase 2 done...time %ld\n", end_time - beg_time);
  
  for (i = 0; i < npnt; i++){
  	if (tripnt[i].n_triangles > 0) free(tripnt[i].tri_pnt);
  }
  
  free(tripnt);

  return(triangle);
}

/*************************************************************************************/
void validate_data_points(p3_gage_struct *c, long numpnt)
{
long i, j, k, m, cont_i, cont_j;

//printf("inside validate...\n");
for(i = 0; i < numpnt - 1; i++)
  {
  cont_i = 1;
  fprintf(stderr, "\rprocessing point %ld", i);
  fflush(stdout);
  for(j = i + 1; j < numpnt && cont_i; j++)
    {
    cont_j = 1;
    for(k = 0; k < c[i].n_contig && cont_i && cont_j; k++)
      {
      if(c[i].contig[k] < i)continue;

      for(m = 0; m < c[j].n_contig && cont_i && cont_j; m++)
        {
        if(c[j].contig[m] < j)continue;

        switch(chk_intersect(c, i, c[i].contig[k], j, c[j].contig[m]))
          {
          case LINE_A_B_LONGER:
            fprintf(stderr, "\nlong  line %ld - %ld crosses %ld - %ld",
                    i, c[i].contig[k],
                    j, c[j].contig[m]);
            cont_i = 0;
            break;

          case LINE_A_B_SHORTER:
            fprintf(stderr, "\nshort line %ld - %ld crosses %ld - %ld",
                    j, c[i].contig[m],
                    i, c[j].contig[k]);
            cont_j = 0;
            break;
          }
        }
      }
    }
  }
return;
}
void free_mem()
{
	int i;
	for(i=0;i<numofradarpts;i++)
	{
		if(radarpts[i].contig != NULL)
		{
			free(radarpts[i].contig);
			radarpts[i].contig = NULL;
		}
	}
	if(radarpts != NULL)
	{
		free(radarpts);
		radarpts = NULL;
	}
}

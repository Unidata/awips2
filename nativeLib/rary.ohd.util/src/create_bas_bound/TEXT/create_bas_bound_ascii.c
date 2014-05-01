/*********************************************************************************
*                                                                                *
* Function:  create_bas_bound_ascii.c                                            *
* Purpose:   Takes the output from an NWSRFS run (@DUMP PUNCH BASIN)             *
*            processes one basin at a time and writes information to an ascii    *
*            file.  Later call to function create_bas_bound_bin reads            *
*            the ascii file and creates the binary file in a standard binary     *
*            format for RFC applications.                                        *
* Written by:  Donna Page - HRL - 9 Feb. 1994                                    *
*                                                                                *
* Modified  by Guoxian Zhou 11 Oct 2002                                          *
*                                                                                *
*********************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define  TRUE    1
#define  FALSE   0
#define  MAXCHAR 121
#define  MAXPOINTS 500

void create_bas_bound_ascii(char *nwsrfs_fname, char *ascii_fname)
{
	FILE   *infile, *outfile;
	int    i, count, num_pairs, len; 
	int    first_line, end_of_pts, basin_written;
	int    order, index;
	double *lat, *lon;
        char   string[MAXCHAR], line[MAXCHAR], desc[MAXCHAR];
	char   *token, id[10];
	char   tokensep[] = "\t,() \012"; 
	div_t  result;

	int    write_basin(char *, char *, int, int, double *, double *, FILE *);
	void   set_variables(int *, int *, int *, int *);

	lat     = (double *)malloc(MAXPOINTS*sizeof(double));
	lon     = (double *)malloc(MAXPOINTS*sizeof(double));

	if(lat==NULL || lon==NULL)
	{
        	printf("Memory allocation failed!");
		exit(1);
	}	

	infile  = fopen(nwsrfs_fname,"r");
	outfile = fopen(ascii_fname,"w");

	if ( (infile==NULL) || (outfile==NULL))
	{
	   printf("Error when attempting to open infile or outfile \n");
	   printf(" infile name=%s\n outfile name=%s\n",nwsrfs_fname,ascii_fname);
	   exit(1);
	}

	order = -1;
	index = 1;

	set_variables(&count, &num_pairs, &first_line, &end_of_pts);

	while(fgets(string,MAXCHAR,infile) != (char *)NULL)
	{
		memset(line, '\0', MAXCHAR);
		len = strlen(string);
		strncpy(line,string,len);

		token = strtok(string, tokensep);
		
  		if( ( token !=NULL) && (strcmp(token,"BASN") == 0) )
		{
			/* set first line flag */
			first_line = TRUE;

			/*Check if the previous basen data is not written to file.
			  in case there is no "AREA", "ELEV" or "END" at the end */
			if ( num_pairs > 0 )  
			{
				basin_written = write_basin(id, desc, order, num_pairs,
							 lat, lon, outfile);
			        count      = -1;
			        num_pairs  = 0;
			        end_of_pts = FALSE;
			}			

            		/* Reset the other parameters 
		           in case there is value for "AREA" or "ELEV" */
		        count      = -1;
		        num_pairs  = 0;
		        end_of_pts = FALSE;

			/* get basin id */
			token = strtok(NULL, tokensep);

                        if(token == NULL) break;
			
			if( (strcmp(token,"ENGL") == 0) || (strcmp(token,"METR") == 0) )
				token = strtok(NULL, tokensep);

                        if (token != NULL)
                                strcpy(id,token);                        

			/* get descriptor - search for ' or ( */
			if(strpbrk(line, "'") != (char *)NULL)
				token = strtok(NULL, "'");
			else
				token = strtok(NULL, "(");

                        if (token != NULL)
                                strcpy(desc,token);
		}      

		if (first_line == TRUE)
		{
			token = strtok(NULL, tokensep);
			first_line = FALSE;
		}
		if ( (token!=NULL)
			 && (strcmp(token,"AREA") != 0)
			 && (strcmp(token,"ELEV") != 0) 
			 && (strcmp(token,"END") != 0) )
		{
			 /* Process the lat-lon data for the line by determining if 
			the count is even or odd.  If count is even, convert the string 
			to a float in the lat array, if odd, put it in the lon array.
			*/
			while ( (token != (char *)NULL) && (end_of_pts == FALSE) )
			{
				count++;

				if (count != 0)
				   result = div(count,2);

    		        	if ( (count == 0) || (result.rem == 0) )
				   lat[num_pairs] = atof(token);
    		        	else
				{
				   lon[num_pairs] = atof(token);
				   num_pairs++;

				   /* Modified by Guoxian Zhou, Memory reallocation dynamically*/
				   if (num_pairs >= (index * MAXPOINTS))
				   {
					index++ ;
					lat    = (double *)realloc(lat, MAXPOINTS*index*sizeof(double));
					lon    = (double *)realloc(lon, MAXPOINTS*index*sizeof(double));
					if(lat==NULL || lon==NULL)
					{
						printf("Memory reallocation failed!");
						exit(1);
					}
			       	   }
				   /* End modification by Guoxian Zhou */  

				}

				token = strtok(NULL, tokensep);

				if (token != NULL)
				{
					/* check to see if you are done with lat-lon values */
					if ( (strcmp(token,"AREA") == 0)
						 || (strcmp(token,"ELEV") == 0)
						 || (strcmp(token,"END") == 0))
					{
					   end_of_pts = TRUE;
					}
				}

			} /* end of while - done processing one line */
		}
		else
		{
		   end_of_pts = TRUE;
		}
	        /* end of if - done processing one basin */

		if ( end_of_pts == TRUE) 
		{
			if ( num_pairs > 0 )  
			{
				basin_written = write_basin(id, desc, order, num_pairs,
							 lat, lon, outfile);
				set_variables(&count, &num_pairs, &first_line, &end_of_pts);
			}

			count      = -1;
			num_pairs  = 0;
			index = 1;
			end_of_pts = FALSE;			
		}
	}
	/* end of while loop - finished with file */
	
	/*Free the memory */
	if(lat !=NULL) free(lat);
	if(lon !=NULL) free(lon);

	fclose(infile);
	fclose(outfile);
}      

int write_basin(char *id, char *name, int order, int num_pairs, 
		double *lat, double *lon, FILE *outfile)
{
   int i;

   /* "name" not currently used by subsequent programs - replace with 
    *  standard holder.
    */
   strcpy(name,"XXX");

   /* write info for one basin to the file */
   fprintf(outfile, "%s %s %d %d\n", id, name, order, num_pairs);
   for(i=0; i<num_pairs; i++)
      fprintf(outfile, "%.4f  %.4f\n", lat[i], lon[i]);

   /* return 1 (TRUE) */
   return(1);  
}   

void set_variables(int *count, int *num_pairs, int *first_line,
		   int *end_of_pts)
{
   /* reinitialize variables before processing the next basin */
   *count      = -1;
   *num_pairs  = 0;
   *first_line = FALSE;
   *end_of_pts = FALSE;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/create_bas_bound/RCS/create_bas_bound_ascii.c,v $";
 static char rcs_id2[] = "$Id: create_bas_bound_ascii.c,v 1.2 2003/12/11 22:05:42 lwu Exp $";}
/*  ===================================================  */

}

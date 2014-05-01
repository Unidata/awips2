/*********************************************************************************
*                                                                                *
* Function:  create_bas_bound_bin.c                                              *
* Purpose:   Takes the output from a create_bas_bound_ascii run and writes       *
*            to a binary file in a standard binary format for RFC applications.  *  
* Written by:  Donna Page - HRL - 9 Feb. 1994                                    *
*                                                                                *
*********************************************************************************/
#include <stdio.h>
#define  MAXPOINTS 500 

typedef struct
{
   float x,y;
}  HRAP;

HRAP LatLongToHrap();

void create_bas_bound_bin(char *ascii_fname, char *bin_fname)
{
   FILE  *infile, *outfile;
   int   i, order, npts;
   int   len_id, len_name;
   char  id[9], name[21];
	HRAP  *hrap;
/*   HRAP  hrap[5000];*/
   float xlat,xlon;
   
   
   hrap     = (HRAP *)malloc(MAXPOINTS*sizeof(HRAP));
   
   infile  = fopen(ascii_fname,"r");
   outfile = fopen(bin_fname,"wb");

   if (infile==NULL) 
   {
        printf("\nError opening infile in create_bas_bound_bin.c!\n");
        printf("infile name=%s \n",ascii_fname);
        exit(1);
   }

  if (outfile==NULL) 
   {
        printf("\nError opening output file in create_bas_bound_bin.c!\n");
        printf("outfile name=%s\n",bin_fname);
        exit(1);
   }
  
   len_id   =  9;
   len_name = 21;

   memset(id, '\0', len_id);
   memset(name, '\0', len_name);
	
   while( (fscanf(infile,"%s %s %d %d", id, name, &order, &npts)) != EOF )
   {
 	  /*Free the memory of */
		if(hrap !=NULL) free(hrap);

		/*Allocate basic memory for hrap*/
		hrap    = (HRAP *)malloc(npts*sizeof(HRAP));
		
		if(hrap==NULL )
		{
			printf("Memory allocation failed!");
			exit(1);
		}	
 
		for (i=0; i<npts; i++)
		{
			fscanf(infile,"%f %f",&xlat,&xlon);
			hrap[i] = LatLongToHrap(xlat,xlon);
		}

		fwrite(id,sizeof(char),len_id,outfile);
		fwrite(name,sizeof(char),len_name,outfile);
		fwrite(&order,sizeof(int),1,outfile);
		fwrite(&npts,sizeof(int),1,outfile);
 		fwrite(hrap,sizeof(HRAP),npts,outfile);

		memset(id, '\0', len_id);
		memset(name, '\0', len_name);
   }

  /*Free the memory of */
	if(hrap !=NULL) free(hrap);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/create_bas_bound/RCS/create_bas_bound_bin.c,v $";
 static char rcs_id2[] = "$Id: create_bas_bound_bin.c,v 1.1 2003/09/25 12:01:11 dws Exp $";}
/*  ===================================================  */

}

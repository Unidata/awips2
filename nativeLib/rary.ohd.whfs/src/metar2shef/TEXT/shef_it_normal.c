/*-------------------------------------------------------------------

   Title:    shef_it_normal                                                 
                                                                     
   Purpose:  To output the metar as is to a file.                        
                                                                     
   Author:   David G. Brandon, CBRFC, Salt Lake City, UT             
                                                                     
   Version:  
             1.0   OCT 31, DGB
             1.1   Sep 16, DGB
	           Change <metar.h> to "metar.h"
             1.2   Feb 1, 2006 RAE
                   Initialize site_id to zero
                    
 ---------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"
#include "metar.h"

#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
#define VAL_CHARS  32                                      /* dgb:01/03/98 */
extern int metar_error_handler( char function[] );
extern void prtDMETR( Decoded_METAR *, FILE * );
extern int  print_obs( FILE *fp, int comment, int header );
extern int DEBUG, VERBOSE;                                 /* dgb:07/18/96 */
extern int JUST_OUTPUT;                                    /* dgb:10/31/00 */
void shef_it_normal( Decoded_METAR *Mptr )
{
int notfound, site_id=0, i;
char tempname[250];
 

	     /* search xref file for metar id */
     notfound = 1;
     i=0;
     while ( notfound )
     {
        if ( strlen(&xref_file[i][0]) < 2 )
	{
	     site_id = 0;
	     notfound = 0;
	}
	i++;
        if ( strstr(xref_file[i],Mptr->stnid) != NULL )
	{
	   notfound = 0;
	   site_id = 1;
	}

     }     

      memset(tempname,0,sizeof(tempname));

     if ( site_id == 1 )
     {
        sprintf(tempname,"%s/%3.3sMTR%s",files_.shef_out,&xref_file[i][6],Mptr->stnid);
              

      if ( (luns_.just = fopen(tempname,"wr")) != NULL )
      {


             if ( luns_.jchn != NULL )
	     {
	        if ( JUST_OUTPUT == 1 )
		{
                  print_obs( luns_.just,  0, 0 );
                  prtDMETR( Mptr, luns_.just );		
		}
		else if ( JUST_OUTPUT == 2 )
		{
                  print_obs( luns_.just,  0, 0 );		
		}
		else if ( JUST_OUTPUT == 3 )
		{
                  prtDMETR( Mptr, luns_.just );		
		}
	     }

          fclose(luns_.just);
	  luns_.just = 0;
     }
     else
     {
        printf("\n error on opening file %s",tempname);
     }
}
}

#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>                                       /* dgb:11/14/00 */
#include "metar_cfg.h"
#include "mtr.h"
#include "global_external.h"

extern int VERBOSE, DEBUG;
extern int FCFG;                                             /* dgb:07/22/96 */
extern int PEDTSEP;
extern int GET_MTR_NAMES;                                    /* dgb:02/04/00 */ 
extern int GET_PC_NAMES;                                     /* dgb:11/20/04 */ 
extern int SALIAS;                                               /* dgb:02/18/00 */
extern int JUST_OUTPUT;                                      /* dgb:10/31/00 */
extern void get_mtr_names();
extern void get_pc_names();
extern void get_salias_names();
extern void get_national_category();
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);

void metar_open()
{
    char *c;
    static char  bufftemp[100];
    void metarclose();
    struct stat buf;                                        /* dgb:11/14/00 */

	/*---------------------------------------------------------------
         VER 1.0   MAY    95 DGB
                   Original
             1.1   JUL 18 96 DGB
	           Allow for opening cfg file locally or in usr/local/bin.
             1.2   JUL 22 96 DGB
                   Allow -fcfg filename option for specifying 
                   configuration file from the command line	
             1.3   FEB 4 2000 DGB
                   Place option to screen out ids from configuration
                   file.  
             1.4   FEB 18 2000 DGB
	           Place alias id option for sm reports.  The
		   alias id table is read in from the cfg file. 
             1.5   OCT 31 2000 DGB
	           Read in national_category information for
		   JUST_OUTPUT operation
             1.6   NOV 14 2000 DGB
		   Check if the first three mandatory directories exist
		   in the cfg file
             1.7   FEB 01 2006 RAE
             Added function prototype for strpat().
		           
	 ---------------------------------------------------------------
    */

    memset(files_.shef_in,0,sizeof(files_.shef_in));
    memset(files_.shef_out,0,sizeof(files_.shef_out));
    memset(files_.shef_decode_err,0,sizeof(files_.shef_decode_err));
    memset(files_.shef_log,0,sizeof(files_.shef_log));
    memset(files_.national_cat,0,sizeof(files_.national_cat));   /* dgb:10/11/00 */

	/*  OPEN CONTROL FILE AND READ FILE NAMES TO BE USED
	   THE FILE 'metar.cfg' CONTAINS NAMES OF ALL FILES NEEDED
	*/

    /* look if path/file was specified in command line */
    if ( FCFG )                                               /* dgb:07/22/96 */
    {
        if ( (luns_.nchn = fopen(tempfiles_.cfg,"r+" )) == NULL ) 
             goto L_900;
    }
    else
    if ( (luns_.nchn = fopen(saocfg,"r")) == NULL )
    {
       memset(bufftemp,0,sizeof(bufftemp));                   /* dgb:07/18/96 */
       sprintf(bufftemp,"/usr/local/bin/%s",saocfg);
       if ( (luns_.nchn = fopen(bufftemp,"r")) == NULL )
       {
             strcpy(tempfiles_.cfg,saocfg);
             goto L_900;
       }
    }


    if ( (fgets(files_.shef_in,sizeof(files_.shef_in),luns_.nchn)) == NULL )
                goto L_930;
   
    if ( (fgets(files_.shef_out,sizeof(files_.shef_out),luns_.nchn)) == NULL )
		goto L_930;

    if ( (fgets(files_.shef_decode_err,sizeof(files_.shef_decode_err),luns_.nchn)) == NULL )
		goto L_930;

    if ( (fgets(cont_.out_flag,sizeof(cont_.out_flag),luns_.nchn)) == NULL )
		goto L_930;

    if ( (fgets(cont_.error_flag,sizeof(cont_.error_flag),luns_.nchn)) == NULL )
		goto L_930;

    if ( (fgets(cont_.post_flag,sizeof(cont_.post_flag),luns_.nchn)) == NULL )
		goto L_930;

    if ( (fgets(shefstr_.pecodes,sizeof(shefstr_.pecodes),luns_.nchn)) == NULL )
		goto L_930;


    if ( GET_MTR_NAMES )                                    /* dgb:02/04/00 */ 
        get_mtr_names();                                    /* dgb:02/04/00 */ 

    if ( SALIAS )                                           /* dgb:02/18/00 */ 
        get_salias_names();                                 /* dgb:02/18/00 */ 

    if ( JUST_OUTPUT )                                      /* dgb:10/31/00 */
        get_national_category();                            /* dgb:10/31/00 */                  

    if ( GET_PC_NAMES )                                    /* dgb:11/20/04 */ 
        get_pc_names();                                    /* dgb:11/20/04 */ 

	    
    if ( fclose(luns_.nchn) != 0 )
	     goto L_940;
     
    if ( (c = strchr(files_.shef_in,10)) != NULL )
          memset(c,0,1);
    if ( (c = strchr(files_.shef_out,10)) != NULL )
          memset(c,0,1);
    if ( (c = strchr(files_.shef_decode_err,10)) != NULL )
          memset(c,0,1);

    if ( stat(files_.shef_in,&buf) == -1 )                  /* dgb:11/14/00 */
         goto L_950;                                        /* dgb:11/14/00 */
    if ( stat(files_.shef_out,&buf) == -1 )                 /* dgb:11/14/00 */
         goto L_951;                                        /* dgb:11/14/00 */
    if ( stat(files_.shef_decode_err,&buf) == -1 )          /* dgb:11/14/00 */
         goto L_952;                                        /* dgb:11/14/00 */
   
 
    if ( PEDTSEP )
    {
       memset(bufftemp,0,sizeof(bufftemp));                   /* dgb:07/18/96 */
       sprintf(bufftemp,"%s/PEDTSEP",files_.shef_decode_err);
       if ( (luns_.mrec = fopen(bufftemp,"a")) == NULL )
       {
             fprintf(stdout,"\nmetar_open:unable to open file-> %s",bufftemp);
             exit(1);
       }
    }
         
           
	/*...  THATS ALL * */

	return;

	/*...  ERROR MESSAGES * */
L_900:
	fprintf( stdout, "\nmetar_decode:ERROR: CANNOT OPEN CONFIG FILE-> %s\n", 
             tempfiles_.cfg ); /*dgb:07/22/96 */
    metarclose();
    exit(0);

L_930:
	fprintf( stdout, "\nmetar_decode:ERROR: READING FILE--> %s\n", saocfg );
    metarclose();
    exit(0);

L_940:
	fprintf( stdout, "\nmetar_decode:ERROR:ABORT ON CLOSING FILE--> %s\n", saocfg );
    metarclose();
    exit(0);
    
L_950:                                                      /* dgb:11/14/00 */
	fprintf( stdout, "\nmetar_decode:ERROR:ABORT directory does not exits %s\n", files_.shef_in );
    metarclose();
    exit(0);

L_951:                                                      /* dgb:11/14/00 */
	fprintf( stdout, "\nmetar_decode:ERROR:ABORT directory does not exits %s\n", files_.shef_out );
    metarclose();
    exit(0);

L_952:                                                      /* dgb:11/14/00 */
	fprintf( stdout, "\nmetar_decode:ERROR:ABORT directory does not exits %s\n", files_.shef_decode_err );
    metarclose();
    exit(0);

} /* end of function */

/* -------------------------------------------------------------------- */
/* entire routine entered:  dgb:02/04/00 
*/
void get_mtr_names()                                             /* dgb:02/04/00 */      
{

int found_begin, i;
char act_line[20];

   if(luns_.nchn != NULL)  
   {
      rewind(luns_.nchn);                                   /* dgb:11/14/99 */	

      if ( DEBUG ) printf("\nmetar_decode:     reading get_mtr_names");
      memset(&act_line[0], 0, sizeof(act_line));
      found_begin = 0;

      i = 0;				
      while((fgets(act_line, sizeof(act_line), luns_.nchn)) != NULL)  
      {
         if ( strpat(act_line,".begin_names",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) /* dgb:12/06/99 */
         {
            found_begin = 1;
            continue;
	 }
         if ( strpat(act_line,".end_names",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) /* dgb:12/06/99 */
         {
             break;
	 }
      

         if ( found_begin == 1 )
         {
            if(sscanf(act_line, "%s", &mtr_names[i][0]) != 1)
	    {  
	       printf("\nmetar_decode:ERROR: not enough items on get_mtr_names line ABORT! %s\n",
	                act_line);
	       exit(1);

            }
	    if ( DEBUG ) printf("\nmetar_decode:     mtr_name: %s",act_line);
            i++;
            if ( i == MAX_NUM_MTR_NAMES )
            {
	      if ( DEBUG ) printf("\nmetar_decode:     Max number of mtr_names in cfg file exceeded.\n");
		   break;
            }	     
	  }
       }
   }
}
/* -------------------------------------------------------------------- */
/* entire routine entered:  dgb:11/20/04 
*/
void get_pc_names()                                             /* dgb:11/20/04 */      
{

int found_begin, i;
char act_line[20];

   if(luns_.nchn != NULL)  
   {
      rewind(luns_.nchn);                                   /* dgb:11/14/99 */	

      if ( DEBUG ) printf("\nmetar_decode:     reading get_mpc_names");
      memset(&act_line[0], 0, sizeof(act_line));
      found_begin = 0;

      i = 0;				
      while((fgets(act_line, sizeof(act_line), luns_.nchn)) != NULL)  
      {
         if ( strpat(act_line,".begin_pc_reset",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) 
         {
            found_begin = 1;
            continue;
	 }
         if ( strpat(act_line,".end_pc_reset",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) 
         {
             break;
	 }
      

         if ( found_begin == 1 )
         {
            if(sscanf(act_line, "%s %s", &pc_names[i][0], &pc_times[i][0]) != 2)
	    {  
	       printf("\nmetar_decode:ERROR: not enough items on get_pc_names line ABORT! %s\n",
	                act_line);
	       exit(1);

            }
	    if ( DEBUG ) printf("\nmetar_decode:     pc_name pc_time: %s ",act_line);
            i++;
            if ( i == MAX_NUM_PC_NAMES )
            {
	      if ( DEBUG ) printf("\nmetar_decode:     Max number of pc_names in cfg file exceeded.\n");
		   break;
            }	     
	  }
       }
   }
}

/* -------------------------------------------------------------------- */
/* entire routine entered:  dgb:02/18/00 
*/
void get_salias_names()                                           /* dgb:02/18/00 */      
{

int found_begin, i;
char act_line[20];

   if(luns_.nchn != NULL)  
   {
      rewind(luns_.nchn);                                   /* dgb:11/14/99 */	

      if ( DEBUG ) printf("\nmetar_decode:     reading get_salias_names");
      memset(&act_line[0], 0, sizeof(act_line));
      found_begin = 0;

      i = 0;				
      while((fgets(act_line, sizeof(act_line), luns_.nchn)) != NULL)  
      {

         if ( strpat(act_line,".begin_sm_alias",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) /* dgb:12/06/99 */
         {
            found_begin = 1;
            continue;
	 }
         if ( strpat(act_line,".end_sm_alias",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) /* dgb:12/06/99 */
         {
             break;
	 }
      

         if ( found_begin == 1 )
         {

            if(sscanf(act_line, "%s %s", &sm_name[i][0], &sm_alias[i][0] ) != 2)
	    {  
	       printf("\nmetar_decode:ERROR: not enough items on get_salias_names line ABORT! %s\n",
	                act_line);
	       exit(1);

            }
	    if ( DEBUG ) printf("\nmetar_decode:     salias_name: %s",act_line);
            i++;
            if ( i == MAX_NUM_MTR_NAMES )
            {
	      if ( DEBUG ) printf("\nmetar_decode:     Max number of sm alias in cfg file exceeded.\n");
		   break;
            }	     
	  }
       }
   }
}

/* -------------------------------------------------------------------- */
/* entire routine entered:  dgb:10/31/00 
*/
void get_national_category()                                             /* dgb:02/04/00 */      
{

int found_begin, i;
char act_line[MAX_F];
char *c;

   if(luns_.nchn != NULL)  
   {
      rewind(luns_.nchn);                                   /* dgb:11/14/99 */	

      if ( DEBUG ) printf("\nmetar_decode:     reading get_national_category");
      memset(&act_line[0], 0, sizeof(act_line));
      found_begin = 0;

      i = 0;				
      while((fgets(act_line, sizeof(act_line), luns_.nchn)) != NULL)  
      {

         if ( strpat(act_line,".begin_national_category",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) /* dgb:12/06/99 */
         {

            found_begin = 1;
            continue;
	 }
         if ( strpat(act_line,".end_national_category",(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(act_line)) >= 0 ) /* dgb:12/06/99 */
         {
             break;
	 }
      

         if ( found_begin == 1 )
         {

            if(sscanf(act_line, "%s", files_.national_cat) != 1)
	    {  
	       printf("\nmetar_decode:ERROR: not enough items on national_cat line ABORT! %s\n",
	                act_line);
	       exit(1);

            }

	      if ( (c = strchr(files_.national_cat,10)) != NULL )
                     memset(c,0,1);

	  }
      }
      if ( strlen(files_.national_cat) == 0 )
      {
          printf("\nmetar_decode: No filename detected for option: .begin_national_ctegory\nAbort!\n");
	  exit(1);      
      }
   }
}

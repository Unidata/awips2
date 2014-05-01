/*=========================================================================*/
/*                         FILE NAME:   st3post.c                          */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   set_fields()                       */
/*                                      ReadParam()                        */
/*                                      Sum24()                            */
/*                                      GageOnly()                         */
/*                                      GetGageData()                      */
/*                                      WeightedValue()                    */
/*                                      MergeData()                        */
/*                                      GetMisbin()                        */
/*                                      obswt()                            */
/*                                      GetPrism()                         */
/*       Modified by Jingtao Deng  Feb. 2006 Change LatLongToHrap() to     */  
/*                                            LatLongToHrapPproc()         */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdlib.h>
#include <stdio.h>
#include <sys/utsname.h>

#include "hrap.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include "read_radarloc.h"
#include "post_stage3_interface.h"
#include "GeneralUtil.h"
#include "read_papref.h" 

#include "read_postparam.h"
/*#include "rfcwide.h" */
#include "postX.h"
#include "get_total_precip.h"
#include "load_PCPP_data.h"
#include "time_convert.h"
#include "read_xmrg_file.h"
#include "get_loc_latlon.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"
#include "GetOS.h"
#include "zoom_data_struct.h"

#include "CurPC.h"
#include "CurPP.h"
#include "RawPC.h"
#include "RawPP.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "RadarLoc.h"
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/*  FUNCTION NAME:   set_fields()                                          */
/*       FUNCTION:   malloc space for arrays and call functions to         */
/*                     generate MPE summed field and gage only        */
/*                     field                                               */
/***************************************************************************

Function type:
   void

Called by function:
   post_analysis

Functions called:
   Sum24
   GetGageData
   GetMisbin
   GageOnly

******************************************** BEGIN set_fields **************/

void set_fields()
{

 int i;
 int max_zoom_on = 50;
  
 precip1 = (short int **)malloc(MAXY*sizeof(short int*));
 precip24 = (int **)malloc(MAXY*sizeof(int *));
 gageonly2 = (short int **)malloc(MAXY*sizeof(short int *)); 
 merge = (short int **)malloc(MAXY*sizeof(short int *));
 ratio = (float **)malloc(MAXY*sizeof(float *));
 misbin = (int **)malloc(MAXY*sizeof(int *));
 XMPTIM = (float **)malloc(MAXY*sizeof(float *));
 pa_zoom_data = (draw_struct *)malloc(max_zoom_on*sizeof(draw_struct));

 for (i=0; i<MAXY; i++)
    {
    precip1[i] = (short int *)malloc(MAXX*sizeof(short int));
    precip24[i] = (int *)malloc(MAXX*sizeof(int));
    gageonly2[i] = (short int *)malloc(MAXX*sizeof(short int));
    merge[i] = (short int *)malloc(MAXX*sizeof(short int));
    ratio[i] = (float *)malloc(MAXX*sizeof(float));
    misbin[i] = (int *)malloc(MAXX*sizeof(int));
    XMPTIM[i] = (float *)malloc(MAXX*sizeof(float));
    }

 

 Sum24();

 GetMisbin();
 
 GetPrism();

 GetGageData();

 if (dbg) printf("After GetGageData\n");

 GageOnly();

}

/********************************************* END set_fields **************/

/********************************************************************/
/*  FUNCTION NAME:   ReadParam()                                    */
/*       FUNCTION:   read input parameters and geo data files       */
/*********************************************************************

Function type:
   void

Called by function:
   main

Functions called:
   read_papref
   read_postparm
   read_geo_data
   set_coloroverlays

******************************************** BEGIN ReadParam ***************/

void ReadParam()
{

  long int irc;
 /*---------------------------------------------------------------------*/
 /*     set defaults                                                    */
 /*---------------------------------------------------------------------*/

 istate=1;
 icity=1;
 iriver=0;
 ibound=0;
 iring=0;
 icounty=0;
 NUMHRS=48;

 /*-------------------------------------------------------------------------*/
 /*     read post analysis preferences from database                        */
 /*-------------------------------------------------------------------------*/

 read_papref();

 /*-------------------------------------------------------------------------*/
 /*     read post analysis parameters from database                         */
 /*-------------------------------------------------------------------------*/

 irc = read_postparam();
 
 if(irc != 0)
 {
   printf("Record not found in S3PostAnalParams table -- Post Analysis stopping\n");
   exit(1);
 }

 printf("minimum gage value (used by gage only analysis)=%f\n",MINVAL);
 printf("minimum distance (used by gage only analysis)=%d\n",MINDIST);
 printf("weight parameter (used by gage only analysis)=%d\n",IWIND);
 printf("weight scale parameter (used by merge)=%f\n",SC);
 printf("rhat (used by merge)=%f\n",rhat);

 /*------------------------------------------------------------*/
 /*     read overlay data                                      */
 /*------------------------------------------------------------*/

 read_geo_data();

 /*------------------------------------------------------------*/
 /*     read colors for overlays                               */
 /*------------------------------------------------------------*/

 set_coloroverlays();

}

/********************************************* END ReadParam ***************/

/***************************************************************************/
/*  FUNCTION NAME:   Sum24()                                               */
/*       FUNCTION:   adds up previously saved MPE merged fields            */
/*                     (xmrg files)                                        */
/***************************************************************************

Function type:
   void

Called by function:
   set_fields

Functions called:
   readmosaic
   read_s3res

ibefore = number of hours before last hour in dates list
inotfound = number of hours for which xmrg files not found
num_hour_anal = actual number of hours analysis is based on
              = durcode - ibefore

******************************************** BEGIN Sum24 *******************/

void Sum24()
{
   int         i, j, k, l,ibefore, inotfound, num_hour_anal, datenum;
   char        *dirname;
   int         len1,len3;
   int         len_fname, idate;
   char        cdate[11] = "", fname[30]="";
   static int first = 1;
   char        os[3]="";
   char        system[6]="";
   int         istat;
   struct utsname uts_struct;
   enum TestByteResult  result = DontFlipBytes;
   OperSys      oper = OS_UNIX;
   
   
 /*malloc space*/
 
 dirname = (char *)malloc(128*sizeof(char)); 
 infile = (char **)malloc(durcode*sizeof(char *));

 for (l=0; l< durcode; l++)
 {
   infile[l] = (char *)malloc(200*sizeof(char));

 }   
 
 /*initialize*/
 
 for (j=0;j<=MAXY-1;j++)
 for (k=0;k<=MAXX-1;k++)
    precip24[j][k] = -1;
        
 
 /*get the system*/
 
 oper = GetOS();
    
 /*read tokens*/
 
 if (first == 1)
 { 
   len1 = strlen("rfcwide_xmrg_dir");
   get_apps_defaults("rfcwide_xmrg_dir",&len1,dirname,&len1);
   if (len1 == 0)
   {
      printf("Invalid token value for $(rfcwide_xmrg_dir), program exit.\n");
      exit(1);
   }  
      
   len3 = strlen("st3_date_form");
   memset(date_form,'\0',3);
   get_apps_defaults("st3_date_form",&len3,date_form,&len3);
   if (len3 == 0)
      strcpy(date_form,"mdY");
      
   first = 0;   
 }
 
 /*find the system- lX or HP*/
      
 uname(&uts_struct); 
 strcpy(system, (char *)uts_struct.sysname);
 if (strcmp(system, "HP-UX") != 0)
    strcpy(os, "LX");    
  
 ibefore = 0;
 inotfound = 0;

 for (i=0; i<durcode; i++)
 {
    datenum = i+iselect-1;
    
    if(datenum < NUMHRS)
    {     
      /*dates is global var in stage3.h and defined as date_struct *dates*/
      
      if ( strcmp( date_form, "mdY") == 0)
      {
         idate = dates[datenum].month * 1000000 + dates[datenum].day*10000 + 
                       dates[datenum].year ;
         sprintf(cdate,"%08d%02d",idate,dates[datenum].hour);
      }
      /*the other form is Ymd*/
      else
         strcpy(cdate,dates[datenum].cdate);
	 	 
      sprintf(fname, "xmrg%sz",cdate);                       
      sprintf(infile[i],"%s/%s",dirname,fname);
      len_fname = strlen(infile[i]);           
      
      /*-----------------*/
      /*  read xmrg file */
      /*-----------------*/
      
      /* Test to determine the system that this file was created on. */
        
      TestXmrgByteOrder_ (infile[i] , & XOR , & result ) ;

      if ( result == FlipTestFailed )
      {
          printf( "\nIn function sum24, the call to \"TestXmrgByteOrder_\" failed for file %s\n",infile[i]);

      }
      
      if (( result != FlipTestFailed) && (oper != OS_ERROR))
      {
	 for (j=0; j<MAXY; j++)
	 {
            read_xmrg_file(&MAXX, &MAXY, &j, infile[i], &len_fname, &istat, precip1[j]);

            if (istat != 0)
            {
	      printf("Error reading %s -- default data substituted\n",infile[i]);
	      inotfound++;
              break;
            }

	    /* the file read is assumed Big Endian
		if Linux, then swap bytes	      
		  misbin and prism files are delivered
		  to sites in Big Endian format
		  if running on Linux, bytes in misbin
		  and prism files are swapped */

            if (result == FlipBytes )
            {		
		 Swap2Bytes_(precip1[j], (size_t *) & MAXX);
            }     	 
            /*-----------------------------------*/
            /*  calculate sum for each HRAP bin  */
            /*-----------------------------------*/        

            for (k=0; k<MAXX; k++)
            {
	       if (precip1[j][k] >= 0)
               {
		  if (precip24[j][k] < 0)
	             precip24[j][k] = 0;
	     
		  precip24[j][k] = precip24[j][k] + precip1[j][k];	
		  
		/*  printf("durcode is %d and precip24[j][k]=%d, j=%d, k=%d\n", 
		          i, precip24[j][k], j, k);	  */

               }
            }
	  } /* end for (j=0; j<MAXY; j++)  */
	} /*end of result != FlipTestFailed*/            
    } /* end if(datenum < NUMHRS)  */
    else
    {      
       ibefore++;
    }
  } /* end for (i=0; i<durcode; i++)  */

  /*--------------------------------------------------------------*/
  /*  if hours are requested which are before first hour in list, */
  /*    then print warning message                                */
  /*  if there are hours for which no xmrg file is found,         */
  /*    then print warning message                                */
  /*--------------------------------------------------------------*/

  if(ibefore > 0)
     printf("warning -- %d hours are before first hour in dates list\n",ibefore);

  if(inotfound > 0)
     printf("warning -- xmrg files not found for %d hours\n",inotfound);

  if(ibefore > 0 || inotfound > 0)
  {
     num_hour_anal = durcode - inotfound - ibefore;
     printf("warning -- analysis will be based on only %d hours\n",num_hour_anal);
  }

}

/********************************************* END Sum24 *******************/

/******************************************************************/
/*  FUNCTION NAME:   GageOnly()                                   */
/*       FUNCTION:   calculate gage only field at each HRAP bin   */
/*                     location                                   */
/******************************************************************

Function type:
   void

Called by function:
   set_fields
   redo_gageonly

Functions called:
   WeightedValue

*********************************** BEGIN GageOnly ****************/

void GageOnly()
{
 int    i, j, k, ii, jj, allzero, lowgage, dist;
 int    im, in, jm, jn, gage_bin;
 double  dx, dy;

 for (j=0;j<=MAXY-1;j++)
 for (i=0;i<=MAXX-1;i++)
 { 
/*-------------------------------------------------------------*/
/*   set value of gage only field equal to gage value          */
/*     at location of gage                                     */
/*-------------------------------------------------------------*/

   gage_bin = -1;
   for(k=0;k<=ngages-1;k++)
    if (gage[k].hrap.x == i && gage[k].hrap.y == j)
    {
      gage_bin = k;
      break;
    }

    if (gage_bin > -1)
    {
      gageonly2[j][i] = (short int)(gage[gage_bin].gval*10);  
    }
    else
    {
/*-------------------------------------------------------------*/
/*     if any of 8 surrounding bins to i,j have precip > 0 in  */
/*     MPE  merged field, then calculate gage only      */
/*        field at i,j based on weighting scheme               */
/*     else                                                    */
/*        if distance from i,j to any non missing gage with    */
/*          value > MINVAL is < MINDIST, then caluclate gage   */
/*          only field at i,j based on weighting scheme        */
/*        else                                                 */
/*          gage only field at i,j = 0.0                       */
/*-------------------------------------------------------------*/

       allzero = TRUE;
       im = i-1;
       if (im < 0) im = 0;
       in = i+1;
       if (in > MAXX-1) in = MAXX-1;
       jm = j-1;
       if (jm < 0) jm = 0;
       jn = j+1;
       if (jn > MAXY-1) jn = MAXY-1;
       for (jj = jm;jj<=jn;jj++)
       for (ii = im;ii<=in;ii++)
	  if (precip24[jj][ii] != 0) allzero = FALSE;

       lowgage = 0;
       
       if (allzero == TRUE)
       {
	  for (k=0; k<ngages;k++)
	  {
	     dx = gage[k].hrap.x - i;
	     dy = gage[k].hrap.y - j;
	     dist = sqrt(dx*dx + dy*dy);

	     if (dist < MINDIST  && gage[k].gval > MINVAL && gage[k].gval != -9999.)
	     {
		lowgage = 1;
		break;
	     }
	  }
	  
	  if (lowgage == 0) gageonly2[j][i] = 0;
       }
       
       if (lowgage != 0 || allzero == FALSE)
	  gageonly2[j][i] = (short int)(WeightedValue(i,j)*10);
      }
    }
}

/********************************************* END GageOnly ****************/

/***************************************************************************/
/*  FUNCTION NAME:   GetGageData()                                         */
/*       FUNCTION:   calculate gage data from CurPP table     */
/***************************************************************************

Function type:
   void 

Called by function:
   set_fields

Functions called:
   load_PP_raw
   get_total_raw_PP
   LatLongToHrapPproc

******************************************** BEGIN GetGageData *************/

void GetGageData()
{
   int         i, m, n, min, ihx, ihy;
   float        low, best=0.0 , hi, glat, glon;
   double       dlat, dlon;
   HRAP         hrap;
   time_t       query_begin_time, query_end_time;
   time_t       start_time, valid_time;
   CurPP        *cppHead = NULL, *cppPtr = NULL;
   int         record_count = 0, status, returnvalue, pc_record_cnt = 0;
   char        date_time_ansi[ANSI_TIME_LEN + 1]= "";   
   int         ending_time_match;
   unsigned char  settings;
   short int     advance;   
   float        min_percent, duration_hour = 0.0;
   int         total_precip_cnt;
   struct total_precip  total_precip;
   
/*----------------------------------*/
/* initialize ngages to 0           */
/*----------------------------------*/

   ngages=0;

/*get the selected date in ansi form. decide the query begin time and end time*/
  
   sprintf(date_time_ansi,"%d-%02d-%02d %02d:00:00",date_time.year,date_time.month,
                  date_time.day,date_time.hour);

   status = yearsec_ansi_to_timet(date_time_ansi, &query_begin_time);
   if (status < 0)
      fprintf(stderr, "ERROR reading time: %s\n", date_time_ansi);
      		  
   status = yearsec_ansi_to_timet(date_time_ansi, &query_end_time);
   if (status < 0)
      fprintf(stderr, "ERROR reading time: %s\n", date_time_ansi);
   
/*load data for the specified query time window */

   cppHead = (CurPP *)load_PP_raw(query_begin_time, query_end_time, 
                               NULL, NULL, 0, CurRawPrecip, &record_count); 
   if (dbg) printf("The total number of PP data in curpp table with specified time is %d\n", record_count);
   
/* free any previously loaded data */
   
   if (gage != NULL)
   {
      free(gage);
      gage = NULL;
   }

/* malloc space for structures to hold gage data  
      if there are no gages, then return */
      
   ngages = record_count;
   if ( ngages == 0 )
   {    
      printf("no gage data found; %s\n", date_time_ansi);
      return;
   }
     
   gage = ( gage_struct * ) malloc ( ngages * sizeof ( gage_struct ) ) ;
   weight = (float *) malloc((ngages)*sizeof(float));
   dist = (double *) malloc((ngages)*sizeof(double));
   
   if ( gage == NULL )
   {
      fprintf(stderr, "malloc failed for gage in GetGageData in Post Aanalysis\n"
                      "-- program stopping\n" ) ;
      exit(0);
   }
   
   if ( weight == NULL )
   {
      fprintf(stderr, "malloc failed for weight in GetGageData in Post Aanalysis\n"
                      "-- program stopping\n" ) ;
      exit(0);
   }
   if ( dist == NULL )
   {
      fprintf(stderr, "malloc failed for dist in GetGageData in Post Aanalysis\n"
                      "-- program stopping\n" ) ;
      exit(0);
   }
   
/* set the arguments for get_total_raw_precip() */
      
   ending_time_match = EXACT_ENDINGTIME_MATCH;
   settings = PRECIP_PP | PRECIP_NO_ACCUM;   
   advance  = 1;	
   min_percent = 0.0;   
   
   valid_time = query_end_time;
   start_time = valid_time - durcode * 3600;
   duration_hour = (float)durcode; 

/* read through gage data sets to store data in the gage structure */
   /* Initialize the gage count to 0. */
   
   i = 0;
   total_precip_cnt = 0;
   cppPtr = cppHead;
   	 	
   while ( cppPtr != (CurPP *) NULL)	 
   {
      total_precip = get_total_raw_precip(NULL, (RawPP **) & cppPtr,
	                                  start_time, valid_time, 
				          ending_time_match,
		                          min_percent, settings, advance, 
				          &pc_record_cnt, &record_count);                
      
      /* Retrieve the Latitude/Longitude of the station. */
      
      returnvalue = get_loc_latlon ( total_precip.lid , & dlat , & dlon ) ;            
      
      glat = (float) dlat;
      glon = (float) dlon;
	 
      /* calculate HRAP coordinates from lat,lon  */
      
     /* hrap = LatLongToHrap(glat, glon) ;*/
     
      hrap = LatLongToHrapPproc(glat, glon) ;
      ihx = (int) hrap.x;
      ihy = (int) hrap.y;
	 
      /* make sure the gage is within the area. if so load the info */
      if ( ( int ) ihx <= (MAXX + XOR) &&
           ( int ) ihy <=(MAXY + YOR) &&
           ( int ) ihx > 0 &&
           ( int ) ihy > 0 &&
	   (total_precip.hours_covered == duration_hour))
      {
           if (dbg)
	       printf("Within required area-total_precip-lid,PE,TS,value,covered_hours:%s %s %s %6.2f, %f\n" , 
	         total_precip.lid, total_precip.PE, total_precip.TS, 
		 total_precip.value, total_precip.hours_covered);
		
         strcpy ( gage[i].id , total_precip.lid ) ;
       /*  strcpy ( gage[k].ts , total_precip.TS ) ;*/
       
         /*the total_precip.value is in inch unit, save gage[].gval in mm unit*/
	 
         gage[i].gval = total_precip.value * 25.4;
  
         gage[i].hrap.x =  ihx;
         gage[i].hrap.y =  ihy;
         gage[i].hrap.x = gage[i].hrap.x - XOR;
         gage[i].hrap.y = gage[i].hrap.y - YOR;
        /*   gage[i].qc = 0;*/ 

       /*-------------------------------------------------*/
       /*  precip24 is in units of hundredths of mm       */
       /*  gval is in units of inch                        */
       /*-------------------------------------------------*/

       low = 999999.;
       hi = 0.;
       min = 999999;

       for (m=gage[i].hrap.x-1;m<=gage[i].hrap.x+1;m++)
       {
          for (n=gage[i].hrap.y-1;n<=gage[i].hrap.y+1;n++)
          {
	     if(n >= 0 && m >= 0 && m < MAXX && n < MAXY)
	     {
	         if (precip24[n][m] < low) 
		    low=precip24[n][m];
	         if (precip24[n][m] > hi)
		    hi=precip24[n][m];
	         if (fabs((precip24[n][m] - gage[i].gval*100)) < min && precip24[n][m] != -9999)
	         {
        	    min = fabs((precip24[n][m] - gage[i].gval*100));
        	    best = precip24[n][m];
	         }
	         
		
		 gage[i].ms = best/100.;
	         gage[i].mslow = low/100.;
	         gage[i].mshi = hi/100.;
	     }
	     else
	     {
	       gage[i].ms = -9999.;
	       gage[i].mslow = -9999;
	       gage[i].mshi = -9999;
	     }

             strcpy(gage[i].edit, "");
          }      
       }              
       i++;     	  
     }
     total_precip_cnt++;
     
   /*In the get_total_raw_precip() already advance the pointer if advance = 1*/
     
   /*  cppPtr = (CurPP *)ListNext(&cppPtr->node);*/ 
     
   }/*end of while clause */
   
   if (cppHead != NULL)
      FreeCurPP(cppHead);  	
      
   /* assign the global variable */
   
   ngages = i;
   
   printf("Total gage number within the required area is %d\n", ngages);
   
   return;
 }  
    			   


/********************************************* END GetGageData *************/

/*********************************************************************/
/*  FUNCTION NAME:   WeightedValue()                                 */
/*       FUNCTION:   calculate gage only field values at HRAP bins   */
/*                     satisfying MINVAL and MINDIST tests using     */
/*                     weighting scheme based on distances to gages  */
/**********************************************************************

Function type:
   float 

Called by function:
   GageOnly

Functions called:
   none

******************************************** BEGIN WeightedValue ***********/

float WeightedValue(i, j)
   int          i, j;
{
   float        ggo;
   int          ifltdx, ig;
   double       dx, dy, denwt;

 for (ig = 0;ig < ngages;ig++)
 {
    dist[ig] = 999.;
 }   
 ifltdx = 0;
 denwt = 0.0;

 /*-------------------------------------------------------------------------*/
 /*     calculate distance between gages and hrap grid bin                  */
 /*     if dist  < idmax then increment denominator for weight              */
 /*-------------------------------------------------------------------------*/

 for (ig=0;ig<ngages;ig++)
 {
    if (gage[ig].hrap.x < MAXX && gage[ig].hrap.y < MAXY && 
        gage[ig].hrap.x >= 0 && gage[ig].hrap.y >= 0 && gage[ig].gval != -9999.)
    {
       dx = gage[ig].hrap.x - i;
       dy = gage[ig].hrap.y - j;
       dist[ig] = sqrt(dx*dx + dy*dy);
  
       if (dist[ig] < MINDIST)
       {
	  ifltdx++;
	  denwt = denwt + exp(-dist[ig]/IWIND);
        }
     }
 }
 if (ifltdx == 0)
 {
    for (ig=0;ig<ngages;ig++)
       weight[ig] = 0;
    ggo = 0.;
 }
 else
 {
     for (ig=0;ig<ngages;ig++)
     {
         weight[ig] = 0.;
         if (dist[ig] < MINDIST)
	     weight[ig] = exp(-dist[ig]/IWIND)/denwt;
     }
    /*--------------------------------------------------------------------*/
    /*     calculate gage only precip for grid bin                        */
    /*--------------------------------------------------------------------*/
    ggo = 0;
    
    for (ig=0;ig<=ngages-1;ig++)
    {
       if (gage[ig].gval != -9999.)
       {
            if(gage[ig].hrap.x < MAXX && gage[ig].hrap.y < MAXY &&
               gage[ig].hrap.x >= 0 && gage[ig].hrap.y >= 0)	
	    {  
	       /*consider XMPTIM[j][i] as default value 1.0 if XMPTIM[gage[ig].hrap.y][gage[ig].hrap.x] is zero*/
	       
	       if (fabs(XMPTIM[gage[ig].hrap.y][gage[ig].hrap.x]- 0.0) < 0.00001)
	       {
	          ggo = ggo +weight[ig]*gage[ig].gval;
	         
	       }	  	  
	       else 	       		  
	          ggo = ggo + weight[ig]*XMPTIM[j][i] * gage[ig].gval/XMPTIM[gage[ig].hrap.y][gage[ig].hrap.x];
	       
	       /*printf("XMPTIM[j][i]=%f,j=%d,i=%d\n", XMPTIM[j][i], j, i);
	       printf("XMPTIM[gage[ig].hrap.y][gage[ig].hrap.x]=%f,hrap.y=%d,hrap.x=%d\n", 
	                     XMPTIM[gage[ig].hrap.y][gage[ig].hrap.x], gage[ig].hrap.y, gage[ig].hrap.x);
			     */
			     
			     
	    }		     
	}
    }     	
 }
 if (dbg) printf ("ggo is %f\n", ggo);
 return ggo;
}

/********************************************* END WeightedValue ***********/

/*********************************************************************/
/*  FUNCTION NAME:   MergeData()                                     */
/*       FUNCTION:   generate merged field data and ratio of         */
/*                     merged field to summed hourly MPE field  */
/*                     at each HRAP bin                              */
/*********************************************************************

Function type:
   void

Called by function:
   merge_data

Functions called:
   obswt

******************************************** BEGIN MergeData ***************/

void MergeData()
{
   float        obswt();
   float        a, lnrhat;
   int          i, j;
   short int    gg, rd;

 if (dbg) printf("*** Merging Data *** \n");

 lnrhat = log((double) rhat);
 if (dbg) printf("rhat = %f\n",rhat);

 for (j = 0;j<=MAXY-1;j++)
 for (i = 0;i<=MAXX-1;i++)
    {
    gg = gageonly2[j][i];
    rd = precip24[j][i]/10;

    if (gg == 0 && rd == 0) merge[j][i] = 0;
    else if (misbin[j][i] == 0) merge[j][i] = gg;
    else if (ngages == 0) merge[j][i] = rd;
    else
      {
      a = obswt(i, j, lnrhat);
      merge[j][i] = a*gg + (1.-a)*rd;
      }

    if (rd <= 0) ratio[j][i] = -1.*(float)merge[j][i];
    else ratio[j][i] = (float)merge[j][i]/(float)rd;
    }
}

/********************************************* END MergeData ***************/

/********************************************************************/
/*  FUNCTION NAME:   GetMisbin()                                    */
/*       FUNCTION:   generate missing bin array                     */
/*********************************************************************

Function type:
   void

Called by function:
   set_fields

Functions called:
   LatLongToHrapPproc
   read_rloc_post

******************************************** BEGIN GetMisbin ***************/

void GetMisbin()
{
   int          j, nrad = 0, i, k;
   HRAP         *cen; 
   double        dx, dy, d;
   char         where[100]="";
   RadarLoc   *RadarLocHead = NULL;
   RadarLoc   *RadarLocPtr = NULL;
   int          len;
   char         dirname[250]="";
   
 /*   read .Apps_defaults token                                        */

 len = strlen("rfcwide_misbin_dir");
 get_apps_defaults("rfcwide_misbin_dir",&len,dirname,&len); 
 
 /*get the number of radar location*/ 
/* read_radarloc(&nrad);*/

 sprintf ( where,"WHERE use_radar='T' order by radid" ) ;
   
 RadarLocHead = GetRadarLoc ( where) ;
 if ( RadarLocHead == NULL )
 {
    printf("There are no radars in the RadarLoc table\n") ;
 }
 else
 {
    RadarLocPtr = (RadarLoc *) ListFirst (& RadarLocHead->list);
    nrad = ListCount (& RadarLocHead->list );
 }
  
 cen = (HRAP *) malloc(nrad*sizeof(HRAP));
 
 /* malloc space and store information in structure */

 nexrad = ( nex_struct * ) malloc ( nrad  * sizeof ( nex_struct ) ) ;
 if (nexrad == NULL)
 {
    fprintf(stderr, "malloc failed for nexrad in GetMisbin in Post Analysis\n");
    exit(0);
 }
      
 printf("%d radars defined\n", nrad);
 printf ( "HRAP coordinates of radar locations:\n" ) ;

 for (i=0; i<=nrad-1; i++)
 {
    if (RadarLocPtr->radid != NULL)
      strcpy ( nexrad[i].id, RadarLocPtr->radid ) ;
    nexrad[i].xlat = RadarLocPtr->lat ;
    nexrad[i].xlong = RadarLocPtr->lon ;
    
    /*cen[i] = LatLongToHrap(nexrad[i].xlat, nexrad[i].xlong);*/
    
    cen[i] = LatLongToHrapPproc(nexrad[i].xlat, nexrad[i].xlong);
    printf("%s x,y = %f %f\n", nexrad[i].id, cen[i].x, cen[i].y);
    
    RadarLocPtr = ( RadarLoc * ) ListNext ( & RadarLocPtr->node ) ;
 }
 
 for (j=0;j<=MAXY-1;j++)
 for (i=0;i<=MAXX-1;i++)
 {
    misbin[j][i]=0;
    for (k=0;k<=nrad-1;k++)
    {
      dx = i - (cen[k].x - XOR);
      dy = j - (cen[k].y - YOR);
      d = sqrt(dx*dx + dy*dy);
      if (d <= 66.)
      {
        misbin[j][i] = 1;
        break;
      }
    }
 }

 /*free the space */
 
 if (RadarLocHead != NULL) 
    FreeRadarLoc(RadarLocHead);
   
      
 if (dbg) printf("leaving GetMisbin\n");
}

/********************************************* END GetMisbin ***************/

/*********************************************************************/
/*  FUNCTION NAME:   obswt()                                         */
/*       FUNCTION:   calculate weight given to gage only and summed  */
/*                       hourly MPE fields on merging process   */
/**********************************************************************

Function type:
   float

Called by function:
   MergeData

Functions called:
   none

******************************************** BEGIN obswt *******************/

float obswt(i, j, lnrhat)
   int          i, j;
   float        lnrhat;
{
   double       dx, dy, dist, dnear;
   int          k;

 dnear = 999.;
 for (k=0;k<=ngages-1;k++)
    {
    dx = gage[k].hrap.x - i;
    dy = gage[k].hrap.y - j;
    dist = sqrt(dx*dx + dy*dy);
    if (dist < dnear) dnear = dist;
    }
 return exp(SC*lnrhat*dnear);


}

/****************************************************************************
  Function Name: GetPrism()
  Read prism data from directory defined from token "rfcwide_prism_dir"
*****************************************************************************/
void GetPrism()
{
   static int	first = 1 ;
   int            i, j, len, lenfn, irc = 0;
   char          dirname[200], prism_filename[200];
   char          date_month[3]="";
   double        factor;
   short int    **prism = NULL;
   char          os[3]="";
   char          system[6]="";
   struct utsname uts_struct;
   OperSys        oper = OS_UNIX;
   enum TestByteResult  result = DontFlipBytes;
   
   /*initialize*/
   
   for (j=0; j<=MAXY-1;j++)
   {
     for (i=0; i<=MAXX-1;i++)
       XMPTIM[j][i]=1.0;
   }
   
   factor = 1.0;
   
   /*get the system*/
   
   oper = GetOS();
   	
   
   /*malloc space*/
   
   prism = (short int **)malloc(MAXY * sizeof(short int*));
   for (j=0; j< MAXY; j++)
       prism[j] = (short int *)malloc(MAXX*sizeof(short int));
       
   memset(prism_filename, '\0', 200);
   memset(dirname, '\0', 200);
   
      
   /*find the system- lX or HP*/
      
   uname(&uts_struct); 
   strcpy(system, (char *)uts_struct.sysname);
   if (strcmp(system, "HP-UX") != 0)
     strcpy(os, "LX");    
   
   /*get prism directory name defined from token*/
   
   if (first == 1)
   {
     len = strlen("rfcwide_prism_dir");
     get_apps_defaults("rfcwide_prism_dir",&len,dirname,&len);
     if (len == 0)
     {
	printf("Invalid token value for $(rfcwide_prism_dir)\n");
     }  

     first = 0;
   }
   
   /*get the selected month*/
   
   sprintf(date_month, "%d", date_time.month);
   
   /*create the file name*/
     
   sprintf(prism_filename, "%s/PRISM_%s", dirname, date_month);
   
   lenfn = strlen(prism_filename); 
   
   /* Test to determine the system that this file was created on. */
        
   TestXmrgByteOrder_ (prism_filename , & XOR , & result ) ;
   if (dbg) printf("The reuslt is %d after TestXmrgByteOrder \n", result);
   
   if ( result == FlipTestFailed )
   {
       /*now assume prism data is Big Endian*/
       
       if (oper == OS_LINUX)          
          result = FlipBytes;
	  
   }
   
   if (( result != FlipTestFailed) && (oper != OS_ERROR))
   {
      for (j = 0; j < MAXY; j++)
      {
	 read_xmrg_file(&MAXX, &MAXY, &j, prism_filename, &lenfn, &irc, prism[j]);  

	 if(irc != 0)
	 {
	    printf ("ERROR reading PRISM from %s --default 1.0 is substituted\n", prism_filename);
	    
	    for (j=0; j<=MAXY-1;j++)           
            for (i=0; i<=MAXX-1;i++)
                XMPTIM[j][i]=1.0;
   	    		     
	    break;
	 }  

	 /* the file read is assumed Big Endian
		if Linux, then swap bytes	      
		  misbin and prism files are delivered
		  to sites in Big Endian format
		  if running on Linux, bytes in misbin
		  and prism files are swapped */

	 if(result == FlipBytes )
	 {
             Swap2Bytes_(prism[j], (size_t *) & MAXX) ;	     	     
	 }


	 for(i = 0; i < MAXX; i++)
	 {
	      XMPTIM[j][i] = prism[j][i] * 1.0 / factor ;
	      if (dbg) printf("XMPTIM[j][i]=%f, j=%d, i=%d\n", XMPTIM[j][i], j, i);

	 }

       }	
   } /*end of result != FlipTestFailed*/
   else
   {
     printf("Default 1.0 is used for prism data\n"); 
     for (j=0; j<=MAXY-1;j++)  
     for (i=0; i<=MAXX-1;i++)
       XMPTIM[j][i]=1.0;
   }

return;
}

  
/********************************************* END obswt *******************/

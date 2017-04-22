#include "decodedpa.h"
#include "closedb.h"
#include "write_stage1_decoded.h"
#include "top_hour_check.h"
#include "copy_to_archive.h"
#include "get_build_num.h"
#include "GeneralUtil.h"

/*---------------------------------------------------------------*/
/*   function for decoding a DPA product and optionally          */
/*      copying the raw DPA product to an archive directory      */
/*                                                               */
/*   beginning with Bld OB3, format of decoded file is           */
/*      Little Endian                                            */
/*                                                               */
/*   calling function: main_decodedpa                            */
/*                                                               */
/*   minoff = minutes off the top-of-the-hour of product         */
/*            for values > 30, minoff = (-1) * (60 - minoff)     */
/*   window = value of dpa_decode_window/dpa_archive_window      */
/*          = window around top of hour for decoding/archiving   */
/*            (units = minutes)                                  */
/*   decon = on/off flag for filtering decoded products          */
/*         = 0 -- filter off                                     */
/*         = 1 -- filter on                                      */
/*   archon = on/off flag for archiving products                 */
/*          = 0 -- do not archive products                       */
/*          = 1 -- archive products based on filter              */
/*   toph_arc_flg = top-of-hour check flag for archiving         */
/*                = 0 -- (default) do not archive product        */
/*                = 1 -- archive product                         */
/*   toph_dec_flg = top-of-hour check flag for decoding          */
/*                = 0 -- (default) decode product                */
/*                = 1 -- do not decode product                   */
/*   rcode = return code from top_hour_check                     */
/*         = 0 -- product is within top-of-hour window           */
/*         = 1 -- product is non-top-of-hour (outside of window) */
/*         = 2 -- product is non-top-of-hour (product closer to  */
/*                top-of-hour has been previously processed      */
/*         = 3 -- product is non-top-of-hour (product at         */
/*                top-of-hour has been previously processed      */
/*---------------------------------------------------------------*/

void decodeDPA(char *rawfname, int *pcipflg, int write_to_db)
{
   
   int i,j,k,n,icount,minute,gminute,len,ier,numparm;
   int nisol, nouti, noutr, nbad, nhour, mode, ttime;
   int volcov, pcip, nblocked, nclutter, nsmoothed;
   int decon, archon, toph_arc_flg, toph_dec_flg, rcode, window, aminoff;
   long int irc;
   short i2, nbytes, hdr[50],symhdr[8],datahdr[5],minoff,hour, ghour, jul_day_num, build_num;
   short nruns,xrun,level,level_out;
   short precip[131][131];
   float bias,maxvald,maxvalh;
   float area, biscan, pctfill, angle, rainarea, params[37];
   char missper[2],cdate[10];
   char outname[256],fname[19], rawf[256];
   char year[3],month[3],day[3];
   char chour[3],cminute[3];
   char *st1dir, *fullrawfname;
   char strdt[22];
   char param38[2];
   char cfilarc[4], cfildec[4], cwind[3], cwind1[3];
   unsigned char *data;
   div_t time;
   
   /*-----------------------------------------------------------------*/
   /*  define union for reading product gen time seconds from header  */
   /*-----------------------------------------------------------------*/
   
   union prod_gen_time {
      short int sec2[2];               
      int sec4;  
   };
   
   union prod_gen_time gen_time_sec;
   
   /*-------------------------------*/
   /*  open the raw DPA file        */
   /*-------------------------------*/
   
   strcpy(rawf, rawfname);
   if((dpafile = fopen(rawf,"rb")) == NULL)
   {
      printf("error opening raw DPA file -- product not decoded\n");
      if ( write_to_db == 1 )
      {
         closedb(&irc);
         if(irc !=0)
         {
	    printf("PostgreSQL error# %ld ",irc);
	    printf(" occurred attempting to close database \n");
         }
      }
      exit(3);
   }
   
   /*---------------------------------------------------*/
   /*     decode header portion of product              */
   /*                                                   */
   /*  read until finding -1                            */
   /*  this signifies beginning of DPA product header   */
   /*---------------------------------------------------*/
   
   icount = 0;
   for (;;)
   {
      n = fread(&i2, sizeof(short), 1, dpafile);
      if (n==EOF || n==0)
      {
	 printf("Error: EOF encountered before reading header");
	 printf(" for %s -- product not decoded\n",rawfname);
	 fclose(dpafile);
	 
	 if ( write_to_db == 1 )
	 { 
	    closedb(&irc);
	    if(irc !=0)
	    {
	       printf("PostgreSQL error# %ld ",irc);
	       printf(" occurred attempting to close database \n");
	    }
         }
	 exit(2);
      }
      
      icount++;
      if (i2 == -1) break;
   }
   
   /*----------------------------*/
   /*  read DPA product header   */
   /*----------------------------*/
   
   n = fread(hdr,sizeof(short), 50, dpafile);
   
   /*---------------------------------------------------*/
   /*   check for product code 81                       */
   /*   this signifies that product is a DPA product    */
   /*---------------------------------------------------*/
   
   if( hdr[5] != 81)
   {
      printf("file %s does not contain a DPA product\n",rawfname);
      fclose(dpafile);
      
      if ( write_to_db == 1 )
      {
         closedb(&irc);
         if(irc !=0)
         {
	    printf("PostgreSQL error# %ld ",irc);
	    printf(" occurred attempting to close database \n");
         }
      }
      exit(2);
   }
   
   /*--------------------------*/
   /* convert julian to z time */
   /*--------------------------*/
   
   ttime = hdr[40];
   time = div(ttime,60);
   hour = (short) time.quot;
   minute = time.rem;
   
   minoff = (short) minute;
   if(minoff > 30) minoff = -1 * (60 - minoff);
   
   strcpy(cdate,convertJulianDate(hdr[39]));
   
   /*--------------------------------*/
   /* create ending date/time value  */
   /*--------------------------------*/
   
   sprintf(strdt, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",cdate[4],cdate[5],cdate[6],cdate[7],
	   cdate[0],cdate[1],cdate[2],cdate[3],hour,minute);
   
   /*-------------------------------------------------------------------------------*/
   /* print info to log                                                             */
   /* check for valid date and time                                                 */
   /*                                                                               */
   /* NOTE: products with 24z time stamp have been determined to be valid products  */
   /*       until fixed, code below will change time stamp of 24z products to 00z   */
   /*          with date = next days's date                                         */
   /*-------------------------------------------------------------------------------*/
   
   bias = (float) hdr[37]/100.;
   maxvalh = (float) hdr[36]/10.;
   level_out = LEVEL_OUT_OF_RANGE;
   
   printf("%s %s %02d:%02d maxvalh=%4.1f filename: %s\n",
	  radid, cdate, hour, minute, maxvalh, rawfname);
   
   if(hour < 0 || hour > 24 || minute < 0 || minute > 60)
   {
      printf("Invalid end precip time found -- product not decoded\n");
      fclose(dpafile);
      
      if ( write_to_db == 1 )
      {
         closedb(&irc);
         if(irc !=0)
         {
    	    printf("PostgreSQL error# %ld ",irc);
	    printf(" occurred attempting to close database \n");
         }
      }
      exit(2);
   }
   
   /*--------------------------*/
   /* 24z product found        */
   /* change hour to 00z       */
   /* change date to next day  */
   /*--------------------------*/
   
   if(hour == 24)
   {
      printf("24z product - hour changed to 00z, date changed to next day\n");
      hour = 0;
      
      jul_day_num = hdr[39];
      jul_day_num++;
      strcpy(cdate,convertJulianDate(jul_day_num));
      
      sprintf(strdt, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",cdate[4],cdate[5],cdate[6],cdate[7],
	      cdate[0],cdate[1],cdate[2],cdate[3],hour,minute);
   }
   
   /*----------------------------------------------------------------*/
   /*   read decode and archive options from .Apps_defaults          */
   /*----------------------------------------------------------------*/
   
   toph_arc_flg = 0;
   toph_dec_flg = 0;
   
   archon = 0;
   decon = 0;
   
   len = strlen("dpa_filter_decode");
   get_apps_defaults("dpa_filter_decode",&len,cfildec,&len);
   len = strlen("dpa_archive");
   get_apps_defaults("dpa_archive",&len,cfilarc,&len);
   
   if(strcmp(cfildec,"ON") == 0 || strcmp(cfildec,"on") == 0) decon = 1;
   if(strcmp(cfilarc,"ON") == 0 || strcmp(cfilarc,"on") == 0) archon = 1;
   
   if(decon == 1)
   {
      /*---------------------------------------------------------*/
      /*   get value for window around top-of-hour for decoding  */
      /*   units = minutes                                       */
      /*   if token is not found, then 10 minutes is assumed     */
      /*---------------------------------------------------------*/
      
      len = strlen("dpa_decode_window");
      get_apps_defaults("dpa_decode_window",&len,cwind,&len);
      
      if(len == 0)
      {
	 window = 10;
      }
      else
      {
	 strncpy(cwind1,cwind,len);
	 window = atoi(cwind1);
      }
      
      top_hour_check(window, strdt, minoff, &rcode);
      
      if(rcode > 0) toph_dec_flg = 1;
   }
   
   if(archon == 1)
   {
      /*---------------------------------------------------------*/
      /*   get value for window around top-of-hour for archiving */
      /*   units = minutes                                       */
      /*   if token is not found, then 10 minutes is assumed     */
      /*---------------------------------------------------------*/
      
      len = strlen("dpa_archive_window");
      get_apps_defaults("dpa_archive_window",&len,cwind,&len);
      
      if(len == 0)
      {
	 window = 10;
      }
      else
      {
	 strncpy(cwind1,cwind,len);
	 window = atoi(cwind1);
      }
      
      /*-----------------------------------------------------------------------*/
      /*   if minutes off the top-of-hour are outside of window for archiving, */
      /*   then mark product for no archiving                                  */
      /*-----------------------------------------------------------------------*/
      
      aminoff = minoff;
      if(aminoff < 0) aminoff = -1 * aminoff;
      if(aminoff <= window) toph_arc_flg = 1;
      
   }
   
   if(toph_dec_flg == 1)
   {
      if(rcode == 1) printf("Product is non-top-of-hour -- product not decoded\n");
      if(rcode == 2) printf("Product closer to top-of-hour previously processed -- product not decoded\n");
      if(rcode == 3) printf("Previous top-of-hour product processed -- product not decoded\n");
      
      fclose(dpafile);
      if(toph_arc_flg == 0)
      {
         if ( write_to_db == 1 )
         {
	    closedb(&irc);
	    if(irc !=0)
	    {
	       printf("PostgreSQL error# %ld ",irc);
	       printf(" occurred attempting to close database \n");
	    }
         }
	 exit(1);
      }
   }
   
   else
   {
      
      /*------------------------------------------------------*/
      /*       decode data portion of product                 */
      /*                                                      */
      /* read product symbology & header of data array packet */
      /*------------------------------------------------------*/
      
      n = fread(symhdr, sizeof(short),8,dpafile);
      n = fread(datahdr, sizeof(short),5,dpafile);
      
      /*------------------------------*/
      /*  initialize precip array     */
      /*------------------------------*/
      
      for (i=0; i<131; i++)
      {
	 for (j=0; j<131; j++)
	 {
	    precip[j][i] = level_out;
	 }
      }
      
      /*------------------------------------------------------------------*/
      /*  datahdr[4] contains the number of rows of data in the product   */
      /*  normally, this is 131                                           */
      /*  if the value is less than 131, then print message and continue  */
      /*------------------------------------------------------------------*/
      
      if(datahdr[4] < 131)
	 printf(" product has only %d rows of data (less than 131)\n",datahdr[4]);
      
      /*-------------------------------------------*/
      /*  read run-length encoded precip data      */
      /*-------------------------------------------*/
      
      for (i=0; i<datahdr[4]; i++)
      {
	 n = fread(&nbytes, sizeof(short),1,dpafile);
	 if(nbytes == -1)
	 {
	    j = i - 1;
	    printf(" incomplete DPA product -- ");
	    printf("precip data complete through row %d\n",j);
	    break;
	 }
	 else if(nbytes < 2 || nbytes > 32000)
	 {
	    printf(" data corruption encountered -- ");
	    printf("product not decoded\n");
	    fclose(dpafile);
            if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 else if (ferror(dpafile) != 0)
	 {
	    printf(" read error occurred -- ");
	    printf("product not decoded\n");
	    fclose(dpafile);
            if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 else if (feof(dpafile) != 0)
	 {
	    printf(" unexpected EOF encountered -- ");
	    printf("product not decoded\n");
	    fclose(dpafile);
            if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 
	 data = (unsigned char *) malloc ((nbytes)*sizeof(unsigned char));
	 if(!data)
	 {
	    printf("malloc of space for precip data record failed -- ");
	    printf("product not decoded\n");
	    fclose(dpafile);
            if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 
	 n = fread(data, sizeof(unsigned char), nbytes,dpafile);
	 if(n != nbytes)
	 {
	    printf(" loss of data encountered --");
	    printf(" number of bytes expected = %d",nbytes);
	    printf(" number of bytes found = %d \n",n);
	    printf("  product not decoded\n");
	    fclose(dpafile);
            if ( write_to_db == 1 )
	    {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 else if (ferror(dpafile) != 0)
	 {
	    printf(" read error occurred -- ");
	    printf("product not decoded\n");
	    fclose(dpafile);
	    if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 else if (feof(dpafile) != 0)
	 {
	    printf(" unexpected EOF encountered -- ");
	    printf("product not decoded\n");
	    fclose(dpafile);
	    if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 
	 nruns = (short)((float)nbytes/2.);
	 if(nruns > 131 || nruns < 1)
	 {
	    printf(" value of nruns < 1 or > 131 in row %d -- product not decoded\n",i);
	    fclose(dpafile);
            if ( write_to_db == 1 )
            {
	       closedb(&irc);
	       if(irc !=0)
	       {
	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
	       }
            }
	    exit(2);
	 }
	 
	 k=0;
	 for (j=0;j<nruns; j++)
	 {
	    xrun = (short) data[j*2];
	    if(xrun > 131 || xrun < 1)
	    {
	       printf(" value of xrun < 1 or > 131 in row %d, set %d  -- product not decoded\n",i,j);
	       fclose(dpafile);
               if ( write_to_db == 1 )
               {
	          closedb(&irc);
	          if(irc !=0)
	          {
		     printf("PostgreSQL error# %ld ",irc);
		     printf(" occurred attempting to close database \n");
	          }
               }
	       exit(2);
	    }
	    level = (short) data[1+j*2];
	    
	    for (n=0; n<xrun; n++)
	    {
	       precip[130-i][k] = level;
	       k++;
	    }
	    
	 }
	 
	 free(data);
	 
      }
      
      /*--------------------------------------------------------------------------*/
      /*  determine if DPA product is generated by ORPG software build 4, 5 or 8  */
      /*  if string = "ADAP(32" is found, then product is build 8                 */
      /*  else if string = "BLOCK" is found, then product is build 5              */
      /*  else product is build 4                                                 */
      /*--------------------------------------------------------------------------*/
      
      build_num = get_build_num();
      
      /*-------------------------------------------------*/
      /*  search for and read adaptation parameters      */
      /*  if found, then write to DpaAdapt table         */
      /*-------------------------------------------------*/
      
      if ( write_to_db == 1 )
      {
	 if(getadapt(&numparm, params, param38) == 0)
	 {
            wrtodb_adapt(build_num, strdt, params, param38);
	 }
	 else
	 {
            printf("    EOF encountered before finding adaptable parameters header");
            printf(" -- product not decoded\n");
            fclose(dpafile);
            if ( write_to_db == 1 )
            {
               closedb(&irc);
               if(irc !=0)
               {
   	          printf("PostgreSQL error# %ld ",irc);
	          printf(" occurred attempting to close database \n");
               }
            }
            exit(2);
	 }
      }
      
      /*---------------------------------------------------------------------------*/
      /*  read supplemental data                                                   */
      /*  value of operational weather mode in supplemental data is not used       */
      /*  if EOF encountered before finding suppl data header, then assume         */
      /*   product is bad (value of *pcipflg returned = 99)                        */
      /*  if unexpected EOF encountered while reading suppl data, then write       */
      /*   record to DPARadar table but do not write decoded file                  */
      /*   (value of *pcipflg returned = 0)                                        */
      /*---------------------------------------------------------------------------*/
      
      *pcipflg = getsuppl(build_num,&nisol,&nouti,&noutr,&area,&biscan,&nbad, 
			  &nhour, &nblocked, &nclutter, &nsmoothed,
			  &pctfill, &angle, &rainarea, &volcov, &mode, missper);
      if(*pcipflg == 99)
      {
	 printf("    EOF encountered before finding supplemental data header"); 
	 printf(" -- product not decoded\n");
	 fclose(dpafile);
         if ( write_to_db == 1 )
         {
	    closedb(&irc);
	    if(irc !=0)
	    {
	       printf("PostgreSQL error# %ld ",irc);
	       printf(" occurred attempting to close database \n");
	    }
         }
	 exit(2);
      }
      
      /*------------------------------------------------------------------*/
      /*    if maxvald > 0.0 and product not flagged as bad, then write   */
      /*      decoded precip data to file                                 */
      /*    if running on HPUX, then convert to Little Endian format      */
      /*       before writing                                             */
      /*------------------------------------------------------------------*/
      
      sprintf(fname,"%s%s%02d%02dZ",radid,cdate,hour,minute);
      
      maxvald = -99.;
      if(*pcipflg == 0) maxvald = 0.0;
      
      if(*pcipflg != 0 && *pcipflg != 1 && *pcipflg != 2 && *pcipflg != 3)
      {
	 ier = 0;
	 len = strlen("dpa_grid_dir");
	 st1dir = (char *)malloc(128*sizeof(char));
	 get_apps_defaults("dpa_grid_dir",&len,st1dir,&len);
	 sprintf(outname,"%s/%s",st1dir,fname);
	 write_stage1_decoded(precip,outname,&hdr[20],&hdr[21],&level_out,&maxvald,&ier);
	 
	 if(maxvald == 0.0) printf("   no decoded file written (maxvald = 0.0)\n");
	 if(ier != 0) printf("FORTRAN error number %d writing decoded file\n",ier);
	 
	 /*-------------------------------------*/
	 /*  close raw DPA file                 */
	 /*-------------------------------------*/
	 
	 fclose(dpafile);
	 
      }
      else
      {
	 if(*pcipflg == 0) printf("   no decoded file written (no precip detected)\n");
	 if(*pcipflg == 1) printf("   no decoded file written (bad rate scan)\n");
	 if(*pcipflg == 2) printf("   no decoded file written (not enough data)\n");
	 if(*pcipflg == 3) printf("   no decoded file written (disk error at radar)\n");
	 
	 /*-------------------------------------*/
	 /*  close raw DPA file                 */
	 /*-------------------------------------*/
	 
	 fclose(dpafile);
      }
      
      /*-----------------------------------------------*/
      /*  construct product generation date and time   */
      /*  sec2[0], sec2[1] union with sec4             */
      /*-----------------------------------------------*/
      
      strcpy(cdate,convertJulianDate(hdr[13]));
      sprintf(year,"%c%c",cdate[6],cdate[7]);
      sprintf(month,"%c%c",cdate[0],cdate[1]);
      sprintf(day,"%c%c",cdate[2],cdate[3]);
      
      gen_time_sec.sec2[0] = hdr[14];   gen_time_sec.sec2[1] = hdr[15];
      time = div(gen_time_sec.sec4,3600);
      ghour = (short) time.quot;
      gminute = time.rem/60;
      sprintf(chour,"%02d",ghour);
      sprintf(cminute,"%02d",gminute);
      
      /*-------------------------------------------------------------------------*/
      /*  write supplemental data and other data to DPARadar table               */
      /*  replace value of operational weather mode with value read from header  */
      /*-------------------------------------------------------------------------*/
      
      pcip = *pcipflg;
      mode = (int) hdr[6];
      if ( write_to_db == 1 )
      {
	 wrtodb_suppl(build_num, strdt, &minoff, &maxvalh, &maxvald,
		      &bias, year, month, day, chour, cminute,
		      &nisol,&nouti,&noutr,&area,&biscan,&nbad, 
		      &nhour,&volcov,&mode,
		      &nblocked,&nclutter,&nsmoothed,&pctfill,&angle,&rainarea,
		      missper,&pcip,fname);
      }
      
   }   /*  end if toph_dec_flg == 1  */
   
   /*-------------------------------------------------------------------------*/
   /*  copy raw DPA product (Big Endian format) to archive dir (if requested) */
   /*-------------------------------------------------------------------------*/
   
   if(toph_arc_flg == 1)
   {
      fullrawfname = (char *)malloc(128*sizeof(char));
      strcpy(fullrawfname,rawfname);
      
      copy_to_archive(minute, hour, cdate, fname, fullrawfname);
   }
   
}

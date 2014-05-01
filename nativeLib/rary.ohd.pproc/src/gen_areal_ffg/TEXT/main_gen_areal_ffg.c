#include <datetime.h>
#include "time_convert.h"
#include "current_GMT_dt.h"
#include "gen_areal_ffg.h"
#include "create_ffg_mosaic.h"
#include "check_RFC_ffg_files.h"
#include "get_site_dimensions.h"
#include "parse_rfc_names.h"
#include "parse_durations.h"
#include "process_areas.h"
#include "create_mos_filename.h"
#include "writeFFGnetCDF.h"
#include "writePerfLog.h"
#include "get_last_run_time.h"
#include "get_hsa.h"
#include "startdb.h"
#include "closedb.h"
#include <sys/utsname.h>

/*---------------------------------------------------------------*/
/*   this function is main driver for generating the average FFG */
/*        values for basins                                      */
/*                                                               */
/*   input:  gridded FFG files from RFCs (netCDF format)         */
/*                                                               */
/*   output: netCDF file containing gridded FFG mosaic           */
/*           records written to ContingencyValue table           */
/*                                                               */
/*---------------------------------------------------------------*/

int gen_areal_ffg_main(int argc, const char ** argv)
{

int logall_flag=0, ret;
int i, irfc, idur, len, num_rfc, num_rfc_files, num_dur, duration, nmalloc=1;
int xor, yor, xsize, ysize;
time_t latest_lastmodtime, last_run_time_t=0;
float lat00, lon00;
long irc;
char input_dir[128], mosaic_dir[128], mosaic_filename[256], siteid[10], climit[3];
char process_name[10], last_run_time_str[22], cminarea[5];

dtime_t GMTtime_dt;

struct utsname   uts_struct;

/*-------------*/
/*   print OS  */
/*-------------*/

printf( "\n%s %s - %s\n", genarealffg_name, genarealffg_ver, genarealffg_date );

uname(&uts_struct);
printf("operating system: %s\n",uts_struct.sysname);

/*-------------------------------------------------*/
/*   read site name from .Apps_defaults tokens     */
/*   currently using st3_rfc for site name         */
/*-------------------------------------------------*/

len = strlen("st3_rfc");
get_apps_defaults("st3_rfc",&len,siteid,&len);
if(len == 0)
{
   printf("st3_rfc token not found -- program stopping\n");
   exit(1);
}
else
   printf("st3_rfc token = %s\n",siteid);

/*-------------------------------------------------*/
/*   read other .Apps_defaults tokens              */
/*-------------------------------------------------*/

len = strlen("gaff_input_dir");
get_apps_defaults("gaff_input_dir",&len,input_dir,&len);
if(len == 0)
{
   printf("gaff_input_dir token not found -- program stopping\n");
   exit(1);
}
else
   printf("gaff_input_dir token value = %s\n",input_dir);

len = strlen("gaff_mosaic_dir");
get_apps_defaults("gaff_mosaic_dir",&len,mosaic_dir,&len);
if(len == 0)
{
   printf("gaff_mosaic_dir token not found -- program stopping\n");
   exit(1);
}
else
   printf("gaff_mosaic_dir token value = %s\n",mosaic_dir);

memset(climit, '\0', 3);
len = strlen("gaff_look_back_limit");
get_apps_defaults("gaff_look_back_limit",&len,climit,&len);
if(len == 0)
{
   printf("gaff_look_back_limit token not found -- value used = 6 hrs\n");
   lookback_limit = 6;
}
else
{
   lookback_limit = atoi(climit);
   printf("lookback_limit = %d hrs\n",lookback_limit);
}

memset(cminarea, '\0', 5);
len = strlen("whfs_min_area_covered");
get_apps_defaults("whfs_min_area_covered",&len,cminarea,&len);
if(len == 0)
{
   printf("whfs_min_area_covered token not found -- value used = 0.90\n");
   min_coverage = 0.90;
}
else
{
   min_coverage = atof(cminarea);
   printf("minimum areal coverage = %5.2f\n",min_coverage);
}

/*----------------------------------------*/
/*   other initializations                */
/*----------------------------------------*/

strcpy(process_name,"gen_ffg");
irc = current_GMT_dt(&GMTtime_dt);
if(irc !=0)
{
   printf("error = %ld getting current GMT",irc);
   exit(1);
}

/*-------------------------------------------------*/
/*   parse RFC names from the gaff_rfc_list token  */
/*   RFC names are stored in rfc_names array       */
/*-------------------------------------------------*/

num_rfc = parse_rfc_names();

/*---------------------------------------------------*/
/*   parse durations from the gaff_durations token   */
/*   durations are stored in ffg_durations array     */
/*---------------------------------------------------*/

num_dur = parse_durations();

/*-------------------------------*/
/*   open database               */
/*-------------------------------*/

startdb(&irc);
if(irc !=0)
{
   printf("PostgreSQL error# %ld ",irc);
   printf(" occurred attempting to open database \n");
   exit(1);
}

/*--------------------------------------------------------*/
/*   get time of last run                                 */
/*   if no time found, then 00z and today's date returned */
/*   print ascii format to log                            */
/*   time_t format used for later comparison              */
/*--------------------------------------------------------*/

get_last_run_time(process_name, last_run_time_str, &irc);
if(irc != 0)
{
   printf("PostgreSQL error %ld attempting to find last run time\n",irc);
   exit(1);
}

printf("last gaff run time = %s Z\n",last_run_time_str);

irc = yearsec_ansi_to_timet(last_run_time_str, &last_run_time_t);
if(irc != 0)
{
   printf("error = %ld from yearsec_ansi_to_timet (main program)\n",irc);
   exit(1);
}

/*-------------------------------------*/
/*  get hsa from Admin table           */
/*  if not found, then use "XXX"       */
/*-------------------------------------*/

get_hsa(&ret);
if(ret != 0) strcpy(hsa,"XXX");
printf("hsa (site) identifier = %s\n",hsa);

/*-------------------------------------*/
/*   loop on FFG durations             */
/*-------------------------------------*/

for(idur = 0; idur < num_dur; idur++)
{

   duration = ffg_durations[idur];
   latest_validtime = 0;
   latest_lastmodtime = 0;
   num_rfc_files = 0;

   printf("\nDURATION = %d hr\n",duration);

   /*-----------------------------------------------------------*/
   /*  loop on RFCs in token list                               */
   /*  save full pathname of RFC files with valid validtimes    */
   /*  determine latest last mod time from all valid files      */
   /*-----------------------------------------------------------*/

   for(irfc = 0; irfc < num_rfc; irfc++)
   {
      check_RFC_ffg_files(input_dir, irfc, num_rfc, duration, &latest_lastmodtime,
                          &num_rfc_files);
   }

   /*---------------------------------------------------*/
   /*  *** first time through loop on duration only *** */
   /*---------------------------------------------------*/

   if(idur == 0)
   {

      /*----------------------------------------------------------------------------*/
      /*   get dimensions of area, HRAP coord of SW corner and lat/lon of NW corner */
      /*   dimensions and coord of SW corner read from coord_xxx.dat geo_dat file   */
      /*   lat/lon of NW corner written to mosaic FFG netcdf file                   */
      /*   xor,yor = HRAP coord of SW corner                                        */
      /*   lat00,lon00 = lat/lon of NW corner                                       */
      /*----------------------------------------------------------------------------*/

      get_site_dimensions(siteid, &xor, &yor, &xsize, &ysize, &lat00, &lon00);

   }

   /*----------------------------------------------------------------------*/
   /*  if all gridded FFG files for all RFCS are missing or too old,       */
   /*    then take next duration                                           */
   /*  (do not write mosaic file, do not write records to db               */
   /*----------------------------------------------------------------------*/

   if(latest_validtime == 0)
   {
      printf("\nall files for all RFCs missing or older than lookback time limit\n");
      printf("no mosaic FFG file or db records will be written for this duration\n");
      continue;  /* take next duration */
   }

   /*-----------------------------------------------------------------------------------*/
   /*  if time of last gen_areal_FFG run > latest last mod time for all RFC files,      */
   /*  then take next duration                                                          */
   /*  (do not write mosaic file, do not write records to db                            */
   /*-----------------------------------------------------------------------------------*/

   if(num_rfc_files > 0 && last_run_time_t > latest_lastmodtime)
   {
      printf("\nno new files have arrived since last gaff run");
      printf(" for duration = %d hr\n",duration);
      printf("no mosaic FFG file or db records will be written for this duration\n");
      continue;  /* take next duration */
   }

   /*-----------------------------------------------------------*/
   /*  loop on RFCs with files with valid validtimes            */
   /*  open and read netCDF file containing gridded FFG data    */
   /*    for each RFC                                           */
   /*                                                           */
   /*  construct mosaic                                         */
   /*  mosaic will consist of pieces of the gridded FFG field   */
   /*    from the RFCs                                          */
   /*-----------------------------------------------------------*/

   for(irfc = 0; irfc < num_rfc_files; irfc++)
   {
      create_ffg_mosaic(irfc, idur, duration, &nmalloc, xor, yor, xsize, ysize);
   }

   /*-------------------------------------------------*/
   /*  create full pathname of mosaic FFG netCDF file */
   /*  create netCDF file                             */
   /*  writeFFGnetCDF routine is located in .../utilfunc/hv_areal/src  */
   /*-------------------------------------------------*/

   create_mos_filename(mosaic_dir, duration, latest_validtime, mosaic_filename);
   printf("mosaic FFG netCDF filename = %s\n",mosaic_filename);

   writeFFGnetCDF(mosaic_filename, lat00, lon00, xsize, ysize, mosaic_ffg_uchar);

   /*------------------------------------------------------------------------*/
   /*  extract basin area info in form of line segs from db                  */
   /*  for each basin, attempt to compute FFG value averaged over basin area */
   /*  if successful and minimum areal coverage exceeded, then               */
   /*    write record to ContingencyValue table                              */
   /*------------------------------------------------------------------------*/

   process_areas(logall_flag, xor, yor, xsize, ysize, duration);

}  /*  end loop on durations  */

/*----------------------------------------------------*/
/*  if array space was previously malloced, then free */
/*----------------------------------------------------*/

if(nmalloc > 1)
{
   for (i=0; i<ysize; i++) free(mosaic_ffg_float[i]);
      free(mosaic_ffg_float);

   free(mosaic_ffg_uchar);
}

/*---------------------------------*/
/*  write record to PerfLog table  */
/*---------------------------------*/

irc = writePerfLog(process_name, &GMTtime_dt);
if(irc !=0)
{
   printf("PostgreSQL error# %ld ",irc);
   printf(" occurred attempting to insert record into PerfLog table\n");
}

/*------------------------------*/
/*   close database             */
/*------------------------------*/

closedb(&irc);
if(irc !=0)
{
   printf("PostgreSQL error# %ld ",irc);
   printf(" occurred attempting to close database\n");
}

return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

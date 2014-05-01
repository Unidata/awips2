#include <sys/stat.h>
#include "gen_areal_ffg.h"
#include "select_ffg_file.h"

/*-----------------------------------------------------------------------------------*/
/*   this function checks the directory for files with valid validtimes AND          */
/*     keeps track of the latest last mod time of all valid files                    */
/*                                                                                   */
/*   Input:                                                                          */
/*   input_dir = directory containing the gridded FFG netCDF files                   */
/*   irfc = RFC number (from parsing RFC name list)                                  */
/*   duration = duration in units of hours                                           */
/*                                                                                   */
/*   Output:                                                                         */
/*   latest_lastmodtime = latest last mod time for all files with valid validtimes   */
/*   num_rfc_files = number of directories containing files with valid validtimes    */
/*-----------------------------------------------------------------------------------*/

void check_RFC_ffg_files(char *input_dir, int irfc, int num_rfc, int duration,
                         time_t *latest_lastmodtime, int *num_rfc_files)
                       
{

int i, ifile, filename_length=256;
char filename[256], dirname[128];
struct stat buf;

/*-------------------------------------------------------------*/
/*   construct directory name for files containing gridded FFG */
/*   directory name consists of RFC name and duration          */
/*-------------------------------------------------------------*/

sprintf(dirname,"%s/%s/%dhr",input_dir,rfc_names[irfc],duration);
/* printf("directory name = %s\n",dirname);  */

/*-------------------------------------------------------------*/
/*   select file containing latest gridded FFG data            */
/*   if no file found (all files too old, all files missing),  */
/*     then exit this routine                                  */
/*-------------------------------------------------------------*/

ifile = select_ffg_file(dirname, filename);
if(ifile == 1)
{
   printf("latest FFG file in %s/%dhr dir = %s\n",rfc_names[irfc],duration,filename);
}
else
{
   return;
}

/*--------------------------------------------------------------*/
/*  if first file found, then malloc space for filenames_array  */
/*--------------------------------------------------------------*/

if(*num_rfc_files == 0)
{

   filenames_array = (char **) malloc(num_rfc*sizeof(char *));
   for(i = 0; i < num_rfc; i++)
   {
      filenames_array[i] = (char *) malloc(filename_length*sizeof(char));
   
      if(!filenames_array[i])
      {
         printf("malloc of space for filenames_array failed -- ");
         printf("program stopping\n");
         exit(5);
      }

   }

}

/*------------------------------*/
/*  save full pathname of file  */
/*------------------------------*/

sprintf(filenames_array[*num_rfc_files],"%s/%s",dirname,filename);

/*----------------------------------*/
/*  get last mod time of this file  */
/*  compare with last mod times of other files */
/*----------------------------------*/

i = stat(filenames_array[*num_rfc_files],&buf);
if(i != 0)
{
   printf("error code = %d returned from stat function\n",i);
   return;
}

if(buf.st_mtime > *latest_lastmodtime) *latest_lastmodtime = buf.st_mtime;

*num_rfc_files = *num_rfc_files + 1;
}

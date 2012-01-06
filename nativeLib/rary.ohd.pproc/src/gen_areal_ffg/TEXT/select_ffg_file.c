#include <dirent.h>
#include <errno.h>
#include "gen_areal_ffg.h"
#include "comp_strings.h"
#include "get_current_time.h"
#include "tadj.h"

int select_ffg_file(char *dir, char file[128])
{

/*----------------------------------------------------------------------*/
/*  this routine queries the directory containing the gridded FFG files */
/*  it sorts the files based on the timestamp of the filenames, applies */
/*    the look-back time and returns the selected filename to the       */
/*    calling routine                                                   */
/*                                                                      */
/*  input:  directory name                                              */
/*  output: filename selected, latest_validtime                         */
/*                                                                      */
/*  latest_validtime is passed through .h file and is written           */
/*    into validtime field of ContingencyValue table                    */
/*                                                                      */
/*  return values:  0 -- no file found                                  */
/*                  1 -- file found                                     */
/*                                                                      */
/*  calling routine:  create_ffg_mosaic                                 */
/*----------------------------------------------------------------------*/

DIR *dirp;
struct dirent *dp;
char **filenames, filename[80], validtime[11];
int i, numfiles, filename_length=80, ivalidt;
int cyear, cmonth, cday, chour, cmin, csec, adj_factor, llimit, current_time;

/*---------------------------*/
/*  open directory           */
/*---------------------------*/

dirp = opendir(dir);
if (dirp == NULL)
{
   printf("failed to open directory (first time)= %s\n",dir);
   printf("errno = %d\n",errno);
   printf("missing data will be used for this RFC\n");
   return 0;
}

/*-----------------------------------------------------------*/
/*   read through files in dir to determine number of files  */
/*-----------------------------------------------------------*/

numfiles = 0;
while ((dp = readdir(dirp)) != NULL)
{
   strcpy(filename, dp->d_name);
   if(strlen(filename) > 2)  numfiles++;      /* exclude entries "." and ".."  */
}
 
/*---------------------------*/
/*  close directory          */
/*---------------------------*/

closedir(dirp);

/*---------------------------------*/
/*  if no files found, then return */
/*---------------------------------*/

/* printf("directory contains %d files\n",numfiles); */
if(numfiles == 0)
{
   printf("FFG dir contains no files - missing data will be assumed\n");
   return 0;
}

/*--------------------------------------------------------*/
/*   malloc space for array containing filenames          */
/*   each filename assumed to be less than filename_length long  */
/*--------------------------------------------------------*/

filenames = (char **) malloc(numfiles*sizeof(char *));
for(i = 0; i < numfiles; i++)
{
   filenames[i] = (char *) malloc(filename_length*sizeof(char));

   if(!filenames[i])
   {
      printf("malloc of space for filenames array failed -- ");
      printf("program stopping\n");
      exit(5);
   }

}

/*---------------------------*/
/*  reopen directory           */
/*---------------------------*/

dirp = opendir(dir);
if (dirp == NULL)
{
   printf("failed to open directory (second time)= %s\n",dir);
   printf("errno = %d\n",errno);
   printf("missing data will be used for this RFC\n");
   return 0;
}

/*---------------------------*/
/*  fill filenames array     */
/*---------------------------*/

i = 0;
while ((dp = readdir(dirp)) != NULL)
{
   strcpy(filename, dp->d_name);
   if(strlen(filename) > 2)
   {
      strcpy(filenames[i], filename);
      i++;
   }
}

/*---------------------------*/
/*  close directory          */
/*---------------------------*/

closedir(dirp);

/*------------------------------------------*/
/*  sort filenames into descending order    */
/*------------------------------------------*/

qsort((void *)filenames, (size_t) numfiles, (size_t)sizeof(int), comp_strings);

/*--------------------------------------------------------------------------------*/
/*  check if time stamp of first filename in list is within lookback time window  */
/*--------------------------------------------------------------------------------*/

/*-----------------------------------------------------*/
/*  get current date/time and adjust by lookback limit */
/*-----------------------------------------------------*/

get_current_time(&cyear, &cmonth, &cday, &chour, &cmin);

csec = 0;
adj_factor = 2;
llimit = -1 * lookback_limit;
TADJ(&cyear, &cmonth, &cday, &chour, &cmin, &csec, &llimit, &adj_factor);

current_time = (cyear *1000000) + (cmonth * 10000) + (cday * 100) + chour;
/* printf("current_time (int) = %d\n",current_time); */

/*-----------------------------------------------------*/
/*  get validtime in int format                        */
/*-----------------------------------------------------*/

sprintf(validtime,"%c%c%c%c%c%c%c%c%c%c",filenames[0][0],filenames[0][1],
                                             filenames[0][2],filenames[0][3],
                                             filenames[0][4],filenames[0][5],
                                             filenames[0][6],filenames[0][7],
                                             filenames[0][9],filenames[0][10]);

ivalidt = atoi(validtime);
/* printf("validtime (int) = %d\n",ivalidt); */

/*--------------------------------------------------------------*/
/*  compare validtime and (current date/time - lookback_limit ) */
/*--------------------------------------------------------------*/

if(ivalidt < current_time)
{
   printf("all files in directory are older than lookback time limit\n");
   printf("missing data will be used for this RFC\n");
   return 0;
}

if(latest_validtime < ivalidt) latest_validtime = ivalidt;

strcpy(file, filenames[0]);

/*---------------------------*/
/*  free array space         */
/*---------------------------*/

for (i=0; i<numfiles; i++) free(filenames[i]);
free(filenames);

return 1;

}

int comp_strings(const void *pe1, const void *pe2)
{
    return -strcmp(*(char **)pe1, *(char **)pe2);
}

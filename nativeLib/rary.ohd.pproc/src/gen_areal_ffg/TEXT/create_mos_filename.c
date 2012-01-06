#include "stdio.h"
#include "gen_areal_ffg.h"

void create_mos_filename(char *dir, int duration, int validtime, char file[256])
{

/*-----------------------------------------------------------------------*/
/*  this routine creates the full pathname of the mosaic FFG netCDF file */
/*                                                                       */
/*  input:  directory name, hsa  id, duration, validtime                 */
/*  output: full pathname of mosaic FFG netCDF file                      */
/*                                                                       */
/*  calling routine:  main                                               */
/*-----------------------------------------------------------------------*/


char cval[11];

sprintf(cval,"%d",validtime);
sprintf(file,"%s/%s%c%c%c%c%c%c%c%c_%c%c00_%02d.ffg",dir,hsa,
                                                     cval[0],cval[1],
                                                     cval[2],cval[3],
                                                     cval[4],cval[5],
                                                     cval[6],cval[7],
                                                     cval[8],cval[9],
                                                     duration);

}

/***********************************************************************
* Module Name: main_DHRPsm.c
*
* Original Author: David T. Miller, OHD/HSEB
*
* Module Creation Date: Sept 25, 2007
*
* Description:
*   main program for the get_dhrpsm executable used to extract the
*   Precipitation Status Message (PSM) from the WSR-88D
*   Digital Hybrid Reflectivity (DHR) radar product
*
*  product name is passed from the command line into this program
*
*  radar products have a filename of the form
*        XXXX/NNNYYY.yyyymmddhhmmss
*         where XXXX = directory name
*               NNN = radar product: DHR
*               YYY = radar id
*               yyyymmddhhmmss = date/time from system clock
*
*  return codes from get_dhrpsm executable to DHRgather script
*
*  0 -- no precipitation at this radar
*
*  1 -- precipitation dectected
*
*
*
*   calling function:
*   functions called:  get_psm
*
* Calling Arguments:
* Name         Input/Output Type          Description
*
* argv[1]      Input        char *        raw DHR file name
*
*
* Required Files/Databases:
* DHR radar data file
*
* Non System Routines Called:
*
*
* Return Value:
* Type          Description
* int           <0 Error, 0 = no precipitation, 1 = precipitation detected from PSM params[4]
*
* Error Codes/Exceptions:
* -1 Error EOF
* -3 Open file error
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name              Type       Description
* raw_filename      char *     holds raw DHR file name from argv[1]
* params[6]         float      holds PSM parsed parameter values
* num_param         int        number of PSM values
* status            int        status of call to get_psm

* Modification History:
* Date        Developer        Action
* 9/25/2007   David T. Miller  Build operational version
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define FILEPATH_LEN           256


int get_psm(int *num_param, float params[6], FILE *fp);

int get_dhrpsm_main( int argc, const char ** argv)
{
	int status, numparms;
	char raw_filename[FILEPATH_LEN] = {'\0'};
	float params[6];
	FILE * fp = NULL;

        numparms = 0;
	strcpy(raw_filename, argv[1]);


        if((fp = fopen(raw_filename, "rb")) == NULL)
        {
           printf("error opening raw DHR file -- supplemental info not decoded\n");
           return -3;
         }

	/*
	 * decode supplemental section
	 */

        status = get_psm(&numparms, params, fp);
        if( status != 0)
        {
           printf("EOF encountered before finding supplemental parameters header");
           printf(" -- product not decoded\n");
           fclose(fp);
           fp = NULL;
	   return -1;
	}
        fclose(fp);
        fp = NULL;

	printf("%d",(int)params[4]);

	if(params[4]>0.0)
	   return 0;
	else
	   return 1;

}

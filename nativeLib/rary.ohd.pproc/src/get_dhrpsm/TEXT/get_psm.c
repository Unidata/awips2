/***********************************************************************
* Filename: get_psm.c
*
* Original Author: David T. Miller
*
* File Creation Date: Sept 25, 2007
*
* Development Group: OHD / HSEB
*
* Description:
* Contains routine for reading Precipitation Status Message (PSM)
* from the Digital Hybrid Reflectivity (DHR) WSR-88D raw radial radar file.
* As of ORPG build 8, there were 3 possible values for the PSM:
* 0 = no precipitation, 1 = precipitation, 2 = light precipitation
* 
* This routine will normally be called by the DHR data gathering script
* and if any radars within a certain area have a PSM > 0, 
* then the Enhanced Multi-sensor Precipitation Estimator (EMPE) start
* script will be called.  If all radar PSMs are 0, EMPE will be stopped.
*
* This is patterned after the get_adapt.c routine developed by Feng Ding of 
* OHD/HSMB.
* 
*
* Calling Arguments:
* Name         Input/Output Type          Description
* num_param    Output       int *         The number of adaptable parameters
* params       Output       float[6]      The precip status message parameter array.
* param38      Output       char[2]       The psm flag parameter.
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* 
*
* Return Value:
* Type          Description
* int           returned status of this routine
*
* Error Codes/Exceptions:
* Will return 1 for error (EOF)
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
* head     char       Used to compare against the file header to find the start
*                     of the PSM section
* str3     char       filled with number of characters in message
* str4     char       4-character filler string to compare against head
* str9     char       filled with string representation of message values converted to float
* blank8   char       compared against str9 and if that is blank, then set the param array value
*                     to missing (-99.0)
*
* Modification History:
* Date             Developer     Action
*
* Sept 25, 2007    David Miller  Original coding
*                   
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <string.h>   
#include <stdlib.h>
#include <stdio.h>




int get_psm(int *num_param, float params[6], FILE *fp)
{
    int n;
    char ch, prevchar;
    char head[4], str4[4], str3[3], str9[9], blank8[9];

    head[0] = 'M';
    head[1] = ' ';
    head[2] = '(';
    head[3] = '\0';
    prevchar  =  ' ';
    blank8[8] = '\0';
    strcpy(blank8,"        ");

    /*----------------------------*/
    /*  search for header         */
    /*----------------------------*/

    for (;;)
    {
        n = fscanf(fp, "%c", &ch);
        if (n == EOF)
        {
            return 1;
        }

        if (ch == 'S' && prevchar == 'P')
        {
            fgets(str4, 4, fp);
            if (strcmp(str4, head) == 0) break;
        }
        prevchar = ch;
    }

    /*-----------------------------*/
    /*  read number of parameters  */
    /*-----------------------------*/

    fgets(str3, 3, fp);
    *num_param = atoi(str3);

    /*-----------------------------*/
    /*  read past ')'              */
    /*-----------------------------*/

    fseek(fp, 1L, SEEK_CUR);

    /*---------------------------------------------*/
    /*  read parameters                            */
    /*                                             */
    /*  all parameters are floats except for last  */
    /*  parameter which is char[1]                 */
    /*---------------------------------------------*/

    for(n = 0; n < *num_param; n++)
    {
        if(fgets(str9, 9, fp) == NULL)
        {
            printf("EOF encountered before completion of"
                   " read of supplement parameters"
                   " -- last parameter read was number %d\n",n);
            return 1;
        }
        if(strcmp(blank8, str9) == 0)
            params[n] = -99.0;
        else
            params[n] = atof(str9);
    }

    return 0;

}

/***********************************************************************
* Filename: get_radid_from_filename.c
*
* Original Author: adopted from DPA decoder
*
* File Creation Date: 
*
* Development Group: OHD 
*
* Description:
* Contains routine for parsing radar id from the filename.
*
* Modules:
* get_radid_from_filename
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "decode_radar_product.h"

/***********************************************************************
* Module Name: get_radid_from_filename
*
* Original Author: adopted from DPA decoder
*
* Module Creation Date: 
*
* Description:
*   This subroutine parses radar id from the filename.
*
*    calling function: main_decode
*
* Calling Arguments:
* Name         Input/Output Type          Description
* filename     Input        char *        The filename of radar product
* radid        Output       char *        The radar ID 
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
* int           The product type.
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer     Action
* 7/07/2006   Guoxian Zhou  Build the operational version
*
***********************************************************************/

int decode_dhr_dsp_get_radid_from_filename(const char *filename, char * radid)
{
    char prod_form[4] = {'\0'};
    char *slash = "/";
    char st1[2] = {'\0'};
    char st2[2] = {'\0'};
    int i;
    char first_radid; 
    productType type = unknown; 
    char tmp_filename[FILEPATH_LEN] = {'\0'};

    strcpy(st2, slash);
    strcpy(tmp_filename, filename);

    /*
     * search for string = "DSP" or "DHR" in first 3 char
     * after last "/" radar id follows the product string.
     */

    for (i = strlen(tmp_filename) - 1; i >= 0; i--)
    {
        sprintf(st1, "%c", tmp_filename[i]);
        if(strcmp(st1, st2) == 0) break;
    }

    sprintf(prod_form, "%c%c%c",
        tmp_filename[i+1], tmp_filename[i+2], tmp_filename[i+3]);

    if( (strcmp(prod_form, "DHR") == 0) ||
        (strcmp(prod_form, "DSP") == 0) )
    {
        if (strcmp(prod_form, "DHR") == 0)
        {
            type = dhr;
        }
        else if (strcmp(prod_form, "DSP") == 0)
        {
            type = dsp;
        }

        first_radid = tmp_filename[i+4];
        if ( first_radid != '_' )
        { 
            if(strlen(tmp_filename) >= (i + 6) )
            {
                sprintf(radid,"%c%c%c",
                    tmp_filename[i+4], tmp_filename[i+5], tmp_filename[i+6]);
            }
        }
        else
        {
            if(strlen(tmp_filename) >= (i + 15) )
            {
                sprintf(radid,"%c%c%c",
                    tmp_filename[i+13], tmp_filename[i+14], tmp_filename[i+15]);
            }
        }       
    }

    return type ;
}

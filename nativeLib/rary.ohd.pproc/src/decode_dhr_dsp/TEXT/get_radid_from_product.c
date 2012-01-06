/***********************************************************************
* Filename: get_radid_from_product.c
*
* Original Author: adopted from DPA decoder
*
* File Creation Date: 
*
* Development Group: OHD 
*
* Description:
* Contains routine for parsing radar id from product header.
*
* Modules:
* get_radid_from_product
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "decode_radar_product.h"

/***********************************************************************
* Module Name: get_radid_from_product
*
* Original Author: adopted from DPA decoder 
*
* Module Creation Date: 
*
* Description:
*   This subroutine parses radar id from product header.
*      radar id follows "DHR" or "DSP" string
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

int decode_dhr_dsp_get_radid_from_product(const char *filename, char * radid)
{
    int n;
    char ch;
    char prevchar = ' ' ;
    char pprevchar = ' ' ;
    productType type = unknown; 
    FILE * fp = NULL;

    /*
     * open product
     */

    if((fp = fopen(filename,"rb")) == NULL)
    {
        printf("error opening product -- product not deocded\n");
        exit(3);
    }

    /*
     * search product character by character for
     * string = "DHR" or "DSP",
     * if not found before EOF, then exit program.
     */

    for (;;)
    {
        n = fscanf(fp, "%c", &ch);

        if (n == EOF) 
        {
            printf("Error: EOF encountered before finding radar identifier");
            printf(" in %s\n", filename);
            fclose(fp);
            exit(2);
        }

        if ( (ch == 'R' && prevchar == 'H' && pprevchar == 'D') ||
             (ch == 'P' && prevchar == 'S' && pprevchar == 'D') )
        {
            if (ch == 'R' && prevchar == 'H' && pprevchar == 'D')
                type = dhr;
            else if (ch == 'P' && prevchar == 'S' && pprevchar == 'D')
                type = dsp;

            fgets(radid, 4, fp);
            break;
        }
        else
        {
            pprevchar = prevchar;
            prevchar = ch;
        }
    }

    fclose(fp);
    fp = NULL;

    return type;

}

/***********************************************************************
* Filename: check_radid.c
*
* Original Author: adopted from DPA decoder
*
* File Creation Date: 
*
* Development Group: OHD 
*
* Description:
* Contains routine for checking the radar identifier
* against the records in the RadarLoc table.
*
* Modules:
* check_radid
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include <string.h>   
#include "decode_radar_product.h"
#include "RadarLoc.h"

/***********************************************************************
* Module Name: check_radid
*
* Original Author: adopted from DPA decoder 
*
* Module Creation Date: 
*
* Description:
*   This subroutine checks the radar identifier
*   against the records in the RadarLoc table.
*
*   calling function: main_decode
*
* Calling Arguments:
* Name       Input/Output   Type        Description
* radid      Input          char *      The radar ID 
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
* Type      Description
* int       The check status.
*      = 0 -- the radar id is in the table and the use_radar field = 'T'
*      = 1 -- the radar id is not in the table
*      = 2 -- the use_radar field = 'F'
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type     Description
*
* Modification History:
* Date        Developer       Action
*
***********************************************************************/

int decode_dhr_dsp_check_radid(const char * radid)
{
    char where[80], useradar_flag[2];
    
    RadarLoc *formHead = NULL ;
    RadarLoc *formPtr = NULL ;

    /*
     * get records in linked list form from RadarLoc table
     */

    sprintf(where, "WHERE radid = '%s'", radid);

    formHead = GetRadarLoc(where);

    if(formHead != NULL)
    {
        formPtr = (RadarLoc*) ListFirst(&formHead->list);

        strcpy(useradar_flag, formPtr->use_radar);
        FreeRadarLoc(formHead);

        if(strcmp(useradar_flag, "F") == 0) 
            return 2;
        else
            return 0;
    }
    else
    {
        FreeRadarLoc(formHead);
        return 1;
    }
}

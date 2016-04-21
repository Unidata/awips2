#include <string.h>   
#include "decodedpa.h"
#include "RadarLoc.h"

int check_radid()

{

/*

this function checks the radar identifier against the records in the RadarLoc table

if the radar id is not in the table (return value = 1)  OR
the use_radar field='F' (return value = 2) 
then radar will not be decoded

calling subroutines: main_decodedpa

*/
      char where[80], useradar_flag[2];

      RadarLoc *formHead,*formPtr;

/*-------------------------------------------------------*/
/*  get records in linked list form from RadarLoc table  */
/*-------------------------------------------------------*/

      sprintf(where,"WHERE radid = '%s'",radid);   
      if((formHead = GetRadarLoc(where)))
      {

        formPtr = (RadarLoc*) ListFirst(&formHead->list);

        strcpy(useradar_flag,formPtr->use_radar);
        FreeRadarLoc(formHead);
        if(strcmp(useradar_flag,"F") == 0) 
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

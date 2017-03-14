
#include "DesiredProductRecord.h"



/**********************************************************************/

void initDesiredProductRecord(DesiredProductRecord *dpr)
{
     dpr->absHead = NULL;
     dpr->absSelectedPos = -1;
     
     dpr->relHead = NULL;
     dpr->relSelectedPos = -1;
    
     return;   
}

/**********************************************************************/

DesiredProductRecord * getDesiredProductRecord(void)
{
     static DesiredProductRecord dpr;
          
     return &dpr;  
}

/**********************************************************************/


/**********************************************************************/

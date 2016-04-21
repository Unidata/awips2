#ifndef DESIRED_PRODUCT_RECORD
#define DESIRED_PRODUCT_RECORD


#include <stdio.h>
#include <stdlib.h>

#include "ArealProductControl.h"
/* #include "HvAbsDesiredProd.h"
   #include "HvRelDesiredProd.h" */

/*******************************************************************************/

typedef struct DesiredProductRecord
{
   
   HvAbsDesiredProd * absHead;
   int absSelectedPos;
   
   
   HvRelDesiredProd * relHead;
   int relSelectedPos;
   
} DesiredProductRecord;


/*******************************************************************************/

void initDesiredProductRecord(DesiredProductRecord *dpr);

DesiredProductRecord * getDesiredProductRecord(void);

/*******************************************************************************/


#endif

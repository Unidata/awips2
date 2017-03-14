#ifndef AREAL_PRODUCT_CONTROL_H
#define AREAL_PRODUCT_CONTROL_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

/*#include "HvAbsDesiredProd.h"
#include "HvRelDesiredProd.h" */

#include "ArealProductSettings.h"
#include "FfmUtils.h"

#include "time_convert.h"
#include "time_defs.h"
#include "DbmsDefs.h"
#include "List.h"

/* December 27, 2005 --> The HvAbsDesiredProd and HvRelDesiredProd tables
   have been removed from the OB7 IHFS database.  Added structures here
   as patch until the HvAreal code can be cleaned up.  Bryon L. */
typedef struct _HvAbsDesiredProd
{
    Node                node;
    long                end_hour;
    long                dur_hours;
    long                lookback_hours;
    char                active[2];
    List                list;
} HvAbsDesiredProd;

typedef struct _HvRelDesiredProd
{   
    Node                node;
    long                end_hour_offset;
    long                dur_hours;
    char                active[2];
    List                list;
} HvRelDesiredProd;


ArealProductSpecifier * 
   loadDesiredSpecifiers(ArealProductControl *DesiredControls,
					       long		  numDesiredControls,
					       int		  *numDesiredSpecifiers);


ArealProductSpecifier * readArealProductControls(int *numSpecifiers);

ArealProductControl * readArealProductControlFromDb(long *numArealProductControls);


ArealProductSpecifier * generateSpecifiers(time_t curTime,
					   ArealProductControl control,
					   long *numSpecs);

int compareSpecifiers(const void *elem1, const void *elem2);
int compareSpecifiers2(const void *elem1, const void *elem2);
int compareSpecifiers3(const void *elem1, const void *elem2);


void printProductDescriptor(ArealProductTypeDescriptor descriptor);
void printArealProduct(const ArealProduct *product);
void printProductSpecifier(ArealProductSpecifier *specifier);
void printArealData(ArealData data);


#endif

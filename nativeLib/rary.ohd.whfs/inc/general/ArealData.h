#ifndef AREAL_DATA_H
#define AREAL_DATA_H

#include  "HvDisplayControl.h"
#include  "mapBackgrounds.h"
#include  "ContingencyValue.h"
#include  "ProcValue.h"
#include  "ArealProductType.h"




void drawArealData(HvDisplayControl *hdc);
     void drawBoundedArealData(HvDisplayControl *hdc);
     void drawGriddedArealData(HvDisplayControl *hdc);


void readBoundedArealData(ArealProductType productType,
  		       HvDisplayControl *hdc);
     
long getMatchingAreaIndex(char *lid, MapArea *areas, long numAreas);


#endif

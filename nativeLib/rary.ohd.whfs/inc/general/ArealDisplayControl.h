#ifndef AREAL_DISPLAY_CONTROL
#define AREAL_DISPLAY_CONTROL


#include "HvDisplayControl.h"
#include "ColorThreshold.h"
#include "ColorBar.h"
#include "ArealProductSettings.h"
#include "ArealProductControl.h"

/*
	routines related to product list
*/

void loadProductList(HvDisplayControl *hdc, int reloadData);
void  loadArealProductList(HvDisplayControl *hdc);
  
void freeHvArealProducts(HvDisplayControl *hdc);
void getArealTimePeriodDataFromString(char *origString,			      
				      time_t *timet, long *duration);

int isEqualProductDescriptor(ArealProductTypeDescriptor d1,
			     ArealProductTypeDescriptor d2);
int isDescriptorEqualExceptResolution(ArealProductTypeDescriptor d1,
			            ArealProductTypeDescriptor d2);

ArealProductTypeDescriptor getProductDescriptorFromGUI(void);

void setArealOptionDataFromGui(HvDisplayControl *hdc);
void setArealFilterDataFromGui(HvDisplayControl *hdc);
void setBoundedArealFilterDataFromGui(HvDisplayControl *hdc);
#ifdef OLD_CODE
void setGriddedArealFilterDataFromGui(HvDisplayControl *hdc);
#endif


void freeArealProduct(ArealProduct *product);


void convertProduct(const ArealProduct *product, HvDisplayControl *hdc);
void fillGrid(const ArealProduct *product, HvDisplayControl *hdc);
void fillAreas(const ArealProduct *product, HvDisplayControl *hdc);


void determineThresholdSetKey(char *thresholdUseName, time_t *duration,
			      HvDisplayControl *hdc);

void createProductDescriptionText(ArealProductTypeDescriptor descriptor,
				      ArealProductSpecifier specifier,
				      char *description1,
				      char *description2);
void drawArealProduct(HvDisplayControl *hdc);
#endif

#ifndef WEATHER_H
#define WEATHER_H

#include "degrib_inc/meta.h"

void FreeUglyString (UglyStringType * ugly);

int ParseUglyString (UglyStringType * ugly, char *wxData, int simpleVer);

void PrintUglyString (UglyStringType *ugly);

#endif

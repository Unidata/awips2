#ifndef STATION_LEGEND_H
#define STATION_LEGEND_H

#include "Xtools.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Xtools.h"
#include "GenericLegendDialog.h"
#include "HvColorList.h"
#include "drawStations.h"





/*
	prototypes

*/

void startStationLegend(Widget parent, GenericLegendDialog *dialog);

void drawStationLegend(struct _GenericLegendDialog *dialog);

#endif

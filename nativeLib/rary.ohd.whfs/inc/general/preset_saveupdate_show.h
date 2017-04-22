#ifndef PRESET_SAVEUPDATE_H
#define PRESET_SAVEUPDATE_H

#include <Xm/Xm.h>

#include "PointDataPresets.h"

/* callbacks */

void add_saveasnew_callbacks();

/* Actions. */
void load_saveasnew ( PointDataPresets * pPresetListHead ,
                      PointDataPresets * pPresetNode ) ;

#endif

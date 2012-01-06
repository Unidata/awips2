#ifndef HV_UTIL_H
#define HV_UTIL_H

#include <time.h>
#include <datetime.h>
#include <sqltypes.h>

#include "MotifWidgets.h"
#include "DbmsUtils.h"
#include "time_convert.h"


#define LONG_NAME_LEN 80
typedef char LongName[LONG_NAME_LEN];


void setEditable(Widget w, Boolean isEditable);
void setFormEditable(Widget parent, Boolean isEditable);

Boolean isNullDt(dtime_t *dt);
time_t  get_yearsec_time_t(dtime_t *dt);
unsigned long getValueColor(double value,
			    unsigned long *colors,
			    long numColors,
			    double *thresholds,
			    long numThresholds,
			    unsigned long defaultColor
			   );

void	printCurrentTime(void);
void    profileTime(char *string);

#endif

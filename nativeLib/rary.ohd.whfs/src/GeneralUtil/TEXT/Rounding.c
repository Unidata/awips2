#include <stdio.h>
#include <math.h>
#include "GeneralUtil.h"

double round_to_places(double numberToRound, int decimalPlacesToMaintain)
{
	long base = 10;
	long roundingFactor = base;
	long temporaryHolder;
	long roundingFactorArray[] = {1, 10, 100, 
				      1000, 10000, 100000,
				      1000000, 10000000, 100000000,
				      1000000000};
	
	if (decimalPlacesToMaintain < 0)
	{
		decimalPlacesToMaintain = 0;
	}
	else if (decimalPlacesToMaintain > 9)
	{
		decimalPlacesToMaintain = 9;
	}
	
	
    // I could have used pow(), but this is faster
	roundingFactor = roundingFactorArray[decimalPlacesToMaintain];
			
	temporaryHolder = floor ( (numberToRound * roundingFactor) + 0.5);
			
	numberToRound = (double)temporaryHolder / roundingFactor;
	
	return numberToRound;
}


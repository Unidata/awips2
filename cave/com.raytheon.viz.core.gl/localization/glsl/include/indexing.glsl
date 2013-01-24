
float HALF_FLOAT_NaN = 65504.0;

/**
 * This function takes an index number and caps it to the range 0-1
 */
float capIndex(float index) {
	if ( index < 0.0 ) {
		index = 0.0;
	} else if ( index > 1.0 ) {
		index = 1.0;
	}
	return index;
}

/**
 * This function linearly finds the index for the rawValue into cmapMin/cmapMax.
 * 65504.0 is treated as NaN for half floats and -1 is returned as special case
 */
float findFloatIndex(float rawValue, float cmapMin, float cmapMax) {
	if ( rawValue == HALF_FLOAT_NaN || rawValue != rawValue) {
		return -1.0;
	}
	float index = ((rawValue - cmapMin) / abs(cmapMax-cmapMin));
	return capIndex(index);
}

/**
 * This function logarithmically finds the index for the rawValue into cmapMin/cmapMax.
 * 65504.0 is treated as NaN for half floats and -1 is returned as special case
 */
float findFloatIndexLog(float rawValue, float cmapMin, float cmapMax, int mirror) {
	if ( rawValue == HALF_FLOAT_NaN ) {
		return -1.0;
	}
	
	float index = 0.0;
    // is this strictly negative, strictly positive or neg to pos scaling?
    if ( cmapMin >= 0.0 && cmapMax >= 0.0 && mirror!=1) {
        if(rawValue < cmapMin){
            index = 0.0;
        }else{
            // simple calculation
            index = ((log(rawValue) - log(cmapMin)) / abs(log(cmapMax)-log(cmapMin)));
        }
    } else if (cmapMin <= 0.0 && cmapMax <= 0.0 && mirror!=1) {
        index = ((log(rawValue) - log(cmapMax)) / abs(log(cmapMin)-log(cmapMax)));
    } else {
        // special case, neg to pos:
        float colorMapMin = cmapMin;
        float colorMapMax = cmapMax;
        float zeroVal = max(colorMapMax, abs(colorMapMin)) * 0.0001;
        if (mirror==1 && (colorMapMin > 0.0 || colorMapMax < 0.0)) {
            if (colorMapMax < 0.0) {
                colorMapMax = -cmapMax;
                rawValue = -rawValue;
                zeroVal = -colorMapMin;
            } else {
                zeroVal = cmapMin;
            }
            colorMapMin = -cmapMax;
        }
        float leftZero = 0.0;
        float rightZero = 0.0;
        float absLogZeroVal = abs(log(zeroVal));

        rightZero = absLogZeroVal + log(colorMapMax);

        float cmapMax2 = abs(colorMapMin);

        leftZero = absLogZeroVal + log(cmapMax2);

        float zeroIndex = leftZero / (leftZero + rightZero);

        // figure out index for texture val
        float absTextureColor = abs(rawValue);
        if (absTextureColor <= zeroVal) {
            index = zeroIndex;
        } else if (rawValue > 0.0) {
            // positive texture color value, find index from 0 to
            // cmapMax:
            float logTexColor = absLogZeroVal + log(rawValue);

            float texIndex = logTexColor / rightZero;
            index = (zeroIndex + ((1.0 - zeroIndex) * texIndex));
        } else {
            // negative texture color value, find index from 0 to
            // cmapMax:
            float logTexColor = absLogZeroVal + log(absTextureColor);

            float texIndex = logTexColor / leftZero;
            index = (zeroIndex - (zeroIndex * texIndex));
        }
    }
    return capIndex(index);
}

/**
 * Given a raw data value linearly determine the index(0-1) into cmapMin/cmapMax
 */
float findIndex(float rawValue, float cmapMin, float cmapMax) {
	float index = ((rawValue - cmapMin) / abs(cmapMax-cmapMin));
	return capIndex(index);
}

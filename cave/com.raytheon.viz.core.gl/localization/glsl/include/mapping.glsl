/**
 * Mapping glsl library for use by other glsl programs.  Defines
 * commonly used structures and functions for data and color mapping 
 */

/**
 * Fields for the raw texture mapping is applied to. isScaled
 * implied data will range 0-1 and need scaling to get raw value
 * where scaleMin maps to 0 and scaleMax maps to 1.
 */
struct DataTexture {
	sampler2D rawTex;
	float noDataValue;
	int isScaled;
	float scaleMin;
	float scaleMax;
};

/** 
 * Fields used for converting from image data values to 
 * colormapping data values. Done to avoid conversions in 
 * application code.  dmv[i] -> cmv[i]. Linear interpolation 
 * is done where non-exact matches are found.  Mappings should 
 * be uploaded as floats so no scaling is needed
 */
struct DataMapping {
	sampler1D dataMappingValues;
	sampler1D colorMappingValues;
	int numMappingValues;
};

struct ColorMapping {
	/** Fields for color map and size. colorMap contains colors to
	 * use for mapping. cmapMin/Max is range colormap is applied over */
	sampler1D colorMap;
	float cmapMin;
	float cmapMax;

	/** Field for alpha masking the colors. alphaMask is a texture the 
	 * same size as colorMap and contains 0s and 1s, 1 indicating alpha
	 * should be set to completely transparent */
	int applyMask;
	sampler1D alphaMask;

	/** Fields for logarithmic and mirrored indexing into the colorMap */
	int isMirrored;
	float logFactor;
	int isLogarithmic;
};

/**
 * Returns the data value for the DataTexture at location.
 */
float getDataValue(DataTexture texture, vec2 location) {
	vec4 textureValue = texture2D(texture.rawTex, location);
	float dataValue = textureValue.r;

	if (texture.isScaled == 1) {
		// Convert to non-scaled value
		dataValue = ((dataValue * (texture.scaleMax - texture.scaleMin))
				+ texture.scaleMin);
	}
	return dataValue;
}

/**
 * Looks up a value in a mapping texture given an index [0-numMappingValues).
 */
float lookupMappingValue(sampler1D mappingTex, int index,
		int numMappingValues) {
	return texture1D(mappingTex, float(index) / float(numMappingValues - 1)).r;
}

/**
 * Converts a data value into a colorMap value given the DataMapping
 */
float dataToColorMapValue(float dataValue, DataMapping mapping) {
	int numMappingValues = mapping.numMappingValues;
	if (numMappingValues == 0) {
		// Short circuit if no mapping is needed
		return dataValue;
	}
	
	// Convert to colormap value
	int lowIndex = 0;
	int highIndex = numMappingValues - 1;

	float lowValue = lookupMappingValue(mapping.dataMappingValues, lowIndex,
			numMappingValues);
	float highValue = lookupMappingValue(mapping.dataMappingValues, highIndex,
			numMappingValues);
	int reversed = 0;
	if (lowValue > highValue) {
		reversed = 1;
		float tmp = lowValue;
		lowValue = highValue;
		highValue = tmp;
	}

	int done = 0;
	// While there is at least one index to check
	while (done == 0) {
		int nextIndex = lowIndex + ((highIndex - lowIndex) / 2);
		if (nextIndex > lowIndex && nextIndex < highIndex) {
			// Look up next value and determine if it is a high or low
			float nextValue = lookupMappingValue(mapping.dataMappingValues, nextIndex,
					numMappingValues);
			if (nextValue < dataValue) {
				if (reversed == 0) {
					lowIndex = nextIndex;
				} else {
					highIndex = nextIndex;
				}
				lowValue = nextValue;
			} else {
				if (reversed == 0) {
					highIndex = nextIndex;
				} else {
					lowIndex = nextIndex;
				}
				highValue = nextValue;
			}
		} else {
			done = 1;
		}
	}

	// Percentage dataValue is linearly between low and high value
	float factor = (dataValue - lowValue) / (highValue - lowValue);
	if (reversed == 1) {
		// Reverse factor for high->low indexing
		factor = 1.0 - factor;
	}

	float lowCmapValue = lookupMappingValue(mapping.colorMappingValues, lowIndex,
			numMappingValues);
	float highCmapValue = lookupMappingValue(mapping.colorMappingValues, highIndex,
			numMappingValues);

	return lowCmapValue + (highCmapValue - lowCmapValue) * factor;
}

/**
 * This function takes an index number and caps it to the range 0-1
 */
float capIndex(float index) {
	if (index < 0.0) {
		index = 0.0;
	} else if (index > 1.0) {
		index = 1.0;
	}
	return index;
}

/**
 * Given a colorMap value linearly determine the index (capped at 0-1)
 * into cmapMin/cmapMax
 */
float getLinearIndex(float cmapValue, float cmapMin, float cmapMax) {
	float index = (cmapValue - cmapMin) / (cmapMax - cmapMin);
	return capIndex(index);
}

/**
 * This function logarithmically finds the index for the cmapValue into 
 * cmapMin/cmapMax (capped at 0-1).
 */
float getLogIndex(float cmapValue, float cmapMin, float cmapMax, int mirror) {
	float index = 0.0;
	// is this strictly negative, strictly positive or neg to pos scaling?
	if (cmapMin >= 0.0 && cmapMax >= 0.0 && mirror != 1) {
		if (cmapValue < cmapMin) {
			index = 0.0;
		} else {
			// simple calculation
			index = ((log(cmapValue) - log(cmapMin))
					/ abs(log(cmapMax) - log(cmapMin)));
		}
	} else if (cmapMin <= 0.0 && cmapMax <= 0.0 && mirror != 1) {
		index = ((log(cmapValue) - log(cmapMax))
				/ abs(log(cmapMin) - log(cmapMax)));
	} else {
		// special case, neg to pos:
		float colorMapMin = cmapMin;
		float colorMapMax = cmapMax;
		float zeroVal = max(colorMapMax, abs(colorMapMin)) * 0.0001;
		if (mirror == 1 && (colorMapMin > 0.0 || colorMapMax < 0.0)) {
			if (colorMapMax < 0.0) {
				colorMapMax = -cmapMax;
				cmapValue = -cmapValue;
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
		float absTextureColor = abs(cmapValue);
		if (absTextureColor <= zeroVal) {
			index = zeroIndex;
		} else if (cmapValue > 0.0) {
			// positive texture color value, find index from 0 to
			// cmapMax:
			float logTexColor = absLogZeroVal + log(cmapValue);

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
 * This function calculates a new index to use based on the logFactor
 * and passed in index
 */
float getLogFactorIndex(float index, float logFactor) {
	if (logFactor > 0.0) {
		float minLog = log(logFactor);
		float maxLog = log(logFactor + 1.0);

		float lg = log(logFactor + index);

		index = (lg - minLog) / (maxLog - minLog);
		if (index < 0.0) {
			index = 0.0;
		} else if (index > 1.0) {
			index = 1.0;
		}
	}
	return index;
}

/**
 * Returns an index for the cmapValue based on the ColorMapping
 */
float getColorMappingIndex(float cmapValue, ColorMapping colorMapping) {
	int logarithmic = colorMapping.isLogarithmic;
	int mirror = colorMapping.isMirrored;
	float logFactor = colorMapping.logFactor;
	float cmapMin = colorMapping.cmapMin;
	float cmapMax = colorMapping.cmapMax;

	float index;
	if (logarithmic == 1) {
		index = getLogIndex(cmapValue, cmapMin, cmapMax, mirror);
	} else {
		index = getLinearIndex(cmapValue, cmapMin, cmapMax);
	}

	// Apply logFactor if set
	if (logFactor > 0.0) {
		index = getLogFactorIndex(index, logFactor);
	}
	return index;
}

/**
 * Returns a color for the index based on the ColorMapping
 */
vec4 getColorByIndex(float index, ColorMapping colorMapping) {
	// Lookup color in colorMap for index
	vec4 textureColor = texture1D(colorMapping.colorMap, index).rgba;

	// Apply alpha mask
	if (colorMapping.applyMask == 1) {
		if (texture1D(colorMapping.alphaMask, index).r != 0.0) {
			textureColor = vec4(textureColor.rgb, 0.0);
		}
	}
	return textureColor;
}

/**
 * Returns a color for the cmapValue based on the ColorMapping
 */
vec4 getColorByValue(float cmapValue, ColorMapping colorMapping) {
	return getColorByIndex(getColorMappingIndex(cmapValue, colorMapping),
			colorMapping);
}
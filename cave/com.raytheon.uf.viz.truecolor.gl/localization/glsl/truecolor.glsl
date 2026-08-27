#include <mapping>


// Multiplier used to store bit mask safely between 0-1
const float maskMultiplier = 8.0;
const int RED_BAND = 0;
const int GREEN_BAND = 1;
const int BLUE_BAND = 2;


uniform sampler2D rawDataTex;
uniform DataTextureInfo rawData;


uniform sampler1D dataMappingDataValues;
uniform sampler1D dataMappingColorValues;
uniform int dataMappingValues;


uniform ColorMapping colorMapping;


uniform sampler2D trueColorTexture;
uniform float height;
uniform float width;


uniform int band;
uniform int expectedMask;


uniform float gamma;


float toBitMask(float alpha) {
    return floor((alpha * maskMultiplier) + 0.5);
}


float fromBitMask(float bitMask) {
    return bitMask / maskMultiplier;
}


bool maskContains(float mask, float bitValue) {
    return mod(floor(mask / bitValue), 2.0) >= 1.0;
}


vec4 getFinalColor() {
	vec4 imageVal = texture2D(trueColorTexture, gl_TexCoord[0].st);
	float r = imageVal.r;
	float g = imageVal.g;
	float b = imageVal.b;
	float a = imageVal.a;


	// Round because of 8-bit floating point precision
	float bitMask = toBitMask(a);
	if (expectedMask > 0 &&
        abs(bitMask - float(expectedMask)) < 0.5) {
		a = 1.0;
	} else {
		a = 0.0;
	}


	return vec4(r, g, b, a);
}


vec4 applyColorBand(int colorband) {
	vec2 xy = gl_FragCoord.xy;
	vec4 curVal = texture2D(trueColorTexture,
			vec2((xy.x / width), (xy.y / height)));


	// Lookup raw data value
	float dataValue = textureToDataValue(rawDataTex, rawData, gl_TexCoord[0].st);


	float r = curVal.r;
	float g = curVal.g;
	float b = curVal.b;
	float a = curVal.a;


	if (dataValue != rawData.noDataValue && dataValue == dataValue) {
		// Convert dataValue to cmapValue
		float cmapValue = dataToColorMapValue(dataValue, dataMappingDataValues, dataMappingColorValues, dataMappingValues);
		float index = pow(getColorMappingIndex(cmapValue, colorMapping), gamma);
		


		float currentMask = toBitMask(a);


		float bitValue = 1.0;
		if (band == GREEN_BAND) {
		    bitValue = 2.0;
		} else if (band == BLUE_BAND) {
		    bitValue = 4.0;
		}
		if (colorband == RED_BAND && index > r) {
			r = index;
		} else if (colorband == GREEN_BAND && index > g) {
			g = index;
		} else if (colorband == BLUE_BAND && index > b) {
			b = index;
		}


		if (!maskContains(currentMask, bitValue)) {
		    // alpha does not contain this bit yet
		    a = fromBitMask(currentMask + bitValue);
		}
	}


	return vec4(r, g, b, a);
}


void main(void) {
	if (band == -1) {
		gl_FragColor = getFinalColor();
	} else {
		gl_FragColor = applyColorBand(band);
	}
}


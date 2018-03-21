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

int toBitMask(float alpha) {
	return int((alpha * maskMultiplier) + 0.5);
}

float fromBitMask(int bitMask) {
	return float(bitMask) / maskMultiplier;
}

vec4 getFinalColor() {
	vec4 imageVal = texture2D(trueColorTexture, gl_TexCoord[0].st);
	float r = imageVal.r;
	float g = imageVal.g;
	float b = imageVal.b;
	float a = imageVal.a;

	// Round because of 8-bit floating point precision
	int bitMask = toBitMask(a);
	if (expectedMask > 0 && bitMask == expectedMask) {
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
		
		if (colorband == RED_BAND && index > r) {
			r = index;
		} else if (colorband == GREEN_BAND && index > g) {
			g = index;
		} else if (colorband == BLUE_BAND && index > b) {
			b = index;
		}
		int bitValue = int(pow(2.0,float(band)));
		int currentMask = toBitMask(a);
		if ((currentMask - (bitValue * int(floor(float(currentMask/bitValue))))) == 0) {
			a = fromBitMask(bitValue);
		}
		// a mod n = a - (n * Fix(a/n))
		// currentMask & bitValue = currentMask - (bitValue * (currentMask/bitValue))
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

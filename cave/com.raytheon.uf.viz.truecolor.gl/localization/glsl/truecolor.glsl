#include <colorUtil>
#include <indexing>

// Multiplier used to store bit mask safely between 0-1
const float maskMultiplier = 8.0;

uniform sampler2D rawTex;
uniform float naturalMin;
uniform float naturalMax;
uniform float cmapMin;
uniform float cmapMax;
uniform int isFloat;

uniform int band;

uniform sampler2D trueColorTexture;
uniform int height;
uniform int width;

uniform float noDataValue;
uniform float alphaStep;
uniform int expectedMask;

int toBitMask(float alpha) {
	return int((alpha * maskMultiplier) + 0.5);
}

float getIndex(sampler2D rawTex, float cmapMin, float cmapMax, float naturalMin, float naturalMax, int isFloat) {
	vec4 textureValue = texture2D(rawTex, gl_TexCoord[0].st);
	float naturalVal = textureValue.r;
	if ( isFloat == 0 ) {
		naturalVal = ((naturalVal * (naturalMax - naturalMin)) + naturalMin);
	}
	
	float index = -1.0;
	if (naturalVal != noDataValue && naturalVal == naturalVal) {
		index = findIndex(naturalVal, cmapMin, cmapMax);
	}
	return index;
}

void main(void)
{
	if ( band == -1 ) {
		vec4 imageVal = texture2D(rawTex,gl_TexCoord[0].st);
		float r = imageVal.r;
		float g = imageVal.g;
		float b = imageVal.b;
		float a = imageVal.a;
		
		// Round because of 8-bit floating point precision
		int bitMask = toBitMask(a);
		if (expectedMask > 0 && bitMask == expectedMask ) {
			a = 1.0;
		} else {
			a = 0.0;
		}
		
		gl_FragColor = vec4(r,g,b,a);
	} else {
		vec2 xy = gl_FragCoord.xy;
		vec4 imageVal = texture2D(rawTex,gl_TexCoord[0].st);
		vec4 curVal = texture2D(trueColorTexture, vec2((xy.x / float(width)), (xy.y / float(height))));
		
		float r = curVal.r;
		float g = curVal.g;
		float b = curVal.b;
		float a = curVal.a;
		
		float index = getIndex(rawTex, cmapMin, cmapMax, naturalMin, naturalMax, isFloat);
		if ( index != -1.0 ) {
			int currentMask = toBitMask(a);
			int bitValue = (1 << band);
			if ( band == 0 && index > r ) {
				r = index;
			} else if ( band == 1 && index > g ) {
				g = index;
			} else if ( band == 2 && index > b ) {
				b = index;
			}
			
			if ( (currentMask & bitValue) == 0 ) {
				// alpha does not contain this bit yet!
				a = (currentMask | bitValue) / maskMultiplier;
			}
		}
		
		gl_FragColor = vec4(r,g,b,a);
	}
}
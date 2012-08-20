#include <colorUtil>
#include <indexing>

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
uniform int checkAlpha;

float getIndex(sampler2D rawTex, float cmapMin, float cmapMax, float naturalMin, float naturalMax, int isFloat) {
	vec4 textureValue = texture2D(rawTex, gl_TexCoord[0].st);
	float naturalVal = textureValue.r;
	if ( isFloat == 0 ) {
		naturalVal = ((naturalVal * (naturalMax - naturalMin)) + naturalMin);
	}
	
	float index = 0.0;
	if (naturalVal != noDataValue && naturalVal == naturalVal) {
		index = findIndex(naturalVal, cmapMin, cmapMax);
	}
	return index;
}

void main(void)
{
	vec2 xy = gl_FragCoord.xy;
	vec4 imageVal = texture2D(rawTex,gl_TexCoord[0].st);
	vec4 curVal = texture2D(trueColorTexture, vec2((xy.x / float(width)), (xy.y / float(height))));
	
	float r = curVal.r;
	float g = curVal.g;
	float b = curVal.b;
	float a = curVal.a;
	
	float index = getIndex(rawTex, cmapMin, cmapMax, naturalMin, naturalMax, isFloat);
	if ( band == 0 && index > r ) {
		r = index;
	} else if ( band == 1 && index > g ) {
		g = index;
	} else if ( band == 2 && index > b ) {
		b = index;
	}
	a = a + alphaStep;
	
	if (checkAlpha == 1 && a < 1.0) {
		a = 0.0;
	}
	
	gl_FragColor = vec4(r,g,b,a);
}
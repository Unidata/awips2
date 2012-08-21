#include <colorUtil>
#include <indexing>

uniform float alpha;
uniform float brightness;
uniform float contrast;

uniform sampler2D rawTexR;
uniform float naturalMinR;
uniform float naturalMaxR;
uniform float cmapMinR;
uniform float cmapMaxR;
uniform int isFloatR;
uniform int invertR;
uniform int applyR;

uniform sampler2D rawTexG;
uniform float naturalMinG;
uniform float naturalMaxG;
uniform float cmapMinG;
uniform float cmapMaxG;
uniform int isFloatG;
uniform int invertG;
uniform int applyG;

uniform sampler2D rawTexB;
uniform float naturalMinB;
uniform float naturalMaxB;
uniform float cmapMinB;
uniform float cmapMaxB;
uniform int isFloatB;
uniform int invertB;
uniform int applyB;


float getIndex(sampler2D rawTex, float cmapMin, float cmapMax, float naturalMin, float naturalMax, int isFloat) {
	vec4 textureValue = texture2D(rawTex, gl_TexCoord[0].st);
	float naturalVal = textureValue.r;
	if ( isFloat == 0 ) {
		naturalVal = ((naturalVal * (naturalMax - naturalMin)) + naturalMin);
	}
	return findIndex(naturalVal, cmapMin, cmapMax);
}

void main(void) {
	float r = 0.0;
	float g = 0.0;
	float b = 0.0;
	
	if ( applyR != 0 ) {
		r = getIndex(rawTexR, cmapMinR, cmapMaxR, naturalMinR, naturalMaxR, isFloatR);
		if ( invertR != 0 ) {
			r = 1.0 - r;
		}
	}
	if ( applyG != 0 ) {
		g = getIndex(rawTexG, cmapMinG, cmapMaxG, naturalMinG, naturalMaxG, isFloatG);
		if ( invertG != 0 ) {
			g = 1.0 - g;
		}
	}
	if ( applyB != 0 ) {
		b = getIndex(rawTexB, cmapMinB, cmapMaxB, naturalMinB, naturalMaxB, isFloatB);
		if ( invertB != 0 ) {
			b = 1.0 - b;
		}
	}
	
	gl_FragColor = applyContrastAlphaBrightness(vec4(r,g,b,alpha), alpha, brightness, contrast);
}

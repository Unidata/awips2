#include <colorUtil>
#include <indexing>

uniform float alpha;
uniform float brightness;
uniform float contrast;
uniform int applyMask;
uniform float naturalMin;
uniform float naturalMax;
uniform float cmapMin;
uniform float cmapMax;
uniform sampler1D colorMap;
uniform sampler2D alphaMask;
uniform sampler2D rawTex;
uniform float colorMapSz;
uniform int isFloat;
uniform int logarithmic;
uniform int mirror;
uniform float logFactor;

void main(void) {
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
	float index = 0.0;
	float rawValue = textureColor.r;
	if ( isFloat == 1 ) {
		if ( logarithmic == 1 ) {
			index = findFloatIndexLog(rawValue, cmapMin, cmapMax, mirror);
		} else {
			index = findFloatIndex(rawValue, cmapMin, cmapMax);
		}
		
		// Special float handling, -1.0 is NaN
		if (index == -1.0){
			gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        	return;
    	}
	} else {
		float naturalValue = ((rawValue * (naturalMax - naturalMin)) + naturalMin);
		index = findIndex(naturalValue, cmapMin, cmapMax);
	}

	// Lookup color in colorMap for index
	if ( logFactor > 0.0 ) {
		index = getLogFactorIndex(index, logFactor);
	}
	textureColor = texture1D(colorMap, index).rgba;
    
    // Apply alpha mask
	if ( applyMask == 1 ) {
    	if ( texture2D(alphaMask , vec2(index,index) ).r != 0.0 ) {
        	textureColor = vec4(textureColor.rgb, 0.0);
    	}
	}
	
	gl_FragColor = applyContrastAlphaBrightness(textureColor, alpha, brightness, contrast);
}
// Simple shader program for applying alpha,brightness, and contrast to the 
// colormap in the same way they are applied to data

#include <colorUtil>
#include <indexing>

uniform float brightness;
uniform float contrast;
uniform float alphaVal;

uniform float bkgrndRed;
uniform float bkgrndGreen;
uniform float bkgrndBlue;

uniform sampler1D colorMap;
uniform sampler2D alphaMask;

uniform int applyMask;
uniform float logFactor;

void main(void){
	// Lookup color in colorMap for index
	float index = gl_TexCoord[0].s;
	if ( logFactor > 0.0 ) {
		index = getLogFactorIndex(index, logFactor);
	}
	vec4 color = texture1D(colorMap, index).rgba;
	
	// Apply alpha mask if set 
    float alpha = color.a;
    if ( applyMask == 1 ) {
        if ( texture2D(alphaMask , vec2(index,index) ).r != 0.0 ) {
            color = vec4(bkgrndRed, bkgrndGreen, bkgrndBlue, alpha);
        }
    }
    if(alpha < 1.0){
		// blend the color with background color, the colorbar should not be transparent
		alpha = 1.0;
		color = vec4(color.r*color.a + bkgrndRed*(1.0 - color.a),
		             color.g*color.a + bkgrndGreen*(1.0 - color.a), 
		             color.b*color.a + bkgrndBlue*(1.0 - color.a), 
		             alpha);
	}

	gl_FragColor = applyContrastAlphaBrightness(color, alphaVal, brightness, contrast);
}
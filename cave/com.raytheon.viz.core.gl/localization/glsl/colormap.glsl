// Simple shader program for applying alpha,brightness, and contrast to the 
// colormap in the same way they are applied to data
#include <mapping>
#include <coloring>

uniform ColorMapping colorMapping;
uniform ColorModifiers modifiers;

uniform float bkgrndRed;
uniform float bkgrndGreen;
uniform float bkgrndBlue;

void main(void){
	float logFactor = colorMapping.logFactor;
	int applyMask = colorMapping.applyMask;
	
	// Lookup color in colorMap for index
	float index = gl_TexCoord[0].s;
	if ( logFactor > 0.0 ) {
		index = getLogFactorIndex(index, logFactor);
	}
	vec4 color = texture1D(colorMapping.colorMap, index).rgba;
	
	// Apply alpha mask if set 
    float alpha = color.a;
    if ( applyMask == 1 ) {
        if ( texture1D(colorMapping.alphaMask , index ).r != 0.0 ) {
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

	gl_FragColor = applyColorModifiers(color, modifiers);
}
// Simple shader program for applying alpha,brightness, and contrast to the 
// colormap in the same way they are applied to data

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

vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

// Given an index(0-1) find the color in the colormap
vec4 findColor(float index){
	if (logFactor > 0.0){
	    float minLog = log(logFactor);
        float maxLog = log(logFactor + 1.0);

        float lg = log(logFactor + index);

        index = (lg - minLog) / (maxLog - minLog);
        if (index < 0.0){
            index = 0.0;
        }
        else if (index > 1.0){
            index = 1.0;
        }
	}
    vec4 color = texture1D(colorMap, index).rgba;
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
    return vec4(color.rgb, alpha);
}

// Apply the preferences for contrast, alpha, and brightness
vec4 applyContrastAlphaBrightness(vec4 color){
    vec3 textureColor3 = vec3(color);
    vec3 adjustedColor   = mix(AvgLuminance, textureColor3, contrast);
    float curAlpha = min(color.a, alphaVal);
    return vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);
}

void main(void){
	vec4 textureColor = findColor(gl_TexCoord[0].s);
	gl_FragColor = applyContrastAlphaBrightness(textureColor);
}
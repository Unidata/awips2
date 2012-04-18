// NOTE: 65504.0 is the maximum half precision float value
// it is used to do masking of GFE grids since NaNs are not supported
// in NVIDIA cards prior to the 8000 series
// The check for textureColor.r == 65504.0 can be replaced by
// isnan(textureColor.r) if support for cards prior to the 8000 series
// is not required.

// NOTE: there is a corresponding conversion from NaN to 65504.0 in
// FloatDataPreparer that will also need to be changed

uniform float brightness;
uniform float contrast;
uniform int doColorMap;
uniform int applyMask;
uniform float naturalMin;
uniform float naturalMax;
uniform float cmapMin;
uniform float cmapMax;
uniform sampler1D colorMap;
uniform sampler2D rawTex;
uniform sampler2D alphaMask;
uniform float alphaVal;
uniform float colorMapSz;
uniform int isFloat;
uniform int logarithmic;
uniform int mirror;
uniform float logFactor;
uniform int doSingleColor;
uniform vec3 singleColor;

vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

// Given a raw data value determine the index(0-1) into the colormap using log scaling
float findIndexLog(float rawValue){
	float index = 0.0;
    // is this strictly negative, strictly positive or neg to pos scaling?
    if ( cmapMin >= 0.0 && cmapMax >= 0.0 && mirror!=1) {
        if(rawValue < cmapMin){
            index = 0.0;
        }else{
            // simple calculation
            index = ((log(rawValue) - log(cmapMin)) / abs(log(cmapMax)-log(cmapMin)));
        }
    } else if (cmapMin <= 0.0 && cmapMax <= 0.0 && mirror!=1) {
        index = ((log(rawValue) - log(cmapMax)) / abs(log(cmapMin)-log(cmapMax)));
    } else {
        // special case, neg to pos:
        float colorMapMin = cmapMin;
        float colorMapMax = cmapMax;
        float zeroVal = max(colorMapMax, abs(colorMapMin)) * 0.0001;
        if (mirror==1 && (colorMapMin > 0.0 || colorMapMax < 0.0)) {
            if (colorMapMax < 0.0) {
                colorMapMax = -cmapMax;
                rawValue = -rawValue;
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
        float absTextureColor = abs(rawValue);
        if (absTextureColor <= zeroVal) {
            index = zeroIndex;
        } else if (rawValue > 0.0) {
            // positive texture color value, find index from 0 to
            // cmapMax:
            float logTexColor = absLogZeroVal + log(rawValue);

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
    return index;
}

// Given a raw data value determine the index(0-1) into the colormap
float findIndex(float rawValue)
{
    float index = 0.0;
    if (isFloat == 1) {
        if (rawValue == 65504.0) {
            return -1.0;
        }
        if (logarithmic == 1){
             index = findIndexLog(rawValue);
        }else{
            index = ((rawValue - cmapMin) / abs(cmapMax-cmapMin));
        }
    } else {
        float naturalValue = ((rawValue * (naturalMax - naturalMin)) + naturalMin);
        index = ((naturalValue - cmapMin) / abs(cmapMax-cmapMin));
    }
    if(index < 0.0) {
        index = 0.0; 
    } else if(index > 1.0) {
        index = 1.0;
    }
    return index;
}

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
            alpha = 0.0;
        }
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

void main(void)
{
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
    float alpha = textureColor.a;
    if(doColorMap == 1) { 
       float index = findIndex(textureColor.r);
       if (index == -1.0){
           gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
           return;
       }
       textureColor = findColor(index);
    }
    
    if ( doSingleColor == 1 ) {
    	textureColor.rgb = singleColor;
    }
    gl_FragColor = applyContrastAlphaBrightness(textureColor);
}

vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

/**
 * This function applies the specified alpha, brightness, and contrast values
 * to the color passed in
 */
vec4 applyContrastAlphaBrightness(vec4 color, float alpha, float brightness, float contrast){
    vec3 textureColor3 = vec3(color);
    vec3 adjustedColor = mix(AvgLuminance, textureColor3, contrast);
    float curAlpha = min(color.a, alpha);
    return vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);
}

/**
 * This function calculates a new index to use based on the logFactor
 */
float getLogFactorIndex(float index, float logFactor) {
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
	return index;
}
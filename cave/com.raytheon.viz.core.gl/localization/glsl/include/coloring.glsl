
struct ColorModifiers {
	float alpha;
	float brightness;
	float contrast;
};

vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

/**
 * This function applies the specified ColorModifier values to the 
 * color passed in
 */
vec4 applyColorModifiers(vec4 color, ColorModifiers modifiers){
	float alpha = modifiers.alpha;
	float brightness = modifiers.brightness;
	float contrast = modifiers.contrast;
	
    vec3 textureColor3 = vec3(color);
    vec3 adjustedColor = mix(AvgLuminance, textureColor3, contrast);
    float curAlpha = min(color.a, alpha);
    return vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);
}

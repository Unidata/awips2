uniform sampler2D tex;
uniform float brightness;
uniform float contrast;
uniform int doColorMap;
uniform float naturalMin;
uniform float naturalMax;
uniform float cmapMin;
uniform float cmapMax;
uniform sampler1D colorMap;
uniform sampler2D rawTex;
uniform float alphaVal;
uniform float colorMapSz;
vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

void main(void)
{
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
    float alpha = textureColor.a;
    if(doColorMap == 1) { 
            // + " float naturalValue = ((textureColor.r * (20000)) - 10000);
       float naturalValue = ((textureColor.r * (naturalMax - naturalMin)) + naturalMin);
       float index = ((naturalValue - cmapMin) / abs(cmapMax-cmapMin));
       if(index < 0.0) {
          index = 0.0; 
       } else if(index > 1.0) {
          index = 1.0;
       }
       vec4 color = texture1D(colorMap, index).rgba;
       alpha = color.a;
       textureColor = vec4(color.rgb, 1.0);
    }
    vec3 textureColor3 = vec3(textureColor);
    vec3 adjustedColor   = mix(AvgLuminance, textureColor3, contrast);
    float curAlpha = min(alpha, alphaVal);
    gl_FragColor = vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);

}

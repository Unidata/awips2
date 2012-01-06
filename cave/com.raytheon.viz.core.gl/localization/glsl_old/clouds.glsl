uniform float brightness;
uniform float contrast;
uniform sampler1D colorMap;
uniform int textureType;
uniform sampler2D cloudTexture2D;
uniform sampler3D cloudTexture3D;
uniform float glaze;
uniform float rime;
uniform float alphaVal;
uniform int clwSize;
uniform float clwRange[8];
uniform float naturalValues[2];
uniform float cmap[];

vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

float getValue(sampler2D s2D, sampler3D s3D, float max, float min) {
    
    float value;
    if(textureType == 3) {
        value = texture3D(s3D, vec3(gl_TexCoord[0])).r;
    }else {
        value = texture2D(s2D, vec2(gl_TexCoord[0])).r;
    }
    
    // scale the value
    value = (value *(max-min)) + min;
    return value;

}

void main(void)
{
    gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0 );
   
    float temp = getValue(cloudTexture2D, cloudTexture3D, naturalValues[1], naturalValues[0] );  

    float index = ((temp - cmap[0]) / (cmap[1]-cmap[0]));
    if(index < 0.0) {
       index = 0.0;
    } else if(index > 1.0) {
       index = 1.0;
    }
    
    vec4 color = texture1D(colorMap, index).rgba;

    float alpha = color.a;
    vec4 textureColor = vec4(color.rgb, 1.0);
    
    vec3 adjustedColor = mix(AvgLuminance, color.rgb, contrast);
    float curAlpha = min(alpha, alphaVal);

////    if(curAlpha > 0.0) {
        gl_FragColor = vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);
 //   } else {
 //       discard;
 //   }
        
}

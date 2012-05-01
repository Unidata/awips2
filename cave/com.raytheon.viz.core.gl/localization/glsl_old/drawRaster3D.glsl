uniform sampler3D tex;
uniform float brightness;
uniform float contrast;
uniform int doColorMap;
uniform float naturalMin;
uniform float naturalMax;
uniform float cmapMin;
uniform float cmapMax;
uniform sampler1D colorMap;
uniform sampler3D rawTex;
uniform float alphaVal;
uniform float colorMapSz;
uniform sampler2D grids[8];
uniform int enabledImpacts[8];
uniform int numberEnabled;

//// actuall pressure levels
//uniform float pressures[20];
//uniform int layers;
//
//
//uniform float texturePres[30];

vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

//float interp1(float y1, float y3, float x1, float x2, float x3) {
//    if(x3 == x1) {
//        x1 += 0.01;
//    }
//    
//    return y1+((y3-y1) *((x2-x1)/(x3-x1)));
//    
//}
//
//// based on metolib pvalue.f 
//float  pvalue() {
//    
//    float z =  vec3(gl_TexCoord[0].xyz).z;
//    int j = int(z *float(layers-1));
//    vec3 lower =  gl_TexCoord[0].xyz;
//    vec3 upper =  gl_TexCoord[0].xyz;
//      
//
//    float lowerVal = texture3D(rawTex, lower).r;
//    float uperVal  = texture3D(rawTex, upper).r;
//
//    for(int i = 0; i < layers; ++i) {
//        
//        if(pressures[i] < texturePres[j]) {
//
//            // assuming texture coordinates are 0-1
//            lower.z += 1/(layers-1);
//            upper.z -= 1/(layers-1);
//            
//            float lowerVal = texture3D(rawTex, lower).r;
//            float upperVal  = texture3D(rawTex, upper).r;
//            
//            float p1 = log(pressures[i-1]);
//            float p2 = log(texturePres[j]);
//            float p3 = log(pressures[i]);
//    
//            return interp1(lowerVal, upperVal, p1, p2, p3 );
//        }
//    }
//    
//}


void main(void)
{
    float textureValue = texture3D(rawTex, gl_TexCoord[0].xyz).r;

    float naturalValue = ((textureValue * (naturalMax - naturalMin)) + naturalMin);
    float index = ((naturalValue - cmapMin) / (cmapMax-cmapMin));
    if(index < 0.0) {
       index = 0.0;
    } else if(index > 1.0) {
       index = 1.0;
    }
    vec4 color = texture1D(colorMap, index).rgba;
    float alpha = color.a;
    vec4 textureColor = vec4(color.rgb, 1.0);
       

    vec3 textureColor3 = vec3(textureColor);
    vec3 adjustedColor = mix(AvgLuminance, textureColor3, contrast);
//
    float curAlpha = min(alpha, alphaVal);
    
    if(curAlpha > 0.0){
        gl_FragColor = vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);
    }
    else {
        discard;
    }
   

}

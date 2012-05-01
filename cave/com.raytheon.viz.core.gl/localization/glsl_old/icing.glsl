uniform float brightness;
uniform float contrast;
uniform sampler1D colorMap;
uniform sampler3D tempTex3D;
uniform sampler3D clwTex3D;
uniform sampler2D tempTex2D;
uniform sampler2D clwTex2D;
uniform sampler2D tempTex;
uniform sampler2D clwTex;
uniform float glaze;
uniform float rime;
uniform float alphaVal;
uniform int clwSize;
uniform float clwRange[4];
uniform float naturalMin[2];
uniform float naturalMax[2];
uniform int textureType;
vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

// shader based on icing algorithm found at
// http://aoaws.caa.gov.tw/htdocs/projects/aoaws/model/icing/algorithm
//


// get the scaled value from either the 2D or 3D texture
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



float getIcing()
{

    float value = 0.0;
    
    float temp = getValue(tempTex2D, tempTex3D, naturalMax[0], naturalMin[0]);
    
    // clw - cloud liquid water
    float clw = getValue(clwTex2D, clwTex3D, naturalMax[1], naturalMin[1]);
        
    if(temp > glaze  || clw < clwRange[0]) {
        // no icing 
        value = 0.0;           
    }     
    else {
            
        if(temp < rime){
           value = 1.0;
        }
        else {
            value = 2.0;
        }
         
        float x = 0.0;        
        for(int i = 0; i < 4; i++) {
            if(clw > clwRange[i] ){
                x += 1.0 ;
               
            } 
        }
        value += x;
    }

    return value;
}


void main(void)
{
    gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0 );
    
// vec4 textureColor = texture2D(tempTex, gl_TexCoord[0].st);
// float alpha = textureColor.a;
   
    float index1 = getIcing();
       
//    if(index1 == 900.0 ) {
//        gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0 );
//
//    }
//    else if(index1 == 10000.0) {
//        gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0 );
//
//    }     else if(index1 == 999.0) {
//        gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0 );
//
//    }
//    else {
    
        float index = (index1 / 5.0);
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
    float curAlpha = min(alpha, alphaVal);
    
    if(alpha > 0.0) {
      gl_FragColor = vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, alpha);
    }
    else {
        discard;
    }

}

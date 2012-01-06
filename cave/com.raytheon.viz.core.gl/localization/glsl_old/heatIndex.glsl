uniform sampler2D temp2D;
uniform sampler3D temp3D;
uniform sampler2D rh2D;
uniform sampler3D rh3D;
uniform int textureType;
uniform float naturalMin[2];
uniform float naturalMax[2];
uniform float severeThreshold;
uniform float moderateThreshold;

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
     
    
    float kelvin = getValue(temp2D, temp3D,  naturalMax[0], naturalMin[0]);
    
    float t = (((kelvin - 273.15) * 1.8) + 32.0);
    
    float rh = getValue(rh2D, rh3D, naturalMax[1], naturalMin[1]);

    
    float hi = t;
    if(t >= 57.0) {
	   hi = -42.379 + (2.04901523*t) + (10.14333127*rh) - (0.22475541*t*rh) - (0.00683783*t*t) - (0.05481717*rh*rh) + (0.00122874*t*t*rh)+ (0.00085282*t*rh*rh) - (0.00000199*((t*rh)*(t*rh)));
    }

    gl_FragColor =vec4(1.0, 0.0, 1.0, 1.0);
    // convert threshold values to fahenheit
    if(hi > (severeThreshold *9.0/5.0 -459.67)) {
        gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);

    } else if(hi > (moderateThreshold *9.0/5.0 -459.67)) {
        gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0);
    }
    else {
        discard;
//        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);

    }
    
}

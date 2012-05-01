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
uniform int doImpact;
uniform int upperThreshold;
uniform int numberGrids;
uniform float severeThreshold[8];
uniform float moderateThreshold[8];
uniform float naturalMins[8];
uniform float naturalMaxes[8];
uniform sampler2D grids[8];
uniform int enabledImpacts[8];
uniform int numberEnabled;
vec3 AvgLuminance = vec3(0.5, 0.5, 0.5);

float impactAnd()
{
	float impactVal = 3.0;
	for(int x=0; x < 8; x++)
	{
		if(x >= numberGrids)
		{
			break;
		}
		if(enabledImpacts[x] == 1)
		{				
			vec4 textureColor = texture2D(grids[x], gl_TexCoord[0].st);
			float naturalValue = ((textureColor.r * (naturalMaxes[x] - naturalMins[x])) + naturalMins[x]);
			if(upperThreshold == 1)
			{
				if(naturalValue < moderateThreshold[x])
				{
					impactVal = 0.0;
				}
				else if(naturalValue < severeThreshold[x])
				{
					if(impactVal == 3.0)
					{
						impactVal = 2.0;
					}
				}			
			}
			else
			{
				if(naturalValue > moderateThreshold[x])
				{
					impactVal = 0.0;
				}
				else if(naturalValue > severeThreshold[x])
				{
					if(impactVal == 3.0)
					{
						impactVal = 2.0;
					}
				}
			}			
		}
	}
	
	return impactVal;
}

float impactOr()
{
	float impactVal = 0.0;
	for(int x=0; x < 8; x++)
	{
		if(x >= numberGrids)
		{
			break;
		}
		if(enabledImpacts[x] == 1)
		{				
			vec4 textureColor = texture2D(grids[x], gl_TexCoord[0].st);
			float naturalValue = ((textureColor.r * (naturalMaxes[x] - naturalMins[x])) + naturalMins[x]);
			if(upperThreshold == 1)
			{
				if(naturalValue > severeThreshold[x])
				{
					impactVal = 3.0;
				}
				else if(naturalValue > moderateThreshold[x])
				{
						impactVal = 2.0;
				}			
			}
			else
			{
				if(naturalValue < severeThreshold[x])
				{
					impactVal = 3.0;
				}
				else if(naturalValue < moderateThreshold[x])
				{
					impactVal = 2.0;
				}
			}			
		}
	}
	
	return impactVal;
}


void main(void)
{
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
    float alpha = textureColor.a;
    if(doImpact == 1)
    {
        float impactVal = 0.0;
	if(numberEnabled > 0)
	{
		impactVal = impactAnd();
	}
	else
	{
		impactVal = 0.0;
	}
              
       float index = (impactVal / 3.0);
       if(index < 0.0) {
          index = 0.0;
       } else if(index > 1.0) {
          index = 1.0;
       }
       vec4 color = texture1D(colorMap, index).rgba;
       alpha = color.a;
       textureColor = vec4(color.rgb, 1.0);
    }
    else if(doColorMap == 1) {
       float naturalValue = ((textureColor.r * (naturalMax - naturalMin)) + naturalMin);
       float index = ((naturalValue - cmapMin) / (cmapMax-cmapMin));
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
    vec3 adjustedColor = mix(AvgLuminance, textureColor3, contrast);
    float curAlpha = min(alpha, alphaVal);
    gl_FragColor = vec4(adjustedColor.r * brightness, adjustedColor.g * brightness, adjustedColor.b * brightness, curAlpha);

}

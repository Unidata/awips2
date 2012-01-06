// this shader program sets values into a mosaic texture
// which is the same size as the screen (frame buffer)

uniform sampler2D radarData;

void main(void)
{
	vec4 radarVal = texture2D(radarData,gl_TexCoord[0].st);
	gl_FragColor = vec4(radarVal.r,0.0,0.0,1.0);
}
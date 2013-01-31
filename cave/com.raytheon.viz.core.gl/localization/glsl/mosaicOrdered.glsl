// this shader program sets values into a mosaic texture
// which is the same size as the screen (frame buffer)

uniform sampler2D imageData;
uniform sampler2D mosaicTexture;
uniform int height;
uniform int width;

void main(void)
{
	vec2 xy = gl_FragCoord.xy;
	vec4 imageVal = texture2D(imageData,gl_TexCoord[0].st);
	vec4 curVal = texture2D(mosaicTexture, vec2((xy.x / float(width)), (xy.y / float(height))));
	// assume 0 or NaN is No Data and should be replaced if another image has better values.
	if ( imageVal.r != 0.0 && imageVal.r == imageVal.r) {
		gl_FragColor = vec4(imageVal.r,0.0,0.0,1.0);
	} else {
		gl_FragColor = vec4(curVal.r,0.0,0.0,1.0);
	}
}
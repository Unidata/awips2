// this shader program sets values into a mosaic texture
// which is the same size as the screen (frame buffer)

#include <mapping>

uniform DataTexture imageData;
uniform DataMapping imageToMosaic;
uniform DataTexture mosaicData;

void main(void) {
	float imageValue = textureToDataValue(imageData, gl_TexCoord[0].st);
	vec2 frag_xy = gl_FragCoord.xy;
	float mosaicValue = textureToDataValue(mosaicData,
			vec2(frag_xy.x / mosaicData.width, frag_xy.y / mosaicData.height));

	float newValue = mosaicValue;
	// No data check/special NaN check
	if (imageValue != imageData.noDataValue && imageValue == imageValue) {
		// Convert image value to mosaic value
		imageValue = dataToColorMapValue(imageValue, imageToMosaic);
		if (imageValue > mosaicValue) {
			newValue = imageValue;
		}
	}
	gl_FragColor = vec4(dataToTextureValue(mosaicData, newValue), 0.0, 0.0,
			1.0);
}
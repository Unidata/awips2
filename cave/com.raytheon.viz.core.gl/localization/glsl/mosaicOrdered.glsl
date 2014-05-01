// this shader program sets values into a mosaic texture
// which is the same size as the screen (frame buffer)

#include <mapping>

uniform sampler2D imageDataTex;
uniform DataTextureInfo imageData;

uniform sampler1D imageToMosaicDataValues;
uniform sampler1D imageToMosaicColorValues;
uniform int imageToMosaicValues;

uniform sampler2D mosaicDataTex;
uniform DataTextureInfo mosaicData;

void main(void) {
	float imageValue = textureToDataValue(imageDataTex, imageData, gl_TexCoord[0].st);
	vec2 frag_xy = gl_FragCoord.xy;
	float mosaicValue = textureToDataValue(mosaicDataTex, mosaicData,
			vec2(frag_xy.x / mosaicData.width, frag_xy.y / mosaicData.height));

	float newValue;
	// No data check/special NaN check
	if (imageValue == imageData.noDataValue || imageValue != imageValue) {
		// Use existing value
		newValue = mosaicValue;
	} else {
		newValue = dataToColorMapValue(imageValue, imageToMosaicDataValues, imageToMosaicColorValues, imageToMosaicValues);
	}
	gl_FragColor = vec4(dataToTextureValue(mosaicData, newValue), 0.0, 0.0,
			1.0);
}
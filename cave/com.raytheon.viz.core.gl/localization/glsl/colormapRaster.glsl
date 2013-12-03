#include <mapping>
#include <coloring>

uniform DataTexture rawData;
uniform DataMapping dataMapping;
uniform ColorMapping colorMapping;
uniform ColorModifiers modifiers;

void main(void) {
	float dataValue = textureToDataValue(rawData, gl_TexCoord[0].st);
	
	// No data check/special NaN check
	if (dataValue == rawData.noDataValue || dataValue != dataValue) {
		gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
		return;
	}
	
	// Convert dataValue to cmapValue
	float cmapValue = dataToColorMapValue(dataValue, dataMapping);
	// Get color for colormapping, given value
	vec4 textureColor = getColorByValue(cmapValue, colorMapping);
	// Apply the color modifiers into gl_FragColor
	gl_FragColor = applyColorModifiers(textureColor, modifiers);
}
#include <mapping>
#include <coloring>

uniform sampler1D dataMappingDataValues;
uniform sampler1D dataMappingColorValues;
uniform int dataMappingValues;

uniform sampler1D colorMappingColorMap;
uniform sampler1D colorMappingAlphaMask;
uniform ColorMapping colorMapping;

uniform ColorModifiers modifiers;

varying float vary_value;

void main(void) {
    float dataValue = vary_value;
    
    // No data check/special NaN check
    if (dataValue != dataValue) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
        return;
    }
    
    // Convert dataValue to cmapValue
    float cmapValue = dataToColorMapValue(dataValue, dataMappingDataValues, dataMappingColorValues, dataMappingValues);
    // Get color for colormapping, given value
    vec4 textureColor = getColorByValue(cmapValue, colorMapping, colorMappingColorMap, colorMappingAlphaMask);
    // Apply the color modifiers into gl_FragColor
    gl_FragColor = applyColorModifiers(textureColor, modifiers);
}
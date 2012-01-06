// this shader program sets values into a mosaic texture
// which is the same size as the screen (frame buffer)
// Use depth buffer texture

uniform sampler2D radarData;
uniform sampler2D mosaicTexture;
uniform sampler2D depthTexture;
uniform int height;
uniform int width;

void main(void)
{
    // TODO: Mimic mosaicMax except use depthTexture to look up current
    // depth buffer info (which will actually store the current distance 
    
}
#include <colorUtil>

uniform float brightness;
uniform float contrast;
uniform float alpha;
uniform sampler2D rawTex;

void main(void) {
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
    gl_FragColor = applyContrastAlphaBrightness(textureColor, alpha, brightness, contrast);
}
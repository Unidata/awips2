#include <colorUtil>

uniform float brightness;
uniform float contrast;
uniform float alpha;
uniform sampler2D rawTex;
uniform vec3 color;

void main(void) {
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
    textureColor.rgb = color;
    gl_FragColor = applyContrastAlphaBrightness(textureColor, alpha, brightness, contrast);
}
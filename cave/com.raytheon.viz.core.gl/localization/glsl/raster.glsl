#include <coloring>

uniform sampler2D rawTex;
uniform ColorModifiers modifiers;

void main(void) {
    vec4 textureColor = texture2D(rawTex, gl_TexCoord[0].st);
    gl_FragColor = applyColorModifiers(textureColor, modifiers);
}
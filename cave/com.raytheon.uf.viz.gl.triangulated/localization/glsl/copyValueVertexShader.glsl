// Vertex shader copies an attribute value to a varying variable so that it can
// be used in the fragment shader 

attribute float attrib_value;
varying float vary_value;

void main(void) {
    vary_value = attrib_value;
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
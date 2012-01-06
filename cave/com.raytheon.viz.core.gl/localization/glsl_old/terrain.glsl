uniform sampler2D terrainMap;
uniform float exaggeration;
uniform int layer;

void main(void)
{
    vec4 newVertexPos;
    float df;
    
   gl_TexCoord[0].xy = gl_MultiTexCoord0.xy;
   vec3 normal = normalize(gl_Normal);
    df = texture2D( terrainMap, gl_TexCoord[0].xy ).r;
    if(layer > 0){
        exaggeration *= layer;
    }
    newVertexPos = vec4(normal *df* exaggeration , 0.0) + gl_Vertex;
    
    gl_Position = gl_ModelViewProjectionMatrix * newVertexPos;
}
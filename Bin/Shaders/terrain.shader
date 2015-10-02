#VERTEX

uniform int I_DETAIL_UV;
uniform int I_CAUSTIC_UV;
#INCLUDE Inc\lighting_uniforms.inc
#INCLUDE Inc\fog_uniforms.inc

varying vec2  ColorUV;
varying vec2  DetailUV;
varying vec2  CausticUV;
#INCLUDE Inc\varying.inc

void main(void)
{
    //UV
	ColorUV    = gl_MultiTexCoord0.xy;
	DetailUV   = ColorUV * I_DETAIL_UV;
	CausticUV  = ColorUV * I_CAUSTIC_UV;

    //Lighting
	vec3 N = normalize(gl_Normal);
    #INCLUDE Inc\lighting.inc
    
    //Vertex
    vec4 Eye      = gl_Vertex;
    #INCLUDE Inc\vertex.inc

    //Shadows
    ShadowCoord = gl_TextureMatrix[7] * Eye;     

    //Fog
    #INCLUDE Inc\fog.inc
}



#FRAGMENT

uniform sampler2D T_COLORTEX;
uniform sampler2D T_DETAILTEX1;
uniform sampler2D T_DETAILTEX2;
uniform sampler2D T_DETAILTEX3;
uniform sampler2D T_WEIGHT_LOOKUP;
uniform sampler2D T_CAUSTICMAP;
uniform sampler2D T_DETAILMAP;
uniform sampler2D T_SHADOWMAP;

#INCLUDE Inc\lighting_uniforms.inc
#INCLUDE Inc\fog_uniforms.inc
#INCLUDE Inc\detail_uniforms.inc
#INCLUDE Inc\water_uniforms.inc

varying vec2  ColorUV;
varying vec2  DetailUV;
varying vec2  CausticUV;
#INCLUDE Inc\varying.inc

void main(void)
{ 
	vec4 Color   = texture2D(T_COLORTEX,      ColorUV);
	vec4 Detail1 = texture2D(T_DETAILTEX1,    DetailUV);
	vec4 Detail2 = texture2D(T_DETAILTEX2,    DetailUV);
	vec4 Detail3 = texture2D(T_DETAILTEX3,    DetailUV);
	vec4 Weights = texture2D(T_WEIGHT_LOOKUP, ColorUV);
	vec4 Caustic = texture2D(T_CAUSTICMAP,    CausticUV);
	Color        += (Detail1*Weights.x + Detail2*Weights.y + Detail3*Weights.z) - 0.5; 
    
	vec2 DUV = DetailUV * 6;
    if (I_DETAIL==1)
    #INCLUDE Inc\detail.inc
    Color = Color * Light;
    
    gl_FragData[1] = vec4(1.0);
	#INCLUDE Inc\shadows.inc
     
    #INCLUDE Inc\water_logic.inc
}


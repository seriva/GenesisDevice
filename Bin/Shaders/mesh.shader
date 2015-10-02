#VERTEX

uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform int I_FLIP_NORMAL;
uniform int I_DO_TREE_ANIM;
uniform float F_ANIMATION_SPEED;
uniform float F_ANIMATION_STRENGTH;
uniform mat4 M_ROTATION;
uniform vec3 V_POSITION;
uniform vec3 V_SCALE;
#INCLUDE Inc\lighting_uniforms.inc

varying vec2  ColorUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

#INCLUDE Inc\transform.inc

void main(void)
{
    //UV
    ColorUV = gl_MultiTexCoord0.xy;

    //Lighting
    vec3 N = normalize(transformVector(gl_Normal, M_ROTATION));
	if(I_FLIP_NORMAL == 1)	
	{
        N = -N;
	}
    #INCLUDE Inc\lighting.inc
    
    //Vertex
    vec4 Eye = gl_Vertex;
    Eye.xyz = (transformVector(Eye.xyz * V_SCALE, M_ROTATION)) + V_POSITION;
    if(I_DO_TREE_ANIM == 1){
        #INCLUDE Inc\foliage_animation.inc
    }
    #INCLUDE Inc\vertex.inc
    
    //Shadows
    ShadowCoord = gl_TextureMatrix[7] * Eye;     

    //Fog 
    #INCLUDE Inc\fog.inc
}




#FRAGMENT

uniform sampler2D T_COLORMAP;
uniform sampler2D T_CAUSTICMAP;
uniform sampler2D T_DETAILMAP;
uniform sampler2D T_SHADOWMAP;

uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;
uniform float I_WATER_HEIGHT;
uniform vec4 V_WATER_COLOR;
uniform float I_WATER_DEPTH;
uniform float I_WATER_MAX;
uniform float I_WATER_MIN;
uniform vec3 V_CAM_POS;
uniform int  I_RECEIVE_SHADOW;
uniform float F_DETAIL_MULT;
uniform int I_DETAIL;
#INCLUDE Inc\lighting_uniforms.inc

varying vec2  ColorUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

void main(void)
{
	vec4 Color   = texture2D(T_COLORMAP, ColorUV);
    vec4 Caustic = texture2D(T_CAUSTICMAP, ColorUV*10);
    
    if (I_DETAIL==1)
    {
        Color.rgb  = (Color.rgb + texture2D(T_DETAILMAP, ColorUV * 4).rgb - F_DETAIL_MULT);
    }
    Color  = Color * Light; 
    
    gl_FragData[1] = vec4(1.0);
    if (I_RECEIVE_SHADOW == 1) 
    {
        #INCLUDE Inc\shadows.inc
    }
    
	#INCLUDE Inc\water_logic.inc

	gl_FragData[0].a = Color.a;
}



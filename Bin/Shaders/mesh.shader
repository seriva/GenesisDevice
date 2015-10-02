#VERTEX

uniform int I_FLIP_NORMAL;
uniform int I_DO_TREE_ANIM;
#INCLUDE Inc\transform_uniform.inc
#INCLUDE Inc\foliage_animation_uniform.inc
#INCLUDE Inc\lighting_uniforms.inc
#INCLUDE Inc\fog_uniforms.inc

varying vec2  ColorUV;
#INCLUDE Inc\varying.inc

#INCLUDE Inc\transform_func.inc

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
    #INCLUDE Inc\transform_apply.inc
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

uniform int  I_RECEIVE_SHADOW;
#INCLUDE Inc\lighting_uniforms.inc
#INCLUDE Inc\fog_uniforms.inc
#INCLUDE Inc\water_uniforms.inc
#INCLUDE Inc\detail_uniforms.inc

varying vec2  ColorUV;
#INCLUDE Inc\varying.inc

void main(void)
{
	vec4 Color   = texture2D(T_COLORMAP, ColorUV);
    vec4 Caustic = texture2D(T_CAUSTICMAP, ColorUV*10);
    
	vec2 DUV = ColorUV * 4;
	#INCLUDE Inc\detail.inc
    Color  = Color * Light; 
    
    gl_FragData[1] = vec4(1.0);
    if (I_RECEIVE_SHADOW == 1) 
    {
        #INCLUDE Inc\shadows.inc
    }
    
	#INCLUDE Inc\water_logic.inc

	gl_FragData[0].a = Color.a;
}



#VERTEX

uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform float F_ANIMATION_SPEED;
uniform float F_ANIMATION_STRENGTH;
#INCLUDE Inc\lighting_uniforms.inc

varying vec2  ColorUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

vec2 uv[4] = {
                vec2(0.99, 0.99),
                vec2(0.99, 0.00),
                vec2(0.0, 0.0),
                vec2(0.0, 0.99)
            };
                
void main(void)
{
    //UV
    ColorUV     = uv[int(gl_MultiTexCoord0.x)];

    //Lighting
	vec3 N = normalize(gl_Normal); 
	#INCLUDE Inc\lighting.inc
    
    //Vertex
	vec4 Eye = gl_Vertex;
	if(ColorUV.y < 0.1)
	{    
		#INCLUDE Inc\foliage_animation.inc
	}
	#INCLUDE Inc\vertex.inc
    
    //Shadows
    ShadowCoord = gl_TextureMatrix[7] * Eye;      
    
    //Fog
	#INCLUDE Inc\fog.inc
}




#FRAGMENT

uniform sampler2D T_GRASSTEX;
uniform sampler2D T_SHADOWMAP;
uniform vec4 V_FOG_COLOR;
#INCLUDE Inc\lighting_uniforms.inc

varying vec2  ColorUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

void main(void)
{
    vec4 Color = texture2D(T_GRASSTEX, ColorUV) * Light;
      
    gl_FragData[1] = vec4(1.0);  
	#INCLUDE Inc\shadows.inc
        
    gl_FragData[0] = mix(Color, V_FOG_COLOR, Fog);
}




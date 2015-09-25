#VERTEX

uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform float F_ANIMATION_SPEED;
uniform float F_ANIMATION_STRENGTH;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

varying vec2  GrassUV;
varying vec4  ShadowCoord;
varying float Fog;
varying vec4  Light;

vec2 uv[4] = {
                vec2(0.99, 0.99),
                vec2(0.99, 0.00),
                vec2(0.0, 0.0),
                vec2(0.0, 0.99)
            };
                
void main(void)
{
	vec3 L = normalize(-V_LIGHT_DIR);
	vec3 N = normalize(gl_Normal); 
	Light  = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);

	vec4 FogEye = ftransform();
	Fog         = clamp((length(FogEye) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	GrassUV     = uv[int(gl_MultiTexCoord0.x)];
	vec4 Eye    = gl_Vertex;

	if(GrassUV.y < 0.1)
	{    
        float cosine = cos(F_ANIMATION_SPEED * gl_Color.r);
        Eye.x        += cosine*F_ANIMATION_STRENGTH*gl_Color.g;
        Eye.z        += cosine*F_ANIMATION_STRENGTH*gl_Color.b;
	}

	gl_Position = gl_ModelViewProjectionMatrix * Eye;
    ShadowCoord = gl_TextureMatrix[7] * gl_Vertex;
}




#FRAGMENT

uniform sampler2D T_GRASSTEX;
uniform sampler2D T_SHADOWMAP;
uniform vec4 V_FOG_COLOR;
uniform float F_LIGHT_SHADOW;

varying vec2  GrassUV;
varying vec4 ShadowCoord;
varying float Fog;
varying vec4 Light;

void main(void)
{
    vec4 Color = texture2D(T_GRASSTEX, GrassUV) * Light;
      
	vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
	float distanceFromLight = texture2D(T_SHADOWMAP,shadowCoordinateWdivide.xy).z;
    if(ShadowCoord.x >= 0.0 && ShadowCoord.x <= 1.0 && ShadowCoord.y >= 0.0 && ShadowCoord.y <= 1.0 && (distanceFromLight < (shadowCoordinateWdivide.z + 0.001))){
        gl_FragData[1].rgb = vec3(F_LIGHT_SHADOW);
    } else {
        gl_FragData[1] = vec4(1.0);
    }
        
    gl_FragData[0] = mix(Color, V_FOG_COLOR, Fog);
}




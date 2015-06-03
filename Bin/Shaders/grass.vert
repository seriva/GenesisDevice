uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform float F_ANIMATION_SPEED;
uniform float F_ANIMATION_STRENGTH;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

varying vec2  GrassUV;
varying vec4 ShadowCoord;
varying float Fog;
varying vec4 Light;

void main(void)
{
	vec3 L = normalize(-V_LIGHT_DIR);
	vec3 N = normalize(gl_Normal); 
	Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);

	vec4 FogEye  = ftransform();
	Fog          = clamp((length(FogEye) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	GrassUV      = gl_MultiTexCoord0.xy;

	vec4 Eye     = gl_Vertex;

	if(GrassUV.y < 0.1)
	{    
        float cosine = cos(F_ANIMATION_SPEED * gl_Color.r);
        Eye.x        += cosine*F_ANIMATION_STRENGTH*gl_Color.g;
        Eye.z        += cosine*F_ANIMATION_STRENGTH*gl_Color.b;
	}

	gl_Position = gl_ModelViewProjectionMatrix * Eye;
    ShadowCoord = gl_TextureMatrix[7] * gl_Vertex;
}


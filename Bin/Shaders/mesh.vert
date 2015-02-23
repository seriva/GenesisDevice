uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform int I_FLIP_NORMAL;

varying vec2  UV;
varying float Fog;
varying vec3 N;
varying vec3 V;

void main(void)
{
	V = vec3(gl_ModelViewMatrix * gl_Vertex); 
	if(I_FLIP_NORMAL == 0)	
	{
		N = normalize(gl_Normal);
	}
	else
	{
		N = normalize(-gl_Normal);
	}
	vec4 Pos = gl_ModelViewProjectionMatrix * gl_Vertex;
	gl_Position    = Pos;
	gl_ClipVertex  = vec4(gl_ModelViewMatrix * gl_Vertex);
	Fog = clamp((length(Pos) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	UV  = gl_MultiTexCoord0.xy;
}

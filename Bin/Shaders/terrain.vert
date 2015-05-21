uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform int I_DETAIL_UV;
uniform int I_CAUSTIC_UV;

varying vec2  ColorUV;
varying vec2  DetailUV;
varying vec2  CausticUV;
varying float Fog;
varying vec3 N;
varying vec3 V;
varying vec3 VWorld;

void main(void)
{
	VWorld = gl_Vertex.xyz;
	V = vec3(gl_ModelViewMatrix * gl_Vertex);       
	N = normalize(gl_Normal);
	vec4 Pos = gl_ModelViewProjectionMatrix * gl_Vertex;
	gl_Position    = Pos;
	gl_ClipVertex  = vec4(gl_ModelViewMatrix * gl_Vertex);
	Fog            = clamp((length(Pos) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	ColorUV        = gl_MultiTexCoord0.xy;
	DetailUV       = ColorUV * I_DETAIL_UV;
	CausticUV      = ColorUV * I_CAUSTIC_UV;
}


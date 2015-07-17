uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;
uniform int I_REFRACTION_UV;
uniform int I_WAVES_UV;

varying vec4  RefrCoords; 
varying vec2  WavesCoords;
varying vec4  ViewCoords;
varying vec3  ViewVector;
varying float Fog;
varying vec4 Light;

void main()
{
	vec3 L = normalize(-V_LIGHT_DIR);
	vec3 N = vec3(0.0, 1.0, 0.0); 
	Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);

	RefrCoords  = gl_MultiTexCoord0 * I_REFRACTION_UV;
    WavesCoords = gl_MultiTexCoord0.xy * I_WAVES_UV;
	ViewCoords  = gl_ModelViewProjectionMatrix * gl_Vertex;

    vec4 FogEye = ftransform();
	Fog         = clamp((length(FogEye) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
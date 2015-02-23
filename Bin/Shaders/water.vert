uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform float F_WAVE_SPEED;
uniform float F_WAVE_STRENGHT;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

varying vec4  RefrCoords; 
varying vec4  ViewCoords;
varying vec2  DepthCoords;
varying vec3  ViewVector;
varying float Fog;
varying vec4 Light;

void main()
{
	vec3 L = normalize(-V_LIGHT_DIR);
	vec3 N = vec3(0.0, 1.0, 0.0); 
	Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);

	vec4 Eye     = ftransform();
	RefrCoords   = gl_MultiTexCoord0;
	DepthCoords  = gl_MultiTexCoord1.xy;
	ViewCoords   = gl_ModelViewProjectionMatrix * gl_Vertex;
	Fog          = clamp((length(Eye    ) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	vec4 EyeVert = gl_Vertex;

	float Cosine = cos(EyeVert.x + F_WAVE_SPEED) * sin(EyeVert.x + F_WAVE_SPEED);
	EyeVert.y    += Cosine * F_WAVE_STRENGHT; 
	gl_Position  = gl_ModelViewProjectionMatrix * EyeVert;
}
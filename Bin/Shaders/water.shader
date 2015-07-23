#VERTEX

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


#FRAGMENT

uniform sampler2D T_REFLECTION;
uniform sampler2D T_DUDVMAP;
uniform sampler2D T_CAUSTICMAP;
uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;
uniform vec4 V_WATER_COLOR;

varying vec4  RefrCoords; 
varying vec2  WavesCoords; 
varying vec4  ViewCoords;
varying float Fog;
varying vec4  Light;

void main()
{
	const float CDistortion = 0.015;
	const float CRefraction = 0.015;
	vec4 DistOffset       = texture2D(T_DUDVMAP, vec2(0,0) ) * CDistortion;
	vec4 DUDVColor        = texture2D(T_DUDVMAP, vec2(RefrCoords + DistOffset ));
	DUDVColor             = normalize(DUDVColor * 2.0  - 1.0) * CRefraction ;
	vec4 ProjCoord        = ViewCoords / ViewCoords.q;
	ProjCoord             = (ProjCoord + 1.0) * 0.5;
	ProjCoord             += DUDVColor;
	ProjCoord             = clamp(ProjCoord , 0.001, 0.999);
    vec4 CausticColor     = texture2D(T_CAUSTICMAP, WavesCoords);
	vec4 ReflectionColor  =  mix(texture2D(T_REFLECTION, ProjCoord.xy) * Light, CausticColor, 0.075);
	
	if(I_UNDER_WATER == 0)
	{
		gl_FragColor = mix(ReflectionColor, V_FOG_COLOR, Fog);
	}
	else
	{
		gl_FragColor = ReflectionColor;
	}
	gl_FragColor.a        = V_WATER_COLOR.a;
}
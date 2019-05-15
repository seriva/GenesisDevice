#VERTEX

#version 120
uniform int I_REFRACTION_UV;
uniform int I_WAVES_UV;
#INCLUDE Inc/lighting_uniforms.inc
#INCLUDE Inc/fog_uniforms.inc

varying vec4  RefrCoords;
varying vec2  WavesCoords;
varying vec4  VWorld;
varying float Fog;

void main()
{
	RefrCoords  = gl_MultiTexCoord0 * I_REFRACTION_UV;
	WavesCoords = gl_MultiTexCoord0.xy * I_WAVES_UV;
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	VWorld      = gl_Position;
	#INCLUDE Inc/fog.inc
}


#FRAGMENT

#version 120
uniform sampler2D T_REFLECTION;
uniform sampler2D T_DUDVMAP;
uniform sampler2D T_CAUSTICMAP;
uniform int I_UNDER_WATER;
uniform vec4 V_WATER_COLOR;
#INCLUDE Inc/lighting_uniforms.inc
#INCLUDE Inc/fog_uniforms.inc

varying vec4  RefrCoords;
varying vec2  WavesCoords;
varying vec4  VWorld;
varying float Fog;

void main()
{
	const float CDistortion = 0.015;
	const float CRefraction = 0.015;
	vec4 DistOffset       = texture2D(T_DUDVMAP, vec2(0,0) ) * CDistortion;
	vec4 DUDVColor        = texture2D(T_DUDVMAP, vec2(RefrCoords + DistOffset ));
	DUDVColor             = normalize(DUDVColor * 2.0  - 1.0) * CRefraction ;
	vec4 ProjCoord        = VWorld / VWorld.q;
	ProjCoord             = (ProjCoord + 1.0) * 0.5;
	ProjCoord             += DUDVColor;
	ProjCoord             = clamp(ProjCoord , 0.001, 0.999);
	vec4 CausticColor     = texture2D(T_CAUSTICMAP, WavesCoords);
	vec4 ReflectionColor  = mix(texture2D(T_REFLECTION, ProjCoord.xy), CausticColor, 0.075);

	if(I_UNDER_WATER == 0)
	{
		gl_FragData[0] = mix(ReflectionColor, V_FOG_COLOR, Fog);
	}
	else
	{
		gl_FragData[0] = ReflectionColor;
	}
	gl_FragData[0].a = V_WATER_COLOR.a;
	gl_FragData[1]   = vec4(1.0);
}

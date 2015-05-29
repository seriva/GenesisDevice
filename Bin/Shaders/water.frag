uniform sampler2D T_REFLECTION;
uniform sampler2D T_DUDVMAP;
uniform sampler2D T_CAUSTICMAP;
uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;
uniform vec4 V_WATER_COLOR;

varying vec4  RefrCoords; 
varying vec2  CausticCoords; 
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
    vec4 CausticColor     = texture2D(T_CAUSTICMAP, CausticCoords);
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
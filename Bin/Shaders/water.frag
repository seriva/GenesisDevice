uniform sampler2D T_REFLECTION;
uniform sampler2D T_DUDVMAP;
uniform sampler2D T_DEPTHMAP;
uniform vec4 V_FOG_COLOR;
uniform vec4 V_WATER_COLOR_CORRECTION;
uniform int I_UNDER_WATER;

varying vec4  RefrCoords; 
varying vec4  ViewCoords;
varying vec2  DepthCoords;
varying float Fog;
varying vec4  Light;

void main()
{
	const float CDistortion = 0.010;
	const float CRefraction = 0.010;
	vec4 DistOffset       = texture2D(T_DUDVMAP, vec2(0,0) ) * CDistortion;
	vec4 DUDVColor        = texture2D(T_DUDVMAP, vec2(RefrCoords + DistOffset ));
	DUDVColor             = normalize(DUDVColor * 2.0  - 1.0) * CRefraction ;
	vec4 ProjCoord        = ViewCoords / ViewCoords.q;
	ProjCoord             = (ProjCoord + 1.0) * 0.5;
	ProjCoord             += DUDVColor;
	ProjCoord             = clamp(ProjCoord , 0.001, 0.999);
	vec4 ReflectionColor  = texture2D(T_REFLECTION, ProjCoord.xy);
	
	if(I_UNDER_WATER == 0)
	{
		gl_FragColor = mix(ReflectionColor * Light, V_FOG_COLOR, Fog);
	}
	else
	{
		gl_FragColor = ReflectionColor * Light;
	}	
	float Depth           = texture2D(T_DEPTHMAP, DepthCoords).x;
	gl_FragColor.a        = 0.3;
}
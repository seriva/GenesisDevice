uniform vec4 V_FOG_COLOR;
uniform sampler2D T_COLORMAP;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;
uniform int I_DO_BLOOM;
uniform int I_UNDER_WATER;

varying vec2 UV;
varying float Fog;
varying vec3 N;

void main(void)
{
	vec3 L = normalize(-V_LIGHT_DIR);   
	vec4 Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);    
	vec4 Color = texture2D(T_COLORMAP, UV);
	if (I_DO_BLOOM == 1)
	{
		if(I_UNDER_WATER == 0)
		{
			gl_FragColor = mix( (Color * Light), V_FOG_COLOR, Fog);	
		}
		else
		{
			gl_FragColor = Color * Light;
		}	
	}
	else
	{
		gl_FragColor = mix( vec4(0.0, 0.0, 0.0, 0.0), V_FOG_COLOR, Fog);
	}
	gl_FragColor.a = Color.a;
}

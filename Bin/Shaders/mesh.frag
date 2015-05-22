uniform sampler2D T_COLORMAP;

uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;
uniform float I_WATER_HEIGHT;
uniform vec4 V_WATER_COLOR;
uniform float I_WATER_DEPTH;
uniform float I_WATER_MAX;
uniform float I_WATER_MIN;
uniform vec3 V_CAM_POS;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

uniform int I_DO_BLOOM;

varying vec2 UV;
varying float Fog;
varying vec3 N;
varying vec3 VWorld;

void main(void)
{
	vec3 L = normalize(-V_LIGHT_DIR);   
	vec4 Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);    
	vec4 Color = texture2D(T_COLORMAP, UV);
	if (I_DO_BLOOM == 1)
	{
		if(I_UNDER_WATER == 0)
		{
            if (VWorld.y > I_WATER_HEIGHT)
            {
                gl_FragColor = mix(Color * Light, V_FOG_COLOR, Fog);
            }
            else
            {
                float waterFog = clamp((log((length(VWorld - V_CAM_POS) * (I_WATER_HEIGHT - VWorld.y)/I_WATER_DEPTH) * I_WATER_MIN) - 1) * I_WATER_MAX, 0, 1); 
                gl_FragColor = mix(Color * Light, V_WATER_COLOR, waterFog);
            }       
		}
		else
		{
            if (VWorld.y > I_WATER_HEIGHT)
            {
                gl_FragColor = mix(Color * Light, V_WATER_COLOR, 0.85);
            }
            else
            {
                float waterFog = clamp((log((length(VWorld - V_CAM_POS) * (I_WATER_HEIGHT - VWorld.y)/I_WATER_DEPTH) * I_WATER_MIN) - 1) * I_WATER_MAX, 0, 1); 
                gl_FragColor = mix(Color * Light, V_WATER_COLOR, waterFog);
            }       
		}
	}
	else
	{
		gl_FragColor = mix( vec4(0.0, 0.0, 0.0, 0.0), V_FOG_COLOR, Fog);
	}
	gl_FragColor.a = Color.a;
}

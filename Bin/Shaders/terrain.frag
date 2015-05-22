uniform sampler2D T_COLORTEX;
uniform sampler2D T_DETAILTEX1;
uniform sampler2D T_DETAILTEX2;
uniform sampler2D T_DETAILTEX3;
uniform sampler2D T_WEIGHT_LOOKUP;
uniform sampler2D T_CAUSTIC_TEX;

uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;
uniform float I_WATER_HEIGHT;
uniform vec4 V_WATER_COLOR;
uniform vec3 V_CAM_POS;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

varying vec2 ColorUV;
varying vec2 DetailUV;
varying vec2 CausticUV;
varying float Fog;
varying vec3 N;
varying vec3 V;
varying vec3 VWorld;

void main(void)
{
	vec3 L = normalize(-V_LIGHT_DIR);   
	vec4 Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,L), 0.0), 0.0, 1.0);     
	vec4 Color         = texture2D(T_COLORTEX,      ColorUV);
	vec4 Detail1       = texture2D(T_DETAILTEX1,    DetailUV);
	vec4 Detail2       = texture2D(T_DETAILTEX2,    DetailUV);
	vec4 Detail3       = texture2D(T_DETAILTEX3,    DetailUV);
	vec4 Weights       = texture2D(T_WEIGHT_LOOKUP, ColorUV);
	vec4 Caustic       = texture2D(T_CAUSTIC_TEX, CausticUV);
	vec4 DetailFinal   = (Detail1*Weights.x + Detail2*Weights.y + Detail3*Weights.z); 
    Color = Color + DetailFinal - 0.5;

    const float c = 0.1;
    const float b = 0.17;
    float dist = length(VWorld - V_CAM_POS);    
    float waterFog = clamp((log((dist * (I_WATER_HEIGHT - VWorld.y)/500) * c) - 1) * b, 0, 1);    

    if(I_UNDER_WATER == 0)
	{
		if (VWorld.y > I_WATER_HEIGHT)
		{
			gl_FragColor = mix(Color * Light, V_FOG_COLOR, Fog);
		}
		else
		{
            gl_FragColor = mix((Color * clamp(Caustic, 0.7, 0.8)) * Light, V_WATER_COLOR, waterFog);
		}	    
	}
	else
	{
		if (VWorld.y > I_WATER_HEIGHT)
		{
            gl_FragColor = mix(Color * Light, V_WATER_COLOR, 0.8);
		}
		else
		{
            gl_FragColor = mix((Color * clamp(Caustic, 0.7, 0.8)) * Light, V_WATER_COLOR, waterFog);
		}
	}
}


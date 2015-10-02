#VERTEX

uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform int I_DETAIL_UV;
uniform int I_CAUSTIC_UV;
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

varying vec2  ColorUV;
varying vec2  DetailUV;
varying vec2  CausticUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

void main(void)
{
    //UV
	ColorUV        = gl_MultiTexCoord0.xy;
	DetailUV       = ColorUV * I_DETAIL_UV;
	CausticUV      = ColorUV * I_CAUSTIC_UV;

    //Lighting
	vec3 N = normalize(gl_Normal);
    Light  = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,normalize(-V_LIGHT_DIR)), 0.0), 0.0, 1.0); 
    
    //Vertex
    vec4 Eye = gl_Vertex;
    VWorld        = Eye.xyz;   
	gl_Position   = gl_ModelViewProjectionMatrix * Eye;
	gl_ClipVertex = gl_ModelViewMatrix * Eye;  

    //Shadows
    ShadowCoord = gl_TextureMatrix[7] * Eye;     

    //Fog
    Fog  = clamp((length(gl_Position) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
}



#FRAGMENT

uniform sampler2D T_COLORTEX;
uniform sampler2D T_DETAILTEX1;
uniform sampler2D T_DETAILTEX2;
uniform sampler2D T_DETAILTEX3;
uniform sampler2D T_WEIGHT_LOOKUP;
uniform sampler2D T_CAUSTICMAP;
uniform sampler2D T_DETAILMAP;
uniform sampler2D T_SHADOWMAP;

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

uniform float F_DETAIL_MULT;
uniform int   I_DETAIL;

varying vec2  ColorUV;
varying vec2  DetailUV;
varying vec2  CausticUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

void main(void)
{ 
	vec4 Color   = texture2D(T_COLORTEX,      ColorUV);
	vec4 Detail1 = texture2D(T_DETAILTEX1,    DetailUV);
	vec4 Detail2 = texture2D(T_DETAILTEX2,    DetailUV);
	vec4 Detail3 = texture2D(T_DETAILTEX3,    DetailUV);
	vec4 Weights = texture2D(T_WEIGHT_LOOKUP, ColorUV);
	vec4 Caustic = texture2D(T_CAUSTICMAP,    CausticUV);
	Color        += (Detail1*Weights.x + Detail2*Weights.y + Detail3*Weights.z) - 0.5; 
    
    if (I_DETAIL==1)
    {   
        Color = (Color + texture2D(T_DETAILMAP, DetailUV * 6) - F_DETAIL_MULT);
    }
    Color = Color * Light;
    
    gl_FragData[1] = vec4(1.0);
	vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
	float distanceFromLight = texture2D(T_SHADOWMAP,shadowCoordinateWdivide.xy).z;
    if(ShadowCoord.x >= 0.0 && ShadowCoord.x <= 1.0 && ShadowCoord.y >= 0.0 && ShadowCoord.y <= 1.0 && (distanceFromLight < (shadowCoordinateWdivide.z + 0.001))){
        gl_FragData[1].rgb = vec3(V_LIGHT_AMB);
    }
     
    if (VWorld.y > I_WATER_HEIGHT)
    {
        if(I_UNDER_WATER == 0)
        {       
            gl_FragData[0] = mix(Color, V_FOG_COLOR, Fog);
        }
        else
        {
            gl_FragData[0] = mix(Color, V_WATER_COLOR, 0.85);
        }
    }
    else
    {
        float waterFog = clamp((log((length(VWorld - V_CAM_POS) * (I_WATER_HEIGHT - VWorld.y)/I_WATER_DEPTH) * I_WATER_MIN) - 1) * I_WATER_MAX, 0, 1); 
        gl_FragData[0] = mix(Color * clamp(Caustic, 0.7, 0.85), V_WATER_COLOR, waterFog);  
    }	    
}


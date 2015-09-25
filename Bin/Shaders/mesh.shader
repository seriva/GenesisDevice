#VERTEX

uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform int I_FLIP_NORMAL;
uniform int I_DO_TREE_ANIM;
uniform float F_ANIMATION_SPEED;
uniform float F_ANIMATION_STRENGTH;
uniform mat4 M_ROTATION;
uniform vec3 V_POSITION;
uniform vec3 V_SCALE;

varying vec2  UV;
varying vec4 ShadowCoord;
varying float Fog;
varying vec3 N;
varying vec3 VWorld;

vec3 transformVector(vec3 n, mat4 mat){
  vec3 newNorm;
  newNorm.x = n.x * mat[0][0] + n.y * mat[1][0] + n.z * mat[2][0] + mat[3][0];
  newNorm.y = n.x * mat[0][1] + n.y * mat[1][1] + n.z * mat[2][1] + mat[3][1];
  newNorm.z = n.x * mat[0][2] + n.y * mat[1][2] + n.z * mat[2][2] + mat[3][2];
  return newNorm;
}

void main(void)
{
	if(I_FLIP_NORMAL == 0)	
	{
        N = normalize(transformVector(gl_Normal, M_ROTATION));
	}
    else
    {
        N = normalize(transformVector(-gl_Normal, M_ROTATION));
    }
	
    vec4 Eye     = gl_Vertex;
    Eye.xyz = (gl_Vertex.xyz * V_SCALE) / 100.0;
    Eye.xyz = transformVector(Eye.xyz, M_ROTATION);
    Eye.xyz = Eye.xyz + V_POSITION;   
    if(I_DO_TREE_ANIM == 1){
        Eye.x        += cos(F_ANIMATION_SPEED * gl_Color.r)*F_ANIMATION_STRENGTH * gl_Color.g;
        Eye.z        += sin(F_ANIMATION_SPEED * gl_Color.r)*F_ANIMATION_STRENGTH * gl_Color.b;    
    }
    vec4 Pos = gl_ModelViewProjectionMatrix * Eye;
    VWorld = Eye.xyz;
    
	gl_Position    = Pos;
	gl_ClipVertex  = vec4(gl_ModelViewMatrix * Eye);
	Fog = clamp((length(Pos) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
	UV  = gl_MultiTexCoord0.xy;
    ShadowCoord = gl_TextureMatrix[7] * Eye;
}




#FRAGMENT

uniform sampler2D T_COLORMAP;
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
uniform int  I_RECEIVE_SHADOW;
uniform float F_LIGHT_SHADOW;
uniform float F_DETAIL_MULT;
uniform int I_DETAIL;

varying vec2 UV;
varying vec4 ShadowCoord;
varying float Fog;
varying vec3 N;
varying vec3 VWorld;

void CalcUnderWaterColor(vec4 Color){
    float waterFog = clamp((log((length(VWorld - V_CAM_POS) * (I_WATER_HEIGHT - VWorld.y)/I_WATER_DEPTH) * I_WATER_MIN) - 1) * I_WATER_MAX, 0, 1); 
    gl_FragData[0] = mix(Color, V_WATER_COLOR, waterFog);
}

void main(void)
{
	vec4 Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,normalize(-V_LIGHT_DIR)), 0.0), 0.0, 1.0); 
    
	vec4 Color = texture2D(T_COLORMAP, UV);
    if (I_DETAIL==1)
    {
        Color.rgb  = (Color.rgb + texture2D(T_DETAILMAP, UV * 4).rgb - F_DETAIL_MULT);
    }
    Color  = Color * Light; 
    
    if (I_RECEIVE_SHADOW == 1) 
    {
        vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
        float distanceFromLight = texture2D(T_SHADOWMAP,shadowCoordinateWdivide.xy).z;
        if(ShadowCoord.x >= 0.0 && ShadowCoord.x <= 1.0 && ShadowCoord.y >= 0.0 && ShadowCoord.y <= 1.0 && (distanceFromLight < (shadowCoordinateWdivide.z + 0.001))){
            gl_FragData[1].rgb = vec3(F_LIGHT_SHADOW);
        } else {
            gl_FragData[1] = vec4(1.0);
        }
    } else {
        gl_FragData[1] = vec4(1.0);
    }
    
    if(I_UNDER_WATER == 0)
    {
        if (VWorld.y > I_WATER_HEIGHT)
        {
            gl_FragData[0] = mix(Color, V_FOG_COLOR, Fog);
        }
        else
        {
            CalcUnderWaterColor(Color);
        }       
    }
    else
    {
        if (VWorld.y > I_WATER_HEIGHT)
        {
            gl_FragData[0] = mix(Color, V_WATER_COLOR, 0.85);
        }
        else
        {
            CalcUnderWaterColor(Color);
        }       
    }

	gl_FragData[0].a = Color.a;
}



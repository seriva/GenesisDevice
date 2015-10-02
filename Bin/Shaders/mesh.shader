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
uniform vec3 V_LIGHT_DIR;
uniform vec4 V_LIGHT_AMB;
uniform vec4 V_LIGHT_DIFF;

varying vec2  ColorUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

vec3 transformVector(vec3 n, mat4 mat){
  vec3 newNorm;
  newNorm.x = n.x * mat[0][0] + n.y * mat[1][0] + n.z * mat[2][0] + mat[3][0];
  newNorm.y = n.x * mat[0][1] + n.y * mat[1][1] + n.z * mat[2][1] + mat[3][1];
  newNorm.z = n.x * mat[0][2] + n.y * mat[1][2] + n.z * mat[2][2] + mat[3][2];
  return newNorm;
}

void main(void)
{
    //UV
    ColorUV = gl_MultiTexCoord0.xy;

    //Lighting
    vec3 N = normalize(transformVector(gl_Normal, M_ROTATION));
	if(I_FLIP_NORMAL == 1)	
	{
        N = -N;
	}
    Light = V_LIGHT_AMB + clamp(V_LIGHT_DIFF * max(dot(N,normalize(-V_LIGHT_DIR)), 0.0), 0.0, 1.0); 
    
    //Vertex
    vec4 Eye = gl_Vertex;
    Eye.xyz = (transformVector(Eye.xyz * V_SCALE, M_ROTATION)) + V_POSITION;
    if(I_DO_TREE_ANIM == 1){
        float AniSpeed = F_ANIMATION_SPEED * gl_Color.r;
        Eye.x          += cos(AniSpeed)*F_ANIMATION_STRENGTH * gl_Color.g;
        Eye.z          += sin(AniSpeed)*F_ANIMATION_STRENGTH * gl_Color.b;    
    }
    VWorld        = Eye.xyz;
	gl_Position   = gl_ModelViewProjectionMatrix * Eye;
	gl_ClipVertex = gl_ModelViewMatrix * Eye;
    
    //Shadows
    ShadowCoord = gl_TextureMatrix[7] * Eye;     

    //Fog 
    Fog = clamp((length(gl_Position) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
}




#FRAGMENT

uniform sampler2D T_COLORMAP;
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
uniform int  I_RECEIVE_SHADOW;
uniform float F_DETAIL_MULT;
uniform int I_DETAIL;

varying vec2  ColorUV;
varying vec4  Light;
varying vec4  ShadowCoord;
varying float Fog;
varying vec3  VWorld;

void main(void)
{
	vec4 Color   = texture2D(T_COLORMAP, ColorUV);
    vec4 Caustic = texture2D(T_CAUSTICMAP, ColorUV*10);
    
    if (I_DETAIL==1)
    {
        Color.rgb  = (Color.rgb + texture2D(T_DETAILMAP, ColorUV * 4).rgb - F_DETAIL_MULT);
    }
    Color  = Color * Light; 
    
    gl_FragData[1] = vec4(1.0);
    if (I_RECEIVE_SHADOW == 1) 
    {
        vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
        float distanceFromLight = texture2D(T_SHADOWMAP,shadowCoordinateWdivide.xy).z;
        if(ShadowCoord.x >= 0.0 && ShadowCoord.x <= 1.0 && ShadowCoord.y >= 0.0 && ShadowCoord.y <= 1.0 && (distanceFromLight < (shadowCoordinateWdivide.z + 0.001))){
            gl_FragData[1].rgb = vec3(V_LIGHT_AMB);
        }
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

	gl_FragData[0].a = Color.a;
}



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

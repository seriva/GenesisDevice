uniform vec4 V_COLOR;
uniform int I_CUSTOM_TRANSLATE;
uniform mat4 M_ROTATION;
uniform vec3 V_POSITION;
uniform vec3 V_SCALE;

vec3 transformNormal(vec3 n, mat4 mat){
  vec3 newNorm;
  newNorm.x = n.x * mat[0][0] + n.y * mat[1][0] + n.z * mat[2][0] + mat[3][0];
  newNorm.y = n.x * mat[0][1] + n.y * mat[1][1] + n.z * mat[2][1] + mat[3][1];
  newNorm.z = n.x * mat[0][2] + n.y * mat[1][2] + n.z * mat[2][2] + mat[3][2];
  return newNorm;
}

void main()
{
    vec4 Eye     = gl_Vertex;
    if (I_CUSTOM_TRANSLATE == 1) {
        Eye.xyz = (gl_Vertex.xyz * V_SCALE) / 100.0;
        Eye.xyz = transformNormal(Eye.xyz, M_ROTATION);
        Eye.xyz = Eye.xyz + V_POSITION;  
    }
	gl_FrontColor = V_COLOR;
	gl_Position = gl_ModelViewProjectionMatrix * Eye;
}
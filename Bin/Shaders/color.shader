#VERTEX

#version 120
uniform vec4 V_COLOR;
uniform int I_CUSTOM_TRANSLATE;
#INCLUDE Inc/transform_uniform.inc

#INCLUDE Inc/transform_func.inc

void main()
{
  vec4 Eye = gl_Vertex;
  if (I_CUSTOM_TRANSLATE == 1) {
    #INCLUDE Inc/transform_apply.inc
  }
  gl_Position = gl_ModelViewProjectionMatrix * Eye;
}



#FRAGMENT

#version 120
uniform vec4 V_COLOR;

void main()
{
	gl_FragColor = V_COLOR;
}

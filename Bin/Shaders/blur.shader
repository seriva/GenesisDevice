#VERTEX
#version 120

void main(void)
{
  gl_Position    = gl_Vertex;
  gl_TexCoord[0] = gl_MultiTexCoord0;
}



#FRAGMENT
#version 120
#extension GL_ARB_shading_language_420pack : enable

uniform sampler2D T_BLUR_IMAGE;
uniform vec4      V_BLUR_OFFSET;

const int   Samples = 8;
const float Sum = 3.0359;

float Weights[9] = {
  1.0000,
  0.9394,
  0.7788,
  0.5697,
  0.3678,
  0.2096,
  0.1053,
  0.0467,
  0.0183
};


void main(void)
{
  vec4 Color = texture2D(T_BLUR_IMAGE, gl_TexCoord[0].xy) * Weights[0];
  vec2 TC;
  vec2 Offset;

  for (int i = 1; i <= Samples; i++)
  {
    Offset =  V_BLUR_OFFSET.xy * float(i);
    TC     =  gl_TexCoord[0].xy + Offset;
    Color  += texture2D(T_BLUR_IMAGE, TC) * Weights[i];
    TC     =  gl_TexCoord[0].xy - Offset;
    Color  += texture2D(T_BLUR_IMAGE, TC) * Weights[i];
  }
  gl_FragColor = Color / (1.0 + 2.0*Sum);
}

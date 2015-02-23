uniform sampler2D T_SOURCE_IMAGE;
uniform sampler2D T_BLUR_IMAGE;
uniform float I_BLOOM_STENGTH;

void main(void)
{
  vec4 Src  = texture2D(T_SOURCE_IMAGE, gl_TexCoord[0].xy);
  vec4 Blur = texture2D(T_BLUR_IMAGE, gl_TexCoord[0].xy);
  gl_FragColor = Src + (Blur * I_BLOOM_STENGTH);
}


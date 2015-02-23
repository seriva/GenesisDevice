uniform sampler2D T_SOURCE_IMAGE;

void main(void)
{
  gl_FragColor = texture2D(T_SOURCE_IMAGE, gl_TexCoord[0].xy);
}


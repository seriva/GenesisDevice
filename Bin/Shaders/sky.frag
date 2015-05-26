uniform sampler2D T_SKYTEX;
uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;

varying vec2  SkyUV;
varying float Fog;

void main(void)
{
  vec4 SkyColor = texture2D(T_SKYTEX, SkyUV);

  if(I_UNDER_WATER == 0)
  {
     gl_FragColor = mix(SkyColor, V_FOG_COLOR, Fog);
  }
  else
  {
     gl_FragColor = SkyColor;  
  }    
}


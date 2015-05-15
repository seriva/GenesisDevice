uniform sampler2D T_GRASSTEX;
uniform vec4 V_FOG_COLOR;

varying vec2  GrassUV;
varying float Fog;
varying vec4 Light;

void main(void)
{
      vec4 GrassColor = texture2D(T_GRASSTEX, GrassUV);
      gl_FragColor    = mix(GrassColor * Light , V_FOG_COLOR, Fog);
}


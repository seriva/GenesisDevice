#VERTEX

#version 120
#extension GL_ARB_shading_language_420pack : enable
#INCLUDE Inc/foliage_animation_uniform.inc
#INCLUDE Inc/lighting_uniforms.inc
#INCLUDE Inc/fog_uniforms.inc

varying vec2  ColorUV;
#INCLUDE Inc/varying.inc

vec2 uv[4] = {
  vec2(0.99, 0.99),
  vec2(0.99, 0.00),
  vec2(0.0, 0.0),
  vec2(0.0, 0.99)
};

void main(void)
{
  //UV
  ColorUV     = uv[int(gl_MultiTexCoord0.x)];

  //Lighting
  vec3 N = normalize(gl_Normal);
  #INCLUDE Inc/lighting.inc

  //Vertex
  vec4 Eye = gl_Vertex;
  if(ColorUV.y < 0.1)
  {
    #INCLUDE Inc/foliage_animation.inc
  }
  #INCLUDE Inc/vertex.inc

  //Shadows
  #INCLUDE Inc/shadows_coords.inc

  //Fog
  #INCLUDE Inc/fog.inc
}




#FRAGMENT

#version 120
uniform sampler2D T_GRASSTEX;
uniform sampler2D T_CAUSTICMAP;
uniform sampler2D T_SHADOWMAP;
#INCLUDE Inc/lighting_uniforms.inc
#INCLUDE Inc/fog_uniforms.inc
#INCLUDE Inc/water_uniforms.inc

varying vec2  ColorUV;
#INCLUDE Inc/varying.inc

void main(void)
{
  vec4 Color = texture2D(T_GRASSTEX, ColorUV) * Light;
  vec4 Caustic = texture2D(T_CAUSTICMAP, ColorUV*2);

  gl_FragData[1] = vec4(1.0);
  #INCLUDE Inc/shadows.inc

  #INCLUDE Inc/water_logic.inc
}

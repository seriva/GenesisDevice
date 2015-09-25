#VERTEX

uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;
uniform float F_ANIMATION_SPEED1;
uniform float F_ANIMATION_SPEED2;
uniform float F_SIZE;

varying vec2  CloudUV1;
varying vec2  CloudUV2;
varying float Fog;

void main(void)
{
     vec4 Eye    = ftransform();
     gl_Position = Eye;
     Fog         = clamp((length(Eye) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
     CloudUV1    = vec2((gl_Vertex.x/F_SIZE)+F_ANIMATION_SPEED1, (gl_Vertex.z/F_SIZE)+F_ANIMATION_SPEED1);
     CloudUV2    = vec2((gl_Vertex.x/F_SIZE)+F_ANIMATION_SPEED2, (gl_Vertex.z/F_SIZE)+F_ANIMATION_SPEED2);
}




#FRAGMENT

uniform sampler2D T_SKYTEX;
uniform vec4 V_FOG_COLOR;
uniform int I_UNDER_WATER;
uniform float I_INTENSITY;

varying vec2  CloudUV1;
varying vec2  CloudUV2;
varying float Fog;

void main(void)
{
  vec4 SkyCloud = texture2D(T_SKYTEX, CloudUV1) * texture2D(T_SKYTEX, CloudUV2);
  vec4 SkyColor = vec4((1.0 - I_INTENSITY)*0.33,
	                   (1.0 - I_INTENSITY)*0.5,
	                    I_INTENSITY*1.0, 0.0);                     
  SkyColor = SkyColor * (1.0 - SkyCloud.x) + SkyCloud;                      
   
  gl_FragData[0] = mix(SkyColor, V_FOG_COLOR, Fog); 
  gl_FragData[1] = vec4(1.0);
}




uniform sampler2D T_GRASSTEX;
uniform sampler2D T_SHADOWMAP;
uniform vec4 V_FOG_COLOR;
uniform float F_LIGHT_SHADOW;

varying vec2  GrassUV;
varying vec4 ShadowCoord;
varying float Fog;
varying vec4 Light;

void main(void)
{
    vec4 Color = texture2D(T_GRASSTEX, GrassUV) * Light;
      
	vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
	float distanceFromLight = texture2D(T_SHADOWMAP,shadowCoordinateWdivide.xy).z;
    if(ShadowCoord.x >= 0.0 && ShadowCoord.x <= 1.0 && ShadowCoord.y >= 0.0 && ShadowCoord.y <= 1.0 && (distanceFromLight < (shadowCoordinateWdivide.z + 0.001)))
        Color = Color * (F_LIGHT_SHADOW + ((1-F_LIGHT_SHADOW) * ShadowCoord.y));
        
    gl_FragColor = mix(Color, V_FOG_COLOR, Fog);
}


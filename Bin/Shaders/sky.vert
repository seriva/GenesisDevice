uniform float F_MIN_VIEW_DISTANCE;
uniform float F_MAX_VIEW_DISTANCE;

varying vec2  SkyUV;
varying float Fog;

void main(void)
{
     vec4 Eye    = ftransform();
     gl_Position = Eye;
     Fog         = clamp((length(Eye) - F_MIN_VIEW_DISTANCE) / F_MAX_VIEW_DISTANCE, 0.0, 1.0);
     SkyUV       = gl_MultiTexCoord0.xy;
}


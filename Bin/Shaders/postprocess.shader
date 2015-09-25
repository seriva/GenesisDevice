#VERTEX

uniform vec2 V_SCREEN_SIZE;
uniform int I_DO_FXAA;

varying vec4 posPos;

#define FXAA_SUBPIX_SHIFT (1.0/4.0)

void main(void)
{
	gl_Position = gl_Vertex;
	gl_TexCoord[0] = gl_MultiTexCoord0;

	if (I_DO_FXAA == 1)
	{    
		vec2 rcpFrame = vec2(1.0/V_SCREEN_SIZE.x, 1.0/V_SCREEN_SIZE.y);
		posPos.xy = gl_MultiTexCoord0.xy;
		posPos.zw = gl_MultiTexCoord0.xy - (rcpFrame * (0.5 + FXAA_SUBPIX_SHIFT));
	}
}



#FRAGMENT
#version 120
#extension GL_EXT_gpu_shader4 : enable

uniform sampler2D T_SOURCE_IMAGE;
uniform sampler2D T_SHADOW_IMAGE;
uniform sampler2D T_DEPTH_IMAGE;
uniform vec2 V_SCREEN_SIZE;
uniform int I_DO_SSAO;
uniform int I_DO_FXAA;
uniform float I_GAMMA;
uniform float I_SSAO_NEAR;
uniform float I_SSAO_FAR;
uniform float I_SSAO_STRENGTH;
uniform int   I_SSAO_SAMPLES; 
uniform float I_SSAO_RADIUS;
uniform int   I_SSAO_ONLY; 

float aoclamp = 0.125; //depth clamp - reduces haloing at screen edges
bool noise = true; //use noise instead of pattern for sample dithering
float noiseamount = 0.0002; //dithering amount
float diffarea = 0.3; //self-shadowing reduction
float gdisplace = 0.4; //gauss bell center //0.4
float lumInfluence = 0.7; //how much luminance affects occlusion

varying vec4 posPos;

#define FXAA_REDUCE_MIN   (1.0/128.0)
#define FXAA_REDUCE_MUL   (1.0/8.0)
#define FXAA_SPAN_MAX     8.0
#define FxaaInt2 ivec2
#define FxaaFloat2 vec2
#define FxaaTexLod0(t, p) texture2DLod(t, p, 0.0)
#define FxaaTexOff(t, p, o, r) texture2DLodOffset(t, p, 0.0, o)
#define PI 3.14159265

vec2 rand(vec2 coord)
{
  float noiseX = ((fract(1.0-coord.s*(V_SCREEN_SIZE.x/2.0))*0.25)+(fract(coord.t*(V_SCREEN_SIZE.y/2.0))*0.75))*2.0-1.0;
  float noiseY = ((fract(1.0-coord.s*(V_SCREEN_SIZE.x/2.0))*0.75)+(fract(coord.t*(V_SCREEN_SIZE.y/2.0))*0.25))*2.0-1.0;

  if (noise)
  {
    noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;
    noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;
  }
  return vec2(noiseX,noiseY)*noiseamount;
}

float readDepth(vec2 coord)
{
  if (gl_TexCoord[0].x<0.0||gl_TexCoord[0].y<0.0) return 1.0;
  else {
    float z_b = texture2D(T_DEPTH_IMAGE, coord ).x;
    float z_n = 2.0 * z_b - 1.0;
    return (2.0 * I_SSAO_NEAR) / (I_SSAO_FAR + I_SSAO_NEAR - z_n * (I_SSAO_FAR-I_SSAO_NEAR));
  }
}

int compareDepthsFar(float depth1, float depth2) {
  float garea = 2.0;
  float diff = (depth1 - depth2)*100.0;
  if (diff<gdisplace)
  {
    return 0;
  } else {
    return 1;
  }
}

float compareDepths(float depth1, float depth2)
{
  float garea = 2.0;
  float diff = (depth1 - depth2)*100.0; 
  if (diff<gdisplace)
  {
    garea = diffarea;
  }

  float gauss = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));
  return gauss;
}

float calAO(float depth,float dw, float dh)
{
  float dd = (1.0-depth)*I_SSAO_RADIUS;

  float temp = 0.0;
  float temp2 = 0.0;
  float coordw = gl_TexCoord[0].x + dw*dd;
  float coordh = gl_TexCoord[0].y + dh*dd;
  float coordw2 = gl_TexCoord[0].x - dw*dd;
  float coordh2 = gl_TexCoord[0].y - dh*dd;

  vec2 coord = vec2(coordw , coordh);
  vec2 coord2 = vec2(coordw2, coordh2);

  float cd = readDepth(coord);
  int far = compareDepthsFar(depth, cd);
  temp = compareDepths(depth, cd);
  if (far > 0)
  {
    temp2 = compareDepths(readDepth(coord2),depth);
    temp += (1.0-temp)*temp2;
  }

  return temp;
}

vec3 FxaaPixelShader( vec4 posPos, sampler2D tex, vec2 rcpFrame)
{
    vec3 rgbNW = FxaaTexLod0(tex, posPos.zw).xyz;
    vec3 rgbNE = FxaaTexOff(tex, posPos.zw, FxaaInt2(1,0), rcpFrame.xy).xyz;
    vec3 rgbSW = FxaaTexOff(tex, posPos.zw, FxaaInt2(0,1), rcpFrame.xy).xyz;
    vec3 rgbSE = FxaaTexOff(tex, posPos.zw, FxaaInt2(1,1), rcpFrame.xy).xyz;
    vec3 rgbM  = FxaaTexLod0(tex, posPos.xy).xyz;

    vec3 luma = vec3(0.299, 0.587, 0.114);
    float lumaNW = dot(rgbNW, luma);
    float lumaNE = dot(rgbNE, luma);
    float lumaSW = dot(rgbSW, luma);
    float lumaSE = dot(rgbSE, luma);
    float lumaM  = dot(rgbM,  luma);

    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));

    vec2 dir; 
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));

    float dirReduce = max(
        (lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL),
        FXAA_REDUCE_MIN);
    float rcpDirMin = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);
    dir = min(FxaaFloat2( FXAA_SPAN_MAX,  FXAA_SPAN_MAX), 
          max(FxaaFloat2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX), 
          dir * rcpDirMin)) * rcpFrame.xy;

    vec3 rgbA = (1.0/2.0) * (
        FxaaTexLod0(tex, posPos.xy + dir * (1.0/3.0 - 0.5)).xyz +
        FxaaTexLod0(tex, posPos.xy + dir * (2.0/3.0 - 0.5)).xyz);
    vec3 rgbB = rgbA * (1.0/2.0) + (1.0/4.0) * (
        FxaaTexLod0(tex, posPos.xy + dir * (0.0/3.0 - 0.5)).xyz +
        FxaaTexLod0(tex, posPos.xy + dir * (3.0/3.0 - 0.5)).xyz);
    float lumaB = dot(rgbB, luma);
    if((lumaB < lumaMin) || (lumaB > lumaMax)) return rgbA;
    return rgbB; 
}
    
void main() 
{ 
	vec3 color;
	if (I_DO_FXAA == 1)
	{  
		vec2 rcpFrame = vec2(1.0/V_SCREEN_SIZE.x, 1.0/V_SCREEN_SIZE.y);
		color = FxaaPixelShader(posPos, T_SOURCE_IMAGE, rcpFrame);
	}
	else
	{
		color = texture2D(T_SOURCE_IMAGE, gl_TexCoord[0].xy).rgb;
	}
    
    color = color * texture2D(T_SHADOW_IMAGE, gl_TexCoord[0].xy).rgb;
    
    if (I_DO_SSAO == 1)
    {
        vec2 noise = rand(gl_TexCoord[0].xy);
        float depth = readDepth(gl_TexCoord[0].xy);

        float w = (1.0 / V_SCREEN_SIZE.x)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));
        float h = (1.0 / V_SCREEN_SIZE.y)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));

        float pw = 0.0;
        float ph = 0.0;

        float ao = 0.0;

        float dl = PI * (3.0 - sqrt(5.0));
        float dz = 1.0 / float(I_SSAO_SAMPLES);
        float l = 0.0;
        float z = 1.0 - dz/2.0;

        for (int i = 0; i < 64; i++)
        {
            if (i > I_SSAO_SAMPLES) break;
            float r = sqrt(1.0 - z);

            pw = cos(l) * r;
            ph = sin(l) * r;
            ao += calAO(depth,pw*w,ph*h);
            z = z - dz;
            l = l + dl;
        }
        
        ao /= float(I_SSAO_SAMPLES);
        ao *= I_SSAO_STRENGTH;
        ao = 1.0-ao;

        vec3 lumcoeff = vec3(0.299,0.587,0.114);
        float lum = dot(color.rgb, lumcoeff);
        vec3 luminance = vec3(lum, lum, lum);
        if (I_SSAO_ONLY == 1) 
        {
           color = vec3(1,1,1); 
        }
        color = vec3(color*mix(vec3(ao),vec3(1.0),luminance*lumInfluence));       
    }

	gl_FragColor = vec4(pow(color, 1.0 / vec3(I_GAMMA)), 1.0);
}


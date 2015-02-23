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
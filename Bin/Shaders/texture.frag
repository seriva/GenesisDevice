uniform sampler2D T_COLORMAP;

varying vec2  UV;

void main()
{
	vec4 Color = texture2D(T_COLORMAP, UV);
	gl_FragColor = gl_Color * Color;
}
uniform sampler2D T_COLORMAP;
uniform vec4 V_COLOR;

varying vec2  UV;

void main()
{
	vec4 Color = texture2D(T_COLORMAP, UV);
	gl_FragColor = V_COLOR * Color;
}
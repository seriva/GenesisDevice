uniform vec4 V_COLOR;

void main()
{
	gl_FrontColor = V_COLOR;
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
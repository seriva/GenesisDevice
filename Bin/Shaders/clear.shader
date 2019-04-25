#VERTEX
#version 120

void main(void)
{
	gl_Position = gl_Vertex;
}

#FRAGMENT
#version 120

uniform vec4 V_COLOR;

void main()
{
	gl_FragData[0] = V_COLOR;
	gl_FragData[1] = vec4(1.0);
}

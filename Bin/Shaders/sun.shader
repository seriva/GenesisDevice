#VERTEX

#version 120
void main()
{
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}

#FRAGMENT

#version 120
uniform sampler2D T_SUNMAP;

void main()
{
	gl_FragColor = texture2D(T_SUNMAP, gl_PointCoord);
}

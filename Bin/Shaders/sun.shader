#VERTEX

void main()
{
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}

#FRAGMENT

uniform sampler2D T_SUNMAP;

void main()
{
        gl_FragColor = texture2D(T_SUNMAP, gl_PointCoord);
}


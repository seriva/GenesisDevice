#VERTEX

uniform vec4 V_COLOR;
uniform int I_CUSTOM_TRANSLATE;
uniform mat4 M_ROTATION;
uniform vec3 V_POSITION;
uniform vec3 V_SCALE;

#INCLUDE Inc\transform.inc

void main()
{
    vec4 Eye = gl_Vertex;
    if (I_CUSTOM_TRANSLATE == 1) {
        Eye.xyz = (transformVector(Eye.xyz * V_SCALE, M_ROTATION)) + V_POSITION;
    }
	gl_FrontColor = V_COLOR;
	gl_Position = gl_ModelViewProjectionMatrix * Eye;
}



#FRAGMENT

uniform vec4 V_COLOR;

void main()
{
	gl_FragColor = V_COLOR;
}


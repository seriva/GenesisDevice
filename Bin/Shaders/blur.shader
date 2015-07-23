#VERTEX

void main(void)
{
   gl_Position    = gl_Vertex;
   gl_TexCoord[0] = gl_MultiTexCoord0;
}



#FRAGMENT

uniform sampler2D T_BLUR_IMAGE;
uniform vec4      V_BLUR_OFFSET;

const int   Samples = 8;
const float Sum = 3.0359;

void main(void)
{
    float Weights[9];
    Weights[0] = 1.0000;
    Weights[1] = 0.9394;
    Weights[2] = 0.7788;
    Weights[3] = 0.5697;
    Weights[4] = 0.3678;
    Weights[5] = 0.2096;
    Weights[6] = 0.1053;
    Weights[7] = 0.0467;
    Weights[8] = 0.0183;

    vec4 Color = texture2D(T_BLUR_IMAGE, gl_TexCoord[0].xy) * Weights[0];
    vec2 TC;
    vec2 Offset;

    for (int i = 1; i <= Samples; i++)
    {
        Offset =  V_BLUR_OFFSET.xy * float(i);
        TC     =  gl_TexCoord[0].xy + Offset;
        Color  += texture2D(T_BLUR_IMAGE, TC) * Weights[i];
        TC     =  gl_TexCoord[0].xy - Offset;
        Color  += texture2D(T_BLUR_IMAGE, TC) * Weights[i];
    }
    gl_FragColor = Color / (1.0 + 2.0*Sum);
}




VWorld        = Eye.xyz;
gl_Position   = gl_ModelViewProjectionMatrix * Eye;
gl_ClipVertex = gl_ModelViewMatrix * Eye;
vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
float distanceFromLight = texture2D(T_SHADOWMAP,shadowCoordinateWdivide.xy).z;
if(ShadowCoord.x >= 0.0 && ShadowCoord.x <= 1.0 && ShadowCoord.y >= 0.0 && ShadowCoord.y <= 1.0 && (distanceFromLight < (shadowCoordinateWdivide.z + 0.001))){
	gl_FragData[1].rgb = vec3(V_LIGHT_AMB);
}
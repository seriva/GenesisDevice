if (VWorld.y > I_WATER_HEIGHT)
{
	if(I_UNDER_WATER == 0)
	{       
		gl_FragData[0] = mix(Color, V_FOG_COLOR, Fog);
	}
	else
	{
		gl_FragData[0] = mix(Color, V_WATER_COLOR, 0.85);
	}
}
else
{
	float waterFog = clamp((log((length(VWorld - V_CAM_POS) * (I_WATER_HEIGHT - VWorld.y)/I_WATER_DEPTH) * I_WATER_MIN) - 1) * I_WATER_MAX, 0, 1); 
	gl_FragData[0] = mix(Color * clamp(Caustic, 0.7, 0.85), V_WATER_COLOR, waterFog);  
}	
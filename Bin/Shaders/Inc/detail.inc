if (I_DETAIL==1)
{
	Color.rgb  = (Color.rgb + texture2D(T_DETAILMAP, DUV).rgb - F_DETAIL_MULT);
}
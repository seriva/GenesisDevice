float AniSpeed = F_ANIMATION_SPEED * gl_Color.r;
Eye.x          += cos(AniSpeed)*F_ANIMATION_STRENGTH * gl_Color.g;
Eye.z          += sin(AniSpeed)*F_ANIMATION_STRENGTH * gl_Color.b;
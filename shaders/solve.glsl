vec3 dicho(int si, vec3 e,vec3 dir,
           float ua,float fa,
	   float ub,float fb,
	   out float ur) {
     if (fa == 0.0) {
       ur = ua;
       return e + ua * dir;
     }
     while (true) {
     	   float umid = 0.5 * (ua + ub);
	   float un = umid;
	   vec3 mid = e + un * dir;
           vec3 x = mid;
	   if (un == ua || un == ub) { ur = un; return x; }

           float fn = f(si,x);

	    if (fn == 0.0) {
	        ur = un;
		return x;
	    }
            if (fa * fn < 0.0) {
		ub = un;
                fb = fn;
            } else {
		ua = un;
		fa = fn;
            }
        }
}

const int SSIZE=32;
const int MAXLAYERS=4;

int solve(vec3 e, vec3 pos, int nb, out vec3[MAXLAYERS] res, out int surfs[MAXLAYERS])
{
    vec3 dir = normalize(pos - e);
    float ustack[SSIZE];
    float fstack[SSIZE];
    float dfstack[SSIZE];
    float ures[MAXLAYERS];
    for (int i = 0; i < MAXLAYERS; i++) {
       ures[i] = 1e32;
    }
    int sp = 0;
    int sr = 0;
    float ubest = far;
    // échantillonnage du rayon
    for(int si = 0; si < LASTS; si++) {
      surface surf = surfaces[si];
      float ua = near;
      vec3 x = e + ua * dir;
      vec3 dtmp;
      float fa = f_df(si,x,dtmp);
      float dfa = dot(dtmp,dir);
      float ub = ua;
      float fb = fa;
      float dfb = dfa;
      float step = (far - near) / float(surf.mindivs+1);
      int i = 0;
      int ssr = 0;
      int sp = 0;
      while (ub < ubest && (i <= surf.mindivs || sp > 0)) {
    	ua = ub;
	fa = fb;
	dfa = dfb;
        if (sp > 0) {
	  ub = ustack[--sp];
	  fb = fstack[sp];
	  dfb = dfstack[sp];
	} else {
	  i += 1;
          ub = near + step * float(i);
          x = e + ub * dir;
          fb = f_df(si,x,dtmp);
	  dfb = dot(dtmp,dir);
        }

	float c = ub - ua;
	float A = dfa;
	float B = (3.0*(fb-fa)-c*(2.0*dfa + dfb))/(c*c);
	float C = (c*(dfa + dfb) - 2.0*(fb - fa))/(c*c*c);
	float D = B*B - 3.0*A*C;
	float t = (rand() - 0.5) * 1e-1 + 0.5;
	float u1 = ua, u2 = ua, u3 = t*ua + (1.0-t)*ub, uc, fc, dfc;
	vec3 xc;
	if (B > 0.0 && D >= 0.0) {
	  float tmp = -B - sqrt(D);
	  u1 = ua + tmp/(3.0*C); u2 = ua + A/tmp;
        } else if (D >= 0.0) {
	  float tmp = -B + sqrt(D);
	  u1 = ua + tmp/(3.0*C); u2 = ua + A/tmp;
	}
	if (u2 < u1) {
	  float tmp = u1; u1 = u2; u2 = tmp;
	}
	bool test1 = ua >= u1 || u1 >= ub, test2 = ua >= u2 || u2 >= ub;
	if (test1 && test2) {
	  float t = (rand() - 0.5) * 1e-1 + 0.75;
	  u1 = t*ua + (1.0-t)*ub;
	  t = (rand() - 0.5) * 1e-1 + 0.25;
	  u2 = t*ua + (1.0-t)*ub;
	}
	else if (test2) {
	  float t = (rand() - 0.5) * 1e-1 + u1<u3 ? 0.25 : 0.75;
	  u2 = t*ua + (1.0-t)*ub;
	}
	else if (test1) {
	  float t = (rand() - 0.5) * 1e-1 + u2<u3 ? 0.25 : 0.75;
	  u1 = t*ua + (1.0-t)*ub;
	}
	if (u2 < u1) {
	  float tmp = u1; u1 = u2; u2 = tmp;
	}
	if (u3 < u1) {
	  float tmp = u1; u1 = u3; u3 = u2; u2 = tmp;
	} else if (u3 < u2) {
	  float tmp = u2; u2 = u3; u3 = tmp;
	}
	float us[5];
	float fs[5];
	float dfs[5];
	bool bad=false;
	{
	   x = e + u2 * dir;
	   float fx = f_df(si,x,dtmp);
	   float dfx = dot(dtmp,dir);
	   us[2] = u2; fs[2] = fx; dfs[2] = dfx;
	   float X = u2 - ua;
	   float f3x = fa + (A + (B + C*X)*X)*X;
	   float df3x = A + (2.0*B + 3.0*C*X)*X;
	   float R1 =  abs(fx - f3x);
	   float R2 =  abs(dfx - df3x)*(ub - ua);
	   float D = abs(fx) + abs(f3x);
	   if (!(R1 <= surf.prec1 * D && R2 <= surf.prec2 * D)) bad = true;
	   xc = x; fc = fx; dfc = dfx; uc = u2;
	}
	if (!bad) {
	   x = e + u1 * dir;
	   float fx = f_df(si,x,dtmp);
	   float dfx = dot(dtmp,dir);
	   us[1] = u1; fs[1] = fx; dfs[1] = dfx;
	   float X = u1 - ua;
	   float f3x = fa + (A + (B + C*X)*X)*X;
	   float df3x = A + (2.0*B + 3.0*C*X)*X;
	   float R1 =  abs(fx - f3x);
	   float R2 =  abs(dfx - df3x)*(ub - ua);
	   float D = abs(fx) + abs(f3x);
	   if (!(R1 <= surf.prec1 * D && R2 <= surf.prec2 * D)) bad = true;
	}
	if (!bad) {
	   x = e + u3 * dir;
	   float fx = f_df(si,x,dtmp);
	   float dfx = dot(dtmp,dir);
	   us[3] = u3; fs[3] = fx; dfs[3] = dfx;
	   float X = u3 - ua;
	   float f3x = fa + (A + (B + C*X)*X)*X;
	   float df3x = A + (2.0*B + 3.0*C*X)*X;
	   float R1 =  abs(fx - f3x);
	   float R2 =  abs(dfx - df3x)*(ub - ua);
	   float D = abs(fx) + abs(f3x);
	   if (!(R1 <= surf.prec1 * D && R2 <= surf.prec2 * D)) bad = true;
	}
	if (bad && sp <= SSIZE - 2 && uc != ua && uc != ub) {
           ustack[sp] = ub;
	   fstack[sp] = fb;
	   dfstack[sp++] = dfb;
	   ustack[sp] = uc;
	   fstack[sp] = fc;
	   dfstack[sp++] = dfc;
	   ub = ua;
	   fb = fa;
	   dfb = dfa;
	   continue;
	}
	us[0] = ua; fs[0] = fa;
	us[4] = ub; fs[4] = fb;
	bool brk = false;
	for (int j = 0; j < 5; j++) {
	   if (fs[j] * fs[j+1] <= 0.0) {
	      float ur;
	      x = dicho(si,e,dir,us[j],fs[j],us[j+1],fs[j+1],ur);
	      if (bf(si,x) > 0.0) continue;
	      if (ur <= ubest) {
	         while (ssr < sr && ures[ssr] < ur) ssr++;
	         if (ssr < nb) {
	            if (sr < nb) sr++;
	            for (int k = sr-1; k > ssr; k--) {
		       ures[k] = ures[k-1];
		       res[k] = res[k-1];
		       surfs[k] = surfs[k-1];
		    }
	            ures[ssr] = ur;
		    res[ssr] = x;
  	            surfs[ssr++] = si;
		    if ((surf.color.a >= 1.0 && dot(df(si,x),dir) > 0.0) ||
		        (surf.back_color.a >= 1.0 && dot(df(si,x),dir) < 0.0)) {
		       ubest = ur;
		       sr = ssr;
		       brk = true;
		       break;
		    }
	         }
	      }
	   }
	}
	if (ssr >= nb || brk) break;
      }
    }

    if (sr == 0) discard;
    return sr;
}

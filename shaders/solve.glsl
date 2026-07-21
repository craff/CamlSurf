vec3 dicho(int si, vec3 e,vec3 dir,
           float ua,float fa,
	   float ub,float fb,
	   out float ur) {
     if (fa == 0) {
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

	    if (fn == 0) {
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
      float fa = f(si,x);
      float dfa = dot(df(si,x),dir);
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
          fb = f(si,x);
	  dfb = dot(df(si,x),dir);
        }

	float c = ub - ua;
	float A = dfa;
	float B = (3*(fb-fa)-c*(2*dfa + dfb))/(c*c);
	float C = (c*(dfa + dfb) - 2*(fb - fa))/(c*c*c);
	float D = B*B - 3*A*C;
	float t = (rand() - 0.5) * 1e-1 + 0.5;
	float u1 = ua, u2 = ua, uc = t*ua + (1-t)*ub, u3 = uc;
	vec3 xc = vec3(0);
	float fc = 0;
	int mid = 3;
	if (B > 0 && D >= 0) {
	  float tmp = -B - sqrt(D);
	  u1 = ua + tmp/(3*C); u2 = ua + A/tmp;
        } else if (D >= 0) {
	  float tmp = -B + sqrt(D);
	  u1 = ua + tmp/(3*C); u2 = ua + A/tmp;
	}
	if (u2 < u1) {
	  float tmp = u1; u1 = u2; u2 = tmp;
	}
	if (!(ua < u1 && u1 < ub)) {
	  float t = (rand() - 0.5) * 1e-1 + 0.75;
	  u1 = t*ua + (1-t)*ub;
	}
	if (!(ua < u2 && u2 < ub)) {
	  float t = (rand() - 0.5) * 1e-1 + 0.25;
	  u2 = t*ua + (1-t)*ub;
	}
	if (u2 < u1) {
	  float tmp = u1; u1 = u2; u2 = tmp;
	}
	if (u3 < u1) {
	  float tmp = u1; u1 = u3; u3 = u2; u2 = tmp;
	  mid = 1;
	} else if (u3 < u2) {
	  float tmp = u2; u2 = u3; u3 = tmp;
	  mid = 2;
	}
	float us[5];
	float fs[5];
	int sq = 0;
	us[sq] = ua; fs[sq++] = fa;
	bool bad=false;
	float mab = min(abs(fa),abs(fb));
	{
	   x = e + u1 * dir;
	   float fx = f(si,x);
	   us[sq] = u1; fs[sq++] = fx;
	   float X = u1 - ua;
	   float f3x = fa + (A + (B + C*X)*X)*X;
	   float error = abs((fx - f3x)/fx);
	   if (!(abs(fx - f3x) <= surf.prec * abs(fx))) bad = true;
	   if (mid == 1) { xc = x; fc = fx; }
	}
	if (!bad || mid == 2) {
	   x = e + u2 * dir;
	   float fx = f(si,x);
	   us[sq] = u2; fs[sq++] = fx;
	   float X = u2 - ua;
	   float f3x = fa + (A + (B + C*X)*X)*X;
	   float error = abs((fx - f3x)/fx);
	   if (!(abs(fx - f3x) <= surf.prec * abs(fx))) bad = true;
	   if (mid == 2) { xc = x; fc = fx; }
	}
	if (!bad || mid == 3) {
	   x = e + u3 * dir;
	   float fx = f(si,x);
	   us[sq] = u3; fs[sq++] = fx;
	   float X = u3 - ua;
	   float f3x = fa + (A + (B + C*X)*X)*X;
	   float error = abs((fx - f3x)/fx);
	   if (!(abs(fx - f3x) <= surf.prec * abs(fx))) bad = true;
	   if (mid == 3) { xc = x; fc = fx; }
	}
	if (bad && sp <= SSIZE - 2 && uc != ua && uc != ub) {
           ustack[sp] = ub;
	   fstack[sp] = fb;
	   dfstack[sp++] = dfb;
	   ustack[sp] = uc;
	   fstack[sp] = fc;
	   dfstack[sp++] = dot(df(si,xc),dir);;
	   ub = ua;
	   fb = fa;
	   dfb = dfa;
	   continue;
	}
	us[sq] = ub; fs[sq++] = fb;
	bool brk = false;
	for (int j = 0; j < sq-1; j++) {
	   if (fs[j] * fs[j+1] <= 0) {
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
		    if ((surf.color.a >= 1.0 && dot(df(si,x),dir) > 0) ||
		        (surf.back_color.a >= 1.0 && dot(df(si,x),dir) < 0)) {
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

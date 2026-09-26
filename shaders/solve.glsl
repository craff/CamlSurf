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
      if (surf.degree == 1) {
        ub = far;
	x = e + ub * dir;
	fb = f_df(si, x, dtmp);
	float ur = ua + (ub - ua) * fa / (fa - fb);
	x = e + ur * dir;
	if (ua <= ur && ur <= ub && bf(si,x) <= 0.0) {
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
	      }
	    }
	  }
	}
	continue;
      }
      else if (surf.degree == 2) {
        ub = far;
	x = e + ub * dir;
	fb = f_df(si, x, dtmp);
	float c = ub - ua;
	float C = fa;
	float B = dfa;
	float A = ((fb-fa)-c*dfa)/(c*c);
	float D = B*B - 4.0*A*C;
	if (D > 0.0) {
	  float tmp = (B > 0.0) ? (-B - sqrt(D)) : (-B + sqrt(D));
          float u1 = ua + tmp / (2.0 * A);
	  float u2 = ua + (2.0 * C) / tmp;
	  if (!(u1 < u2)) {
	    float tmp = u1; u1 = u2; u2 = tmp;
	  }
	  vec3 x1 = e + u1 * dir;
 	  if (ua <= u1 && u1 <= ub && bf(si,x1) <= 0.0) {
 	    if (u1 <= ubest) {
	      while (ssr < sr && ures[ssr] < u1) ssr++;
	      if (ssr < nb) {
	        if (sr < nb) sr++;
	        for (int k = sr-1; k > ssr; k--) {
	          ures[k] = ures[k-1];
	          res[k] = res[k-1];
	          surfs[k] = surfs[k-1];
	        }
	        ures[ssr] = u1;
	        res[ssr] = x1;
 	        surfs[ssr++] = si;
	        if ((surf.color.a >= 1.0 && dot(df(si,x1),dir) > 0.0) ||
		    (surf.back_color.a >= 1.0 && dot(df(si,x1),dir) < 0.0)) {
		  ubest = u1;
 		  sr = ssr;
		  continue;
	        }
	      }
	    }
	  }
 	  if (ssr >= nb) continue;
	  vec3 x2 = e + u2 * dir;
 	  if (ua <= u2 && u2 <= ub && bf(si,x2) <= 0.0) {
 	    if (u2 <= ubest) {
	      while (ssr < sr && ures[ssr] < u2) ssr++;
	      if (ssr < nb) {
	        if (sr < nb) sr++;
	        for (int k = sr-1; k > ssr; k--) {
	          ures[k] = ures[k-1];
	          res[k] = res[k-1];
	          surfs[k] = surfs[k-1];
	        }
	        ures[ssr] = u2;
	        res[ssr] = x2;
 	        surfs[ssr++] = si;
	        if ((surf.color.a >= 1.0 && dot(df(si,x2),dir) > 0.0) ||
		    (surf.back_color.a >= 1.0 && dot(df(si,x2),dir) < 0.0)) {
		  ubest = u2;
 		  sr = ssr;
	        }
	      }
	    }
	  }
        }
	continue;
      }
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

	hermite H = hermite3_make(ua,fa,dfa,ub,fb,dfb);
	float du = ub - ua;
	float u1 = ua, u2 = ua;
	hermite3_critical(H, u1, u2);
	float us[4];
	float fs[4];
	float uc = ua; float fc; float dfc; float C = 0.0; float D;
	bool bad=false;
	us[0] = ua; fs[0] = fa;
	int usn = 1;
	if (ua < u1 && u1 < ub)
	{
	   x = e + u1 * dir;
	   float fx = f_df(si,x,dtmp);
	   float dfx = dot(dtmp,dir);
	   us[usn] = u1; fs[usn] = fx; usn += 1;
	   bad = !WH(H, du, u1, fx, dfx, surf, D) || bad;
	   if (D > C) {
	       uc = u1; fc = fx; dfc = dfx; C = D;
	   }
	}
	if (ua < u2 && u2 < ub)
	{
	   x = e + u2 * dir;
	   float fx = f_df(si,x,dtmp);
	   float dfx = dot(dtmp,dir);
	   us[usn] = u2; fs[usn] = fx; usn += 1;
	   bad = !WH(H, du, u2, fx, dfx, surf, D) || bad;
	   if (D > C) {
	       uc = u2; fc = fx; dfc = dfx; C = D;
	   }
	}
	us[usn] = ub; fs[usn] = fb; usn += 1;
	float width = 1.0/float(surf.nb_samples+1);
	for (int j = 1; j <= surf.nb_samples; j++) {
	   float t = float(j)*width;
	   float u3 = ua + du*t;
	   /*if (abs(u1 - u3) < width/5.0 || abs(u2-u3) < width/5.0) continue;*/
	   x = e + u3 * dir;
	   float fx = f_df(si,x,dtmp);
	   float dfx = dot(dtmp,dir);
	   bad = !WH(H, du, u3, fx, dfx, surf, D) || bad;
	   if (D > C && u3 != ua && u3 != ub) {
	       uc = u3; fc = fx; dfc = dfx; C = D;
	   }
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
	bool brk = false;
	for (int j = 0; j < usn-1; j++) {
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

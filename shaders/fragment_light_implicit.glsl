void main()
{
  init_surfaces();
  vec3 ipos = (InvModelView * vec4(pos,1.0)).xyz;
  vec3 ieyePos = (InvModelView * vec4(eyePos,1.)).xyz;
  vec3 res[MAXLAYERS];
  int surfs[MAXLAYERS];
  int nb = 1;
  nb = solve(ieyePos,ipos,4,res,surfs);
  vec4 gPos;
  vec3 fcolor = vec3(0.0);
  for (int i=nb-1;i>=0;i--) {
    int sid = surfs[i];
    surface surf = surfaces[sid];
    vec3 p = res[i];
    vec4 m_position = ModelView * vec4(p,1.0);
    gPos = Projection * m_position;

    vec3 n = normalize (NormalMatrix * df(sid,p));
    vec3 halfV,lightDir;
    float NdotL,NdotHV;

    lightDir = normalize(lightPos - m_position.xyz);

    /* The ambient term will always be present */
    vec4 line_color;
    float color_factor = cf(surf.id,p,line_color);
    vec4 mcolor = surf.color;
    if (color_factor < 3.0) {
       mcolor = (1.0 - color_factor/3.0) * line_color
       	       + (color_factor/3.0) * mcolor;
    }
    vec4 col = lightAmbient * mcolor;

    NdotL = dot(n,lightDir);
    if (NdotL > 0.) {
      col += lightDiffuse * mcolor * NdotL;
      if (i == 0) {
        halfV = normalize(lightPos - 2.0 * m_position.xyz);
        NdotHV = abs(dot(n,halfV));
        col += surf.specular * pow(NdotHV, surf.shininess);
      }
    } else {
      col -= lightDiffuse * mcolor * NdotL;
      if (i == 0) {
        halfV = normalize(lightPos - 2.0 * m_position.xyz);
        NdotHV = abs(dot(-n,halfV));
        col += surf.specular * pow(NdotHV, surf.shininess);
      }
    }
    fcolor = col.rgb * mcolor.a + fcolor  * (1.0 - mcolor.a);
  }
  FragColor = vec4(fcolor,1);
}

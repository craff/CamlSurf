uniform float time;
uniform mat3 NormalMatrix;
uniform mat4 Projection,ModelView,InvModelView;
uniform float screen_size;
uniform vec3 eyePos,lightPos,backgroundColor;
uniform vec4 lightDiffuse,lightAmbient;
uniform float far;
uniform float near;

in vec3 pos;
out vec4 FragColor;

struct surface {
  int id;
  int mindivs;
  int degree;
  float prec;
  int nb_samples;
  vec4 color;
  vec4 back_color;
  float specular;
  float shininess;
};

const int MAXS=100;

int LASTS = 0;
surface[MAXS] surfaces;

struct hermite {
  float fa;
  float A;
  float B;
  float C;
  float a;
};

hermite hermite3_make(float a, float fa, float dfa,
                      float b, float fb, float dfb)
{
    if (abs(fa) > abs(fb)) {
        float tmp;

        tmp = a;   a = b;   b = tmp;
        tmp = fa;  fa = fb; fb = tmp;
        tmp = dfa; dfa = dfb; dfb = tmp;
    }

    float c = b - a;
    float A = dfa;
    float B = (3.0 * (fb - fa) - c * (2.0 * dfa + dfb))
              / (c * c);
    float C = (c * (dfa + dfb) - 2.0 * (fb - fa))
              / (c * c * c);

    return hermite(fa, A, B, C, a);
}

void hermite3_critical(hermite H, out float x1, out float x2)
{
    float D = H.B * H.B - 3.0 * H.A * H.C;

    if (D >= 0.0) {
        float tmp;

        if (H.B > 0.0)
            tmp = -H.B - sqrt(D);
        else
            tmp = -H.B + sqrt(D);

        x1 = H.a + tmp / (3.0 * H.C);
        x2 = H.a + H.A / tmp;
	if (x1 > x2) { tmp = x1; x1 = x2; x2 = tmp; }
    }
}

float hermite3_fdf(hermite H, float x, out float dfx)
{
    float u = x - H.a;

    float fx  = ((H.C * u + H.B) * u + H.A) * u + H.fa;
    dfx = (3.0 * H.C * u + 2.0 * H.B) * u + H.A;
    return fx;
}

bool WH(hermite H, float du, float x, float fx, float dfx, surface surf,
        out float C)
{
    float hx, dhx;
    hx = hermite3_fdf(H, x, dhx);
/*    float R1 = abs(hx - fx) / (abs(fx) + abs(hx));
    float R2 = abs(dhx - dfx) / (abs(dfx) + abs(dhx));;*/
    float R1 = abs(hx - fx) / length(vec2(fx, dhx));
    float R2 = abs(dhx - dfx) / length(vec2(dfx, dhx));;

    C = isnan(R1) ? R2 : isnan(R2) ? R1 : R1*R2;

    return C < surf.prec * surf.prec;
}
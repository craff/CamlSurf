uniform float time;
uniform mat3 NormalMatrix;
uniform mat4 Projection,ModelView,InvModelView;
uniform float screen_size;
uniform vec3 eyePos,lightPos;
uniform vec4 lightDiffuse,lightAmbient;
uniform float far;
uniform float near;

in vec3 pos;
out vec4 FragColor;

float ipow(float x, int n)
{
    if (n == 0)
        return 1.0;

    bool neg = n < 0;
    if (neg)
        n = -n;

    float r = 1.0;
    float p = x;

    while (n > 0)
    {
        if ((n & 1) != 0)
            r *= p;

        p *= p;
        n >>= 1;
    }

    return neg ? 1.0 / r : r;
}

struct surface {
  int id;
  int mindivs;
  float prec;
  vec4 color;
  float specular;
  float shininess;
};

const int MAXS=100;

int LASTS = 0;
surface[MAXS] surfaces;

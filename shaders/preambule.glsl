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

uint rng_state;

void srand(uint seed)
{
    rng_state = seed;
}

uint rand_uint()
{
    rng_state = 1664525u * rng_state + 1013904223u;
    return rng_state;
}

float rand()
{
    return float(rand_uint()) * (1.0 / 4294967296.0);
}

struct surface {
  int id;
  int mindivs;
  float prec1;
  float prec2;
  vec4 color;
  vec4 back_color;
  float specular;
  float shininess;
};

const int MAXS=100;

int LASTS = 0;
surface[MAXS] surfaces;

in vec3 in_position;
out vec3 pos;
uniform mat4 Projection;
void main()
{
  pos = in_position;
  gl_Position = Projection * vec4(in_position,1.0);
}

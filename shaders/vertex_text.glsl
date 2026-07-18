uniform mat4 Projection;

in vec3 in_position;

in vec2 in_tex_coordinates;
out vec2 tex_coordinates;

void main()
{
  tex_coordinates = in_tex_coordinates;
  gl_Position = Projection * vec4(in_position,1.0);
}
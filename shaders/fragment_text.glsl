uniform sampler2D texture_text;
in vec4 m_position;
in vec2 tex_coordinates;
out vec4 FragColor;

void main()
{
  vec4 value = texture(texture_text, tex_coordinates);
  FragColor= value;
}
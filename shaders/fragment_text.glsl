uniform sampler2D texture_text;
in vec4 m_position;
in vec2 tex_coordinates;
uniform vec3 text_color;
out vec4 FragColor;

void main()
{
  vec4 value = texture(texture_text, tex_coordinates);
  FragColor= vec4(text_color, value.r);
}
#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

void main()
{
  vec2 uv   = fragCoord;
  vec4 Cd   = rgba;
  //vec4 font_clr = texture(checkerboard, vec2(uv.x, uv.y));
  fragColor = vec4( Cd.x, Cd.y, Cd.z, Cd.a );
  //vec4 font_clr = vec4(1,1,1,1);
  //fragColor     = font_clr;
}

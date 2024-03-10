#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;

void main()
{
  vec4 Cd = rgba;
  fragColor = vec4( Cd.x, Cd.y, Cd.z, Cd.a );
  //fragColor = vec4( 1.0f, 0.0f, 0.0f, Cd.a );
}

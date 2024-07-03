#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

// Constants

#define iTime (u_time/1.0)

float udSegment( in vec2 p, in vec2 a, in vec2 b )
{
  vec2 ba = b-a;
  vec2 pa = p-a;
  float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  return length(pa-h*ba);
}

vec2  iResolution = vec2(1280, 720);
float aspectRatio = iResolution.x / iResolution.y;
vec2 pxCoord = fragCoord;

void main()
{
  vec2 ar = vec2(aspectRatio, 1.0);
  vec2 p  = pxCoord * ar;

  vec2 v1  = ( vec2(0.25 + 0.1*sin(iTime),0.5 + 0.1*cos(iTime))*ar );
  vec2 v2  = ( vec2(0.75,0.5)*ar );
    
  float d = udSegment( p, v1, v2 );
    
  vec3 col = vec3(1.0,1.0,1.0);
  col     *= sin(d*100);
    
  fragColor = vec4(col,1.0);
}

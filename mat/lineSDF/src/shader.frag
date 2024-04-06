#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

float udSegment( in vec2 p, in vec2 a, in vec2 b )
{
  vec2 ba = b-a;
  vec2 pa = p-a;
  float h =clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  return length(pa-h*ba);
}

void main()
{
  vec2 p = (2.0*fragCoord-iResolution.xy)/iResolution.y;
  vec2 m = (2.0*iMouse.xy-iResolution.xy)/iResolution.y;
  p *= 1.4;
  m *= 1.4;
    
  vec2 v1 = cos( iTime*0.5 + vec2(0.0,1.00) + 0.0 );
  vec2 v2 = cos( iTime*0.5 + vec2(0.0,3.00) + 1.5 );
  float th = 0.3*(0.5+0.5*cos(iTime*1.1+1.0));
    
  float d = udSegment( p, v1, v2 ) - th;
    
  vec3 col = vec3(1.0) - sign(d)*vec3(0.1,0.4,0.7);
  col *= 1.0 - exp(-3.0*abs(d));
  col *= 0.8 + 0.2*cos(120.0*d);
  col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.015,abs(d)) );

  if( iMouse.z>0.001 )
    {
      d = udSegment(m, v1, v2) - th;
      col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, abs(length(p-m)-abs(d))-0.0025));
      col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, length(p-m)-0.015));
    }
    
  fragColor = vec4(col,1.0);
}

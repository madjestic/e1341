#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

#define EDGE   0.005
#define SMOOTH 0.0025

float SDFCircle( in vec2 coords, in vec2 offset )
{
  coords -= offset;
  float v = coords.x * coords.x + coords.y * coords.y - EDGE*EDGE;
  vec2  g = vec2(1.0 * coords.x, 1.0 * coords.y);
  return v/length(g);
}

float det(vec2 a, vec2 b) { return a.x*b.y-b.x*a.y; }

vec2 get_distance_vector(vec2 b0, vec2 b1, vec2 b2) {
  float a=det(b0,b2), b=2.0*det(b1,b0), d=2.0*det(b2,b1); 
  float f=b*d-a*a;
  vec2 d21=b2-b1, d10=b1-b0, d20=b2-b0;
  vec2 gf=2.0*(b*d21+d*d10+a*d20);
  gf=vec2(gf.y,-gf.x);
  vec2 pp=-f*gf/dot(gf,gf);
  vec2 d0p=b0-pp;
  float ap=det(d0p,d20), bp=2.0*det(d10,d0p);
  // (note that 2*ap+bp+dp=2*a+b+d=4*area(b0,b1,b2))
  float t=clamp((ap+bp)/(2.0*a+b+d), 0.0, 1.0);
  return mix(mix(b0,b1,t),mix(b1,b2,t),t);
}

float approx_distance(vec2 p, vec2 b0, vec2 b1, vec2 b2) {
  return length(get_distance_vector(b0-p, b1-p, b2-p));
}

//-----------------------------------------------------------------------------
void main()
{	
  //float aspectRatio = iResolution.x / iResolution.y;
  vec2 iResolution  = vec2(1280, 720);
  float aspectRatio = iResolution.x / iResolution.y;
  vec2 pxCoord = fragCoord;
  pxCoord.x *= aspectRatio.x;
  vec2 iMouse = vec2(sin(u_time*1)*1000 + 1000,0);
  vec2 pedfrcent = pxCoord - vec2(0.25,0.5);

  vec2 mouse = (iMouse.xy / iResolution.xy) - vec2(0.25,0.5);
  mouse.x *= aspectRatio;
  vec2 A = vec2(0.0,0.0);
  vec2 B = length(iMouse.xy) > 0.0 ? mouse : vec2(-0.3,0.2);
  vec2 C = vec2(1.0,0.0);

  vec3 color = vec3(1.0,1.0,1.0);
  float dist = SDFCircle(pedfrcent, A);
  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(1.0,0.0,0.0),vec3(1.0,1.0,1.0),dist);
    }
    
  dist = SDFCircle(pedfrcent, B);
  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(0.0,1.0,0.0),vec3(1.0,1.0,1.0),dist);
    }    
    
  dist = SDFCircle(pedfrcent, C);
  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(0.0,0.0,1.0),vec3(1.0,1.0,1.0),dist);
    }    

  dist = approx_distance(pedfrcent, A, B, C);
  if (dist < EDGE + SMOOTH)
    {
      dist = smoothstep(EDGE - SMOOTH,EDGE + SMOOTH,dist);
      color *= vec3(dist);
    }
       
  fragColor = vec4(color,1.0);
}

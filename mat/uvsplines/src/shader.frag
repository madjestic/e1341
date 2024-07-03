#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

#define EDGE   0.05
#define SMOOTH 0.01
#define Z {0,0,1};

float det(vec2 a, vec2 b) { return a.x*b.y-b.x*a.y; }

// Bezier Interpolation Factor based on parameterized coords
vec3 bifuv(vec3 Cd) {

  vec3 p0 = vec3(0.0f,0.5f,0); 
  vec3 p1 = vec3(0.5f,0.5f,0) + vec3(0,Cd.r*1.2,0); 
  vec3 p2 = vec3(1.0f,0.5f,0);

  vec3 offsetP = vec3(fragCoord, 0);

  vec3 p0p = p0 - offsetP;
  vec3 p1p = p1 - offsetP;
  vec3 p2p = p2 - offsetP;

  vec3 z0 = Z;

  float a = determinant(mat3(p0p, p2p, z0));
  float b = determinant(mat3(p1p, p0p, z0));
  float d = determinant(mat3(p2p, p1p, z0));
  
  vec3 gf = 4*b*(p2p-p1p) + 4*d*(p1p-p0p) + 2*a*(p2p-p0p);
  gf      = vec3(gf.y, -gf.x, gf.z);
  vec3 pp = ((gf*(a*a - 4*b*d))/dot(gf,gf));

  float t0 = (determinant(mat3(p0p-p2p, p0p-pp,z0))+(2*determinant(mat3(p0p-pp,p0p-p1p,z0))))/(2*(a+b+d));
  float t  = clamp (t0, 0, 1);
  vec3  m  = mix(mix(p0p,p1p,t),mix(p1p,p2p,t),t);

  return m;
}

//-----------------------------------------------------------------------------
void main()
{	
  vec3  Cd = rgba.xyz;
  float dist;

  dist = length(bifuv(Cd));
  float x = 1.0f - dist;
  //x = hermite(x);
  x = smoothstep (0.99,1.0,x);
  
  Cd        = vec3(x);
  fragColor = vec4(Cd,x);
}

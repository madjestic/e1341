#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

#define EDGE   0.01
#define SMOOTH 0.0025
#define Z {0,0,1};

float SDFCircle( in vec2 coords, in vec2 offsetP )
{
  coords -= offsetP;
  float v = coords.x * coords.x + coords.y * coords.y - EDGE*EDGE;
  vec2  g = vec2(1.0 * coords.x, 1.0 * coords.y);
  return v/length(g);
}

float det(vec2 a, vec2 b) { return a.x*b.y-b.x*a.y; }

// Bezier Interpolation Factor
vec3 bif( in vec3 offsetP
	, in vec3 p0
	, in vec3 p1
	, in vec3 p2 ) {
  vec3 p0p = p0 - offsetP;
  vec3 p1p = p1 - offsetP;
  vec3 p2p = p2 - offsetP;

  vec3 z0 = Z;

  float a = determinant(mat3(p0p, p2p, z0 ));
  float b = determinant(mat3(p1p, p0p, z0 ));
  float d = determinant(mat3(p2p, p1p, z0 ));
  
  vec3 gf = 4*b*(p2p-p1p) + 4*d*(p1p-p0p) + 2*a*(p2p-p0p);
  gf      = vec3(gf.y, -gf.x, gf.z);
  vec3 pp = ((gf*(a*a - 4*b*d))/dot(gf,gf));

  float t0 = (determinant(mat3(p0p-p2p, p0p-pp,z0))+(2*determinant(mat3(p0p-pp,p0p-p1p,z0))))/(2*(a+b+d));
  float t  = clamp (t0, 0, 1);
  vec3  m  = mix(mix(p0p,p1p,t),mix(p1p,p2p,t),t);

  return m;
}

float udSegment( in vec2 p, in vec2 a, in vec2 b )
{
  vec2 ba = b-a;
  vec2 pa = p-a;
  float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  return length(pa-h*ba);
}

//-----------------------------------------------------------------------------
void main()
{	
  //float aspectRatio = iResolution.x / iResolution.y;
  vec2 iResolution  = vec2(1280, 720);
  float aspectRatio = iResolution.x / iResolution.y;
  vec2 pxCoord = fragCoord;
  pxCoord.x *= aspectRatio.x;
  vec2 iMouse = vec2(0.5,0.5);//vec2(sin(u_time*1)*1000 + 1000,0);
  vec2 offsetP = pxCoord - vec2(0.35,0.5);

  vec2 A = vec2(-0.25,0.0);
  vec2 B = vec2(0.5,0.9);
  vec2 C = vec2(1.0,0.0);

  vec3 color = vec3(1.0,1.0,1.0);
  //float dist = 1.0f;
  float dist = SDFCircle(offsetP, A);
  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(1.0,0.0,0.0),vec3(1.0,1.0,1.0),dist);
    }
    
  dist = SDFCircle(offsetP, B);
  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(0.0,1.0,0.0),vec3(1.0,1.0,1.0),dist);
    }    
    
  dist = SDFCircle(offsetP, C);
  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(0.0,0.0,1.0),vec3(1.0,1.0,1.0),dist);
    }    

  dist = length(bif(vec3(offsetP, 0), vec3(A,0), vec3(B,0), vec3(C,0)));
    if (dist < EDGE + SMOOTH)
    {
      dist = smoothstep(EDGE - SMOOTH,EDGE + SMOOTH,dist);
      color *= vec3(dist);
    }

    
    fragColor = vec4(vec3(0.75f),0.5f-color.r);
}

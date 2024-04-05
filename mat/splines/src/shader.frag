#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

#define P0  0.25
#define P1  (sin(u_time*1.5) * 0.5)
#define P2  0.21
#define P3  -0.1
#define P4  0.2
#define P5  0.25
#define P6  -0.25
#define P7  0.0

#define EDGE   0.005
#define SMOOTH 0.0025

/* void main() */
/* { */
/*   vec2 uv       = fragCoord; */
/*   vec4 font_clr = texture(checkerboard, vec2(uv.x, uv.y)); */
/*   fragColor     = font_clr; */
/*   //fragColor     = vec4(0,0,0,0); */
/* } */

float N_i_1 (in float t, in float i)
{
  // return 1 if i < t < i+1, else return 0
  return step(i, t) * step(t,i+1.0);
}

float N_i_2 (in float t, in float i)
{
  return
    N_i_1(t, i)       * (t - i) +
    N_i_1(t, i + 1.0) * (i + 2.0 - t);
}

float N_i_3 (in float t, in float i)
{
  return
    N_i_2(t, i)       * (t - i) / 2.0 +
    N_i_2(t, i + 1.0) * (i + 3.0 - t) / 2.0;
}

float SplineValue(in float t)
{
  return
    P0 * N_i_3(t, 0.0) +
    P1 * N_i_3(t, 1.0) +
    P2 * N_i_3(t, 2.0) +
    P3 * N_i_3(t, 3.0) +
    P4 * N_i_3(t, 4.0) +
    P5 * N_i_3(t, 5.0) +
    P6 * N_i_3(t, 6.0) +
    P7 * N_i_3(t, 7.0);      
}

// F(x,y) = F(x) - y
float F ( in vec2 coords )
{
  // time in this curve goes from 0.0 to 10.0 but values
  // are only valid between 2.0 and 8.0
  float T = coords.x*6.0 + 2.0;
  return SplineValue(T) - coords.y;
}

// signed distance function for F(x,y)
float SDF( in vec2 coords )
{
  float v = F(coords);
  float slope = dFdx(v) / dFdx(coords.x);
  return abs(v)/length(vec2(slope, -1.0));
}

// signed distance function for Circle, for control points
float SDFCircle( in vec2 coords, in vec2 offset )
{
  coords -= offset;
  float v = coords.x * coords.x + coords.y * coords.y - EDGE*EDGE;
  vec2  g = vec2(1.0 * coords.x, 1.0 * coords.y);
  return v/length(g);
}

//-----------------------------------------------------------------------------
void main()
{	
  //float aspectRatio = iResolution.x / iResolution.y;
  vec2 iResolution  = vec2(1280, 720);
  float aspectRatio = iResolution.x / iResolution.y;
  vec2 pxCoord = fragCoord;
  pxCoord.x *= aspectRatio.x;

  float dist = 1.0 - SDFCircle(pxCoord, vec2(0.5*aspectRatio.x,0.5));
  vec3 color = vec3(1.0,1.0,1.0);

  if (dist < EDGE + SMOOTH)
    {
      dist = max(dist, 0.0);
      dist = smoothstep(EDGE,EDGE + SMOOTH,dist);
      color *= mix(vec3(0.0,0.0,1.0),vec3(1.0,1.0,1.0),dist);
    }

  color = vec3(dist,0.0,0.0);  
        
  dist = SDF(pxCoord + vec2(-0.25*aspectRatio.x, -0.5));
  if (dist < EDGE + SMOOTH)
    {
      dist = smoothstep(EDGE - SMOOTH, EDGE + SMOOTH, dist);
      color *= vec3(dist);
    }

  fragColor = vec4(color,1.0);
}

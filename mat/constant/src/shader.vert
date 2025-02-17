#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vRGBA;
layout(location = 2) in vec2 uvCoords;

uniform float u_time;
uniform mat4 camera;
uniform mat4 persp;
uniform mat4 xform;

// Output data ; will be interpolated for each fragment.
out vec4 rgba;
out vec2 fragCoord;

void main()
{
  mat4 cameraRot =
    mat4
    ( camera[0]
    , camera[1]
    , camera[2]
    , vec4(0,0,0,1));

  vec4 position = vec4(vPosition,1.0);	

  gl_Position
    = persp
    * cameraRot
    * xform
    * position;
	
  // Logarithmic correction.  The idea is to blend linear coordinates with
  // logarithmic, so that near object (up to a few thousand meters appear
  // in linear space and the rest is logarithmic
  // TODO: move it to shared lib.glsl code	
  // float x = length(gl_Position.xyz*0.1);
  // gl_Position.z = mix (f1(x, s1), f2(x, s2), mixF(x, far));
   

  // The color of each vertex will be interpolated
  // to produce the color of each fragment
  rgba      = vRGBA;
  fragCoord = uvCoords;
}

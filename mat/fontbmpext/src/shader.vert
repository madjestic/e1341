#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vRGBA;
layout(location = 2) in vec2 uvCoords;

uniform float u_time;
uniform mat4 camera;
uniform mat4 persp;
uniform mat4 xform0;
uniform mat4 xform;

// Output data ; will be interpolated for each fragment.
out vec4 rgba;
out vec2 fragCoord;
//out float u_time;

void main()
{
  mat4 xformOffset0 =
    mat4 ( vec4(xform0[0])
	 , vec4(xform0[1])
	 , vec4(xform0[2])
	   , vec4(xform0[3].x,xform0[3].y,-0.0125,1) //-0.012
	 );

  vec4 position = vec4(vPosition + vec3(0,0,1.0),1.0);	

  gl_Position
    = persp
    * xformOffset0
    * position;
	
  rgba      = vRGBA;
  fragCoord = uvCoords;
}

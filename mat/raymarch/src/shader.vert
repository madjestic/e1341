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
//out float u_time;

mat4 rotateY(float angle) {
	float angleS = 0.01f * angle;
    float c = cos(angleS);
	float s = sin(angleS);

    return mat4(
        c, 0, -s, 0,
        0, 1,  0, 0,
        s, 0,  c, 0,
		0, 0,  0, 1
    );
}

mat4 xformID =
	mat4(
		vec4(1,0,0,0),
		vec4(0,1,0,0),
		vec4(0,0,1,0),
		vec4(0,0,0,1)); 

void main()
{

	mat4 cameraID = 
		mat4 ( vec4 (1,0,0,0)
		     , vec4 (0,1,0,0)
		     , vec4 (0,0,1,0)
			 , vec4 (0,0,(camera[3][2]),1)
			);

	mat4 xformID = 
		mat4 ( vec4 (1,0,0,0)
		     , vec4 (0,1,0,0)
		     , vec4 (0,0,1,0)
			 , vec4 (0,0,xform[3][2],1)
			);

	vec4 position = vec4(vPosition,1.0);	

	gl_Position
		= persp
		* xformID
		* position;
	
	rgba      = vRGBA;
	fragCoord = uvCoords;
}

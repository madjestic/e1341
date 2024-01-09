#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;
uniform mat4 camera;
uniform mat4 xform;
uniform vec2 u_resolution;
uniform mat4 persp;

// params:
// p: arbitrary point in 3D space
// c: the center of our sphere
// r: the radius of our sphere

const int MAX_MARCHING_STEPS = 255;
const float MIN_DIST = 0.0;
const float MAX_DIST = 100.0;
const float EPSILON = 0.0001;

float intersectSDF(float distA, float distB) {
    return max(distA, distB);
}

float unionSDF(float distA, float distB) {
    return min(distA, distB);
}

float differenceSDF(float distA, float distB) {
    return max(distA, -distB);
}

float sphereSDF(vec3 samplePoint)
{
    return length(samplePoint) - 1.0;
}

float cubeSDF(vec3 p) {
    // If d.x < 0, then -1 < p.x < 1, and same logic applies to p.y, p.z
    // So if all components of d are negative, then p is inside the unit cube
    vec3 d = abs(p) - vec3(1.0, 1.0, 1.0);
    
    // Assuming p is inside the cube, how far is it from the surface?
    // Result will be negative or zero.
    float insideDistance = min(max(d.x, max(d.y, d.z)), 0.0);
    
    // Assuming p is outside the cube, how far is it from the surface?
    // Result will be positive or zero.
    float outsideDistance = length(max(d, 0.0));
    
    return insideDistance + outsideDistance;
}

float sceneSDF(vec3 samplePoint)
{
    //return sphereSDF(samplePoint);
	//return cubeSDF(samplePoint);
   float sphereDist = sphereSDF(samplePoint / 1.2) * 1.2;
   float cubeDist = cubeSDF(samplePoint);
   return intersectSDF(cubeDist, sphereDist);
}	


float shortestDistanceToSurface(vec3 eye, vec3 marchingDirection, float start, float end)
{
    float depth = start;
    for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
        float dist = sceneSDF(eye + depth * marchingDirection);
        if (dist < EPSILON) {
			return depth;
        }
        depth += dist;
        if (depth >= end) {
            return end;
        }
    }
    return end;
}

// vec2 normalize2D(vec2 v)
// {
// 	return vec2(v.x/(v.x*2))
// }

vec3 rayDirection(float fieldOfView, vec2 size, vec2 fragCoord)
{
	fragCoord.y = 1.0 - fragCoord.y; // flip v (uv-coords)
	fragCoord.x = (fragCoord.x-0.5f)/(size.y/size.x) + 0.5f;
    vec2 xy = fragCoord - 0.5;
    float z = (size.y / size.x) / tan(radians(fieldOfView) / 2.0);
	return normalize(vec3(xy,-z));
	
}

vec3 estimateNormal(vec3 p)
{
    return normalize(vec3(
        sceneSDF(vec3(p.x + EPSILON, p.y, p.z)) - sceneSDF(vec3(p.x - EPSILON, p.y, p.z)),
        sceneSDF(vec3(p.x, p.y + EPSILON, p.z)) - sceneSDF(vec3(p.x, p.y - EPSILON, p.z)),
        sceneSDF(vec3(p.x, p.y, p.z  + EPSILON)) - sceneSDF(vec3(p.x, p.y, p.z - EPSILON))
    ));
}

vec3 phongContribForLight(vec3 k_d, vec3 k_s, float alpha, vec3 p, vec3 eye,
                          vec3 lightPos, vec3 lightIntensity)
{
    vec3 N = estimateNormal(p);
    vec3 L = normalize(lightPos - p);
    vec3 V = normalize(eye - p);
    vec3 R = normalize(reflect(-L, N));
    
    float dotLN = dot(L, N);
    float dotRV = dot(R, V);
    
    if (dotLN < 0.0) {
        // Light not visible from this point on the surface
        return vec3(0.0, 0.0, 0.0);
    } 
    
    if (dotRV < 0.0) {
        // Light reflection in opposite direction as viewer, apply only diffuse
        // component
        return lightIntensity * (k_d * dotLN);
    }
    return lightIntensity * (k_d * dotLN + k_s * pow(dotRV, alpha));
}

vec3 phongIllumination(vec3 k_a, vec3 k_d, vec3 k_s, float alpha, vec3 p, vec3 eye)
{
    const vec3 ambientLight = 0.5 * vec3(1.0, 1.0, 1.0);
    vec3 color = ambientLight * k_a;
    
    vec3 light1Pos = vec3(4.0 * sin(u_time),
                          2.0,
                          4.0 * cos(u_time));
    vec3 light1Intensity = vec3(0.4, 0.4, 0.4);
    
    color += phongContribForLight(k_d, k_s, alpha, p, eye,
                                  light1Pos,
                                  light1Intensity);
    
    vec3 light2Pos = vec3(2.0 * sin(0.37 * u_time),
                          2.0 * cos(0.37 * u_time),
                          2.0);
    vec3 light2Intensity = vec3(0.4, 0.4, 0.4);
    
    color += phongContribForLight(k_d, k_s, alpha, p, eye,
                                  light2Pos,
                                  light2Intensity);    
    return color;
}

mat4 viewMatrix(vec3 eye, vec3 center, vec3 up) {
    // Based on gluLookAt man page
    vec3 f = normalize(center - eye);
    vec3 s = normalize(cross(f, up));
    vec3 u = cross(s, f);
    return mat4(
        vec4(s, 0.0),
        vec4(u, 0.0),
        vec4(-f, 0.0),
        vec4(0.0, 0.0, 0.0, 1)
    );
}

void main()
{
  mat3 viewRot =
	  mat3( camera[0].xyz
		  , -camera[1].xyz
		  , camera[2].xyz );
	
  vec3 viewDir	  = rayDirection(45.0, u_resolution.xy, fragCoord);
  //viewDir *= inverse(viewRot);
  vec3 eye	  = (camera)[3].xyz;
  //vec3 eye = vec3(0,0,5);
  //eye	*= inverse(viewRot);
  //mat4 viewToWorld = viewMatrix(eye, vec3(0.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0));
  mat4 viewToWorld = viewMatrix(eye, xform[3].xyz, vec3(0.0, 1.0, 0.0));
  //vec3 worldDir = (viewToWorld * vec4(viewDir, 0.0)).xyz;
  vec3 worldDir = (inverse(camera) * vec4(viewDir, 0.0)).xyz;
  
  float dist = shortestDistanceToSurface(eye, worldDir, MIN_DIST, MAX_DIST);

  if (dist > MAX_DIST - EPSILON) {
	   // Didn't hit anything
	   fragColor = vec4(0.0, 0.0, 0.0, 0.0);
	  return;
  }
  
  //The closest point on the surface to the eyepoint along the view ray
  vec3 p = eye + dist * worldDir;
  
  vec3 K_a = vec3(0.2, 0.2, 0.2);
  vec3 K_d = vec3(0.7, 0.2, 0.2);
  vec3 K_s = vec3(1.0, 1.0, 1.0);
  float shininess = 10.0;
  
  vec3 color = phongIllumination(K_a, K_d, K_s, shininess, p, eye);
  
  fragColor = vec4(color, 1.0);	 
  
  //fragColor = vec4(camera[2].xyz, 1.0);
  
  //vec2 uv       = fragCoord;
  //vec4 font_clr = texture(checkerboard, vec2(uv.x, uv.y));
  //fragColor     = font_clr;
  //fragColor = vec4( 1.0f, 0.0f, 0.0f, 1.0f );
	
  // fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
  // fragColor = vec4( 0.0, Cd.y, Cd.z, A );
  // fragColor = vec4( shaded_color, 1.0 );
  // fragColor = vec4( ro, 1.0 );
	
}

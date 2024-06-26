#version 150

uniform float time;
uniform vec2 resolution;
uniform vec4 mouse;

const float PI = 3.14159265;
const float angle = 60.0;
const float fov = angle * 0.5 * PI / 180.0;
vec3  cPos = vec3(0.0, 0.0, 3.0);
const float sphereSize = 1.0;
const vec3 lightDir = vec3(-0.577, 0.577, 0.577);

float distanceFunc(vec3 p){
    return length(p) - sphereSize;
}

vec3 getNormal(vec3 p){
    float d = 0.0001;
    return normalize(vec3(
        distanceFunc(p + vec3(  d, 0.0, 0.0)) - distanceFunc(p + vec3( -d, 0.0, 0.0)),
        distanceFunc(p + vec3(0.0,   d, 0.0)) - distanceFunc(p + vec3(0.0,  -d, 0.0)),
        distanceFunc(p + vec3(0.0, 0.0,   d)) - distanceFunc(p + vec3(0.0, 0.0,  -d))
    ));
}

in VertexData
{
    vec4 v_position;
    vec3 v_normal;
    vec2 v_texcoord;
} inData;

out vec4 fragColor;

#define R           resolution
#define T           time
#define M           mouse

#define PI          3.14159265359

#define MAX_DIST    30.00
#define MIN_DIST    0.001

// Simple Raymarching Setup
mat2 rot(float a) { return mat2(cos(a),sin(a),-sin(a),cos(a)); }

float box(vec3 p, vec3 b) {
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}
float sphere(vec3 p, float r) {
    return length(p) - r;
}

mat3 rotateX(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return mat3(
        1.0, 0.0, 0.0,
        0.0, c, -s,
        0.0, s, c
    );
}

mat3 rotateY(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return mat3(
        c, 0.0, s,
        0.0, 1.0, 0.0,
       -s, 0.0, c
    );
}

mat3 rotateZ(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    return mat3(
        c, -s, 0.0,
        s, c, 0.0,
        0.0, 0.0, 1.0
    );
}
// global vars
vec3 hit,hitPoint;

// map 3D scene

vec2 map(vec3 p){
    vec2 res = vec2(1e5,0.);
    // Rotate cube
// Calculate rotation angles
    float angleX = sin(time * 0.5) * PI * 0.5;
    float angleY = sin(time * 0.3) * PI * 0.5;
    float angleZ = sin(time * 0.4) * PI * 0.5;

    // Combine rotations
    mat3 rot = rotateX(angleX) * rotateY(angleY) * rotateZ(angleZ);
    vec3 rotatedP = rot * (p - vec3(0, 1., 0)); // Apply rotation

    // box
    float bx = box(rotatedP, vec3(1)) - .015;
    if(bx < res.x) {
        res = vec2(bx, 2.);
        hit = p;
    }
//    // box
//    float bx = box(p-vec3(0,1.,0),vec3(1))-.015;
//    // if the distance of bx is less than res.x
//    // we set res.x to bx
//    if(bx<res.x) {
//        res = vec2(bx,2.);
//        // track the object hit for texturing
//        hit = p;
//    }
    
    // sphere
    float a = 2.0;
    vec3 SphereCenterPos = vec3(a*sin(time),3,a*cos(time)); 
    float s = sphere(p-SphereCenterPos,1.0); // 球の中心を(0,1,0)に設定し、半径を1.0に
    if(s<res.x) {
        res = vec2(s,3.);
        hit = p;
    }
    
    
    // floor
    // 位置
    float fl = p.y+1.;
    if(fl<res.x) {
        res = vec2(fl,1.);
        hit = p;
    }
    return res;
}

// return the surface normal.
// a normal is the direction perpendicular to a surface.
vec3 normal(in vec3 p, in float t) {
    t *= MIN_DIST;
    vec2 eps = vec2(t, 0.0);
    vec3 n = vec3(
        map(p+eps.xyy).x - map(p-eps.xyy).x,
        map(p+eps.yxy).x - map(p-eps.yxy).x,
        map(p+eps.yyx).x - map(p-eps.yyx).x);
    return normalize(n);
}

void main()
{
    // Set Coordinates from -1 to 1
    vec2 F = gl_FragCoord.xy;
    vec2 uv = (2.* F.xy-R.xy)/max(R.x,R.y);

    // ray order and ray direction (camera/view)
    vec3 ro = vec3(0, 0, 8);
    vec3 rd = normalize(vec3(uv, -1.0));
    
    // mouse //
    float x = -.5;
    float y = -(M.x/R.x*2.-1.)*PI;
    
    // rotation of ray order & direction
    mat2 rx =rot(x);
    mat2 ry =rot(y);
    ro.zy*=rx;ro.xz*=ry;
    rd.zy*=rx;rd.xz*=ry;

    vec3 C = vec3(0);
    
    // variable to hold our material id
    float m = 0.;
    // variable to hold the distance
    float d = 0.;
    // vector for position
    vec3 p;
    
    // ray marching loop
    for(int i=0;i<100;i++)
    {
        // your location = ray order + ray direction * distance
        p = ro + rd * d;
        // march our map scene
        vec2 ray = map(p);
        // check to see if we're too close or way over and break
        if(abs(ray.x)<MIN_DIST*d||d>MAX_DIST)break;
        // set vars 
        d += ray.x;
        m  = ray.y;
    } 
  
    // if the distance is less than our max - draw the pixel
    if(d<MAX_DIST)
    {
        // global var trick
        // track the hit if we're drawing something
        hitPoint = hit;
      
        // get the normal vector for shading
        vec3 n = normal(p,d);
        // light position
        vec3 lpos =  vec3(5.,18,8.5);
        vec3 l = normalize(lpos);

        float diff = clamp(dot(n,l),0.,1.);
        
        // soft shadows
        float shdw = 1.0;
        float t=.025;
        for( int i =0; i<20; i++ ) {
            float h = map(p + l*t).x;
            if( h<MIN_DIST ) { shdw = 0.; break; }
            shdw = min(shdw, 18.*h/t);
            t += h;
            if( shdw<MIN_DIST || t>32. ) break;
        }
        diff = mix(diff,diff*shdw,.75);
        
        // specular lighting - verbose
        vec3 view = normalize(p - ro);
        vec3 ret = reflect(normalize(lpos), n);
        float spec =  0.75 * pow(max(dot(view, ret), 0.), 24.);

        // the var to store our object color based on ID
        vec3 h = vec3(.05);

        // assign color based on the material set in the map
        if (m==1.) {
            vec2 sv = fract(hitPoint.xz*.25)-.5;
            if(sv.x*sv.y>0.) h = vec3(0.1,0.1,0.1);
            else h = vec3(0.,0.,0.);
        }
        
        if(m==2.){
//          vec2 uv=hitPoint.xz*.5;
//          h = mix(vec3(1.,.7,.3),vec3(0),mod(floor(hitPoint.y*5.),2.));
            h = vec3(1.,1.,1.);
        }
        if(m==3.){
            h = vec3(1.0, 0.0, 0.0); // 球に赤色を割り当て
        }
      
        // shading = color * diffused + specular
        C = h * diff + spec;
    }
    // distance based fog
    C = mix(vec3(.05),C,exp(-.0015*d*d*d));
    
    // gamma correction - NEVER FORGET YOUR GAMMA!
    C = pow(C, vec3(.4545)); 
    // out screen
    fragColor = vec4(C,1.0);
    
    /////////////////////
    
    
}

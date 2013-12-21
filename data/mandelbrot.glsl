#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

#define PROCESSING_COLOR_SHADER

uniform vec2 topLeft;
uniform vec2 bottomRight;
uniform vec2 resolution;

vec4 hue1 = vec4(0, 0, 0, 1);
vec4 hue2 = vec4(0.1, 0.1, 0.8, 1);
vec4 hue3 = vec4(0.9, 0.1, 0.1, 1);
vec4 hue4 = vec4(0.9, 0.9, 0.1, 1);

void main(void) {
    vec2 p0 = mix(topLeft, bottomRight, gl_FragCoord.xy / resolution.xy);
    float x = 0;
    float y = 0;
    int iteration = 0;
    int max_iteration = 1000;
    while( x*x + y*y < 4 && iteration < max_iteration) {
        float xtemp = x*x - y*y + p0.x;
        y = 2*x*y + p0.y;
        x = xtemp;
        iteration = iteration + 1;
    }
    float zn = sqrt(x*x + y*y);
    float nu = log( log(zn) / log(2) ) / log(2);
    float amount = (iteration + 1 - nu) / max_iteration;
    vec4 color = mix(hue1, hue2, smoothstep(0, 1.0/3, amount));
    color = mix(color, hue3, smoothstep(1.0/3, 2.0/3, amount));
    color = mix(color, hue4, smoothstep(2.0/3, 3.0/3, amount));
    gl_FragColor = color;
}

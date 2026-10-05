//
//  ARShaders.metal
//  iFITS Start
//
//  GPU code for the AR / 3-D view: the cube (ray marching through every voxel the ray
//  crosses, no smoothing), the grid lines, and the camera picture in AR.
//

#include <metal_stdlib>
using namespace metal;

// Must match ARVolumeUniforms in ARVolumeRenderer.swift.
struct VolumeUniforms {
    float4x4 inverseMVP;   // clip space → box (model) space
    float4 boxMin;         // xyz: corner of the box
    float4 boxSize;        // xyz: size of the box, w: its longest side
    float4 params;         // x: value shown transparent, y: value shown most opaque, z: density
    uint4 dims;            // xyz: voxels on each axis, w: most voxels one ray can cross
};

struct FullscreenOut {
    float4 position [[position]];
    float2 ndc;
};

// One triangle that covers the screen.
vertex FullscreenOut arFullscreenVertex(uint vid [[vertex_id]]) {
    float2 uv = float2(float((vid << 1) & 2), float(vid & 2));
    FullscreenOut out;
    out.ndc = uv * 2.0 - 1.0;
    out.position = float4(out.ndc, 0.0, 1.0);
    return out;
}

// Follows the ray through the box one voxel at a time (3-D DDA), so every voxel it crosses is
// counted once, by the length of ray inside it. Each voxel shows its raw value: colour from the
// colormap, opacity rising with the value (low values clear, high values solid).
fragment float4 arVolumeFragment(FullscreenOut in [[stage_in]],
                                 constant VolumeUniforms &u [[buffer(0)]],
                                 texture3d<float, access::read> volume [[texture(0)]],
                                 texture2d<float, access::read> colormap [[texture(1)]]) {
    float4 nearH = u.inverseMVP * float4(in.ndc, 0.0, 1.0);
    float4 farH = u.inverseMVP * float4(in.ndc, 1.0, 1.0);
    float3 nearP = nearH.xyz / nearH.w;
    float3 farP = farH.xyz / farH.w;
    float3 dir = farP - nearP;
    float rayLength = length(dir);
    if (!(rayLength > 0.0)) { discard_fragment(); }
    dir /= rayLength;
    // No exact zeros, so the divisions below stay finite.
    dir = select(dir, copysign(float3(1e-7), dir), abs(dir) < 1e-7);

    // Where the ray enters and leaves the box.
    float3 boxMin = u.boxMin.xyz;
    float3 boxMax = u.boxMin.xyz + u.boxSize.xyz;
    float3 invDir = 1.0 / dir;
    float3 t0 = (boxMin - nearP) * invDir;
    float3 t1 = (boxMax - nearP) * invDir;
    float3 tSmall = min(t0, t1);
    float3 tLarge = max(t0, t1);
    float tEnter = max(max(tSmall.x, tSmall.y), max(tSmall.z, 0.0));
    float tExit = min(min(tLarge.x, tLarge.y), min(tLarge.z, rayLength));
    if (!(tExit > tEnter)) { discard_fragment(); }

    // The same ray in voxel units (0…dims on each axis).
    int3 dims = int3(u.dims.xyz);
    float3 voxelsPerUnit = float3(u.dims.xyz) / u.boxSize.xyz;
    float3 origin = (nearP - boxMin) * voxelsPerUnit;
    float3 d = dir * voxelsPerUnit;
    float3 entry = origin + d * tEnter;
    int3 cell = clamp(int3(floor(entry)), int3(0), dims - 1);
    int3 stepDir = int3(sign(d));
    float3 invD = 1.0 / d;
    float3 nextBoundary = float3(cell) + select(float3(0.0), float3(1.0), stepDir > 0);
    float3 tMax = (nextBoundary - origin) * invD;      // ray parameter at the next voxel wall
    float3 tDelta = abs(invD);                          // ray parameter across one voxel

    float lo = u.params.x;
    float invRange = 1.0 / max(u.params.y - u.params.x, 1e-30);
    float density = u.params.z / max(u.boxSize.w, 1e-6);
    float4 sum = float4(0.0);
    float t = tEnter;

    for (uint i = 0; i < u.dims.w; ++i) {
        float tNext = min(min(tMax.x, tMax.y), min(tMax.z, tExit));
        float segment = max(tNext - t, 0.0);
        float value = volume.read(uint3(cell)).r;
        if (!isnan(value) && segment > 0.0) {
            float s = clamp((value - lo) * invRange, 0.0, 1.0);
            if (s > 0.0) {
                float alpha = 1.0 - exp(-density * s * s * segment);
                uint index = min(uint(s * 255.0 + 0.5), 255u);
                float3 color = colormap.read(uint2(index, 0)).rgb;
                sum.rgb += (1.0 - sum.a) * alpha * color;
                sum.a += (1.0 - sum.a) * alpha;
                if (sum.a > 0.995) { break; }
            }
        }
        if (tNext >= tExit) { break; }
        // Step into the neighbouring voxel through the nearest wall.
        if (tMax.x <= tMax.y && tMax.x <= tMax.z) {
            cell.x += stepDir.x;
            tMax.x += tDelta.x;
        } else if (tMax.y <= tMax.z) {
            cell.y += stepDir.y;
            tMax.y += tDelta.y;
        } else {
            cell.z += stepDir.z;
            tMax.z += tDelta.z;
        }
        if (any(cell < int3(0)) || any(cell >= dims)) { break; }
        t = tNext;
    }
    return sum;   // premultiplied alpha
}

// MARK: Grid lines

struct LineVertex {
    float4 position;   // xyz in box (model) space
    float4 color;
};

struct LineOut {
    float4 position [[position]];
    float4 color;
};

vertex LineOut arLineVertex(uint vid [[vertex_id]],
                            constant LineVertex *vertices [[buffer(0)]],
                            constant float4x4 &mvp [[buffer(1)]]) {
    LineOut out;
    out.position = mvp * float4(vertices[vid].position.xyz, 1.0);
    out.color = vertices[vid].color;
    return out;
}

fragment float4 arLineFragment(LineOut in [[stage_in]]) {
    return float4(in.color.rgb * in.color.a, in.color.a);
}

// MARK: Camera picture (AR)

struct CameraVertex {
    float2 position;   // clip space
    float2 texCoord;   // camera image
};

struct CameraOut {
    float4 position [[position]];
    float2 texCoord;
};

vertex CameraOut arCameraVertex(uint vid [[vertex_id]],
                                constant CameraVertex *vertices [[buffer(0)]]) {
    CameraOut out;
    out.position = float4(vertices[vid].position, 0.0, 1.0);
    out.texCoord = vertices[vid].texCoord;
    return out;
}

// The camera delivers YCbCr (full range BT.601); this turns it into RGB.
fragment float4 arCameraFragment(CameraOut in [[stage_in]],
                                 texture2d<float, access::sample> yTexture [[texture(0)]],
                                 texture2d<float, access::sample> cbcrTexture [[texture(1)]]) {
    constexpr sampler s(mip_filter::linear, mag_filter::linear, min_filter::linear);
    const float4x4 ycbcrToRGB = float4x4(float4(+1.0000f, +1.0000f, +1.0000f, +0.0000f),
                                         float4(+0.0000f, -0.3441f, +1.7720f, +0.0000f),
                                         float4(+1.4020f, -0.7141f, +0.0000f, +0.0000f),
                                         float4(-0.7010f, +0.5291f, -0.8860f, +1.0000f));
    float4 ycbcr = float4(yTexture.sample(s, in.texCoord).r, cbcrTexture.sample(s, in.texCoord).rg, 1.0);
    return ycbcrToRGB * ycbcr;
}

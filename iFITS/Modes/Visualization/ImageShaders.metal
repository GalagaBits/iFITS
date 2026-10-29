//
//  ImageShaders.metal
//  iFITS Start
//
//  GPU code that draws the 2-D image (Visualization mode). The raw pixel values live on the GPU
//  in a texture; every time the screen is drawn, each screen pixel works out which image pixel it
//  shows and colours it with the colour table. Changing the clip range, scaling or colormap only
//  changes a few numbers (or the small colour table), never the big image texture.
//
//  Works with MetalImageRenderer.swift (which uploads the textures and fills ImageUniforms).
//

#include <metal_stdlib>
using namespace metal;

// Must match ImageUniforms in MetalImageRenderer.swift (same order, same sizes).
struct ImageUniforms {
    float2 viewSize;        // size of the view, in points
    float2 translation;     // where image pixel (0, 0) lands on screen, in points (tx, ty)
    float  pointsPerPixel;  // screen points covered by one image pixel (the zoom)
    float  contentScale;    // drawable pixels per point (2 or 3 on Retina screens)
    float  clipMin;         // value drawn with the first colour of the table
    float  tableScale;      // (table size - 1) / (clip max - clip min), or 0 if the range is empty
    float4 tileRect;        // this tile, in image pixels: x, y, width, height
    float4 screenRect;      // part of the screen this tile covers, in points: minX, minY, maxX, maxY
};

struct ImageVertexOut {
    float4 position [[position]];
};

// Four corners (a triangle strip) covering the tile's rectangle on screen.
vertex ImageVertexOut imageVertex(uint vid [[vertex_id]],
                                  constant ImageUniforms &u [[buffer(0)]]) {
    float2 corner = float2(float(vid & 1), float(vid >> 1));        // (0,0) (1,0) (0,1) (1,1)
    float2 p = mix(u.screenRect.xy, u.screenRect.zw, corner);        // points
    // Points → Metal's clip space (−1…1, y up).
    float2 ndc = float2(p.x / u.viewSize.x * 2.0 - 1.0, 1.0 - p.y / u.viewSize.y * 2.0);
    ImageVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    return out;
}

// Colours one screen pixel. Same steps as FITSRenderer.render on the CPU:
//   index = clamp((value − clipMin) × tableScale, 0, table size − 1), colour = table[index].
fragment float4 imageFragment(ImageVertexOut in [[stage_in]],
                              constant ImageUniforms &u [[buffer(0)]],
                              texture2d<float, access::read> values [[texture(0)]],
                              texture2d<float, access::read> table [[texture(1)]]) {
    // Screen pixel centre → screen point → image pixel (the inverse of imageToScreenTransform).
    float2 point = in.position.xy / u.contentScale;
    float2 imagePos = (point - u.translation) / u.pointsPerPixel;
    // Which pixel of this tile (whole pixels: nearest neighbour, so zoomed-in pixels stay square).
    float2 local = floor(imagePos - u.tileRect.xy);
    local = clamp(local, float2(0.0), u.tileRect.zw - 1.0);
    float v = values.read(uint2(local)).r;

    uint lastIndex = table.get_width() - 1;
    uint index;
    // NaN and ±Inf are found from the bits, because fast math (on for this project) lets the
    // compiler assume they never happen, so isnan() / isinf() can't be trusted.
    uint bits = as_type<uint>(v);
    if ((bits & 0x7f800000u) == 0x7f800000u) {
        if ((bits & 0x007fffffu) != 0u) {
            return float4(0.0);                 // NaN: transparent, so the black background shows
        }
        // +Inf → top of the table, −Inf → bottom (as on the CPU; an empty range gives the bottom).
        bool positive = (bits & 0x80000000u) == 0u;
        index = (positive && u.tableScale > 0.0) ? lastIndex : 0u;
    } else {
        float f = (v - u.clipMin) * u.tableScale;
        f = clamp(f, 0.0, float(lastIndex));
        index = uint(f);                        // rounds down, like Int(f) on the CPU
    }
    return table.read(uint2(index, 0));
}

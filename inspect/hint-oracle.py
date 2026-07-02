#!/usr/bin/env python3
"""Phase-0 hinting oracle: dump FreeType's HINTED outline points (the font's own
TrueType bytecode interpreter, classic v35 = full x+y grid-fit) at a given ppem.
Ground truth for scribe's hinting. Usage: hint-oracle.py <font.ttf> <ppem> <chars>
Emits JSON: {glyph, ppem, upem, adv, contours:[[[x,y,on],...],...]} with x,y in
26.6 fixed (px*64). "adv" is the hinted advance in 26.6."""
import sys, json, ctypes
import freetype
from freetype.raw import FT_Property_Set

def pin_interpreter_v35():
    v = ctypes.c_uint(35)
    try:
        FT_Property_Set(freetype.get_handle(), b"truetype", b"interpreter-version", ctypes.byref(v))
        return True
    except Exception as e:
        return f"could not pin v35: {e}"

def dump(font, ppem, chars):
    face = freetype.Face(font)
    face.set_pixel_sizes(ppem, ppem)
    # FT_LOAD_NO_AUTOHINT forces the font's native bytecode hinter; keep the outline
    flags = freetype.FT_LOAD_NO_AUTOHINT | freetype.FT_LOAD_NO_BITMAP
    out = []
    for ch in chars:
        gid = face.get_char_index(ord(ch))
        face.load_glyph(gid, flags)
        o = face.glyph.outline
        pts, tags, ends = o.points, o.tags, o.contours
        contours, start = [], 0
        for e in ends:
            c = [[pts[i][0], pts[i][1], (tags[i] & 1)] for i in range(start, e+1)]
            contours.append(c); start = e+1
        out.append({"glyph": ch, "gid": gid, "ppem": ppem, "upem": face.units_per_EM,
                    "adv": face.glyph.advance.x, "contours": contours})
    return out

if __name__ == "__main__":
    font, ppem, chars = sys.argv[1], int(sys.argv[2]), sys.argv[3]
    pinned = pin_interpreter_v35()
    print(json.dumps({"pinned_v35": pinned, "glyphs": dump(font, ppem, chars)}))

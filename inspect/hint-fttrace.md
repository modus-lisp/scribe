# FreeType interpreter introspection (per-instruction trace)

To diff weft's hinting execution against FreeType's *instruction by instruction*
(projection/freedom vectors, rp/zp, and every glyph-zone point), we build a
trace-enabled FreeType and patch its interpreter loop to dump graphics-state.

## Build (once)
```
cd ~/ftbuild/freetype-2.13.2
./configure --prefix=$HOME/ftbuild/install --without-zlib --without-png \
  --without-harfbuzz --without-brotli CFLAGS="-DFT_DEBUG_LEVEL_TRACE -g -O1"
make -j && make install
# point freetype-py at it:
cp $HOME/ftbuild/install/lib/libfreetype.so.6.20.1 \
   ~/.local/lib/python3.11/site-packages/freetype/libfreetype.so   # (.orig = release backup)
```
The patch (src/truetype/ttinterp.c, after the opcode-trace block) emits, gated on
env `WEFT_HINT_TRACE`, one `STATE` line per instruction:
`STATE pc= op= proj=(x,y) free=(x,y) rp=.. zp=.. loop= | i:(cur.x,cur.y)* ...`

## Capture FreeType's trace
```
WEFT_HINT_TRACE=1 FT2_DEBUG="ttinterp:7" \
  python3 inspect/hint-oracle.py <font> <ppem> "<char>" 2>ft.txt >/dev/null
grep '^STATE' ft.txt
```

## Capture weft's matching trace (same format)
```
WEFT_HINT_TRACE=1 sbcl --load inspect/hint-weftdump.lisp <font> <ppem> "<char>" 2>weft.txt
```
weft's `run-program` emits the identical `STATE` format when `*hint-trace*` is set.
Diff the two: the first `STATE` line that differs is the divergent instruction.

# Test-corpus font licenses

The fonts in this directory are vendored **only as test fixtures** for scribe's
differential gates. They are redistributable under their respective licenses,
reproduced/identified below. scribe itself (the code) is MIT — see the top-level
`LICENSE`.

## DejaVuSans.ttf, DejaVuSansMono.ttf
**DejaVu Fonts** — a permissive, Bitstream-Vera-derived license. Free to use,
embed, redistribute, and modify with attribution; the fonts may not be sold by
themselves. Copyright © Bitstream Inc.; DejaVu changes © the DejaVu authors.
Upstream: https://dejavu-fonts.github.io/  (full license: `LICENSES/DejaVu.txt`)

## C059-Roman.otf, NimbusSans-Regular.otf
**URW++ base35** (the "(URW)++ Core 35" set) — released by URW++ under the
**GNU AGPL v3 with a font-embedding exception** (the AGPL Font Exception permits
embedding and redistribution of the fonts, including inside other works, without
the AGPL's terms extending to those works). Copyright © URW++ Design and
Development GmbH. Upstream: https://github.com/ArtifexSoftware/urw-base35-fonts
(license + exception: `LICENSES/URW-base35.txt`).

## Not included: New York
Apple's **New York** is used in local development (variable-font / WOFF tests)
but is **not redistributable**, so it is gitignored and never committed. The
`tools/dmg-fonts.lisp` extractor reproduces it locally from Apple's public
download for those who have accepted Apple's license.

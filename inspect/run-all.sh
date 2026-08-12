#!/usr/bin/env sh
# Offline gate suite — table/cmap/shape vs vendored oracles (no Python needed).
set -e
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
# The tree is rooted at THIS checkout and inherits nothing: the gate must test
# the code in front of it, never a local-projects symlink to some other copy.
# Both dependencies live under deps/ as submodules for exactly that reason —
# brotli-pure (WOFF2) and cram (the WOFF1 zlib inflate).
export CL_SOURCE_REGISTRY="(:source-registry (:tree \"$here\") :ignore-inherited-configuration)"
exec sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "scribe"))' \
  --load "$here/inspect/run-all.lisp"
